import pandas as pd
from openai import OpenAI
import os
import time
import re

# Prerequieries
"""
ollama pull llama3:8b # Pull model
"""

# Import pathological reports
def import_reports(report_filename):
    """
    Imports pathological reports from an Excel file.

    Args:
        report_filename: Name of the Excel file containing the reports.

    Returns:
        report_df: Pandas dataframe containing the imported reports.
    """
    report_df = pd.read_excel(report_filename)
    report_df['Barcode'] = report_df['Barcode'].fillna(method='ffill')
    report_df.columns = ['ID', 'Report']
    return report_df


# Extract report features using OpenAI library
def extract_feature_gpt(report, query, model):
    """
    Extracts the desired information from a pathological report using OpenAI GPT.

    Args:
        report: Pathological report to be analyzed.
        query: String with a query indicating the features to be extracted.
        model: Specifiy the ollama model

    Returns:
        extracted_feature: Extracted feature from the report.
    """

    client = OpenAI(
        base_url='http://localhost:11434/v1',
        api_key='ollama',  # required, but unused
    )

    response = client.chat.completions.create(
        model=model,
        messages=[
            {"role": "system", "content": query},
            {"role": "user", "content": report},
        ]
    )

    extracted_feature = response.choices[0].message.content
    return extracted_feature


# Perform data aggregation for a dataframe with pathological reports
def data_aggregation(report_df, json_dir, query, res_path, stderr_out, model):
    """
    Performs data aggregation for a pandas data frame with pathological reports.

    Args:
        report_df: Pandas dataframe containing the pathological reports.
        json_dir: Directory with JSON data from zero-shot
        query: String with a query indicating the features to be extracted.
        model: Specifiy the ollama model

    Returns:
    """
    
    # Create a directory for the results
    if not os.path.exists(res_path):
        os.makedirs(res_path)

    # Loop through the reports keeping track of the reports for each ID
    id_counts = {}

    for i, row in report_df.iterrows():
        report_id = row['ID']
        
        print(report_id)

        # Check if the ID occurred before
        if report_id in id_counts:
            id_counts[report_id] += 1
        else:
            id_counts[report_id] = 1

        # Load zero-shot output
        zeroshot_file = json_dir + report_id + '_' + str(id_counts[report_id]) + '.json'
        try:
            with open(zeroshot_file, 'r') as f:
                zeroshot_data = f.read()
        except Exception as e:
                print(f"Failed to load {i}: {e}")

        # Remove time variable
        zeroshot_data = re.sub(r"\n\n# Time:.*", "", zeroshot_data)

        print(id_counts[report_id])

        # Check if file already exisits in output folder - if not use gpt
        file_name = res_path + report_id + '_' + str(id_counts[report_id]) + '.json'

        if not os.path.exists(file_name):

            # Prepare model input (Report + Zero-shot string)
            model_input = 'The report reads:\n' + \
                row['Report'] + '\nThe large language model output containing the JSON object reads:\n' + \
                zeroshot_data + '\nYour answer should be structured in JSON format again.'

            # Start time
            start_time = time.time()

            # ollama call
            feature_str = extract_feature_gpt(report = model_input, query = query, model = model)

            # End time and time difference.
            end_time = time.time()
            time_taken = round(end_time - start_time, 2)

            # Export JSON file
            try:
                output = feature_str + '\n\n# Time: ' + str(time_taken)
                with open(file_name, 'w') as file:
                    file.write(output)
                
            except IOError as e:
                with open(stderr_out, "a") as file:
                    file.write(f"Failed to write to file {file_name}. Error: {e}" + "\n")


# Main code
def main():
    # File names
    report_filename = 'report_data_example.xlsx'

    # Import data
    report_df = import_reports(report_filename)

    # Parameters for data aggregation
    query = """Given an original pathology report and a large language model output containing a JSON object with key information extracted from this report, please compare the extracted information against the details in the original report. Your task is to verify the accuracy of the extracted information and ensure its completeness based on the provided categories. Each category has specific response options that you must strictly follow. If any discrepancies or omissions are found, correct the JSON object accordingly. The categories and their response options are as follows:

    1.	WHO (ISUP) Grade Group: [GG1/GG2/GG3/GG4/GG5/Not mentioned]

    2.	T-Stage (TNM): [pT0/pT2a/pT2b/pT2c/pT3a/pT3b/pT4/Not mentioned]

    3.	N-Stage (TNM): [pNx/pN0/pN1/Not mentioned]

    4.	Number of Lymph Nodes examined: [Integer/Not mentioned]

    5.	Lymph Nodes with Metastasis: [Integer/Not mentioned]

    6.	Resection Margins: [R0/R1/Rx/Not mentioned]

    7.	Histologic Subtype: [acinar/ductal/mixed/Not mentioned]

    8.	Primary Gleason Pattern: [3/4/5/Not mentioned]

    9.	Secondary Gleason Pattern: [3/4/5/Not mentioned]

    10.	Tertiary Gleason Pattern: [3/4/5/Not mentioned]

    11.	Percentage of Secondary Gleason Pattern: [Integer/Not mentioned]

    Ensure that each field adheres to the specified options. If an answer option specifies 'Integer', ensure the response is provided as an integer value. If a category is not mentioned in the original report, mark it as "Not mentioned". Do not add or infer information that is not explicitly stated in the report.
    """
    
    # Zero-shot output directory
    input_dir = './model_4bit_zeroshot_json_output/'

    # Output path for results
    output_dir = './model_4bit_cov_json/'
    stderr_out = './model_4bit_cov_report_stderr.txt'

    # Perform data aggregation
    data_aggregation(report_df, input_dir, query, output_dir, stderr_out, model='model') # Select model


# Execute the main function
if __name__ == '__main__':
    main()

