import pandas as pd
import json
import numpy as np
import os
import re
from collections import Counter

# Read JSON files
def combine_json_files(directory_path, stats_path, json_validation_path):
    """Read JSON files with raw results of the language model and combine them to one nested JSON file.
    Args:
        directory_path: path to the stored json files
        stats_path: path to a file which stores the variable occurrences
        json_validation_path: path to a file whicht stores the json validation status

    Returns:
        combined_data: Nested JSON file
    """

    # Stats: Function to count frequency of extracted variables
    def stats_var_freq(feature_str, stats_path):

        # Counting frequency of extracted variables
        lines = feature_str.strip().split('\n') # Splitting the string into lines and cleaning it
        keys = [line.split(':')[0].strip().strip('"') for line in lines if ':' in line] # Extracting keys
        key_counts = Counter(keys)
        key_df = pd.DataFrame(list(key_counts.items()), columns=['Key', 'Counts']) # Prepare data frame
        key_df.insert(0, 'ID', filename)

        # Check if the file already exists
        key_df.to_csv(stats_path, sep='\t', mode='a', index=False, header=not os.path.exists(stats_path))

    # Function to merge data into master dictionary
    def merge_master_dictinary(master_dict, json_data, filename):
        filename_tmp = re.sub('.json', '', filename)
        report_id = re.sub(r'_.*$', '', filename_tmp)
        version_id = re.sub(r'(?s)^.*?_', '', filename_tmp)
        master_dict.setdefault(report_id, {})[version_id] = json_data 

    combined_data = {}
    # Loop through all files in the directory
    for filename in os.listdir(directory_path):

        # Ensuring only JSON files are processed
        if filename.endswith(".json"):

            # Load data
            with open(os.path.join(directory_path, filename), 'r') as f:
                data = f.read()

            # Extract time value
            time_value = re.search(r'# Time:\s*([\d\.]+)', data)
            time_value = float(time_value.group(1))

            # Remove time string
            feature_str = re.sub(r"\n\n# Time:.*", "", data)

            # Try loading JSON directly
            try:

                json_data = json.loads(feature_str)

                # Add time variable
                json_data['time'] = time_value

                # Data Aggregation
                merge_master_dictinary(combined_data, json_data, filename)

                # Store variable frequencies
                stats_var_freq(feature_str, stats_path)

                # Save JSON validation status
                json_validation_out = pd.DataFrame({'ID': [filename], 'Processing': ['No preprocessing']})
                json_validation_out.to_csv(json_validation_path, sep='\t', mode='a',
                                           index=False, header=not os.path.exists(json_validation_path))

            # Try load JSON with cleaned data
            except json.JSONDecodeError as e:  
                
                # 1. Clean up: Remove first/last occurence of { and }
                try:

                    feature_str_clean = re.sub(r'^.*?\{', "{", feature_str, flags=re.DOTALL)
                    feature_str_clean = re.sub(r'\}[^}]*$', '}', feature_str_clean, flags=re.DOTALL)

                    # Convert data to json
                    json_data = json.loads(feature_str_clean)

                    # Add time variable
                    json_data['time'] = time_value

                    # Data Aggregation
                    merge_master_dictinary(combined_data, json_data, filename)

                    # Store variable frequencies
                    stats_var_freq(feature_str_clean, stats_path)

                    # Save JSON validation status
                    json_validation_out = pd.DataFrame({'ID': [filename], 'Processing': ['First-Last bracket']})
                    json_validation_out.to_csv(json_validation_path, sep='\t', mode='a',
                                            index=False, header=not os.path.exists(json_validation_path))

                except json.JSONDecodeError as e:

                    # 2. Clean up: Remove last/first occurence of { and }
                    try:

                        feature_str_clean = re.sub(r".*\{", "{", feature_str, flags=re.DOTALL)
                        feature_str_clean = re.sub(r"\}.*", "}", feature_str_clean, flags=re.DOTALL)

                        # Convert data to json
                        json_data = json.loads(feature_str_clean)

                        # Add time variable
                        json_data['time'] = time_value

                        # Data Aggregation
                        merge_master_dictinary(combined_data, json_data, filename)

                        # Store variable frequencies
                        stats_var_freq(feature_str_clean, stats_path)

                        # Save JSON validation status
                        json_validation_out = pd.DataFrame({'ID': [filename], 'Processing': ['Last-First bracket']})
                        json_validation_out.to_csv(json_validation_path, sep='\t', mode='a',
                                                index=False, header=not os.path.exists(json_validation_path))

                    except json.JSONDecodeError as e:
                        json_validation_out = pd.DataFrame({'ID': [filename], 'Processing': ['Invalid JSON']})
                        json_validation_out.to_csv(json_validation_path, sep='\t', mode='a',
                                                   index=False, header=not os.path.exists(json_validation_path))

    return combined_data


# Concatenate report results from JSON file
def get_latest_data(case_data):
    """
    Extract the latest data from a case considering 'Not mentioned' values.
    """

    sorted_timepoints = sorted(case_data.keys(), reverse=True)

    # Initialize with the latest time point data
    latest_data = case_data[sorted_timepoints[0]].copy()

    for timepoint in sorted_timepoints[1:]:
        for key, value in case_data[timepoint].items():
            # If the key's value is 'Not mentioned' in the latest data, use the previous timepoint's value
            if key in latest_data: # Check if key already exists in json (only for markers mistakenly included by the model)
                if latest_data[key] == 'Not mentioned':
                    latest_data[key] = value
            else:
                latest_data[key] = value
    return latest_data


# Import case annotations
def import_annotations(annotation_filename):
    """
    Imports case annotations from an Excel file.

    Args:
        annotation_filename: Name of the Excel file containing the annotations.

    Returns:
        annotation_df: Pandas dataframe containing the imported annotations.
    """
    annotation_df = pd.read_excel(annotation_filename)
    annotation_df.columns.values[0] = 'ID'
    return annotation_df


# Preprocessing of the annotation data
def preprocess_annotation(file):
    """
    Specific preprocessing of the clinical annotation data to match the output of the language model. Here only for the basic parameters.
    """

    data = import_annotations(file)
    data.fillna('Not mentioned', inplace=True)

    # Split the LYM_POS column into LYM_POS (Number nodal metastasis) and LYM_TOTAL (Total number of lymph nodes)
    data[['LYM_POS', 'LYM_TOTAL']] = data['LYM_POS'].str.split('/', expand=True)

    # Split the GP_3 column to separate the Gleason Pattern and its proportion
    data[['GP_3', 'GP_3_PERC']] = data['GP_3'].str.split(',', expand=True)
    data['GP_3_PERC'] = data['GP_3_PERC'].str.replace('%', '').str.strip() # remove white space and % signs

    # Transform GP_2_PERC from fraction to percentage
    data['GP_2_PERC'] = data['GP_2_PERC'] * 100
    data['GP_2_PERC'] = np.floor(pd.to_numeric(data['GP_2_PERC'], errors='coerce')).astype('Int64')

    # Add WHO Grade Group column
    gp_1 = pd.to_numeric(data['GP_1'], errors='coerce')
    gp_2 = pd.to_numeric(data['GP_2'], errors='coerce') 

    conditions = [
        (gp_1.isna() | gp_2.isna()),
        (gp_1 + gp_2 <= 6),
        (gp_1 == 3) & (gp_2 == 4),
        (gp_1 == 4) & (gp_2 == 3),
        (gp_1 + gp_2 == 8),
        (gp_1 + gp_2 >= 9),
    ]

    choices = ['Kein Bericht', 'GG1', 'GG2', 'GG3', 'GG4', 'GG5']
    data['GRADE_GROUP'] = np.select(conditions, choices, default='Nan')

    # Subset the data frame for the relevant parameters
    out = data[['ID','pT','pN', 'LYM_POS', 'LYM_TOTAL', 'GRADE_GROUP', 'GP_1', 'GP_2', 'GP_2_PERC', 'GP_3', 'GP_3_PERC', 'R', 'SUBTYPE']]

    return out


# JSON Entities to Annotation Columns Matching
def json_to_annotation():
    """
    Returns a dictionary which matches the entities of the json file with the columns of the annotation data frame with the clinical informations.
    """

    annotation_dict = {
        'WHO (ISUP) Grade Group':'GRADE_GROUP',
        'T-Stage (TNM)':'pT',
        'N-Stage (TNM)':'pN',
        'Number of Lymph Nodes examined':'LYM_TOTAL',
        'Lymph Nodes with Metastasis':'LYM_POS',
        'Resection Margins':'R',
        'Histologic Subtype':'SUBTYPE',
        'Primary Gleason Pattern':'GP_1',
        'Secondary Gleason Pattern':'GP_2',
        'Tertiary Gleason Pattern':'GP_3',
        'Percentage of Secondary Gleason Pattern':'GP_2_PERC',
        'time':'Nan'
    }

    return annotation_dict


def merge_json_annotation(json_data, annotation_df, id_col_map):
    """
    Creates a data frame containing the results of the llm JSON and the attribute values of the annotation file (ground truth).

    Args:
        json_data: JSON file with llm results.
        annotation_df: Data frame with annotation data (preprocess_annotation() output)
        id_col_map: json_to_annotation() output

    Returns:
        Data frame with four columns (ID, Key, JSON_value, Annotation_value)
    """

    # Create an empty DataFrame to store the results
    result_df = pd.DataFrame(columns=['ID', 'Key', 'JSON_Value', 'Annotation_Value'])

    # Iterate over each patient ID in the JSON data
    for patient_id, patient_data in json_data.items():

        combined_keys = list(set(id_col_map.keys()).union(set(patient_data.keys())))

        # Iterate over each attribute of the patient in the JSON data
        for key in combined_keys:

            # Check if marker was requested in the prompt (id_col_map)
            if key in id_col_map.keys(): # marker was requested
                # Check if JSON key can be mapped to the annotation data frame column (e.g. time varible is not)
                if id_col_map[key] in annotation_df.columns:
                    annotation_value = annotation_df[annotation_df['ID'] == patient_id][id_col_map[key]].squeeze()
                else:
                    annotation_value = 'No annotation'
            else: # marker was not requested
                annotation_value = 'Marker not requested'

            # Check if model gave answer for the marker
            if key in patient_data.keys():              
                json_value = patient_data[key] # model gave an answer for the marker
            else:
                json_value = 'No answer given'  # model did not give an answer for the marker

            # Append the data to the result DataFrame
            result_dict = {
                'ID': patient_id,
                'Key': key,
                'JSON_Value': json_value,
                'Annotation_Value': annotation_value
                }
            
            result_df = pd.concat([result_df, pd.DataFrame([result_dict])], ignore_index=True)

    return result_df


# Analysis

# Read JSON File
json_raw = combine_json_files(
    './input_json_files/', stats_path='model_var_stats.tsv', json_validation_path='model_json_status.tsv')

# Get the latest updates for each report
json_processed = {}
for case, case_data in json_raw.items():
    json_processed[case] = get_latest_data(case_data)

# Read and Preprocess Annotation data
annotation_processed = preprocess_annotation('Database_example.xlsx')

# JSON keys to Annotation columns
key_mapping = json_to_annotation()

# Results
results = merge_json_annotation(json_processed, annotation_processed, key_mapping)
results = results.astype(str)
results['Match'] = np.where(results['JSON_Value'] == results['Annotation_Value'], '1', '0')

results.to_csv('model_results.tsv', sep='\t', index=False)


