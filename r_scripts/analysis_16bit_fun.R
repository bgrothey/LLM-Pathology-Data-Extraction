### Functions for the analysis of Large Language Model results ###
library(tidyverse)
library(readxl)
library(koRpus)
library(koRpus.lang.en)
library(koRpus.lang.de)
library(tm)
library(pheatmap)


# Invalid reports filter (excludes reports from analyses)
invalid_reports <- function() {
  
  invalid_ids <- c('...', '...')
  
  return(invalid_ids)
  
}


# Ambivalent Gleason/Grade Group
ambivalent_grade <- function() {
  
  tmp <- c('ID31',
           'ID32',
           'ID303',
           'ID220',
           'ID298')
  
  return(tmp)
  
}


# Grade Group not mentioned
na_grade_group <- function() {
  
  tmp <- c('ID54',
           'ID55',
           'ID75',
           'ID12',
           'ID22',
           'ID53',
           'ID264',
           'ID175')
  
  return(tmp)
  
}


# Retrieve IDs of Analyzed Cases
case_filter <- function(data_base = './PRAD_ground_truth.xlsx',
                        report_file = './report_files/PRAD_reports_eng.xlsx') {
  
  # Read PCA database
  pca_database <- read_excel(data_base) %>%
    select(ID) %>%
    unique()
  
  # Read PCA reports
  pca_reports <- read_excel(report_file) %>%
    select(ID) %>%
    unique() %>%
    na.omit()
  
  # Read Case IDs to be excluded
  case_rm <- invalid_reports()
  
  # Data Aggregation
  tmp <- merge(pca_database, pca_reports, by = 1); colnames(tmp) <- 'ID'
  out <- data.frame(ID = tmp[!tmp$ID %in% case_rm, ])
  
  return(out)
  
}


# Preprocess model results files
preprocess_results <- function(results_path) {
  #' @param results_path model results output file from report_analysis.py
  #' @return filtered results file
  #' @examples
  #' ... 
  
  tmp <- read.delim(results_path) %>%
    # Remove time variable
    filter(Key != 'time') %>% 
    # Remove invalid cases
    filter(ID %in% case_filter()$ID) %>% 
    # Assign Grade '-' to Grade Group with ambivalent Gleason/Grade Group
    mutate(Annotation_Value = 
             ifelse(ID %in% ambivalent_grade() & Key == 'WHO (ISUP) Grade Group',
                    '-', Annotation_Value)) %>%
    # Assign Grade 'Not mentioned' to cases with no Grade Group mentioned
    mutate(Annotation_Value = 
             ifelse(ID %in% na_grade_group() & Key == 'WHO (ISUP) Grade Group',
                    'Not mentioned', Annotation_Value)) %>%
    # Change annotation value of mixed (...) to mixed
    mutate(Annotation_Value = 
           ifelse(grepl('mixed', Annotation_Value), 'mixed', Annotation_Value)) %>%
    # Change annotation value nan to Not mentioned
    mutate(Annotation_Value = 
             ifelse(Annotation_Value == 'nan' | Annotation_Value == '<NA>',
                    'Not mentioned', Annotation_Value)) %>%
    # Update Match
    mutate(Match = ifelse(JSON_Value == Annotation_Value, 1, 0)) %>%
    # Assign '-' to Match if Annotation_Value '-'
    mutate(Match = ifelse(Annotation_Value == '-', '-', Match))
  
  return(tmp)
  
}


# Time quantification
time_quantification <- function(file_path) {
  
  tmp1 <- readLines(file_path)
  tmp2 <- tmp1[grepl('# Time: ', tmp1)]
  time <- as.numeric(gsub('# Time: ', '', tmp2))
  
  return(time)
  
}


# JSON Stats: Frequency Valid/Total JSON and Preprocessing/Valid JSON 
json_stats <- function(stderr_data) {
  
  tmp <- read.delim(stderr_data) %>%
    mutate(ID = str_remove(ID, "_.*")) %>%
    filter(!ID %in% invalid_reports()) # 579 valid reports in total (587 - invalid_reports)
  
  n_reports <- nrow(tmp)
  n_valid <- tmp %>%
    filter(Processing != 'Invalid JSON') %>%
    nrow()
  n_invalid <- tmp %>%
    filter(Processing == 'Invalid JSON') %>%
    nrow()
  n_preprocessing <- tmp %>%
    filter(str_detect(Processing, 'bracket')) %>%
    nrow()
  
  out <- data.frame(freq_valid_jsons = n_valid/n_reports,
                    freq_preprocessing = n_preprocessing/n_valid, n_valid = n_valid)
  
  return(out)
  
}


# Match invalid JSON files between two runs (Reference vs Retest)
match_invalid_json <- function(model_id, path_table) {
  
  stderr_path <- path_table[path_table$Model_ID == model_id, ]
  
  stderr_reference <- read.delim(stderr_path$Reference)
  colnames(stderr_reference)[2] <- 'reference'
  stderr_retest <- read.delim(stderr_path$Retest)
  colnames(stderr_retest)[2] <- 'retest'
  
  tmp <- merge(stderr_reference, stderr_retest, by = 'ID') %>%
    mutate(ID = str_remove(ID, "_.*")) %>%
    filter(!ID %in% invalid_reports()) # 579 valid reports in total (587 - invalid_reports)
  
  reference_invalid <- tmp %>%
    filter(reference == 'Invalid JSON') %>%
    nrow()
  
  reference_retest_invalid <- tmp %>%
    filter(reference == 'Invalid JSON' & retest == 'Invalid JSON') %>%
    nrow()
  
  out <- data.frame(freq_recurrent_invalid = reference_retest_invalid/reference_invalid)
  
  return(out)
  
}


# Retrieve Analyzed Features
get_analyzed_features <- function() {
  
  features <- data.frame(features = 
                           c('WHO (ISUP) Grade Group',
                             'T-Stage (TNM)',
                             'N-Stage (TNM)',
                             'Number of Lymph Nodes examined',
                             'Lymph Nodes with Metastasis',
                             'Resection Margins',
                             'Histologic Subtype',
                             'Primary Gleason Pattern',
                             'Secondary Gleason Pattern',
                             'Tertiary Gleason Pattern',
                             'Percentage of Secondary Gleason Pattern'))
  
  return(features)
  
}


# Function to process individual cases so that all requested features are included
## If a case is not stored in the results file all features will be counted as false
process_single_case <- function(case_id, file_id, llm_results, features) {
  
  tmp1 <- merge(llm_results, case_id, by = 1) %>%
    select(ID, Key, Match)
  tmp2 <- merge(tmp1, features, by.x = 'Key', by.y = 'features', all.y = TRUE) %>%
    mutate(Match = ifelse(is.na(Match), 0, Match))
  
  return(data.frame('File_ID' = file_id, 'Case_ID' = case_id, 'Key' = tmp2$Key, 'Match' = tmp2$Match))
  
}


# Frequency Repetitions and Additional Categories (report level)
frequency_repetitions_categories <- function(llm_results, model_id) {
  
  repetitions <- list()
  additional_categories <- list()
  for (i in unique(llm_results$ID)) {
    
    tmp <- llm_results[llm_results$ID == i, ]
    
    # Repetition (report level, requested and additional categories)
    repetitions[[i]] <- data.frame(ID = i, Repetition = ifelse(any(tmp$Counts > 1), TRUE, FALSE))
    
    # Additional categories (report level)
    additional_categories[[i]] <- data.frame(ID = i,
      add_categories = ifelse(nrow(tmp[!tmp$Key %in% get_analyzed_features()$features, ]) > 0, TRUE, FALSE))
    
  }

  # Frequency of repetitions
  repetitions_tmp <- do.call(rbind, repetitions)
  repetitions_stats <- data.frame(model_id = model_id,
    freq_repetition = sum(repetitions_tmp$Repetition) / nrow(repetitions_tmp))
  
  # Frequency of additional categories
  categories_tmp <- do.call(rbind, additional_categories)
  categories_stats <- data.frame(model_id = model_id,
    freq_additional_categories = sum(categories_tmp$add_categories) / nrow(categories_tmp))
  
  out <- list(repetitions = repetitions_stats, additional_categories = categories_stats)
  
  return(out)
  
}


# No answer given (case level)
no_answer_case_lvl <- function(file_names) {
  
  out <- list()
  for (i in file_names) {
    
    tmp1 <- read.delim(i) 
    tmp2 <- tmp1[!tmp1$ID %in% invalid_reports(), ] %>%
      merge(., get_analyzed_features(), by.x = 'Key', by.y = 'features')
    
    n_no_answer <- nrow(tmp2[tmp2$JSON_Value == 'No answer given', ])
    n_valid_reports <- nrow(tmp2)
    model_id <- gsub('.*/|_results.tsv', '', i)
    
    out[[i]] <- data.frame(Model_ID = model_id,
                           n_no_answer = n_no_answer,
                           n_valid_reports = n_valid_reports)
    
  }
  
  return(out)
  
}


# Function to read Excel file and fill NA values in the ID column with the previous non-NA value
import_reports <- function(report_filename) {
  
  report_df <- read_excel(report_filename) %>%
    arrange(ID, Sub_ID) %>%
    select(ID, Report)
  
  return(report_df)
  
}


# Function to calculate lexical richness measures using korpus
korpus_lexical_diversity <- function(text, language) {
  
  
  # Create a corpus from the text vector
  corpus <- Corpus(VectorSource(text))
  
  # Apply cleaning steps
  removeURL <- function(x) gsub("http[s]?://\\S+", "", x)
  corpus <- tm_map(corpus, content_transformer(removeURL))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords(language))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Convert the cleaned corpus back into a plain text string
  cleanedText <- sapply(corpus, as.character)
  
  # Tokenize the text using koRpus
  tokenized.obj <- tokenize(txt = cleanedText, format = 'obj', lang = language)
  
  ttr <- TTR(tokenized.obj, force.lang = language)
  mtld <- MTLD(tokenized.obj, force.lang = language)
  n_token <- nrow(tokenized.obj@tokens)
  
  return(data.frame(TTR = ttr@TTR, MTLD = mtld@MTLD$MTLD, Tokens = n_token, row.names = NULL))
  
} 


# Function to loop through a report data frame and calculate TTR
report_ld <- function(file, language) {
  
  input_df <- import_reports(file) %>%
    filter(Report != '-')
  out <- list()
  for (i in 1:nrow(input_df)) {
    
    tmp <- korpus_lexical_diversity(
      as.character(input_df[i, 'Report']),
      language = language)
    out[[i]] <- data.frame(ID = input_df[i, 'ID'], tmp)
    
  }
  
  results <- do.call(rbind, out)
  
} 


# Function to process individual cases so that all requested features are included
  # - Update from process_single_case() for the hallucination analysis
  # - Uses JSON_Value instead of Match for further analysis
process_single_case_hallucination <- function(case_id, file_id, llm_results, features) {
  
  tmp1 <- llm_results %>%
    filter(Report == case_id) %>%
    select(Report, Key, Value)
  tmp2 <- merge(tmp1, features, by.x = 'Key', by.y = 'features', all.y = TRUE)
  
  return(data.frame('File_ID' = file_id, 'Case_ID' = case_id, 'Key' = tmp2$Key, 'JSON_Value' = tmp2$Value))
  
}


# Heatmap
costum_heatmap <- function(mat, breaks = seq(-1, 1, by=0.01),
                        gradient_colors = c("#0073C2FF", "white", "#CD534CFF"),
                        row_annotation = NULL, annotation_colors = NULL,
                        show_colnames = TRUE, show_rownames = TRUE,
                        display_numbers = TRUE, cluster_cols = TRUE,
                        cluster_rows = TRUE, legend = TRUE, fontsize = 10,
                        fontsize_number = 10, round_decimal = 1) {
  
  #' @param mat correlation matrix, IDs stored in column and row names
  #' @param breaks numerical vector which stores the values for the color gradient
  #' @param gradient_colors character vector with color names for the gradient
  #' @param row_annotation data frame with row annotations - rownames must match mat input
  #' @return Correlation Heatmap
  #' @examples
  #' gradient_colors = c("navy", "white", "red")
  #' gradient_colors = c("#0073C2FF", "white", "#EFC000FF")
  
  
  if(is.data.frame(row_annotation) & is.list(annotation_colors)) {
    tmp <- pheatmap(mat,
                    clustering_distance_rows = "euclidean",
                    clustering_distance_cols = "euclidean",
                    clustering_method = "complete",
                    cluster_rows = cluster_rows,
                    cluster_cols = cluster_cols,
                    color = colorRampPalette(gradient_colors)(length(breaks)),
                    breaks = breaks,
                    border_color = 'lightgrey',
                    fontsize = fontsize,
                    annotation_row = row_annotation,
                    annotation_colors = annotation_colors,
                    show_colnames = show_colnames,
                    show_rownames = show_rownames,
                    display_numbers = round(mat, round_decimal),
                    fontsize_number = fontsize_number,
                    legend = legend,
                    angle_col = 45)
  } else {
    tmp <- pheatmap(mat,
                    clustering_distance_rows = "euclidean",
                    clustering_distance_cols = "euclidean",
                    clustering_method = "complete",
                    cluster_rows = cluster_rows,
                    cluster_cols = cluster_cols,
                    color = colorRampPalette(gradient_colors)(length(breaks)),
                    breaks = breaks,
                    border_color = 'lightgrey',
                    fontsize = fontsize,
                    show_colnames = show_colnames,
                    show_rownames = show_rownames,
                    display_numbers = round(mat, round_decimal),
                    fontsize_number = fontsize_number,
                    legend = legend,
                    angle_col = 45)
  }
  
  
  return(tmp)
  
}


# GGPlot Standard Parameters
ggplot_std_theme <- function() {
  
  #' @param
  #' @return Parameters for a standard ggplot theme
  #' @examples
  #' ... 
  
  std_theme <- theme(text = element_text(size=12),
                     panel.grid.minor=element_blank(),
                     panel.grid.major=element_blank(),
                     panel.background=element_blank(),
                     panel.border=element_blank(),
                     axis.line = element_line(linewidth = 1),
                     axis.ticks = element_line(linewidth = 1),
                     axis.ticks.length = unit(0.15, "cm"),
                     legend.position = "none")
  
  return(std_theme)
  
}  




