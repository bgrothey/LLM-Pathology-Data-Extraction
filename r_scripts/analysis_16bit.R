### Analysis of Large Language Model results ###
source("./analysis_16bit_fun.R")
library(ggplot2)
library(ggridges)
library(webr)


# Data aggregation -----

# Model results
case_ids <- case_filter()
analyzed_features <- get_analyzed_features() # Requested Features of the prompt
input_files <- list.files('./model_results', full.names = TRUE)

out <- list()
for (i in input_files) {
  
  file_id <- gsub('./model_results/|_results.tsv', '', i) # adapt accordingly to folder/file name
  llm_results <- preprocess_results(i)
  
  out <- c(out, lapply(case_ids$ID, process_single_case, file_id = file_id,
                       llm_results = llm_results, features = analyzed_features))
  
}

results <- do.call(rbind, out)
results_wide <- results %>%
  pivot_wider(names_from = File_ID, values_from = Match)

write.table(results_wide, file = 'model_results_raw.tsv', row.names = FALSE,
            col.names = TRUE, sep = '\t', quote = FALSE)


# Figure 2 -----

# Fig. 2A: Global accuracy

  # Load model results
  results_wide <- read.delim('model_results_raw.tsv')

  # Process model results
  results_processed <- results_wide %>%
    filter(!if_any(c(gpt4_eng, gpt4_ger,
                     llama2_13b_eng, llama2_13b_ger, llama2_70b_eng, llama2_70b_ger,
                     llama3_8b_eng, llama3_8b_ger, llama3_70b_eng, llama3_70b_ger),
                   ~ grepl("-", .))) %>%
    mutate(across(c(gpt4_eng, gpt4_ger,
                    llama2_13b_eng, llama2_13b_ger, llama2_70b_eng, llama2_70b_ger,
                    llama3_8b_eng, llama3_8b_ger, llama3_70b_eng, llama3_70b_ger),
                  ~ as.numeric(.)))

  # Calculate mean accuracy for each model
  accuracy_global_level <- data.frame(Accuracy = apply(results_processed[,-c(1:2)], 2, mean)) %>% 
    rownames_to_column(., var = "Model") %>%
    mutate(Accuracy = round(Accuracy, digits = 2))
  
  # Doughnut plots
  for (i in accuracy_global_level$Model) {
    
    tmp1 <- accuracy_global_level %>% filter(Model == i)
    tmp <- data.frame(Model = c('Correct', 'False'),
                       Count = c(tmp1$Accuracy * 100, (1 - tmp1$Accuracy) * 100))
    
    pdf(paste('fig2a_', i, '.pdf', sep = ''), width = (9 / 2.54) * 0.55, height = (6.75/ 2.54) * 0.55)
    print(PieDonut(tmp, aes(Model, count=Count), r0 = 0.45, r1 = 0.9,
             pieLabelSize = 2.8, titlesize = 3.5))
    dev.off()
    
  }

# Fig. 2B: Feature level accuracy

  # Load model results
  results_wide <- read.delim('model_results_raw.tsv')
  
  # Process model results: Categorie 2 == Categorie 1
  results_processed <- results_wide %>%
    filter(!if_any(c(gpt4_eng, gpt4_ger,
                     llama2_13b_eng, llama2_13b_ger, llama2_70b_eng, llama2_70b_ger,
                     llama3_8b_eng, llama3_8b_ger, llama3_70b_eng, llama3_70b_ger),
                   ~ grepl("-", .))) %>%
    mutate(across(c(gpt4_eng, gpt4_ger,
                    llama2_13b_eng, llama2_13b_ger, llama2_70b_eng, llama2_70b_ger,
                    llama3_8b_eng, llama3_8b_ger, llama3_70b_eng, llama3_70b_ger),
                  ~ as.numeric(.)))
  
  # Calculate mean feature level accuracy
  accuracy_feature_level <- results_processed %>% 
    group_by(Key) %>%
    summarize(across(c(gpt4_eng, gpt4_ger,
                       llama2_13b_eng, llama2_13b_ger, llama2_70b_eng, llama2_70b_ger,
                       llama3_8b_eng, llama3_8b_ger, llama3_70b_eng, llama3_70b_ger), mean))

  # Matrix plot
  tmp1 <- accuracy_feature_level %>% column_to_rownames(var = 'Key')
  
  # Change row names
  
    # Create a named vector of new names where names are the old names to replace
    new_rownames <- c('Histologic Subtype' = 'Histologic subtype',
                   'T-Stage (TNM)' = 'pT-stage',
                   'N-Stage (TNM)' = 'pN-stage',
                   'Lymph Nodes with Metastasis' = 'Number LN with Mts.',
                   "Number of Lymph Nodes examined" = 'Number LN examined',
                   "Resection Margins" = 'Surgical margin',
                   "Primary Gleason Pattern" = 'Primary GP',
                   "Secondary Gleason Pattern" = 'Secondary GP',
                   "Percentage of Secondary Gleason Pattern" = 'Secondary GP (%)',
                   "Tertiary Gleason Pattern" = 'Tertiary GP',
                   "WHO (ISUP) Grade Group" = 'WHO Grade Group')
  
    # Use match to find positions of old names in the list and replace them
    row_indices <- match(row.names(tmp1), names(new_rownames))
    
    # Replace only those names that are found in new_rownames
    row.names(tmp1)[!is.na(row_indices)] <- new_rownames[na.omit(row_indices)]
    
  # Change column names
    
    # Create a named vector of new names where names are the old names to be replaced
    new_colnames <- c("gpt4_eng" = "GPT4 (eng)",
                      "gpt4_ger" = "GPT4 (ger)",
                      "llama2_13b_eng" = "Llama2 13B (eng)",
                      "llama2_13b_ger" = "Llama2 13B (ger)",
                      "llama2_70b_eng" = "Llama2 70B (eng)",
                      "llama2_70b_ger" = "Llama2 70B (ger)",
                      "llama3_8b_eng" = "Llama3 8B (eng)",
                      "llama3_8b_ger" = "Llama3 8B (ger)",
                      "llama3_70b_eng" = "Llama3 70B (eng)",
                      "llama3_70b_ger" = "Llama3 70B (ger)")
    
    # Use match to find positions of old names in the list and replace them
    col_indices <- match(colnames(tmp1), names(new_colnames))
    
    # Replace only those names that are found in new_colnames
    colnames(tmp1)[!is.na(col_indices)] <- new_colnames[na.omit(col_indices)]
    
  # Custom order for the row names
  custom_order <- c('Histologic subtype',
                    'pT-stage',
                    'pN-stage',
                    'Number LN with Mts.',
                    'Number LN examined',
                    'Surgical margin',
                    'Primary GP',
                    'Secondary GP',
                    'Secondary GP (%)',
                    'Tertiary GP',
                    'WHO Grade Group')
    
      # Create an index based on the custom order of the row names
      index <- match(custom_order, rownames(tmp1))
      
      # Reorder the dataframe rows based on the index
      tmp2 <- tmp1[index, ]
    
  # Change cells to percent values (*100)
  chart_input <- round(tmp2 * 100, 1)
  
  pdf("fig2b.pdf", width = (12 / 2.54) * 0.95, height = (10 / 2.54))
  costum_heatmap(as.matrix(chart_input),
              breaks = seq(0, 100, by=1),
              gradient_colors = c("white", "lightsteelblue"),
              cluster_cols = FALSE, cluster_rows = FALSE, legend = FALSE,
              fontsize = 7, fontsize_number = 7) # #CD534CFF, slategrey, seagreen, #69b3a2, lightsteelblue, #436685
  dev.off()
  

# Fig. 2A + 2B (Supplementary Table): Precision, Recall, F1-Score 
  case_ids <- case_filter()
  analyzed_features <- get_analyzed_features()
  input_files <- list.files('./model_results', full.names = TRUE)
  
  # Loop through input files
  out <- list()
  for (i in input_files) {
    
    file_id <- gsub('./model_results/|_results.tsv', '', i) # adapt accordingly to folder/file name
    llm_results <- preprocess_results(i) %>%
      filter(Annotation_Value != '-')
    
    # Loop through parameter
    for (j in analyzed_features$features) {
      
      tmp <- llm_results %>%
        filter(Key == j) %>%
        merge(., case_ids, by =1, all.y = TRUE) # include cases where no answer was given/category is missing
      
      # Select valid categories from ground truth
      categories <- unique(tmp$Annotation_Value)
      
      # True positive
      tp <- tmp %>%
        filter(JSON_Value == Annotation_Value) %>%
        nrow()
      
      # False positive
      fp <- tmp %>%
        filter(JSON_Value != Annotation_Value) %>%
        filter(!(JSON_Value %in% categories)) %>%
        nrow()
      
      # False negative
      fn <- tmp %>%
        filter(JSON_Value != Annotation_Value) %>%
        filter(JSON_Value %in% categories) %>%
        nrow() + sum(is.na(tmp$Key)) # include cases where no answer was given/category is missing
      
      out[[paste(i, j, sep = '_')]] <- data.frame(ID = file_id, Parameter = j, tp = tp, fp = fp, fn = fn)
      
    }
    
  }
  
  results <- do.call(rbind, out)
  
  # Calculate Precision, Recall, F1-Score: Global Level
  aggregated_results <- results %>%
    group_by(ID) %>%
    summarize(tp = sum(tp), fp = sum(fp), fn = sum(fn))
  
  stats_global_lvl <- data.frame(Model = aggregated_results$ID,
                                 Precision = aggregated_results$tp / (aggregated_results$tp + aggregated_results$fp),
                                 Recall = aggregated_results$tp / (aggregated_results$tp + aggregated_results$fn), row.names = NULL) %>%
    mutate(F1_Score = 2 * ((Precision * Recall) / (Precision + Recall)))
  
  write.table(stats_global_lvl, file = 'Suppl_Table_6.tsv', row.names = FALSE,
              col.names = TRUE, sep = '\t', quote = FALSE)
  
  # Calculate Precision, Recall, F1-Score: Feature Level
  stats_feature_lvl <- data.frame(results[,1:2],
                                  Precision = results$tp / (results$tp + results$fp),
                                  Recall = results$tp / (results$tp + results$fn), row.names = NULL) %>%
    mutate(F1_Score = 2 * ((Precision * Recall) / (Precision + Recall))) %>%
    rename(Model = ID)
  
  write.table(stats_feature_lvl, file = 'Suppl_Table_7.tsv', row.names = FALSE,
              col.names = TRUE, sep = '\t', quote = FALSE)
  
  
# Fig. 2C: JSON generation (Valid/Total JSON)

  # Frequency Valid/Total JSON and Preprocessing/Valid JSON
  files <- list.files('./json_status', full.names = TRUE)
  
  out <- list()
  for (i in files) {out[[i]] <- json_stats(i)}
  
  results_valid_jsons <- do.call(rbind, out)
  results_valid_jsons <- data.frame(A1 = 
                                      gsub('./json_status/|_json_status.tsv', '', # adapt accordingly to folder/file name
                                           row.names(results_valid_jsons)), results_valid_jsons, row.names = NULL)
  
  results_valid_jsons
  
  # Dumbbell Plot
  replacements <- c("gpt4" = "GPT4",
                    "llama2_13b" = "Llama2 13B",
                    "llama2_70b" = "Llama2 70B",
                    "llama3_8b" = "Llama3 8B",
                    "llama3_70b" = "Llama3 70B")
  
  tmp <- data.frame(Model = str_replace(results_valid_jsons$A1, "_[^_]*$", ""),
                     Language = str_replace(results_valid_jsons$A1, ".*_", ""),
                     Frequency = results_valid_jsons$freq_valid_jsons) %>%
    pivot_wider(., names_from = Language, values_from = Frequency) %>%
    mutate(Model = str_replace_all(Model, replacements)) %>%
    mutate(Model = factor(Model, levels = c(
      "GPT4", "Llama2 13B", "Llama2 70B", "Llama3 8B", "Llama3 70B")))

  fig2c <- ggplot(tmp, aes(x=reorder(Model, desc(Model)), y=eng)) +
    geom_segment(aes(x=reorder(Model, desc(Model)) ,xend=reorder(Model, desc(Model)), y=eng, yend=ger), color="grey", linewidth = 0.5) +
    geom_point(size=6, color="lightsteelblue") +
    geom_point(aes(y=ger), size=6, color="#69b3a2", alpha=0.5) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank()) +
    ylim(0,1) +
    ylab("") +
    xlab("")
  
  ggsave("fig2c.pdf",
         plot = fig2c,
         width = 9,
         height = 6.75,
         units = 'cm',
         dpi = 800)
  
  # (RETEST) Frequency Valid/Total JSON and Preprocessing/Valid JSON (mention in text)
  files <- list.files('./json_status_retest', full.names = TRUE)
  
  out <- list()
  for (i in files) {out[[i]] <- json_stats(i)}
  
  results_valid_jsons_retest <- do.call(rbind, out)
  results_valid_jsons_retest <- data.frame(A1 = gsub('./json_status_retest/|_json_status_retest.tsv', '', # adapt accordingly to folder/file name
                                                     row.names(results_valid_jsons_retest)), results_valid_jsons_retest, row.names = NULL)
  results_valid_jsons_retest

  # Match invalid JSON files between two runs (Reference vs Retest)
  
    # Create a file path table
    files <- list.files('./json_status', full.names = TRUE)
    names(files) <- gsub('./json_status/|_json_status.tsv', '', files) # adapt accordingly to folder/file name
    files_retest <- list.files('./json_status_retest', full.names = TRUE)
    names(files_retest) <- gsub('./json_status_retest/|_json_status.tsv', '', files_retest) # adapt accordingly to folder/file name
    
    path_table <- merge(files, files_retest, by = 'row.names')
    colnames(path_table) <- c('Model_ID', 'Reference', 'Retest')
    
    # Loop through each file and match invalid JSON
    out <- list()
    for (i in path_table$Model_ID) {out[[i]] <- match_invalid_json(model_id = i, path_table = path_table)}
    
    recurrent_invalid <- do.call(rbind, out)
    recurrent_invalid <- data.frame(A1 = gsub('./json_status_retest/|_json_status_retest.tsv', '', # adapt accordingly to folder/file name
                                              row.names(recurrent_invalid)), recurrent_invalid, row.names = NULL)
    recurrent_invalid
  

# Fig. 2D: Average time per report
  
  # Individual and Average time
  files <- list.files('./json_raw', full.names = TRUE)
  
  out_distribution <- list()
  out_average <- list()
  for (i in files) {
    
    file_paths <- list.files(path = i, full.names = T)
    
    # Retrieve individual time values
    individual_time <- do.call(rbind, lapply(file_paths, time_quantification))
    
    # Calculate average time for each model
    average_time <- mean(individual_time)
    
    # Generate model ID
    model_id <- gsub('./json_raw/', '', i) # adapt accordingly to folder/file name
    
    # Create Output
    out_distribution[[i]] <- data.frame(model_id = model_id, time = individual_time)
    out_average[[i]] <- data.frame(model_id = model_id, average_time = average_time)
    
  }
  
  time_distribution <- do.call(rbind, out_distribution)
  time_average <- do.call(rbind, out_average)

  # Density plot
  replacements <- c("gpt4_eng" = "GPT4 (eng)",
                    "gpt4_ger" = "GPT4 (ger)",
                    "llama2_13b_eng" = "Llama2 13B (eng)",
                    "llama2_13b_ger" = "Llama2 13B (ger)",
                    "llama2_70b_eng" = "Llama2 70B (eng)",
                    "llama2_70b_ger" = "Llama2 70B (ger)",
                    "llama3_8b_eng" = "Llama3 8B (eng)",
                    "llama3_8b_ger" = "Llama3 8B (ger)",
                    "llama3_70b_eng" = "Llama3 70B (eng)",
                    "llama3_70b_ger" = "Llama3 70B (ger)")
  time_distribution <- time_distribution %>%
    mutate(model_id = str_replace(model_id, '_json', '')) %>%
    mutate(model_id = str_replace_all(model_id, replacements)) %>%
    mutate(model_id = factor(model_id, levels = c(
      "Llama3 70B (ger)", "Llama3 70B (eng)",
      "Llama3 8B (ger)", "Llama3 8B (eng)",
      "Llama2 70B (ger)", "Llama2 70B (eng)",
      "Llama2 13B (ger)", "Llama2 13B (eng)",
      "GPT4 (ger)", "GPT4 (eng)"
    )))
    
  # Replace strings in the dataframe
   fig2d <- ggplot(time_distribution, aes(x = time, y = model_id, fill = model_id)) +
      geom_density_ridges(alpha = 0.8) +
      theme_ridges() + 
      theme(axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none") +
      xlim(c(0, 100)) +
      scale_fill_manual(values = c("GPT4 (eng)" = "#69b3a2",
                                   "GPT4 (ger)" = "#69b3a2",
                                   "Llama2 13B (eng)" = "#CD534C",
                                   "Llama2 13B (ger)" = "#CD534C", 
                                   "Llama2 70B (eng)" = "#8DA0CB",
                                   "Llama2 70B (ger)" = "#8DA0CB",
                                   "Llama3 8B (eng)" = "lightyellow3",
                                   "Llama3 8B (ger)" = "lightyellow3", 
                                   "Llama3 70B (eng)" = "lightcyan3",
                                   "Llama3 70B (ger)" = "lightcyan3"))
  
  ggsave("fig2d.pdf",
         plot = fig2d,
         width = 9 * 1.2,
         height = 6.75 * 1.2,
         units = 'cm',
         dpi = 800)
  
  
# Figure 3 -----

# Fig. 3C: Hallucinations
  
  # Llama2 70B: Phylloides Tumor with invalid JSON! 
  # Manual readout performed!
  
  # Data aggregation and Analysis
  rnd_reports <- read_excel('./random_reports/random_reports.xlsx')
  case_id <- rnd_reports$Barcode
  input_files <- list.files('./random_reports', pattern = '_results.tsv', full.names = TRUE, )
  
  out <- list()
  for (i in input_files) {
    
    # File ID
    file_id <- gsub('./random_reports/|_results.tsv', '', i) # adapt accordingly to folder/file name
    
    # Read LLM Results
    llm_results <- read.delim(i) 
    
    # Complete results: every id/feature
    complete_results <- lapply(case_id, process_single_case_hallucination, file_id = file_id,
                               llm_results = llm_results, features = get_analyzed_features())
    
    # Frequency of hallucinations
    tmp <- do.call(rbind, complete_results)
    freq_hallucination <- nrow(tmp[!grepl('Not mentioned', tmp$JSON_Value), ]) / nrow(tmp)
    
    out[[i]] <- data.frame(Model = file_id, freq_hallucination = freq_hallucination)
    
  }
  
  results <- do.call(rbind, out)
  
  # Doughnut plots
  for (i in results$Model) {
    
    tmp1 <- results %>% filter(Model == i)
    tmp <- data.frame(Model = c('Hallucination', 'Correct'),
                      Count = c(tmp1$freq_hallucination * 100, (1 - tmp1$freq_hallucination) * 100))
    
    pdf(paste('fig3c_', i, '.pdf', sep = ''), width = (9 / 2.54) * 0.55, height = (6.75/ 2.54) * 0.55)
    print(PieDonut(tmp, aes(Model, count=Count), r0 = 0.45, r1 = 0.9,
                   pieLabelSize = 2.8, titlesize = 3.5))
    dev.off()
    
  }
  
  
# Fig. 3D: Text complexitiy analysis
  
  # Load model results
  results_wide <- read.delim('model_results_raw.tsv')
  
  # Process model results
  results_processed <- results_wide %>%
    filter(!if_any(c(gpt4_eng, gpt4_ger,
                     llama2_13b_eng, llama2_13b_ger, llama2_70b_eng, llama2_70b_ger,
                     llama3_8b_eng, llama3_8b_ger, llama3_70b_eng, llama3_70b_ger),
                   ~ grepl("-", .))) %>%
    mutate(across(c(gpt4_eng, gpt4_ger,
                    llama2_13b_eng, llama2_13b_ger, llama2_70b_eng, llama2_70b_ger,
                    llama3_8b_eng, llama3_8b_ger, llama3_70b_eng, llama3_70b_ger),
                  ~ as.numeric(.)))
  
  # Calculate case level error
  results_case_level_error <- results_processed %>%
    group_by(Case_ID) %>%
    summarise(gpt4_eng = mean(gpt4_eng),
              gpt4_ger = mean(gpt4_ger),
              llama2_13b_eng = mean(llama2_13b_eng),
              llama2_13b_ger = mean(llama2_13b_ger),
              llama2_70b_eng = mean(llama2_70b_eng),
              llama2_70b_ger = mean(llama2_70b_ger),
              llama3_8b_eng = mean(llama3_8b_eng),
              llama3_8b_ger = mean(llama3_8b_ger),
              llama3_70b_eng = mean(llama3_70b_eng),
              llama3_70b_ger = mean(llama3_70b_ger)) 
  
  # Calculate lexical-diversity parameter
  ld_eng <- report_ld('./report_files/Report_example_eng.xlsx', 'en')
  ld_ger <- report_ld('./report_files/Report_example_ger.xlsx', 'de')
  
  # Correlation ENG
  tmp_eng <- results_case_level_error %>% # english
    select(., Case_ID, contains('_eng')) %>%
    merge(., ld_eng, by = 1) %>%
    mutate(MTLD = ifelse(is.finite(MTLD), MTLD, NA)) %>%
    group_by(Case_ID) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)))
  
  cor_eng <- cor(tmp_eng[,-1], use = 'pairwise.complete.obs')
  cor_eng <- data.frame(Feature_eng = row.names(cor_eng), cor_eng)
  cor_eng[,-1] <- lapply(cor_eng[,-1], function(x) if(is.numeric(x)) round(x, 2) else x)
  cor_eng <- data.frame(cor_eng, row.names = NULL)

  # Matrix plot
  tmp <- cor_eng %>%
    filter(Feature_eng %in% c('gpt4_eng', 'llama2_13b_eng', 'llama2_70b_eng', 'llama3_8b_eng', 'llama3_70b_eng')) %>%
    select(Feature_eng, Tokens, TTR, MTLD) %>%
    mutate(Feature_eng = str_replace_all(Feature_eng, c("gpt4_eng" = "GPT4 (eng)",
                                                        "llama2_13b_eng" = "Llama2 13B (eng)",
                                                        "llama2_70b_eng" = "Llama2 70B (eng)",
                                                        "llama3_8b_eng" = "Llama3 8B (eng)",
                                                        "llama3_70b_eng" = "Llama3 70B (eng)"))) %>%
    column_to_rownames(var = 'Feature_eng')

  pdf("fig3d_eng.pdf", width = (12 / 2.54) * 0.5, height = (10 / 2.54) * 0.45)
  costum_heatmap(as.matrix(tmp),
                 breaks = seq(-1, 1, by=0.01),
                 gradient_colors = c("lightsteelblue", "white", "#CD534C"),
                 cluster_cols = FALSE, cluster_rows = FALSE, legend = FALSE,
                 fontsize = 8, fontsize_number = 8, round_decimal = 2) # #CD534CFF, slategrey, seagreen, #69b3a2, lightsteelblue, #436685
  dev.off()
  
  # Correlation GER
  tmp_ger <- results_case_level_error %>% # german
    select(., Case_ID, contains('_ger')) %>%
    merge(., ld_ger, by = 1) %>%
    mutate(MTLD = ifelse(is.finite(MTLD), MTLD, NA)) %>%
    group_by(Case_ID) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)))
  
  cor_ger <- cor(tmp_ger[,-1], use = 'pairwise.complete.obs')
  cor_ger <- data.frame(Feature_ger = row.names(cor_ger), cor_ger)
  cor_ger[,-1] <- lapply(cor_ger[,-1], function(x) if(is.numeric(x)) round(x, 4) else x)
  cor_ger <- data.frame(cor_ger, row.names = NULL)
  
  # Matrix plot
  tmp <- cor_ger %>%
    filter(Feature_ger %in% c('gpt4_ger', 'llama2_13b_ger', 'llama2_70b_ger', 'llama3_8b_ger', 'llama3_70b_ger')) %>%
    select(Feature_ger, Tokens, TTR, MTLD) %>%
    mutate(Feature_ger = str_replace_all(Feature_ger, c("gpt4_ger" = "GPT4 (ger)",
                                                        "llama2_13b_ger" = "Llama2 13B (ger)",
                                                        "llama2_70b_ger" = "Llama2 70B (ger)",
                                                        "llama3_8b_ger" = "Llama3 8B (ger)",
                                                        "llama3_70b_ger" = "Llama3 70B (ger)"))) %>%
    column_to_rownames(var = 'Feature_ger')
  
  pdf("fig3d_ger.pdf", width = (12 / 2.54) * 0.5, height = (10 / 2.54) * 0.45)
  costum_heatmap(as.matrix(tmp),
                 breaks = seq(-1, 1, by=0.01),
                 gradient_colors = c("lightsteelblue", "white", "#CD534C"),
                 cluster_cols = FALSE, cluster_rows = FALSE, legend = FALSE,
                 fontsize = 8, fontsize_number = 8, round_decimal = 2) # #CD534CFF, slategrey, seagreen, #69b3a2, lightsteelblue, #436685
  dev.off()
  

  
  
