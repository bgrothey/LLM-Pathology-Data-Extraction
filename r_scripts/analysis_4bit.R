### Analysis of Large Language Model results ###
source("./analysis_4bit_fun.R")
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
  
  file_id <- gsub('./model_results/|_results.tsv|llama3_8b_4bit_', '', i) # adapt accordingly to folder/file name
  llm_results <- preprocess_results(i)
  
  out <- c(out, lapply(case_ids$ID, process_single_case, file_id = file_id,
                       llm_results = llm_results, features = analyzed_features))
  
}

results <- do.call(rbind, out)
results_wide <- results %>%
  pivot_wider(names_from = File_ID, values_from = Match)

write.table(results_wide, file = 'llama3_8b_4bit_results_raw.tsv', row.names = FALSE,
            col.names = TRUE, sep = '\t', quote = FALSE)


# Figure 4 -----


# Fig. 4A: Global accuracy

  # Load model results
  results_wide <- read.delim('llama3_8b_4bit_results_raw.tsv')
  
  # Process model results
  results_processed <- results_wide %>%
    filter(!if_any(c(cot_eng, cot_ger,
                     cov_eng, cov_ger,
                     fewshot_eng, fewshot_ger,
                     rephrase_eng, rephrase_ger,
                     zeroshot_eng, zeroshot_ger),
                   ~ grepl("-", .))) %>%
    mutate(across(c(cot_eng, cot_ger,
                    cov_eng, cov_ger,
                    fewshot_eng, fewshot_ger,
                    rephrase_eng, rephrase_ger,
                    zeroshot_eng, zeroshot_ger),
                  ~ as.numeric(.)))
  
  # Calculate mean accuracy for each model
  accuracy_global_level <- data.frame(Accuracy = apply(results_processed[,-c(1:2)], 2, mean)) %>% 
    rownames_to_column(., var = "Model")
  
  # Doughnut plots
  for (i in accuracy_global_level$Model) {
    
    tmp1 <- accuracy_global_level %>% filter(Model == i)
    tmp <- data.frame(Model = c('Correct', 'False'),
                      Count = c(tmp1$Accuracy * 100, (1 - tmp1$Accuracy) * 100))
    
    pdf(paste('fig4a_', i, '.pdf', sep = ''), width = (9 / 2.54) * 0.5, height = (6.75/ 2.54) * 0.5)
    print(PieDonut(tmp, aes(Model, count=Count), r0 = 0.45, r1 = 0.9,
                   pieLabelSize = 2.8, titlesize = 3.5))
    dev.off()
    
  }

  
# Fig. 4B: Feature level accuracy
  
  # Load model results
  results_wide <- read.delim('llama3_8b_4bit_results_raw.tsv')
  
  # Process model results
  results_processed <- results_wide %>%
    filter(!if_any(c(cot_eng, cot_ger,
                     cov_eng, cov_ger,
                     fewshot_eng, fewshot_ger,
                     rephrase_eng, rephrase_ger,
                     zeroshot_eng, zeroshot_ger),
                   ~ grepl("-", .))) %>%
    mutate(across(c(cot_eng, cot_ger,
                    cov_eng, cov_ger,
                    fewshot_eng, fewshot_ger,
                    rephrase_eng, rephrase_ger,
                    zeroshot_eng, zeroshot_ger),
                  ~ as.numeric(.)))
  
  # Mean accuracy for each model for each features
  accuracy_feature_level <- results_processed %>% 
    group_by(Key) %>%
    summarize(across(c(cot_eng, cot_ger,
                       cov_eng, cov_ger,
                       fewshot_eng, fewshot_ger,
                       rephrase_eng, rephrase_ger,
                       zeroshot_eng, zeroshot_ger), mean))
  
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
    new_colnames <- c("cot_eng" = "CoT (eng)",
                      "cot_ger" = "CoT (ger)",
                      "cov_eng" = "CoV (eng)",
                      "cov_ger" = "CoV (ger)",
                      "fewshot_eng" = "Few-shot (eng)",
                      "fewshot_ger" = "Few-shot (ger)",
                      "rephrase_eng" = "Rephrase (eng)",
                      "rephrase_ger" = "Rephrase (ger)",
                      "zeroshot_eng" = "Zero-shot (eng)",
                      "zeroshot_ger" = "Zero-shot (ger)")
    
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
  
  pdf("fig4b.pdf", width = (12 / 2.54) * 0.95, height = (10 / 2.54))
  costum_heatmap(as.matrix(chart_input),
                 breaks = seq(0, 100, by=1),
                 gradient_colors = c("white", "lightsteelblue"),
                 cluster_cols = FALSE, cluster_rows = FALSE, legend = FALSE,
                 fontsize = 8, fontsize_number = 7) # #CD534CFF, slategrey, seagreen, #69b3a2, lightsteelblue, #436685
  dev.off()

  
# Fig. 4A + 4B (Supplementary Table): Precision, Recall, F1-Score 
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
  
  stats_global_lvl <- data.frame(Model_Strategy = aggregated_results$ID,
                                 Precision = aggregated_results$tp / (aggregated_results$tp + aggregated_results$fp),
                                 Recall = aggregated_results$tp / (aggregated_results$tp + aggregated_results$fn), row.names = NULL) %>%
    mutate(F1_Score = 2 * ((Precision * Recall) / (Precision + Recall)))
  
  write.table(stats_global_lvl, file = 'f1_llama3_global.tsv', row.names = FALSE,
              col.names = TRUE, sep = '\t', quote = FALSE)
  
  # Calculate Precision, Recall, F1-Score: Feature Level
  stats_feature_lvl <- data.frame(results[,1:2],
                                  Precision = results$tp / (results$tp + results$fp),
                                  Recall = results$tp / (results$tp + results$fn), row.names = NULL) %>%
    mutate(F1_Score = 2 * ((Precision * Recall) / (Precision + Recall))) %>%
    rename(Model_Strategy = ID)
  
  write.table(stats_feature_lvl, file = 'f1_llama3_parameter.tsv', row.names = FALSE,
              col.names = TRUE, sep = '\t', quote = FALSE)
  
  
# Fig. 4C: JSON generation (Valid/Total JSON)
  
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
  new_names <- c("llama3_8b_4bit_cot" = "CoT",
                 "llama3_8b_4bit_cov" = "CoV",
                 "llama3_8b_4bit_fewshot" = "Few-shot",
                 "llama3_8b_4bit_rephrase" = "Rephrase",
                 "llama3_8b_4bit_zeroshot" = "Zero-shot")
  
  tmp <- data.frame(Model = str_replace(results_valid_jsons$A1, "_[^_]*$", ""),
                    Language = str_replace(results_valid_jsons$A1, ".*_", ""),
                    Frequency = results_valid_jsons$freq_valid_jsons) %>%
    pivot_wider(., names_from = Language, values_from = Frequency) %>%
    arrange(Model) %>%
    mutate(Model = str_replace_all(Model, new_names))
  
  fig4c <- ggplot(tmp, aes(x=reorder(Model, desc(Model)), y=eng)) +
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
  
  ggsave("fig4c.pdf",
         plot = fig4c,
         width = 9,
         height = 6.75,
         units = 'cm',
         dpi = 600)
  
  
# Fig. 4D: Average time per report
  
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
  time_distribution <- time_distribution %>%
    mutate(model_id = str_replace_all(model_id, c('llama3_8b_4bit_' = '', '_json' = ''))) %>%
    mutate(model_id = str_replace_all(model_id, c('_eng' = ' (eng)', '_ger' = ' (ger)'))) %>%
    mutate(model_id = str_replace_all(model_id, c('cot' = 'CoT',
                                                  'cov' = 'CoV',
                                                  'fewshot' = 'Few-shot',
                                                  'rephrase' = 'Rephrase',
                                                  'zeroshot' = 'Zero-shot'))) %>%
    mutate(model_id = factor(model_id, levels = c(
      'Zero-shot (ger)', 'Zero-shot (eng)',
      'Rephrase (ger)', 'Rephrase (eng)',
      'Few-shot (ger)', 'Few-shot (eng)',
      'CoV (ger)', 'CoV (eng)',
      'CoT (ger)', 'CoT (eng)'
    )))
  
  fig4d <- ggplot(time_distribution, aes(x = time, y = model_id, fill = model_id)) +
    geom_density_ridges(alpha = 0.8) +
    theme_ridges() + 
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    xlim(c(0, 100)) + 
    scale_fill_manual(values = c("Zero-shot (ger)" = "#69b3a2",
                                 "Zero-shot (eng)" = "#69b3a2",
                                 "Few-shot (ger)" = "#CD534C",
                                 "Few-shot (eng)" = "#CD534C", 
                                 "Rephrase (ger)" = "#8DA0CB",
                                 "Rephrase (eng)" = "#8DA0CB",
                                 "CoV (ger)" = "lightcyan3",
                                 "CoV (eng)" = "lightcyan3",
                                 "CoT (ger)" = "lightyellow3",
                                 "CoT (eng)" = "lightyellow3"),)
  
  ggsave("fig4d.pdf",
         plot = fig4d,
         width = 9 * 1.2,
         height = 6.75 * 1.2,
         units = 'cm',
         dpi = 800)


