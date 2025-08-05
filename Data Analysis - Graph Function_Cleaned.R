# Data Analysis - Graph Function_Cleaned.R

library(ggplot2)
library(survey)
library(dplyr)
library(forcats)
library(data.table)
library(scales) # For percent()
library(ggthemes) # For theme_tufte
library(tibble) # For tibble() in the robust calculate_survey_stats_for_var

# Helper function to create a base theme
theme_custom_graphs <- function() {
  ggthemes::theme_tufte() +
    theme(
      axis.text = element_text(size = 12, color = "black", family = "Montserrat"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      axis.title = element_text(size = 18, face = "bold", color = "black", family = "Montserrat"),
      axis.title.x = element_blank(),
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "black", family = "Montserrat"),
      legend.text = element_text(size = 12, color = "black", family = "Montserrat"),
      legend.position = "bottom",
      strip.text = element_text(size = 15, face = "bold", color = "black", family = "Montserrat")
    )
}

# Helper function to create a survey subset object
create_survey_subset <- function(data_df,
                                 subset_col_name,
                                 subset_value,
                                 weights_col_name = "weightsDesignNonResponseRaked", # Internal name
                                 strata_col_name = "strata", # Internal name
                                 bootstrap_flag = FALSE, # Internal name
                                 unweighted_flag = FALSE) { # Internal name
  
  data_df_subset <- data_df[data_df[[subset_col_name]] == subset_value, ]
  
  current_weights_formula <- as.formula(paste0("~", weights_col_name))
  current_strata_formula <- as.formula(paste0("~", strata_col_name))
  
  if (unweighted_flag) {
    data_df_subset$temp_weights <- 1
    survey_obj <- svydesign(ids = ~1,
                            strata = current_strata_formula,
                            weights = ~temp_weights,
                            data = data_df_subset)
  } else {
    survey_obj <- svydesign(ids = ~1,
                            strata = current_strata_formula,
                            weights = current_weights_formula,
                            data = data_df_subset)
    if (bootstrap_flag) {
      survey_obj <- as.svrepdesign(survey_obj, type = "bootstrap")
    }
  }
  return(survey_obj)
}

# Helper function to calculate survey statistics (ROBUST VERSION)
calculate_survey_stats_for_var <- function(survey_design, variable_name, subset_name) {
  var_formula <- as.formula(paste0("~`", variable_name, "`"))
  
  # Perform svytable and convert to data.frame
  df_intermediate <- svytable(var_formula, Ntotal = 100, design = survey_design) %>%
    as.data.frame()
  
  # Handle different outputs from as.data.frame(svytable(...))
  if (nrow(df_intermediate) == 0) {
    # If svytable result is empty (e.g., subset has no observations for this variable)
    # Return a 0-row tibble with the expected column structure to prevent rbindlist errors
    df_final <- tibble::tibble(
      value = character(), 
      Freq = numeric(), 
      subset = subset_name, 
      variable = variable_name
    )
  } else if (ncol(df_intermediate) == 2) {
    # Standard case: two columns (levels and frequencies)
    # Ensure names are 'value' and 'Freq'
    names(df_intermediate) <- c("value", "Freq")
    df_final <- df_intermediate %>%
      dplyr::mutate(subset = subset_name, variable = variable_name)
  } else if (ncol(df_intermediate) == 1 && names(df_intermediate)[1] == "Freq") {
    # Case where svytable might have returned only a Freq column
    df_final <- tibble::tibble(
      value = as.character(NA), 
      Freq = df_intermediate$Freq,
      subset = subset_name,
      variable = variable_name
    )
  } else {
    # Fallback for unexpected structures, issue a warning
    warning(paste("Unexpected structure from svytable for variable:", variable_name, 
                  "and subset:", subset_name, ". Column names:", 
                  paste(names(df_intermediate), collapse=", ")))
    # Attempt to coerce to standard if possible, or return empty structure
    if ("Freq" %in% names(df_intermediate) && ncol(df_intermediate) > 0) {
      # If 'Freq' column exists, rename the first column to 'value'
      # This assumes the first column is the one with the levels/categories
      df_intermediate <- df_intermediate %>% dplyr::rename(value = 1) 
      df_final <- df_intermediate %>%
        dplyr::mutate(subset = subset_name, variable = variable_name)
    } else { # If 'Freq' is not found or structure is too different, return empty
      df_final <- tibble::tibble(
        value = character(), Freq = numeric(), 
        subset = subset_name, variable = variable_name
      )
    }
  }
  
  # Ensure Freq is numeric, as round() expects it
  if ("Freq" %in% names(df_final)) {
    # Coerce to numeric; if it was already numeric, this does no harm
    # If it was character (e.g. from an odd svytable output), this attempts conversion
    df_final$Freq <- as.numeric(as.character(df_final$Freq))
  } else { 
    # This path should ideally not be taken if the above logic correctly structures df_final
    # Ensure Freq column exists even if all attempts above failed to create it, to prevent error in next step
    df_final$Freq <- numeric() 
  }
  
  return(df_final)
}


##### |>  Run Self Created Functions -----
##### |>> Section -----
graph_section <- function(dataWeighted_Used,
                          selectedVariables_vec, # Vector of variable names
                          selectedVariable_Desc_df, # Dataframe with VARNAME, VAR_LABEL
                          rank_by_value = "Selected",
                          value_levels = NULL,
                          guide_rows = 2,
                          palette = NULL,
                          selected_Title = NULL,
                          geom_text = TRUE, 
                          facet_row_combined = 2,
                          unweighted = FALSE,
                          weights = "weightsDesignNonResponseRaked", 
                          strata = "strata", 
                          bootstrap = FALSE, 
                          ## CHANGED ##: subset_variable is now optional (defaults to NULL)
                          subset_variable = NULL,
                          overall_include = FALSE,
                          subset_variable_order = c("Civic Leaders",
                                                    "Community Contributors",
                                                    "Potential Contributors",
                                                    "Disengaged Youth")){
  
  if (!all(c("VARNAME", "VAR_LABEL") %in% names(selectedVariable_Desc_df))) {
    stop("selectedVariable_Desc_df must contain VARNAME and VAR_LABEL columns.")
  }
  value_mapping <- setNames(selectedVariable_Desc_df$VAR_LABEL, selectedVariable_Desc_df$VARNAME)
  
  ## CHANGED ##: Added conditional logic based on whether a subset_variable is provided
  if (!is.null(subset_variable)) {
    # This block runs when a subset_variable IS provided (original behavior)
    subset_categories <- unique(dataWeighted_Used[[subset_variable]])
    all_stats_list <- list()
    
    for (var_name in selectedVariables_vec) {
      subset_stats_list <- list()
      for (cat_val in subset_categories) {
        survey_subset_obj <- create_survey_subset(data_df = dataWeighted_Used, subset_col_name = subset_variable, subset_value = cat_val, weights_col_name = weights, strata_col_name = strata, bootstrap_flag = bootstrap, unweighted_flag = unweighted)
        subset_stats_list[[as.character(cat_val)]] <- calculate_survey_stats_for_var(survey_subset_obj, var_name, as.character(cat_val))
      }
      
      if (overall_include) {
        surveyObject_overall <- svydesign(ids = ~1, strata = as.formula(paste0("~", strata)), weights = as.formula(paste0("~", weights)), data = dataWeighted_Used)
        subset_stats_list[["Overall"]] <- calculate_survey_stats_for_var(surveyObject_overall, var_name, "Overall")
      }
      all_stats_list[[var_name]] <- rbindlist(subset_stats_list)
    }
    
    statistic_All <- rbindlist(all_stats_list)
    
  } else {
    # This block runs when subset_variable IS NULL (new behavior for overall graph)
    all_stats_list <- list()
    for (var_name in selectedVariables_vec) {
      surveyObject_overall <- svydesign(ids = ~1, strata = as.formula(paste0("~", strata)), weights = as.formula(paste0("~", weights)), data = dataWeighted_Used)
      all_stats_list[[var_name]] <- calculate_survey_stats_for_var(surveyObject_overall, var_name, "Overall")
    }
    statistic_All <- rbindlist(all_stats_list)
  }
  
  # Common data manipulation
  statistic_All_Processed <- statistic_All %>%
    mutate(Freq = round(Freq, 1),
           variable = recode(variable, !!!value_mapping),
           value = fct_relevel(value, value_levels))
  
  ## CHANGED ##: Ranking and re-leveling only happens if there are subsets to rank
  if (!is.null(subset_variable)) {
    rank_basis <- subset_variable_order[[1]]
    
    if (length(rank_by_value) == 1) {
      statistic_All_Rank <- statistic_All_Processed %>%
        filter(subset == rank_basis, value == rank_by_value) %>%
        arrange(Freq) %>% pull(variable) %>% as.vector()
    } else {
      statistic_All_Rank <- statistic_All_Processed %>%
        filter(subset == rank_basis, value %in% as.vector(rank_by_value)) %>%
        group_by(subset, variable) %>%
        summarise(Freq = sum(Freq), .groups = 'drop') %>%
        arrange(Freq) %>% pull(variable) %>% as.vector()
    }
    
    statistic_All_Used <- statistic_All_Processed %>%
      mutate(variable = fct_relevel(as.factor(variable), statistic_All_Rank),
             subset = fct_relevel(as.factor(subset), subset_variable_order))
  } else {
    # For the overall graph, no ranking is needed, but we still need the final variable name
    statistic_All_Used <- statistic_All_Processed
  }
  
  graph_plot <- ggplot(statistic_All_Used) +
    geom_bar(aes(x = variable, y = Freq, fill = value),
             stat = "identity", position = "fill") +
    scale_fill_manual(values = palette) +
    ylab("Percent of Respondents") + xlab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(33)) +
    labs(fill = NULL, title = selected_Title) +
    theme_custom_graphs() +
    guides(fill = guide_legend(nrow = guide_rows, reverse = TRUE)) +
    coord_flip()
  
  ## CHANGED ##: Faceting is now conditional on subset_variable being present
  if (!is.null(subset_variable)) {
    graph_plot <- graph_plot + facet_wrap(~subset, nrow = facet_row_combined)
  }
  
  if (geom_text) { 
    graph_plot <- graph_plot +
      geom_text(aes(x = variable, y = Freq, group = value, label = round(Freq, 0)),
                position = position_fill(vjust = .5), size = 4, family = "Montserrat")
  }
  
  return(graph_plot)
}

##### |>> Single -----
graph_single <- function(dataWeighted_Used,
                         selectedVariable_name, 
                         value_levels = NULL,
                         guide_rows = 2,
                         palette = NULL,
                         selected_Title = NULL,
                         geom_text = TRUE, 
                         unweighted = FALSE,
                         weights = "weightsDesignNonResponseRaked", 
                         strata = "strata", 
                         bootstrap = FALSE, 
                         ## CHANGED ##: subset_variable is now optional (defaults to NULL)
                         subset_variable = NULL,
                         overall_include = FALSE,
                         subset_variable_order = c("Civic Leaders",
                                                   "Community Contributors",
                                                   "Potential Contributors",
                                                   "Disengaged Youth")){
  
  all_stats_list <- list()
  
  ## CHANGED ##: Added conditional logic
  if (!is.null(subset_variable)) {
    # Original behavior when subset_variable is provided
    subset_categories <- unique(dataWeighted_Used[[subset_variable]])
    for (cat_val in subset_categories) {
      survey_subset_obj <- create_survey_subset(data_df = dataWeighted_Used, subset_col_name = subset_variable, subset_value = cat_val, weights_col_name = weights, strata_col_name = strata, bootstrap_flag = bootstrap, unweighted_flag = unweighted)
      all_stats_list[[as.character(cat_val)]] <- calculate_survey_stats_for_var(survey_subset_obj, selectedVariable_name, as.character(cat_val))
    }
    
    if (overall_include) {
      surveyObject_overall <- svydesign(ids = ~1, strata = as.formula(paste0("~", strata)), weights = as.formula(paste0("~", weights)), data = dataWeighted_Used)
      all_stats_list[["Overall"]] <- calculate_survey_stats_for_var(surveyObject_overall, selectedVariable_name, "Overall")
    }
    
  } else {
    # New behavior for an overall graph
    surveyObject_overall <- svydesign(ids = ~1, strata = as.formula(paste0("~", strata)), weights = as.formula(paste0("~", weights)), data = dataWeighted_Used)
    all_stats_list[["Overall"]] <- calculate_survey_stats_for_var(surveyObject_overall, selectedVariable_name, "Overall")
    # If there's no subset, there's no order to apply, so we define it simply
    subset_variable_order <- "Overall"
  }
  
  statistic_All <- rbindlist(all_stats_list) %>%
    mutate(Freq = round(Freq, 1),
           value = fct_relevel(value, value_levels),
           subset = fct_relevel(as.factor(subset), subset_variable_order))
  
  # The rest of the plotting code works as-is, since it plots the 'subset' column,
  # which will correctly contain either multiple categories or the single "Overall" category.
  graph_plot <- ggplot(statistic_All) +
    geom_bar(aes(x = subset, y = Freq, fill = value),
             stat = "identity", position = "fill") +
    scale_fill_manual(values = palette) +
    ylab("Percent of Respondents") +
    xlab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(33)) +
    labs(fill = NULL, title = selected_Title) +
    theme_custom_graphs() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    guides(fill = guide_legend(nrow = guide_rows, reverse = TRUE)) +
    coord_flip()
  
  if (geom_text) { 
    graph_plot <- graph_plot +
      geom_text(aes(x = subset, y = Freq, group = value, label = round(Freq, 0)),
                position = position_fill(vjust = .5), size = 4, family = "Montserrat")
  }
  
  return(graph_plot)
}
##### |>> SQD - Section -----
graph_sqd_section <- function(imputed_statistics, 
                              selectedVariable_Desc_df, 
                              rank_by_value = "Selected",
                              value_levels = NULL,
                              guide_rows = 2,
                              palette = NULL,
                              selected_Title = NULL,
                              geom_text = TRUE, 
                              facet_row_combined = 2,
                              subset_variable,
                              subset_variable_order = c("Civic Leaders", 
                                                        "Community Contributors", 
                                                        "Potential Contributors", 
                                                        "Disengaged Youth")){
  
  if (!all(c("VARNAME", "VAR_LABEL") %in% names(selectedVariable_Desc_df))) {
    stop("selectedVariable_Desc_df must contain VARNAME and VAR_LABEL columns.")
  }
  value_mapping <- setNames(selectedVariable_Desc_df$VAR_LABEL, selectedVariable_Desc_df$VARNAME)
  
  statistic_All <- imputed_statistics %>% 
    rename(value = rowname, Freq = results, subset = subset_variable) %>%
    select(subset, variable, value, Freq) %>%
    mutate(Freq = Freq * 100,
           Freq = round(Freq, 1), # This is where Freq must exist and be numeric
           variable = recode(variable, !!!value_mapping),
           value = fct_relevel(value, value_levels))
  
  statistic_All_Processed <- statistic_All %>%
    mutate(Freq = round(Freq, 1),
           variable = recode(variable, !!!value_mapping),
           value = fct_relevel(value, value_levels))
  
  ## CHANGED ##: Ranking and re-leveling only happens if there are subsets to rank
  if (!is.null(subset_variable)) {
    rank_basis <- subset_variable_order[[1]]
    
    if (length(rank_by_value) == 1) {
      statistic_All_Rank <- statistic_All_Processed %>%
        filter(subset == rank_basis, value == rank_by_value) %>%
        arrange(Freq) %>% pull(variable) %>% as.vector()
    } else {
      statistic_All_Rank <- statistic_All_Processed %>%
        filter(subset == rank_basis, value %in% as.vector(rank_by_value)) %>%
        group_by(subset, variable) %>%
        summarise(Freq = sum(Freq), .groups = 'drop') %>%
        arrange(Freq) %>% pull(variable) %>% as.vector()
    }
    
    statistic_All_Used <- statistic_All_Processed %>%
      mutate(variable = fct_relevel(as.factor(variable), statistic_All_Rank),
             subset = fct_relevel(as.factor(subset), subset_variable_order))
  } else {
    # For the overall graph, no ranking is needed, but we still need the final variable name
    statistic_All_Used <- statistic_All_Processed
  }
  
  graph_plot <- ggplot(statistic_All_Used) +
    geom_bar(aes(x = variable, y = Freq, fill = value),
             stat = "identity", position = "fill") +
    scale_fill_manual(values = palette) +
    facet_wrap(~subset, nrow = facet_row_combined) +
    ylab("Percent of Respondents") +
    xlab(NULL) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = scales::wrap_format(33)) +
    labs(fill = NULL, title = selected_Title) +
    theme_custom_graphs() +
    guides(fill = guide_legend(nrow = guide_rows, reverse = TRUE)) +
    coord_flip()
  
  if (geom_text) { 
    graph_plot <- graph_plot +
      geom_text(aes(x = variable, y = Freq, group = value, label = round(Freq, 0)),
                position = position_fill(vjust = .5), size = 4, family = "Montserrat")
  }
  
  return(graph_plot)
}
