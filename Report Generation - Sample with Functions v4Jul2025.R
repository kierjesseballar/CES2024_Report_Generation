library(poLCA)
library(readxl)
library(writexl)
library(cowplot)
library(ggthemes)
library(showtext) # needs fig.showtext = TRUE
library(sysfonts)
library(scales)
library(survey)
library(forcats)
library(RColorBrewer)
library(stratification)
library(data.table)
library(ggplot2)
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

source("Data Analysis - Graph Function_Cleaned.R")
# source("Data Analysis - Graph Function.R")
source("Data Analysis - Imputation Inference Function_Cleaned.R")
`%nin%` <- Negate(`%in%`)

options(survey.adjust.domain.lonely = TRUE)
options(survey.lonely.psu = "average")

##### || Load Datasets -----
##### |> Unique Courses and Category Mapping -----
courses_mapped <- read_xlsx("CES 2024 - Courses for Classification, Thea Classified v24Feb2025.xlsx",
                            sheet = 2) %>% 
  rename(degree_classification = Categories) 

##### |> Read Data ------
rawData <- read_xlsx("CES 2024 - All Schools, Weighted v11Feb2025.xlsx",
                     guess_max = 7000,
                     sheet = "Data") %>% 
  mutate(uniqueID = str_c(schoolNameSample, studentID, degree, 
                          sex, svydate,
                          sep = "-")) %>% 
  relocate(uniqueID) %>% 
  left_join(courses_mapped) %>% 
  mutate(YearLevel = case_when(
    YearLevel == "1st years" ~ "Freshmen",
    YearLevel == "2nd years" ~ "Sophomores",
    YearLevel == "3rd years" ~ "Juniors",
    YearLevel == "4th years" ~ "Seniors",
  ))

##### |> Read Dictionary and MICE Guide -----
dictionary <- read_xlsx("datadictionary_20250126.xlsx", sheet = 2) %>% 
  select(VARNAME, VAR_LABEL, Year, Module) %>% 
  mutate(Module = case_when(
    is.na(Module) ~ "Not SQD",
    T ~ Module
  ))

miceGuide <- read_xlsx("ces2024_SQDvars_20250127_SetGuide - v24Feb2025.xlsx")

##### || Example 1 (Visualizing a Section): Job Factors -----
##### |> Step 1) ------
# The first we need to specify is the variables to be selected for 
# the graphs. 

selectedVariables <- c("job_salary",
                       "job_values",
                       "job_employeerights",
                       "job_character",
                       "job_colleagues",
                       "job_purpose",
                       "job_growth",
                       "job_educ",
                       "job_easyjob",
                       "job_benefits")

##### |> Step 2) ------
# Create the graph title. 
selected_Title <- c("Important Job Factors across Year Levels")

# Note: The VAR_LABEL will be used as the label in the graph later so it is 
# important to examine if the label is informative and not too lengthly. 
# The temp data frame is used to look up the variable labels and correct 
# them in an excel file for efficieny. 

##### |> Step 3) ------
# Clean up the VAR_LABELS for the graph. 
temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

temp

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = str_remove(VAR_LABEL, "Job factor: ")) %>%  
  # The edited VAR_LABEL in the temp.xlsx can be easily copy pasted in the next
  # mutate lines. You can use the CONCATENATE function in excel to 
  # automate the "typing" of the mutate lines for the proper VAR_LABEL.
  mutate(VAR_LABEL = case_when(
    VARNAME == 'job_values' ~ 'Align with values',
    VARNAME == 'job_employeerights' ~ 'Protection of employee rights',
    T ~ VAR_LABEL
  ))

##### |> Select Data
data_used_temp <- rawData %>% 
  select(
    # IDs
    uniqueID, strata, gender, 
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    all_of(selectedVariables), 
    ~as.character(.)
  ))

value_levels_temp <- data_used_temp %>% 
  select(all_of(selectedVariables)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

##### |> Step 4) ------
# It is also important to examine the value levels in the variables 
# to ensure that the levels are properly ordered (positive to negative scale)
# and are as concise as possible. The value_levels_temp object shows the 
# levels and the results will be used to edit the mutate lines in the 
# data_used object.
value_levels_temp

data_used <- data_used_temp %>% 
  mutate(across(
    all_of(selectedVariables),
    ~case_when(
      . == "1 - Very Unimportant" ~ "Very Unimportant",
      . == "2 - Somewhat Unimportant" ~ "Somewhat Unimportant",
      . == "3 - Somewhat Important" ~ "Somewhat Important",
      . == "4 - Very Important" ~ "Very Important",
      T ~ .
    )
  )) %>%
  mutate(across(
    all_of(selectedVariables),
    ~as.factor(.)
  ))

##### |> Specify value labels 
value_levels <- data_used %>% 
  select(all_of(selectedVariables)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

##### |> Step 5) ------
# Specify the level ordering here. 
value_levels_used <- value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Very Important", 
                             "Somewhat Important",
                             "Somewhat Unimportant", 
                             "Very Unimportant")) %>% 
  mutate(level = fct_rev(level)) 

##### |> Step 6) -----
# Run graph function and edit palette accordingly.
palette <- c("#D73027", 
             "#F46D43", 
             "#66BD63",
             "#1A9850")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  # The rank_by_value argument arranges the graph in terms of the level specified.
  # This argument will take on either a single level like "Very Important" 
  # or will add up two levels like Very Important and Somewhat Important.
  # rank_by_value = c("Very Important, Somewhat Important"), 
  rank_by_value = c("Very Important"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1, # Edit this argument to increase the number of rows in the legend.
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T, # To enable the number labels in the graphs, set to TRUE. 
  facet_row_combined = 1,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # Set to TRUE if statistics will be computed through bootstrapping.
  # This subset variable can be changed to any grouping variable though
  # for the general and school reports, year level is the appropriate grouping.
  subset_variable = "YearLevel",
  subset_variable_order = c("Freshmen", "Sophomores", "Juniors", "Seniors"))

graph


##### || Example 2 (Visualizing one variable): Overall College Satisfaction -----
##### |> Step 1) ------
# The first we need to specify is the variables to be selected for 
# the graphs. 

selectedVariables <- c("satisfyOverallCollege")

##### |> Step 2) ------
# Create the graph title. 
selected_Title <- c("Overall College Experience Satisfaction across Year Levels")

# Note: The VAR_LABEL will be used as the label in the graph later so it is 
# important to examine if the label is informative and not too lengthly. 
# The temp data frame is used to look up the variable labels and correct 
# them in an excel file for efficieny. 

##### |> Step 3) ------
# Clean up the VAR_LABELS for the graph. 
temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

temp

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = str_remove(VAR_LABEL, "Satisfaction: ")) %>%  
  # The edited VAR_LABEL in the temp.xlsx can be easily copy pasted in the next
  # mutate lines. You can use the CONCATENATE function in excel to 
  # automate the "typing" of the mutate lines for the proper VAR_LABEL.
  mutate(VAR_LABEL = case_when(
    VARNAME == 'satisfyOverallCollege' ~ 'Satisfaction',
    T ~ VAR_LABEL
  ))

##### |> Select Data
data_used_temp <- rawData %>% 
  select(
    # IDs
    uniqueID, strata, gender, 
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    all_of(selectedVariables), 
    ~as.character(.)
  ))

value_levels_temp <- data_used_temp %>% 
  select(all_of(selectedVariables)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

##### |> Step 4) ------
# It is also important to examine the value levels in the variables 
# to ensure that the levels are properly ordered (positive to negative scale)
# and are as concise as possible. The value_levels_temp object shows the 
# levels and the results will be used to edit the mutate lines in the 
# data_used object.
value_levels_temp

data_used <- data_used_temp %>% 
  # Disable this line of code since the levels do not need to be edited. 
  # mutate(across(
  #   all_of(selectedVariables),
  #   ~case_when(
  #     . == "1 - Very Unimportant" ~ "Very Unimportant",
  #     . == "2 - Somewhat Unimportant" ~ "Somewhat Unimportant",
  #     . == "3 - Somewhat Important" ~ "Somewhat Important",
  #     . == "4 - Very Important" ~ "Very Important",
  #     T ~ .
  #   )
  # )) %>%
  mutate(across(
    all_of(selectedVariables),
    ~as.factor(.)
  ))

##### |> Specify value labels 
value_levels <- data_used %>% 
  select(all_of(selectedVariables)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

##### |> Step 5) ------
# Specify the level ordering here. 
value_levels_used <- value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Very Satisfied", 
                             "Somewhat Satisfied",
                             "Neither Satisfied nor Dissatisfied",
                             "Somewhat Dissatisfied", 
                             "Very Dissatisfied")) %>% 
  mutate(level = fct_rev(level)) 

##### |> Step 6) -----
# Run graph function and edit palette accordingly.
palette <- c("#D73027", 
             "#F46D43", 
             "#f6f6dd",
             "#66BD63",
             "#1A9850")

graph <- graph_single(
  dataWeighted_Used = data_used,
  selectedVariable_name = selectedVariables,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1, # Edit this argument to increase the number of rows in the legend.
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T, # To enable the number labels in the graphs, set to TRUE. 
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # Set to TRUE if statistics will be computed through bootstrapping.
  # This subset variable can be changed to any grouping variable though
  # for the general and school reports, year level is the appropriate grouping.
  subset_variable = "YearLevel",
  subset_variable_order = rev(c("Freshmen", "Sophomores", "Juniors", "Seniors")))

graph

##### || Example 3 (Visualizing SQD variables): Self-rated Traits -----
##### |> Step 1) ------
# The first we need to specify is the variables to be selected for 
# the graphs. 
selectedVariables <- c("trait23",
                       "trait28",
                       "trait1",
                       "trait30",
                       "trait11",
                       "trait7",
                       "trait8",
                       "trait4",
                       "trait26",
                       "trait16",
                       "trait10",
                       "trait17",
                       "trait13",
                       "trait18",
                       "trait21",
                       "trait6",
                       "trait32")

##### |> Step 2) ------
# Then we specify the core variables among the sqd variables.
core_variables <- c("trait23", "trait28", "trait1", "trait30")

##### |> Step 3) ------
# Create the graph title. 
selected_Title <- c("Self-Rated Skills")

# Note: The VAR_LABEL will be used as the label in the graph later so it is 
# important to examine if the label is informative and not too lengthly. 
# The temp data frame is used to look up the variable labels and correct 
# them in an excel file for efficieny. 

##### |> Step 4) ------
# Clean up the VAR_LABELS for the graph. 
temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

temp

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = str_remove(VAR_LABEL, "Self-Rating: ")) %>%  
  # The edited VAR_LABEL in the temp.xlsx can be easily copy pasted in the next
  # mutate lines. You can use the CONCATENATE function in excel to 
  # automate the "typing" of the mutate lines for the proper VAR_LABEL.
  mutate(VAR_LABEL = case_when(
    VARNAME == 'trait18' ~ 'Open-Mindedness',
    VARNAME == 'trait21' ~ 'Computer Skills',
    T ~ VAR_LABEL
  ))

##### |> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects"

sqd_files <- list.files(sqd_folder)

sqd_objects <- list()
for(i in 1:length(selectedVariables)){
  selected_variable <- selectedVariables[[i]]
  
  sqd_index <- miceGuide %>% 
    filter(VARNAME == selected_variable) %>% 
    select(rowid) %>% 
    pull()
  
  if(is_empty(sqd_index) == TRUE){
    print(str_c(selected_variable, " is probably a core variable."))
    
  } else{
    sqd_object_name <- str_c("miceObject_Index_",
                             sqd_index, ".rds")
    
    sqd_object <- read_rds(str_c(
      sqd_folder, "\\", sqd_object_name))
    
    sqd_objects[[i]] <- sqd_object
  }
  
}

sqd_objects <- compact(sqd_objects)

##### |> Computes the statistics for the SQD Variables
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = sexism_classification,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "YearLevel") %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

##### |> Step 5) ------
# Ensure that in the rowname, the actual variable names are 
# removed through mutate. 
mean_imputed_all_vars <- rbindlist(mean_imputed_container) %>% 
  mutate(rowname = str_remove_all(rowname, 
                                  pattern = "trait\\d+"))

##### |> Combine Core variables and SQD variables 
##### |>> Get survey designs per year levels
core_survey_designs <- list()
yearLevels <- unique(rawData$YearLevel)

for(i in 1:4){
  subset_variable <- "YearLevel"
  
  survey_design <- svydesign(ids = ~1,
                             strata = ~strata,
                             weights = ~weightsDesignNonResponseRaked,
                             data = rawData) %>% 
    subset(get(subset_variable) == yearLevels[[i]])
  
  survey_design_used <- subset(survey_design, 
                               YearLevel == yearLevels[[i]])
  
  core_survey_designs[[i]] <- survey_design_used
}

##### |>> Compute averages
results_core <- list()
for(k in 1:4){
  selected_design <- core_survey_designs[[k]]
  
  results_year <- list()
  for(i in 1:length(core_variables)){
    selected_core_variable <- core_variables[[i]]
    formula <- str_c("~", selected_core_variable) %>% 
      as.formula()
    
    results <- svytable(formula, Ntotal = 100, design = selected_design) %>% 
      as.data.frame() %>% 
      rename(levels = 1) %>% 
      mutate(variable = selected_core_variable) %>% 
      # need to align the decimal places with the SQD results
      mutate(Freq = Freq*.01)
    
    results_year[[i]] <- results
  }
  
  results_core[[k]] <- rbindlist(results_year) %>% 
    mutate(year = yearLevels[[k]])
}

results_core_final <- rbindlist(results_core) %>% 
  rename(rowname = levels,
         results = Freq,
         YearLevel = year)

##### |> Bind core and sqd results 
mean_imputed_all_vars_used <- mean_imputed_all_vars %>% 
  mutate(YearLevel = case_when(
    YearLevel == "1st years" ~ "Freshmen",
    YearLevel == "2nd years" ~ "Sophomores",
    YearLevel == "3rd years" ~ "Juniors",
    YearLevel == "4th years" ~ "Seniors",
  )) %>% 
  rbind(results_core_final, fill = TRUE)

##### |> Specify value labels 
value_levels <- mean_imputed_all_vars_used %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Excellent",
                             "Above Average",
                             "Below Average",
                             "Poor")) %>% 
  mutate(level = fct_rev(level)) 


##### |> Run graph function
palette <- c("#D73027", 
             "#F46D43", 
             "#f6f6dd",
             "#66BD63",
             "#1A9850")

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars_used,
  selectedVariable_Desc_df = selectedVariable_Desc,
  # The rank_by_value argument arranges the graph in terms of the level specified.
  # This argument will take on either a single level like "Very Important" 
  # or will add up two levels like Very Important and Somewhat Important.
  # rank_by_value = c("Very Important, Somewhat Important"), 
  rank_by_value = "Excellent",
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  # This subset variable can be changed to any grouping variable though
  # for the general and school reports, year level is the appropriate grouping.
  subset_variable = "YearLevel",
  subset_variable_order = c("Freshmen", "Sophomores", "Juniors", "Seniors"))

graph
