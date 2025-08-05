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

##### || Load Data -----
##### |> Unique Courses and Category Mapping -----
courses_mapped <- read_xlsx("CES 2024 - Courses for Classification, Thea Classifed v4Aug2025.xlsx",
                            sheet = 2) %>% 
  rename(degree_classification = Categories) %>% 
  select(-`Broader_Categories (see sheet 1 for mapping)`) %>% 
  mutate(degree = str_to_upper(degree)) %>% 
  mutate(degree = str_trim(degree))

##### |> Read Data ------
rawData <- read_xlsx("CES 2024 - All Schools, Weighted v4Aug2025.xlsx",
                     guess_max = 10000,
                     sheet = 1) %>% 
  mutate(uniqueID = str_c(schoolNameSample, studentID, degree, 
                          sex, svydate,
                          sep = "-")) %>% 
  relocate(uniqueID) %>% 
  mutate(degree = str_to_upper(degree)) %>% 
  mutate(degree = str_trim(degree)) %>% 
  left_join(courses_mapped) %>% 
  distinct(uniqueID, .keep_all = TRUE)

unique(rawData$finaleligibility)

# Check for uniqueID duplicates
length(unique(rawData$uniqueID)) == nrow(rawData)

##### |> Read Dictionary and MICE Guide -----
dictionary <- read_xlsx("ces2024_datadictionary_20250126.xlsx", 
                        sheet = 2) %>% 
  select(VARNAME, VAR_LABEL, Year, Module) %>% 
  mutate(Module = case_when(
    is.na(Module) ~ "Not SQD",
    T ~ Module
  ))

miceGuide <- read_xlsx("ces2024_SQDvars_20250127_SetGuide - v24Feb2025.xlsx")

##### |> LCA Clustering Object -----
finalLCAModel <- readRDS("finalLCAModelNClass_4_rep_5000_v18Jul2025_CES2024.rds")
finalLCAModel[["call"]]

##### Convergence 
convergence <- finalLCAModel[["attempts"]] %>% 
  as.data.frame() %>% 
  rename(likelihood = 1) %>% 
  rowid_to_column() %>% 
  mutate(max_value =  max(finalLCAModel[["attempts"]], na.rm = TRUE)) %>% 
  mutate(likelihood = as.double(likelihood),
         max_value = as.double(max_value)) %>% 
  mutate(max_value_check = case_when(
    likelihood == max_value ~ "equal", 
    T ~ "not_equal"
  )) %>% 
  filter(max_value_check == "equal") %>% 
  select(rowid) %>% 
  pull() %>% 
  min()

convergence

##### |> Summarize LCA -----
finalLCAModel

##### |> Append Predictions to the rawData -----
clusterAssignments <- finalLCAModel[["predclass"]] %>% 
  as.data.frame() %>% 
  rename(clusterID = 1) %>% 
  rowid_to_column() %>% 
  mutate(clusterID = case_when(
    clusterID == 3 ~ "Civic Advocates",
    clusterID == 2 ~ "Drawable Crowd",
    clusterID == 1 ~ "Disengaged Youth",
    clusterID == 4 ~ "Reliable Supporters"
  )) 

clusterData <- rawData %>% 
  mutate(
    across(.cols = starts_with(c("civicaction_follow", "civicaction_likeshare", 
                       "civicaction_blog", "civicaction_post", 
                       "civicaction_article", "civicaction_discussonline", 
                       "civicaction_soughtnews", "civicaction_discussoffline", 
                       "civicaction_donatedcharity", "civicaction_contactgovt", 
                       "civicaction_elected", "civicaction_joinedonline", 
                       "civicaction_joinedoffline", "civicaction_union", 
                       "civicaction_movement", "civicaction_strike", 
                       "civicaction_advocacy", "civicaction_grant",
                       "civicaction_project", "civicaction_donatedpol",
                       "civicaction_finsupport", "civicaction_publichearings", 
                       "civicaction_positionpaper", "civicaction_lobbying")),
           ~case_when(
             . == "Once in the past yea" ~ "Once in the past year",
             T ~ . 
           ))) %>% 
  mutate(
    across(.cols = starts_with(c("civicaction_follow", "civicaction_likeshare", 
                       "civicaction_blog", "civicaction_post", 
                       "civicaction_article", "civicaction_discussonline", 
                       "civicaction_soughtnews", "civicaction_discussoffline", 
                       "civicaction_donatedcharity", "civicaction_contactgovt", 
                       "civicaction_elected", "civicaction_joinedonline", 
                       "civicaction_joinedoffline", "civicaction_union", 
                       "civicaction_movement", "civicaction_strike", 
                       "civicaction_advocacy", "civicaction_grant",
                       "civicaction_project", "civicaction_donatedpol",
                       "civicaction_finsupport", "civicaction_publichearings", 
                       "civicaction_positionpaper", "civicaction_lobbying")),
           ~case_when(
             . == "Decline to answer" ~ NA,
             . == "I have not done this and I would not do it regardless of the situation" ~ 1,
             . == "I  have not done this, but I might do it if something important happens in the future." ~ 2,
             . == "Once in the past year" ~ 3,
             . == "2-3 times in the past year" ~ 4,
             . == "More than 3 times in the past year" ~ 5
           ))) %>% 
  select(uniqueID, schoolNameSample, starts_with("civicaction")) %>% 
  na.omit() %>% # critical in matching since LCA removes the NA
  rowid_to_column() %>% # then make the rowID AFTER we omit the na
  select(rowid, uniqueID, schoolNameSample) 

clusterData_with_uniqueID_clusterID <- clusterData %>% 
  left_join(clusterAssignments) %>% 
  relocate(uniqueID, rowid, clusterID) %>% 
  filter(!is.na(clusterID)) %>% 
  select(uniqueID, clusterID)

clusterData_Full_Assigned <- rawData %>% 
  left_join(clusterData_with_uniqueID_clusterID) %>% 
  rename(YearLevel = yearlevel) %>% 
  rename(FormNumber = formnumber)  %>% 
  filter(!is.na(clusterID))

# Checking 
nrow(clusterData_Full_Assigned) == nrow(rawData)

non_missing_cluster <- clusterData_Full_Assigned %>% 
  filter(!is.na(clusterID))

nrow(non_missing_cluster) == nrow(finalLCAModel[["y"]])

full_respondent_count <- rawData %>% 
  group_by(schoolNameSample) %>% 
  summarise(full_count = n())
  
missing_clusters <- clusterData_Full_Assigned %>% 
  filter(is.na(clusterID)) %>% # 
  group_by(schoolNameSample) %>% 
  summarise(count = n()) %>% 
  left_join(full_respondent_count) %>% 
  mutate(percent = round((count/full_count*100), 1))

##### |||||||||| Data Visualizations |||||||||| -----
##### |||||||||| Part 2: The Four Faces of Civic Engagement |||||||||| -----
##### || a.	Introduction of the Groups -----
##### |> 1.	Cluster Descriptions - Civic Engagement (Axis 1.a) -----
# Axis 1: Voicing out political opinions
axis1.a <-  c("civicaction_contactgovt", "civicaction_elected", 
              "civicaction_joinedonline", "civicaction_joinedoffline",
              "civicaction_union", "civicaction_movement", 
              "civicaction_strike", "civicaction_lobbying")


axis1.b <- c("civicaction_blog", "civicaction_article", "civicaction_positionpaper", 
             "civicaction_donatedpol", "civicaction_finsupport", "civicaction_publichearings")

# Axis 2: Engagement with Civil Society
axis2 <- c("civicaction_advocacy", "civicaction_grant", "civicaction_project")

# Axis 3: Political Discussions in the Community
axis3 <- c("civicaction_follow", "civicaction_likeshare", "civicaction_post",
           "civicaction_discussonline", "civicaction_soughtnews",
           "civicaction_discussoffline")

##### |>> Select Variables
selectedVariables <- axis1.a

selected_Title <- c("Political Activism")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_blog' ~ 'Wrote about soc-pol issues on a blog or website',
    VARNAME == 'civicaction_article' ~ 'Wrote an opinion article to a newspaper or online site',
    VARNAME == 'civicaction_contactgovt' ~ 'Contacted elected official on an issue or concern',
    VARNAME == 'civicaction_elected' ~ 'Ran for and/or held public office',
    VARNAME == 'civicaction_joinedonline' ~ 'Joined a political group on social media',
    VARNAME == 'civicaction_joinedoffline' ~ 'Joined or volunteered for a political party',
    VARNAME == 'civicaction_union' ~ 'Joined labor group in filing complaint',
    VARNAME == 'civicaction_movement' ~ 'Participated in a movement for social change',
    VARNAME == 'civicaction_strike' ~ 'Took part in a demonstration or strike',
    VARNAME == 'civicaction_donatedpol' ~ 'Donated to a political figure or cause',
    VARNAME == 'civicaction_finsupport' ~ 'Requested support from govt and/or officials',
    VARNAME == 'civicaction_publichearings' ~ 'Attended public hearings or consultations with govt',
    VARNAME == 'civicaction_positionpaper' ~ 'Wrote and submitted a position paper to a legislator',
    VARNAME == 'civicaction_lobbying' ~ 'Lobbied with local or national government agencies',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  )) 

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "More than 3 times in the past year",
                             "2-3 times in the past year",
                             "Once in the past year",
                             "Have not done but might do if something important happens",
                             "Have not done and will never do")) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette = c("#D7191C", 
            "#FDAE61", 
            "#A6D96A", 
            "#22C154",
            "#1A9641")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("More than 3 times in the past year", 
                    "2-3 times in the past year"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 2,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
  )

graph

##### |> 1.	Cluster Descriptions - Civic Engagement (Axis 1.b) -----
# Axis 1: Voicing out political opinions
axis1.a <-  c("civicaction_contactgovt", "civicaction_elected", 
              "civicaction_joinedonline", "civicaction_joinedoffline",
              "civicaction_union", "civicaction_movement", 
              "civicaction_strike", "civicaction_lobbying")


axis1.b <- c("civicaction_blog", "civicaction_article", "civicaction_positionpaper", 
             "civicaction_donatedpol", "civicaction_finsupport", "civicaction_publichearings")

# Axis 2: Engagement with Civil Society
axis2 <- c("civicaction_advocacy", "civicaction_grant", "civicaction_project")

# Axis 3: Political Discussions in the Community
axis3 <- c("civicaction_follow", "civicaction_likeshare", "civicaction_post",
           "civicaction_discussonline", "civicaction_soughtnews",
           "civicaction_discussoffline")

##### |>> Select Variables
selectedVariables <- axis1.b

selected_Title <- c("Political Visibility")
graphTitle <- c("CES2024Forum_1_a cluster description axis 1b")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Civic action:")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_blog' ~ 'Wrote about soc-pol issues on a blog or website',
    VARNAME == 'civicaction_article' ~ 'Wrote an opinion article to a newspaper or online site',
    VARNAME == 'civicaction_contactgovt' ~ 'Contacted elected official on an issue or concern',
    VARNAME == 'civicaction_elected' ~ 'Ran for and/or held public office',
    VARNAME == 'civicaction_joinedonline' ~ 'Joined a political group on social media',
    VARNAME == 'civicaction_joinedoffline' ~ 'Joined or volunteered for a political party',
    VARNAME == 'civicaction_union' ~ 'Joined labor group in filing complaint',
    VARNAME == 'civicaction_movement' ~ 'Participated in a movement for social change',
    VARNAME == 'civicaction_strike' ~ 'Took part in a demonstration or strike',
    VARNAME == 'civicaction_donatedpol' ~ 'Donated to a political figure or cause',
    VARNAME == 'civicaction_finsupport' ~ 'Requested support from govt and/or officials',
    VARNAME == 'civicaction_publichearings' ~ 'Attended public hearings or consultations with govt',
    VARNAME == 'civicaction_positionpaper' ~ 'Wrote and submitted a position paper to a legislator',
    VARNAME == 'civicaction_lobbying' ~ 'Lobbied with local or national government agencies',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "More than 3 times in the past year",
                             "2-3 times in the past year",
                             "Once in the past year",
                             "Have not done but might do if something important happens",
                             "Have not done and will never do")) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette = c("#D7191C", 
            "#FDAE61", 
            "#A6D96A", 
            "#22C154",
            "#1A9641")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("More than 3 times in the past year", 
                    "2-3 times in the past year"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 2,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 1.	Cluster Descriptions - Civic Engagement (Axis 2) -----
# Axis 1: Voicing out political opinions
axis1.a <-  c("civicaction_contactgovt", "civicaction_elected", 
              "civicaction_joinedonline", "civicaction_joinedoffline",
              "civicaction_union", "civicaction_movement", 
              "civicaction_strike", "civicaction_lobbying")


axis1.b <- c("civicaction_blog", "civicaction_article", "civicaction_positionpaper", 
             "civicaction_donatedpol", "civicaction_finsupport", "civicaction_publichearings")

# Axis 2: Engagement with Civil Society
axis2 <- c("civicaction_advocacy", "civicaction_grant", "civicaction_project")

# Axis 3: Political Discussions in the Community
axis3 <- c("civicaction_follow", "civicaction_likeshare", "civicaction_post",
           "civicaction_discussonline", "civicaction_soughtnews",
           "civicaction_discussoffline")

##### |>> Select Variables
selectedVariables <- axis2

selected_Title <- c("Social Impact Endeavors")
graphTitle <- c("CES2024Forum_1_a cluster description axis 2")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Civic action:")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_advocacy' ~ 'Formed an advocacy group',
    VARNAME == 'civicaction_grant' ~ 'Received grant to implement community programs',
    VARNAME == 'civicaction_project' ~ 'Implemented CSO project',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "More than 3 times in the past year",
                             "2-3 times in the past year",
                             "Once in the past year",
                             "Have not done but might do if something important happens",
                             "Have not done and will never do")) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette = c("#D7191C", 
            "#FDAE61", 
            "#A6D96A", 
            "#22C154",
            "#1A9641")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("More than 3 times in the past year", 
                    "2-3 times in the past year"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 2,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 1.	Cluster Descriptions - Civic Engagement (Axis 3) -----
# Axis 1: Voicing out political opinions
axis1.a <-  c("civicaction_contactgovt", "civicaction_elected", 
              "civicaction_joinedonline", "civicaction_joinedoffline",
              "civicaction_union", "civicaction_movement", 
              "civicaction_strike", "civicaction_lobbying")


axis1.b <- c("civicaction_blog", "civicaction_article", "civicaction_positionpaper", 
             "civicaction_donatedpol", "civicaction_finsupport", "civicaction_publichearings")

# Axis 2: Engagement with Civil Society
axis2 <- c("civicaction_advocacy", "civicaction_grant", "civicaction_project")

# Axis 3: Political Discussions in the Community
axis3 <- c("civicaction_follow", "civicaction_likeshare", "civicaction_post",
           "civicaction_discussonline", "civicaction_soughtnews",
           "civicaction_discussoffline")

##### |>> Select Variables
selectedVariables <- axis3

selected_Title <- c("Voicing Out Political Opinions")
graphTitle <- c("CES2024Forum_1_a cluster description axis 3")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Civic action:")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "More than 3 times in the past year",
                             "2-3 times in the past year",
                             "Once in the past year",
                             "Have not done but might do if something important happens",
                             "Have not done and will never do")) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette = c("#D7191C", 
            "#FDAE61", 
            "#A6D96A", 
            "#22C154",
            "#1A9641")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("More than 3 times in the past year", 
                    "2-3 times in the past year"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 2,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### || b.	Profile of Each Group -----
##### |> 1.	Demographics (Sex) -----
##### |>> Select Variables
selectedVariables <- "gender"

selected_Title <- c("Sex at Birth Distribution")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette = c("#D7191C", 
            "#1A9641")

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")))
graph

##### |> 1.	Demographics (Year Level) -----
##### |>> Select Variables
selectedVariables <- "YearLevel"

selected_Title <- c("Year Level Distribution")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- c("#990000", "#000099",  "#FFC20E", "#009900")

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

##### |> 1. Demographics (Degree) -----
##### |>> Select Variables
selectedVariables <- "degree_classification"

selected_Title <- c("Civic Engagement and College Degrees")
graphTitle <- c("CES2024Forum_2_ca college degrees and civic ed")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == 'Information Technology' ~ 'ICT',
      . == 'Agriculture' ~ 'Science, Math, Agriculture',
      . == 'Education and Teacher Training' ~ 'Education and Teacher Training',
      . == 'Science and Math' ~ 'Science, Math, Agriculture',
      . == 'Engineering' ~ 'Engineering, Maritime, and Architecture',
      . == 'Health Sciences' ~ 'Health Sciences',
      . == 'Arts and Humanities' ~ 'Social Science and Humanities',
      . == 'Social Sciences' ~ 'Social Science and Humanities',
      . == 'Business Administration' ~ 'Business Administration',
      . == 'Architecture' ~ 'Engineering, Maritime, and Architecture',
      . == 'Maritime' ~ 'Engineering, Maritime, and Architecture',
      . == 'Others' ~ 'Others',
      . == 'Hospitality' ~ 'Hospitality',
      . == 'Communication' ~ 'Communications',
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  )) 

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>%
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <- value_levels  %>% 
  mutate(level = fct_relevel(level,
                             "Social Science and Humanities",
                             "Engineering, Maritime, and Architecture",
                             "Health Sciences",
                             "Business Administration",
                             "Hospitality and Tourism",
                             "ICT",
                             "Science, Math, Agriculture",
                             "Education and Teacher Training",
                             "Communications",
                             "Others"
  )) %>%
  mutate(level = fct_rev(level))

##### |>>> Run graph function
palette = c("#D9D9D9", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
            "#FDB462", "#B3DE69", "#FCCDE5", "#8DD3C7", "#BC80BD",
            "skyblue")

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 4,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

##### |> 1.	Demographics (Income) -----
##### |>> Select Variables
selectedVariables <- "parentinc"

selected_Title <- c("Estimated Parents' Monthly Income")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Below 20 000",
                             "₱ 20 001 – ₱ 40 000",
                             "₱ 40 001 – ₱ 60 000",
                             "₱ 60 001 – ₱ 80 000",
                             "₱ 80 001 – ₱ 100 000",
                             "₱ 100 001 – ₱ 120 000",
                             "More than ₱ 120 000")) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "Greens",
                      n = 8) %>% 
  str_replace(pattern = "#005A32", "lightgrey") %>% 
  rev()

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 2,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

##### |> 2. Family Support -----
##### |>> Select Variables
selectedVariables <- c("famsupport1", "famsupport2", 
                       "famsupport3", "famsupport4")

selected_Title <- c("Level of Family Support")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Family Support: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Strongly Agree", 
                               "Agree",
                               "Neither Agree nor Disagree",
                               "Disagree", 
                               "Strongly Disagree"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("More than 3 times in the past year", 
                    "2-3 times in the past year"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 3.	Filipino values -----
##### |>> Select Variables
selectedVariables <- c("pinoyvalue_propriety", "pinoyvalue_solidarity", 
                       "pinoyvalue_equality", "pinoyvalue_sociability", 
                       "pinoyvalue_determination", "pinoyvalue_dignity",
                       "pinoyvalue_gratitude", "pinoyvalue_resistance",
                       "pinoyvalue_humanity", "pinoyvalue_freedom")

selected_Title <- c("Importance of 'Filipino' Values")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Filipino value: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects - 7 Jul 2025"
sqd_files <- list.files(sqd_folder)

sqd_objects <- list()
for(i in 1:length(selectedVariables)){
  selected_variable <- selectedVariables[[i]]
  
  sqd_index <- miceGuide %>% 
    filter(VARNAME == selected_variable) %>% 
    select(rowid) %>% 
    pull()
  
  sqd_object_name <- str_c("miceObject_Index_",
                           sqd_index, "v7Jul2025.rds")
  
  sqd_object <- read_rds(str_c(
    sqd_folder, "\\", sqd_object_name))
  
  sqd_objects[[i]] <- sqd_object
}

##### Map rowid and uniqueid since the sqd raw data removed uniqueID
uniqueIDMap <- rawData %>%
  select(-rowID) %>% 
  rowid_to_column() %>% 
  select(rowid, uniqueID) 

##### Generate svytable
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = clusterData_Full_Assigned,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "clusterID",
    uniqueIDMap = uniqueIDMap) %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

mean_imputed_all_vars <- rbindlist(mean_imputed_container)  %>% 
  mutate(rowname = str_remove_all(rowname, "[[:alpha:]_]")) %>% 
  mutate(rowname = case_when(
    rowname == "1" ~ "1 = Least Important",
    rowname == "5" ~ "5 = Most Important",
    T ~ rowname
  ))

##### |>>> Specify value labels 
value_levels <- mean_imputed_all_vars %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "1 = Least Important",
                             "2",
                             "3",
                             "4",
                             "5 = Most Important")) %>% 
  mutate(level = level)

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars,
  rank_by_value = "5 = Most Important",
  selectedVariable_Desc_df = selectedVariable_Desc,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 4.	Personal life goals-----
##### |>> Select Variables
selectedVariables <- c(
  ###### UPDATE LATER AFTER SQD SET 6 ----
                       # "goals1", "goals2", "goals3", "goals4", "goals5",
                       # "goals6", "goals7", "goals8", "goals9", "goals10", 
                       # "goals11", "goals12",
                       "goals13", "goals14", "goals15",
                       "goals16", "goals17", "goals18")

core_variables <- c("goals7", "goals8")

selected_Title <- c("Importance of Life Goals and Aspirations")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Goals: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects - 7 Jul 2025"
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

##### Map rowid and uniqueid since the sqd raw data removed uniqueID
uniqueIDMap <- rawData %>%
  select(-rowID) %>% 
  rowid_to_column() %>% 
  select(rowid, uniqueID) 

##### Generate svytable
##### |> Computes the statistics for the SQD Variables
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = clusterData_Full_Assigned,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "clusterID",
    uniqueIDMap = uniqueIDMap) %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

mean_imputed_all_vars <- rbindlist(mean_imputed_container) %>% 
  mutate(rowname = str_remove_all(rowname, "^.*? - ")) 

##### |> Combine Core variables and SQD variables 
##### |>> Get survey designs per year levels
core_survey_designs <- list()
yearLevels <- unique(rawData$YearLevel)

##### |>>> Specify value labels 
value_levels <- mean_imputed_all_vars %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Very Important", "Somewhat Important",
                             "Somewhat Unimportant", "Very Unimportant")) %>% 
  mutate(level = fct_rev(level))

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 4)

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars,
  rank_by_value = "Very Important",
  selectedVariable_Desc_df = selectedVariable_Desc,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 5.	Why go to college? -----
##### |>> Select Variables
selectedVariables <- c("attcollr6", "attcollr12", "attcollr2", "attcollr14",
                       "attcollr13", "attcollr4", "attcollr5", "attcollr8",
                       "attcollr11")

selected_Title <- c("Reasons in Going to College")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Why Go To College: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Important", "Somewhat Important", 
                               "Neither unimportant nor important", 
                               "Somewhat Not Important", "Not Important")
                             )) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = "Important",
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |||||||||| Part 3: The Drivers of Engagement: What Shapes an Active Citizen? |||||||||| -----
##### || a.	The Role of the University: How does the campus environment foster engagement? -----
##### |> 2.	Involvement in college organizations, RERUN IN MICE LATER -----
##### |>> Select Variables
selectedVariables <- c("orglead_art", "orglead_charity", "orglead_union", 
                       "orglead_professional", "orglead_sports", 
                       "orglead_youth", "orglead_govtled", "orglead_church",
                       "orglead_politicalparty", "orglead_advocacy")

core_variables <- NULL

selected_Title <- c("Leadership in Organizations")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Involvement as a leader: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects - 7 Jul 2025"

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
                             sqd_index, "v7Jul2025.rds")
    
    sqd_object <- read_rds(str_c(
      sqd_folder, "\\", sqd_object_name))
    
    sqd_objects[[i]] <- sqd_object
  }
  
}

sqd_objects <- compact(sqd_objects)

##### Map rowid and uniqueid since the sqd raw data removed uniqueID
uniqueIDMap <- rawData %>%
  select(-rowID) %>% 
  rowid_to_column() %>% 
  select(rowid, uniqueID) 

##### Generate svytable
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = clusterData_Full_Assigned,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "clusterID",
    uniqueIDMap = uniqueIDMap) %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

mean_imputed_all_vars <- rbindlist(mean_imputed_container) %>% 
  mutate(rowname = str_remove_all(rowname, "^.*? - ")) 

##### |>>> Specify value labels 
value_levels <- mean_imputed_all_vars %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "To a great extent", "To a good extent",
                             "To some extent", "To a little extent",
                             "Not at all"
                             )) %>% 
  mutate(level = level)

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars,
  rank_by_value = "5 = Most Important",
  selectedVariable_Desc_df = selectedVariable_Desc,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 3.	NSTP -----
##### |>> Select Variables
selectedVariables <- "nstp"

selected_Title <- c("NSTP Taken")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "Set1", n = 3)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph


##### |> 4. NSTP influence -----
##### |>> Select Variables
selectedVariables <- "nstpinf"

selected_Title <- c("Influence of NSTP on Civic Consciousness")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>%
  mutate(level = fct_relevel(level,
                             "Extremely influential", 
                             "Very influential",
                             "Somewhat influential",
                             "Slightly influential", 
                             "Not at all influential")) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

##### || b.	The Influence of Peers, Politics, and Social Media: What external forces matter? -----
##### |> 1.	Conversation Topics, FINISH AFTER SET 12 -----
##### |>> Select Variables
selectedVariables <- c("topics2", "topics5", "topics11", "topics12", "topics4",
                       "topics6", "topics8", "topics9"
                       # "topics1", "topics3", 
                       # "topics10", "topics7"
                       )

core_variables <- NULL

selected_Title <- c("Common Conversation Topics")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Convo Topics: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects - 7 Jul 2025"

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
                             sqd_index, "v7Jul2025.rds")
    
    sqd_object <- read_rds(str_c(
      sqd_folder, "\\", sqd_object_name))
    
    sqd_objects[[i]] <- sqd_object
  }
  
}

sqd_objects <- compact(sqd_objects)

##### Map rowid and uniqueid since the sqd raw data removed uniqueID
uniqueIDMap <- rawData %>%
  select(-rowID) %>% 
  rowid_to_column() %>% 
  select(rowid, uniqueID) 

##### Generate svytable
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = clusterData_Full_Assigned,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "clusterID",
    uniqueIDMap = uniqueIDMap) %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

mean_imputed_all_vars <- rbindlist(mean_imputed_container) %>% 
  mutate(rowname = str_remove_all(rowname, "topics\\d+")) 

##### |>>> Specify value labels 
value_levels <- mean_imputed_all_vars %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Very Often", 
                             "Often",
                             "Sometimes",
                             "Rarely", 
                             "Never"
  )) %>% 
  mutate(level = level)

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars,
  rank_by_value = c("Very Often"),
  selectedVariable_Desc_df = selectedVariable_Desc,
  value_levels = rev(levels(value_levels_used$level)),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 2.	Internet influence -----
##### |>> Select Variables
selectedVariables <- c("internet1", "internet2", "internet3", "internet4", 
                       "internet5", "internet6", "internet7")

selected_Title <- c("Influence on the Internet on Topics")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Internet influence: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Good influence",
                               "Somewhat good influence", 
                               "No influence", 
                               "Somewhat bad influence")
  )) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 4)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = "Good influence",
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph





##### |> 3.	Political influence -----
##### |>> Select Variables
selectedVariables <- c("influencepol_fam", "influencepol_friends", 
                       "influencepol_leadrel", "influencepol_leadcomm", 
                       "influencepol_leadpol", "influencepol_leadacad",
                       "influencepol_media", "influencepol_socmed",
                       "influencepol_onlinenews", "influencepol_celeb",
                       "influencepol_currentofficials")

selected_Title <- c("Influence on Political Thinking")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Political influence from:")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c( "Very High",
                                "Somewhat High",
                                "Neutral",
                                "Somewhat Low",
                                "Very Low"
                                )
  )) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Very High", "Somewhat High"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 3.	Teacher Influence -----
##### |>> Select Variables
selectedVariables <- c("teachinf1", "teachinf2", "teachinf3", "teachinf4")

selected_Title <- c("Teacher Influence on Civics")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Teacher influence: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Extremely influential", 
                             "Very influential",
                             "Somewhat influential",
                             "Slightly influential", 
                             "Not at all influential")) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Extremely influential", "Very influential"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)


##### || c.	The Power of Personal Beliefs: How do internal attitudes drive action? -----
##### |> 1.	Developing Political Orientations -----
##### |>> Select Variables
selectedVariables <- c("elect_youth", "elect_nodesire", "elect_youthcommunity", 
                       "elect_ownduty")

selected_Title <- c("Attitudes on Civic Effectiveness")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Teacher influence: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Strongly Agree", 
                               "Agree",
                               "Neither Disagree nor Agree",
                               "Disagree", 
                               "Strongly Disagree"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Strongly Agree", 
                    "Agree"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

#### |> 2.	Satisfaction with the Government -----
##### |>> Select Variables
selectedVariables <- c("govtpres", "govtsen", "govtdist", "govtgov", "govtbrgy")

selected_Title <- c("Satisfaction with the Government")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Satisfaction with govt: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  # filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

unique(data_used$YearLevel)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Extremely Satisfied", 
                               "Satisfied",
                               "Neutral",
                               "Dissatisfied", 
                               "Extremely Dissatisfied"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Extremely Satisfied", 
                    "Satisfied"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> 3.	State of Philippine Democracy -----
##### |>> Select Variables
selectedVariables <- "statedem"

selected_Title <- c("State of the Philippine Democracy")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Democracy is still highly followed and implemented.", 
                               "Democracy is oftentimes being followed and implemented.",
                               "Democracy is somewhat being followed and implemented.", 
                               "Democracy is not being followed nor implemented.",
                               "I cannot define or understand what democracy is."))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 3,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

#### |> 4.	Political and democratic values -----
##### |>> Select Variables
selectedVariables <- c("dem_policy", "dem_leader", "dem_media", 
                       "dem_questiondecisions")

selected_Title <- c("Liberal Values")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Satisfaction with govt: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  # filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

unique(data_used$YearLevel)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Strongly Agree", 
                               "Agree",
                               "Neither Disagree nor Agree",
                               "Disagree", 
                               "Strongly Disagree"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Strongly Agree", 
                    "Agree"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |||||||||| 4.	Part 4: The Ripple Effect: The Consequences of Engagement |||||||||| -----
##### || a.	Impact on Skills, Ambition, and Academics -----
##### |> i.	Current academic performance this year -----
##### |>> Select Variables
selectedVariables <- "expperform"

selected_Title <- c("Expected Academic Performance for School Year")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Excellent", 
                               "Above Average",
                               "Average", 
                               "Below Average", 
                               "Poor"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

#### |> ii	Satisfaction with the learning environment -----
##### |>> Select Variables
selectedVariables <- c("lenvsat10", "lenvsat26", "lenvsat27", "lenvsat28",
                       "lenvsat29", "lenvsat17", "lenvsat16", "lenvsat22",
                       "lenvsat35", "lenvsat5", "lenvsat32", "lenvsat36", 
                       "lenvsat37", "lenvsat2", "lenvsat15", "lenvsat4", 
                       "lenvsat7", "lenvsat12", "lenvsat38", "lenvsat19", 
                       "lenvsat9", "lenvsat13", "lenvsat14", "lenvsat39", 
                       "lenvsat18", "lenvsatoa1")

selected_Title <- c("Satisfaction with School Offerings")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Satisfaction on learning environment: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  # filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

unique(data_used$YearLevel)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Very Satisfied", 
                               "Satisfied",
                               "Neither satisfied nor dissatisfied",
                               "Dissatisfied", 
                               "Very dissatisfied"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Very Satisfied", 
                    "Satisfied"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

#### |> iii	College activities -----
##### |>> Select Variables
selectedVariables <- c("act1", "act_llm", "act42", "act67", "act17", 
                       "act21", "act58", "act13", "act8", "act65", "act25",
                       "act38", "act63", "act10", "act14", "act16", "act59",
                       "act64", "act41")

selected_Title <- c("Level of Engagement in College Activities")

core_variables <- c("act_llm")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Goals: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects - 7 Jul 2025"
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
                             sqd_index, "v7Jul2025.rds")
    
    sqd_object <- read_rds(str_c(
      sqd_folder, "\\", sqd_object_name))
    
    sqd_objects[[i]] <- sqd_object
  }
  
}

sqd_objects <- compact(sqd_objects)

##### Generate svytable
##### |> Computes the statistics for the SQD Variables
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = clusterData_Full_Assigned,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "clusterID",
    uniqueIDMap = uniqueIDMap) %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

mean_imputed_all_vars <- rbindlist(mean_imputed_container) %>% 
  mutate(rowname = str_remove_all(rowname, "^.*? - ")) 

##### Map rowid and uniqueid since the sqd raw data removed uniqueID
uniqueIDMap <- rawData %>%
  select(-rowID) %>% 
  rowid_to_column() %>% 
  select(rowid, uniqueID) 

##### |> Combine Core variables and SQD variables 
##### |>> Get survey designs per year levels
core_survey_designs <- list()
clusters <- unique(clusterData_Full_Assigned$clusterID)

for(i in 1:length(clusters)){
  subset_variable <- "clusterID"
  
  survey_design <- svydesign(ids = ~1,
                             strata = ~strata,
                             weights = ~weightsDesignNonResponseRaked,
                             data = clusterData_Full_Assigned) %>% 
    subset(get(subset_variable) == clusters[[i]])
  
  survey_design_used <- subset(survey_design, 
                               clusterID == clusters[[i]])
  
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
    mutate(clusterID = clusters[[k]])
}

results_core_final <- rbindlist(results_core) %>% 
  rename(rowname = levels,
         results = Freq,
         clusterID = clusterID)

##### |> Bind core and sqd results 
mean_imputed_all_vars_used <- mean_imputed_all_vars %>% 
  rbind(results_core_final, fill = TRUE) %>% 
  mutate(rowname = str_remove_all(rowname, "[a-zA-Z]+\\d+"))

##### |>>> Specify value labels 
value_levels <- mean_imputed_all_vars_used %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Very Often", 
                               "Often",
                               "Sometimes",
                               "Rarely", 
                               "Never"))) %>%  
  mutate(level = fct_rev(level))

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars_used,
  rank_by_value = "Very Often", 
  selectedVariable_Desc_df = selectedVariable_Desc,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

#### |> iv.	College activities - spent -----
##### |>> Select Variables
selectedVariables <- c("spent12", "spent39", "spent19", "spent23", "spent14",
                       "spent45", "spent32", "spent46", "spent43", "spent24",
                       "spent1", "spent11", "spent51", "spent52", "spent53", 
                       "spent54", "spent55", "spent56", "spent57", "spent58", 
                       "spent59", "spent60", "spent61", "spent62", "spent63",
                       "spent64")

selected_Title <- c("Time Spent on Acad and Non-Acad Activities")

core_variables <- c("spent51", "spent52")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Goals: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects - 7 Jul 2025"
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
                             sqd_index, "v7Jul2025.rds")
    
    sqd_object <- read_rds(str_c(
      sqd_folder, "\\", sqd_object_name))
    
    sqd_objects[[i]] <- sqd_object
  }
  
}

sqd_objects <- compact(sqd_objects)

##### Generate svytable
##### |> Computes the statistics for the SQD Variables
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = clusterData_Full_Assigned,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "clusterID",
    uniqueIDMap = uniqueIDMap) %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

mean_imputed_all_vars <- rbindlist(mean_imputed_container) %>% 
  mutate(rowname = str_remove_all(rowname, "^.*? - ")) 

##### Map rowid and uniqueid since the sqd raw data removed uniqueID
uniqueIDMap <- rawData %>%
  select(-rowID) %>% 
  rowid_to_column() %>% 
  select(rowid, uniqueID) 

##### |> Combine Core variables and SQD variables 
##### |>> Get survey designs per year levels
core_survey_designs <- list()
clusters <- unique(clusterData_Full_Assigned$clusterID)

for(i in 1:length(clusters)){
  subset_variable <- "clusterID"
  
  survey_design <- svydesign(ids = ~1,
                             strata = ~strata,
                             weights = ~weightsDesignNonResponseRaked,
                             data = clusterData_Full_Assigned) %>% 
    subset(get(subset_variable) == clusters[[i]])
  
  survey_design_used <- subset(survey_design, 
                               clusterID == clusters[[i]])
  
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
    mutate(clusterID = clusters[[k]])
}

results_core_final <- rbindlist(results_core) %>% 
  rename(rowname = levels,
         results = Freq,
         clusterID = clusterID)

##### |> Bind core and sqd results 
mean_imputed_all_vars_used <- mean_imputed_all_vars %>% 
  rbind(results_core_final, fill = TRUE) %>% 
  mutate(rowname = str_remove_all(rowname, "[a-zA-Z]+\\d+"))

##### |>>> Specify value labels 
value_levels <- mean_imputed_all_vars_used %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Very Often", 
                               "Often",
                               "Sometimes",
                               "Rarely", 
                               "Never"))) %>%  
  mutate(level = fct_rev(level))

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars_used,
  rank_by_value = "Very Often", 
  selectedVariable_Desc_df = selectedVariable_Desc,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

#### |> vi.	Self-rated abilities -----
##### |>> Select Variables
selectedVariables <- c("trait23", "trait28", "trait1", "trait30", "trait11",
                       "trait7", "trait8", "trait4", "trait26", "trait16",
                       "trait10", "trait17", "trait13", "trait18", "trait21",
                       "trait6", "trait32")

selected_Title <- c("Self-Rated Traits and Abilities")

core_variables <- c("trait23", "trait28", "trait1", "trait30")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Self-Rating: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    T ~ VAR_LABEL
  ))

##### |>>> Read SQD Objects
sqd_folder <- "C:\\Users\\kdbal\\Desktop\\WORK\\FEU Public Policy Center\\CES\\CES 2024\\CES2024DataAnalysis\\COARE\\Imputed RDS Objects - 7 Jul 2025"
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
                             sqd_index, "v7Jul2025.rds")
    
    sqd_object <- read_rds(str_c(
      sqd_folder, "\\", sqd_object_name))
    
    sqd_objects[[i]] <- sqd_object
  }
  
}

sqd_objects <- compact(sqd_objects)

##### Generate svytable
##### |> Computes the statistics for the SQD Variables
mean_imputed_container <- list()
for(i in 1:length(sqd_objects)){
  selected_mice_object <-  sqd_objects[[i]]
  
  imputed_statistic <- mice_impute_subset(
    cluster_data = clusterData_Full_Assigned,
    weights = "weightsDesignNonResponseRaked",
    miceObject = selected_mice_object,
    unweighted = F,
    boostrap = F,
    subset_variable = "clusterID",
    uniqueIDMap = uniqueIDMap) %>% 
    mutate(variable = selected_mice_object[[1]][[2]])
  
  mean_imputed_container[[i]] <- imputed_statistic
  
  print(i)
}

mean_imputed_all_vars <- rbindlist(mean_imputed_container) %>% 
  mutate(rowname = str_remove_all(rowname, "^.*? - ")) 

##### Map rowid and uniqueid since the sqd raw data removed uniqueID
uniqueIDMap <- rawData %>%
  select(-rowID) %>% 
  rowid_to_column() %>% 
  select(rowid, uniqueID) 

##### |> Combine Core variables and SQD variables 
##### |>> Get survey designs per year levels
core_survey_designs <- list()
clusters <- unique(clusterData_Full_Assigned$clusterID)

for(i in 1:length(clusters)){
  subset_variable <- "clusterID"
  
  survey_design <- svydesign(ids = ~1,
                             strata = ~strata,
                             weights = ~weightsDesignNonResponseRaked,
                             data = clusterData_Full_Assigned) %>% 
    subset(get(subset_variable) == clusters[[i]])
  
  survey_design_used <- subset(survey_design, 
                               clusterID == clusters[[i]])
  
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
    mutate(clusterID = clusters[[k]])
}

results_core_final <- rbindlist(results_core) %>% 
  rename(rowname = levels,
         results = Freq,
         clusterID = clusterID)

##### |> Bind core and sqd results 
mean_imputed_all_vars_used <- mean_imputed_all_vars %>% 
  rbind(results_core_final, fill = TRUE) %>% 
  mutate(rowname = str_remove_all(rowname, "[a-zA-Z]+\\d+"))

##### |>>> Specify value labels 
value_levels <- mean_imputed_all_vars_used %>% 
  select(rowname) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Excellent", 
                               "Above Average",
                               "Average", 
                               "Below Average", 
                               "Poor"))) %>% 
  mutate(level = fct_rev(level))

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars_used,
  rank_by_value = c("Excellent", 
                    "Above Average"), 
  selectedVariable_Desc_df = selectedVariable_Desc,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### || b. Impact on Well-being and Future Plans: -----
##### |> i.	Satisfaction with life -----
##### |>> Select Variables
selectedVariables <- "satpres"

selected_Title <- c("Satisfaction with Life Currently")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used) == nrow(clusterData_Full_Assigned)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Very Satisfied", 
                               "Satisfied",
                               "Neither Satisfied nor Dissatisfied",
                               "Dissatisfied", 
                               "Very Dissatisfied"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 3,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph






##### |> ii.	Improvement in future forecast -----
##### |>> Select Variables
selectedVariables <- "satfuture"

selected_Title <- c("Optimism with the Future")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used) == nrow(clusterData_Full_Assigned)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("My life will improve significantly", 
                               "My life will slightly improve",
                               "Nothing will change, everything will be the same",
                               "My life will become slightly worse", 
                               "My life will worsen significantly"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 3,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

#### |> iii.	Feelings -----
##### |>> Select Variables
selectedVariables <- c("feels1", "feels2", "feels3", "feels4", "feels5", 
                       "feels6", "feels7", "feels8", "feels9_family",
                       "feels9_friends", "feels10", "feels11")

selected_Title <- c("Self Rated Mental Health")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Feeling: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  # filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

unique(data_used$YearLevel)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Very Often", 
                               "Often",
                               "Sometimes",
                               "Rarely", 
                               "Never"))) %>%
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Very Often", 
                    "Often"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph






#### |> iv.	Life outlook and Resilience -----
##### |>> Select Variables
selectedVariables <- c("outlook_life", "outlook_paths", "outlook_peers",
                       "outlook_resourceful", "outlook_pasthelp", 
                       "outlook_persistence")

selected_Title <- c("Optimism and Resilience")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Outlook: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  # filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  )) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~str_remove_all(., "^.*? - ")
  ))

unique(data_used$YearLevel)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Strongly Agree", 
                               "Agree",
                               "Neutral",
                               "Disagree", 
                               "Strongly Disagree"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Strongly Agree", 
                    "Agree"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph





#### |> v.	Career paths -----
##### |>> Select Variables
selectedVariables <- c("path_advocacy", "path_govtemployee", 
                       "path_govtelected", "path_uniformed")

selected_Title <- c("Career Paths in the Government and NGOs")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Career path: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  # filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  )) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~str_remove_all(., "^.*? - ")
  ))

unique(data_used$YearLevel)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Not at all interested", "Slightly interested", 
                             "Moderately interested", "Quite Interested", 
                             "Very interested")) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 5)

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Quite Interested", 
                    "Very interested"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### || c. Impact on Democratic Habits: -----
#### |> i.	Interest in leadership positions -----
##### |>> Select Variables
selectedVariables <- c("ileaderorg", "ileadergov")

selected_Title <- c("Interest in Leading Student Orgs and in the (Future) Government")

temp <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL)

# write_xlsx(temp, "temp.xlsx")

selectedVariable_Desc <- dictionary %>% 
  filter(VARNAME %in% selectedVariables) %>% 
  select(VARNAME, VAR_LABEL) %>% 
  mutate(VAR_LABEL = str_remove_all(VAR_LABEL, "Interested in leadership positions: ")) %>% 
  mutate(VAR_LABEL = str_trim(VAR_LABEL)) %>% 
  mutate(VAR_LABEL = case_when(
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>%
  # filter(YearLevel == "1st years") %>% 
  select(
    # IDs
    uniqueID, clusterID, strata,
    YearLevel, FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Once in the past yea" ~ "Once in the past year",
      . == "I  have not done this, but I might do it if something important happens in the future." ~ "Have not done but might do if something important happens",
      . == "I have not done this and I would not do it regardless of the situation" ~ "Have not done and will never do",
      T ~ .
    )
  )) %>%
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  )) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               YearLevel, FormNumber, weightsDesignNonResponseRaked),
    ~str_remove_all(., "^.*? - ")
  ))

unique(data_used$YearLevel)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  select(-c(uniqueID, clusterID, strata,
            YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level,
                             "Yes", "No")) %>% 
  mutate(level = fct_rev(level))

##### |>>> Run graph function
palette <- c("#D73027", 
             "#1A9850")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Yes"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> ii.	More young people in office? -----
##### |>> Select Variables
selectedVariables <- "officeyouth"

selected_Title <- c("Should more young people run for office?")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Yes", "No"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- c("#D73027", 
            "#1A9850")

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph





##### |> iii.	Perceptions of an ideal leader -----
##### |>> Select Variables
selectedVariables <- "idealleader"

selected_Title <- c("What is the ideal leader that this country needs?")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "A leader that is inclusive and open. A participative leading style that involves a team of people to collaborate and make critical decisions together and as a team." ~ "Inclusive, open, and participative; makes critical decisions with a team",
      . == "A leader that is a visionary. Encourages resourcefulness and innovative thinking in tackling challenging goals while sticking to deadlines." ~ "Visionary, resourceful, and innovative; tackles challenging goals within deadlines",
      . == "A leader that is aggressive. Needs to be in-charge and leadership to be based on control. Expects promptness and perfection from the people him/her." ~ "Aggressive; leadership based on control; expects promptness and perfection",
      . == "A leader that is hands-off and allows its group members to make decisions. Trusts that people make good decisions while periodically monitoring their performance and offers ongoing feedback." ~ "Hands-off; trusts people to make good decisions; offers feedback and monitoring",
      . == "A leader that invests on building its people’s long-term strengths.  Helps people set goals to work towards and give them regular feedback they need to reach success." ~ "Invests in building people's strengths",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "Set1", n = 5)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 5,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph





##### |> iv.	Voting patterns - vote_past -----
##### |>> Select Variables
selectedVariables <- "vote_past"

selected_Title <- c("Have you voted in the following?")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      . == "Literacy Training Service (LTS)" ~ "LTS",
      . == "Reserve Officers' Training Corps (ROTC)" ~ "ROTC",
      . == "Civic Welfare Training Service (CWTS)" ~ "CWTS",
      . == "Reserve Officers Training Corps (ROTC)" ~ "ROTC", 
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("I have participated in both local and national elections",
                               "National elections only",
                               "Local elections only",
                               "I have not voted yet in any government elections"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "RdYlGn", n = 4)
palette <- c("#D7191C", "#A6D96A", "#6ED96A", "#1A9641")

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 2,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph

##### |> v.	Voting patterns - vote_why -----
##### |>> Select Variables
selectedVariables <- "vote_why"

selected_Title <- c("If you have voted, what are your reasons for voting?")

##### |>>> Select Data
data_used_temp <- clusterData_Full_Assigned %>%
  select(uniqueID, selectedVariables) %>% 
  separate_rows(selectedVariables, sep = ", ") %>% 
  # drop_na is useful here if you don't want a column named `<NA>`
  tidyr::drop_na(selectedVariables) 

##### clean options
unique(data_used_temp$vote_why)

data_used_temp <- clusterData_Full_Assigned %>%
  select(uniqueID, clusterID, strata,
         FormNumber, weightsDesignNonResponseRaked,
         selectedVariables) %>% 
  separate_rows({{selectedVariables}}, sep = ", ") %>% 
  filter(!is.na(selectedVariables)) %>% 
  mutate(across(all_of(selectedVariables),
                ~case_when(
                  . == "It is my civic duty to vote." ~ "Civic duty",
                  . == "I believe it is important to vote." ~ "Important",
                  . == "It is a way of expressing my opinions/views." ~ "Express opinions/views",
                  . == "It is my right to vote." ~ "Right to vote",
                  . == "I do it out of habit." ~ "Habit",
                  . == "I just go with my family and friends who are also voting." ~ "Go with family",
                  . == "It’s a way of creating change in the country." ~ "Create change",
                  is.na(.) ~ "Have not voted before",
                  T ~ "Others"
                ))) %>% 
  distinct() %>% 
  mutate(value = "Selected") %>% 
  pivot_wider(
    names_from = !!(selectedVariables),
    values_from = value,
    values_fill = "Not Selected"
  ) 

##### Get the column names
vars_id <- c("uniqueID", "clusterID", "strata",
             "FormNumber", "weightsDesignNonResponseRaked")

vars_expanded <- discard(colnames(data_used_temp),
                         ~. %in% vars_id)

data_used <- data_used_temp %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(vars_expanded)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>> Use current column names as the axis text in the graph 
selectedVariable_Desc <- tibble(VARNAME = vars_expanded) %>% 
  mutate(VAR_LABEL = VARNAME)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Selected", 
                               "Not Selected"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- c("#F9FEFB", 
             "#1A9850")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = vars_expanded,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Selected"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> vi.	Voting patterns - vote_how -----
##### |>> Select Variables
selectedVariables <- "vote_how"

selected_Title <- c("How did you decide who to vote for?")

##### |>>> Select Data
data_used_temp <- clusterData_Full_Assigned %>%
  select(uniqueID, selectedVariables) %>% 
  separate_rows(selectedVariables, sep = ", ") %>% 
  # drop_na is useful here if you don't want a column named `<NA>`
  tidyr::drop_na(selectedVariables) 

##### clean options
unique(data_used_temp[, selectedVariables])

data_used_temp <- clusterData_Full_Assigned %>%
  select(uniqueID, clusterID, strata,
         FormNumber, weightsDesignNonResponseRaked,
         selectedVariables) %>% 
  separate_rows({{selectedVariables}}, sep = ", ") %>% 
  filter(!is.na(selectedVariables)) %>% 
  mutate(across(all_of(selectedVariables),
                ~case_when(
                  . == "I decide on my own." ~ "Decide on their own",
                  . == "I follow my parents’ decisions on who to vote for." ~ "Follow parents",
                  . == "I follow my friends’ decisions on who to vote for." ~ "Follow friends",
                  . == "I follow my religious adviser’s decisions on who to vote for." ~ "Follow religious advisers",
                  . == "I follow celebrities’ decisions on who to vote for." ~ "Follow celebrities",
                  . == "I follow my teacher’s decisions on who to vote for." ~ "Follow teachers",
                  . == "I follow my community leader’s decisions on who to vote for. " ~ "Follow community leaders",
                  is.na(.) ~ "Have not voted before",
                  T ~ "Others"
                ))) %>% 
  distinct() %>% 
  mutate(value = "Selected") %>% 
  pivot_wider(
    names_from = !!(selectedVariables),
    values_from = value,
    values_fill = "Not Selected"
  ) 

##### Get the column names
vars_id <- c("uniqueID", "clusterID", "strata",
             "FormNumber", "weightsDesignNonResponseRaked")

vars_expanded <- discard(colnames(data_used_temp),
                         ~. %in% vars_id)

data_used <- data_used_temp %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(vars_expanded)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>> Use current column names as the axis text in the graph 
selectedVariable_Desc <- tibble(VARNAME = vars_expanded) %>% 
  mutate(VAR_LABEL = VARNAME)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Selected", 
                               "Not Selected"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- c("#F9FEFB", 
             "#1A9850")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = vars_expanded,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Selected"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> vii.	Voting patterns - vote_infosrc -----
##### |>> Select Variables
selectedVariables <- "vote_infosrc"

selected_Title <- c("Primary Source of Information on Electoral Candidates")

##### |>>> Select Data
data_used_temp <- clusterData_Full_Assigned %>%
  select(uniqueID, selectedVariables) %>% 
  separate_rows(selectedVariables, sep = ", ") %>% 
  # drop_na is useful here if you don't want a column named `<NA>`
  tidyr::drop_na(selectedVariables) 

##### clean options
unique(data_used_temp[, selectedVariables])

data_used_temp <- clusterData_Full_Assigned %>%
  select(uniqueID, clusterID, strata,
         FormNumber, weightsDesignNonResponseRaked,
         selectedVariables) %>% 
  separate_rows({{selectedVariables}}, sep = ", ") %>% 
  filter(!is.na(selectedVariables)) %>% 
  mutate(across(all_of(selectedVariables),
                ~case_when(
                  . == "I do not look for information on the candidates." ~ "Do not look for info",
                  T ~ .
                ))) %>% 
  distinct() %>% 
  mutate(value = "Selected") %>% 
  pivot_wider(
    names_from = !!(selectedVariables),
    values_from = value,
    values_fill = "Not Selected"
  ) 

##### Get the column names
vars_id <- c("uniqueID", "clusterID", "strata",
             "FormNumber", "weightsDesignNonResponseRaked")

vars_expanded <- discard(colnames(data_used_temp),
                         ~. %in% vars_id)

data_used <- data_used_temp %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(vars_expanded)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Civic Welfare and Training Service (CWTS)" ~ "CWTS",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>> Use current column names as the axis text in the graph 
selectedVariable_Desc <- tibble(VARNAME = vars_expanded) %>% 
  mutate(VAR_LABEL = VARNAME)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_relevel(level, 
                             c("Selected", 
                               "Not Selected"))) %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- c("#F9FEFB", 
             "#1A9850")

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = vars_expanded,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Selected"),
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  facet_row_combined = 1,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = c("Civic Advocates",
                            "Reliable Supporters",
                            "Drawable Crowd",
                            "Disengaged Youth")
)

graph

##### |> viii.	Voting patterns - vote_comms -----
##### |>> Select Variables
selectedVariables <- "vote_comms"

selected_Title <- c("Most effective communication in understanding candidates")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "A leader that is inclusive and open. A participative leading style that involves a team of people to collaborate and make critical decisions together and as a team." ~ "Inclusive, open, and participative; makes critical decisions with a team",
      . == "A leader that is a visionary. Encourages resourcefulness and innovative thinking in tackling challenging goals while sticking to deadlines." ~ "Visionary, resourceful, and innovative; tackles challenging goals within deadlines",
      . == "A leader that is aggressive. Needs to be in-charge and leadership to be based on control. Expects promptness and perfection from the people him/her." ~ "Aggressive; leadership based on control; expects promptness and perfection",
      . == "A leader that is hands-off and allows its group members to make decisions. Trusts that people make good decisions while periodically monitoring their performance and offers ongoing feedback." ~ "Hands-off; trusts people to make good decisions; offers feedback and monitoring",
      . == "A leader that invests on building its people’s long-term strengths.  Helps people set goals to work towards and give them regular feedback they need to reach success." ~ "Invests in building people's strengths",
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "Set3", n = 10)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 5,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph











##### |> ix.	Voter registration -----
##### |>> Select Variables
selectedVariables <- "voter"

selected_Title <- c("Are you currently registered to vote?")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "No, I am of voting-age and I plan to register before the 2025 Elections." ~ "No but will plan to register before the elections",      
      . == "No, I am of voting-age but I will not register for the 2025 Elections." ~ "No and will NOT plan to register before the elections",      
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level)) %>% 
  mutate(level = fct_rev(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "Set1", n = 3)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 2,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))

graph

##### |> x.	Plan to vote in 2025 -----
##### |>> Select Variables
selectedVariables <- "vote2025"

selected_Title <- c("Do you plan to vote in the upcoming 2025 Philippine Elections?")

##### |>>> Select Data
data_used <- clusterData_Full_Assigned %>% 
  # filter(YearLevel != "1st years") %>% 
  select(
    # IDs
    # uniqueID, clusterID, strata,
    # YearLevel, FormNumber, weightsDesignNonResponseRaked,
    uniqueID, clusterID, strata,
    FormNumber, weightsDesignNonResponseRaked,
    # Selected Variables 
    all_of(selectedVariables)) %>% 
  mutate(across(
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~case_when(
      . == "Yes, I am currently registered to vote." ~ "Yes",      
      . == "No, I will not vote but I am eligible to vote by the 2025 Elections." ~ "No (Eligible)",   
      . == "No, since I am not eligible to vote in the 2025 Elections." ~ "No (Ineligible)",   
      T ~ .
    )
  )) %>%
  mutate(across(
    # .cols = -c(uniqueID, clusterID, strata,
    #            YearLevel, FormNumber, weightsDesignNonResponseRaked),
    .cols = -c(uniqueID, clusterID, strata,
               FormNumber, weightsDesignNonResponseRaked),
    ~as.factor(.)
  ))

nrow(data_used)

##### |>>> Specify value labels 
value_levels <- data_used %>% 
  # select(-c(uniqueID, clusterID, strata,
  #           YearLevel, FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(-c(uniqueID, clusterID, strata,
            FormNumber, weightsDesignNonResponseRaked)) %>% 
  select(1) %>% 
  rename(level = 1) %>% 
  distinct(level) %>% 
  mutate(level = as.factor(level)) %>% 
  mutate(level = fct_rev(level))

value_levels

value_levels_used <-  value_levels %>% 
  mutate(level = fct_rev(level)) 

##### |>>> Run graph function
palette <- brewer.pal(name = "Set1", n = 3)

graph <- graph_single(
  selectedVariable_name = selectedVariables,
  dataWeighted_Used = data_used,
  value_levels = levels(value_levels_used$level),
  guide_rows = 1,
  palette = palette,
  selected_Title = selected_Title,
  geom_text = T,
  unweighted = F,
  weights = "weightsDesignNonResponseRaked",
  bootstrap = F, # bootstrap
  subset_variable = "clusterID",
  subset_variable_order = rev(c("Civic Advocates",
                                "Reliable Supporters",
                                "Drawable Crowd",
                                "Disengaged Youth")))
graph











