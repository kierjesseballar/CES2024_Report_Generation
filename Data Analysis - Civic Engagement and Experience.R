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
courses_mapped <- read_xlsx("CES 2024 - Courses for Classification, Thea Classified v24Feb2025.xlsx",
                            sheet = 2) %>% 
  rename(degree_classification = Categories) 

##### |> Read Data ------
rawData <- read_xlsx("CES 2024 - All Schools, Weighted v7Jul2025.xlsx",
                     guess_max = 7000,
                     sheet = "Data") %>% 
  mutate(uniqueID = str_c(schoolNameSample, studentID, degree, 
                          sex, svydate,
                          sep = "-")) %>% 
  relocate(uniqueID) %>% 
  left_join(courses_mapped)

##### |> Read Dictionary and MICE Guide -----
dictionary <- read_xlsx("datadictionary_20250126.xlsx", sheet = 2) %>% 
  select(VARNAME, VAR_LABEL, Year, Module) %>% 
  mutate(Module = case_when(
    is.na(Module) ~ "Not SQD",
    T ~ Module
  ))

miceGuide <- read_xlsx("ces2024_SQDvars_20250127_SetGuide - v24Feb2025.xlsx")

##### |> LCA Clustering Object -----
finalLCAModel <- readRDS("finalLCAModelNClass_4_v15Feb2025_CES2024Forum.rds")
finalLCAModel[["call"]]

