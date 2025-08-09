library(readxl)
library(poLCA)
library(dplyr)
library(stringr)

##### Read Data ------
rawData <- read_xlsx("CES 2024 - All Schools, Weighted v7Jul2025.xlsx",
                     guess_max = 10000,
                     sheet = 1) %>% 
  mutate(uniqueID = str_c(schoolNameSample, studentID, degree, 
                          sex, svydate,
                          sep = "-")) %>% 
  relocate(uniqueID) %>% 
  distinct(uniqueID, .keep_all = TRUE)

unique(rawData$finaleligibility)

# Check for uniqueID duplicates
length(unique(rawData$uniqueID)) == nrow(rawData)

all_duplicated_rows <- rawData %>%
  group_by(uniqueID) %>%
  filter(n() > 1) %>%
  ungroup() %>% 
  arrange(uniqueID) 

##### Select Civic Engagement Clustering Variables -----
clusterData <- rawData %>% 
  select(uniqueID,
         starts_with(c("civicaction_follow", "civicaction_likeshare", 
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
                       "civicaction_positionpaper", "civicaction_lobbying"))) %>% 
  mutate(
    across(.cols = -c(uniqueID),
           ~case_when(
             . == "Once in the past yea" ~ "Once in the past year",
             T ~ . 
           )
    )
  ) %>% 
  mutate(
    across(.cols = -c(uniqueID),
           ~case_when(
             . == "Decline to answer" ~ NA,
             . == "I have not done this and I would not do it regardless of the situation" ~ 1,
             . == "I  have not done this, but I might do it if something important happens in the future." ~ 2,
             . == "Once in the past year" ~ 3,
             . == "2-3 times in the past year" ~ 4,
             . == "More than 3 times in the past year" ~ 5
           )
    )
  ) 

##### LCA Clustering -----
modelForm <- cbind(civicaction_follow,
                   civicaction_likeshare,
                   civicaction_blog,
                   civicaction_post,
                   civicaction_article,
                   civicaction_discussonline,
                   civicaction_soughtnews,
                   civicaction_discussoffline,
                   civicaction_donatedcharity,
                   civicaction_contactgovt,
                   civicaction_elected,
                   civicaction_joinedonline,
                   civicaction_joinedoffline,
                   civicaction_union,
                   civicaction_movement,
                   civicaction_strike,
                   civicaction_advocacy,
                   civicaction_grant,
                   civicaction_project,
                   civicaction_donatedpol,
                   civicaction_finsupport,
                   civicaction_publichearings,
                   civicaction_positionpaper,
                   civicaction_lobbying) ~ 1

##### | Check AIC of Clusters BEFOREHAND -----
##### |> Final Decision: Use 4 for final model -----
startTime <- Sys.time()

set.seed(102)
nrep = 10*500 # 1 min per 10 nreps in COARE

finalLCAModel <- poLCA(formula = modelForm, 
                       data = clusterData, 
                       nclass = 4, 
                       maxiter = 100000, 
                       graphs = FALSE,
                       nrep = nrep, # for global maxima as per EM algorithm, avoid local
                       # for finding number of clusters, set nrep = 20 first
)

file_name <- str_c("finalLCAModelNClass_4_rep_", nrep, "_v18Jul2025_CES2024.rds")
saveRDS(finalLCAModel, file_name)

endTime <- Sys.time()

endTime - startTime

