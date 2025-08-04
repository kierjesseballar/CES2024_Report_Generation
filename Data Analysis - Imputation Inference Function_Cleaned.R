mice_impute_subset <- function(cluster_data = NULL,
                               weights = "weightsDesignNonResponseRaked",
                               miceObject = NULL,
                               unweighted = F,
                               boostrap = F,
                               subset_variable = "clusterID",
                               ## CHANGED ##: Default for order is now NULL to be more generic.
                               subset_variable_order = NULL,
                               uniqueIDMap = NULL
                               ){
  library(mice)
  library(mitools)
  
  ##### Load MICE Object
  miceData <- miceObject[[1]][[1]]
  miceTarget <- miceObject[[1]][[2]]
  miceTarget
  
  if(!is.null(cluster_data)){
    ##### Load Unique ID and Row ID Mapping 
    uniqueID_rowID_map <- uniqueIDMap
    
    ##### Get subset variable and unique ID
    ## CHANGED ##: Selects the column specified in `subset_variable` instead of hardcoding `clusterID`.
    uniqueID_subsetID_map <- cluster_data %>%
      select(uniqueID, all_of(subset_variable))
    
    ##### Convert the MICE Object to long data
    miceDataLong <- mice::complete(miceObject[[1]][[1]],
                                   action = "long",
                                   include = TRUE) %>% 
      mutate(rowid = if_else(row_number() %% nrow(miceData[["data"]]) == 0,  nrow(miceData[["data"]]),
                             row_number() %%  nrow(miceData[["data"]]))) %>% 
      left_join(uniqueID_rowID_map) %>%
      relocate(rowid, uniqueID) %>%
      left_join(uniqueID_subsetID_map) %>% ## CHANGED ##: Joins the dynamically selected subset variable.
      ## CHANGED ##: Relocates the column specified in `subset_variable`.
      relocate(rowid, uniqueID, all_of(subset_variable)) %>% 
      ## CHANGED ##: Filters NAs from the column specified in `subset_variable`.
      filter(!is.na(.data[[subset_variable]])) %>% 
      ## CHANGED ##: Relocates the column specified in `subset_variable` again.
      relocate(`.imp`, `.id`, uniqueID, all_of(subset_variable))  
    
    ## CHANGED ##: The print statement is now more generic.
    print(str_c("Imputed ", subset_variable, " equal to cluster data length?: ",
                nrow(miceDataLong)/21 == nrow(cluster_data)))
  } else{
    miceDataLong <- mice::complete(miceObject[[1]][[1]],
                                   action = "long",
                                   include = TRUE)
  }
  
  
  ##### |> Make a mids object to signal that it is an imputed dataset
  midsObject <- as.mids(miceDataLong)
  
  ##### |> Extract the imputed datasets for the survey R package 
  miceExtractedList <- lapply(seq(midsObject$m),
                              function(im) complete(midsObject, im)) %>% 
    imputationList()
  
  ##### |> Estimate the parameter from the imputed datasets
  ######### |>> Make survey objects -----
  weights_used <- weights
  weights_formula <- str_c("~", weights_used) %>% 
    as.formula()
  
  if(!is.null(cluster_data)){
    subset_categories <- cluster_data %>% 
      select(all_of(subset_variable)) %>% 
      distinct() %>% 
      pull()
    
  } else{
    subset_categories <- miceDataLong %>% 
      select(any_of(subset_variable)) %>% 
      distinct() %>% 
      pull()
  }
  
  if(unweighted == T){
    miceExtractedList <- miceExtractedList %>% 
      mutate(weights = 1)
    
    surveyObject_Subsets <- list()
    for(i in 1:length(subset_categories)){
      selected_subset <- subset_categories[[i]]
      
      surveyObject <- svydesign(ids = ~1,
                                strata = ~strata,
                                weights = ~weights,
                                data = miceExtractedList) %>% 
        subset(get(subset_variable) == selected_subset)
      
      surveyObject_Subsets[[i]] <- surveyObject
    }
  } else if(unweighted == F){
    
    surveyObject_Subsets <- list()
    for(i in 1:length(subset_categories)){
      selected_subset <- subset_categories[[i]]
      
      if(boostrap == T){
        surveyObject <- svydesign(ids = ~1,
                                  strata = ~strata,
                                  weights = weights_formula,
                                  data = miceExtractedList) %>% 
          subset(get(subset_variable) == selected_subset) %>%
          as.svrepdesign(type = "bootstrap")
        
      } else if(boostrap == F){
        surveyObject <- svydesign(ids = ~1,
                                  strata = ~strata,
                                  weights = weights_formula,
                                  data = miceExtractedList) %>% 
          subset(get(subset_variable) == selected_subset) 
      }
      
      surveyObject_Subsets[[i]] <- surveyObject
    }
  }
  
  ##### >> Estimate parameter of interest 
  ##### >>> Mean -----
  formula <- str_c("~", miceTarget) %>% 
    as.formula()
  
  # Get separate estimates per imputed dataset
  estimatesPerImputedData_container <- list()
  
  for(i in 1:length(subset_categories)){
    selectedSurveyObject <- surveyObject_Subsets[[i]]
    
    estimatesPerImputedData <- with(selectedSurveyObject,
                                    svymean(formula, 
                                            se = T, 
                                            na.rm = T, 
                                            ci = T))
    
    estimatesPerImputedData_container[[i]] <- list()
    estimatesPerImputedData_container[[i]][[1]] <- estimatesPerImputedData
    estimatesPerImputedData_container[[i]][[2]] <- data.frame(selectedSurveyObject[["designs"]][[1]][["variables"]]) %>% 
      select(any_of(subset_variable)) %>% 
      distinct() %>% 
      pull()
    
  }
  
  ##### >> Combine each of the imputed estimates throug Rubin's Rules
  # missInfor = rate of missing information statistic as per literature
  estimatesRubinCombined_Container <- list()
  
  for(i in 1:length(subset_categories)){
    selected_estimatesPerImputedData <- estimatesPerImputedData_container[[i]][[1]]
    selected_subset_category <- estimatesPerImputedData_container[[i]][[2]]
    
    estimatesRubinCombined <- summary(MIcombine(selected_estimatesPerImputedData)) %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      ## CHANGED ##: Column name is now dynamic based on the subset_variable.
      mutate(!!subset_variable := selected_subset_category)
    
    estimatesRubinCombined_Container[[i]] <- estimatesRubinCombined
  }
  
  estimatesRubinCombined_Subsets <- rbindlist(estimatesRubinCombined_Container)
  
  ## CHANGED ##: Renaming here is not strictly necessary if the above is done, but leaving for consistency.
  ## This part might need adjustment in your downstream script depending on desired final column names.
  ## For now, I have commented it out to avoid potential conflicts.
  # estimatesRubinCombined_Subsets <- estimatesRubinCombined_Subsets %>%
  #   rename(subset_category = all_of(subset_variable))
  
  return(estimatesRubinCombined_Subsets)
}