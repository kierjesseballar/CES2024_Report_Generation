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
library(openxlsx)
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

##### || FIT INDICES -----
##### >> Load Preliminary Models -----
lcaModelContainers <- read_rds("lcaModelContainersNClass2To8_v18Jul2025_CES2024.rds")

##### Get fit indices
# finalLCAModel[["llik"]] # log likelihood
# finalLCAModel[["bic"]] # BIC
# finalLCAModel[["aic"]] # AIC
# finalLCAModel[["npar"]] # npar
# finalLCAModel[["resid.df"]] # residual df
# finalLCAModel[["Gsq"]] # Likelihood ratio/deviance statistic

# p-value of the chi-square distributed test statistics
# note G² = -2 * (LL_your_model - LL_saturated), where LL_sat always greater than LL_model
# 1 - pchisq(finalLCAModel[["Chisq"]], 
#        df = finalLCAModel[["resid.df"]], 
#        lower.tail = T)
# 
# pchisq(finalLCAModel$Chisq, finalLCAModel$resid.df, lower.tail = F)
# 
# 1 - pchisq(finalLCAModel[["Gsq"]], 
#        df = finalLCAModel[["resid.df"]], 
#        lower.tail = T)
# 
# G2 <- -2 * (lcaModelContainers[[4]]$llik - lcaModelContainers[[5]]$llik)
# diff_df <- lcaModelContainers[[5]]$npar - lcaModelContainers[[4]]$npar
# 
# 1 - pchisq(G2, diff_df, lower.tail = T)

##### |> Get fit indices -----
fit_table <- data.frame(
  Cluster = 2:7,
  LL = numeric(6),
  BIC = numeric(6),
  AIC = numeric(6),
  Npar = numeric(6),
  L2 = numeric(6),
  df = numeric(6),
  p_value = numeric(6)
)

# Loop through the number of classes from 1 to 5
for (i in (2-1):(7-1)) {
  lca_model <- lcaModelContainers[[i+1]]
  
  # Extract the fit indices
  fit_table$LL[i] <- lca_model$llik
  fit_table$BIC[i] <- lca_model$bic
  fit_table$AIC[i] <- lca_model$aic
  fit_table$Npar[i] <- lca_model$npar
  fit_table$L2[i] <- lca_model$Gsq
  fit_table$df[i] <- lca_model$resid.df
  
  fit_table$p_value[i] <- 1 - pchisq(lca_model$Gsq, lca_model$resid.df,
                                     lower.tail = TRUE)
                                     
  
}

##### |> Run LRT for nested models with n_class -----
num_models <- length(lcaModelContainers)
num_comparisons <- num_models - 2 # e.g., for models 2-7, we have 5 comparisons

lrt_table <- data.frame(
  Comparison = character(num_comparisons),
  LR_Statistic = numeric(num_comparisons),
  df_diff = numeric(num_comparisons),
  p_value = numeric(num_comparisons),
  stringsAsFactors = FALSE
)

# Loop through the models to perform pairwise comparisons
for (k in 2:(num_models - 1)) {
  
  simple_model <- lcaModelContainers[[k]]
  complex_model <- lcaModelContainers[[k+1]]
  
  # Perform the Likelihood-Ratio Test 
  lr_stat <- -2 * (simple_model$llik - complex_model$llik)
  diff_df <- complex_model$npar - simple_model$npar
  p_value_lrt <-  1 - pchisq(lca_model$Gsq, lca_model$resid.df,
                             lower.tail = TRUE)
  row_idx <- k - 1 
  
  lrt_table$Comparison[row_idx] <- paste(k, "vs", k + 1)
  lrt_table$LR_Statistic[row_idx] <- lr_stat
  lrt_table$df_diff[row_idx] <- diff_df
  lrt_table$p_value[row_idx] <- p_value_lrt
}


##### || Scree Plots -----
graph_data <- fit_table %>% 
  select(Cluster, BIC, AIC) %>% 
  pivot_longer(-Cluster) %>% 
  rename(`Fit Index` = name)

ggplot(graph_data) +
  geom_line(aes(
    x = Cluster,
    y = value, 
    color = `Fit Index`), 
    linewidth = 1) +
  geom_point(aes(
    x = Cluster,
    y = value, 
    color = `Fit Index`),
    size = 1.5) +
  theme(legend.position = "bottom")


##### || Check between 3 and 4 -----
lcaModelContainers[[3]]
lcaModelContainers[[4]]
lcaModelContainers[[5]]

##### || Save Results -----
##### |> Get final model -----
finalLCAModel <- readRDS("finalLCAModelNClass_4_rep_5000_v18Jul2025_CES2024.rds")
rds_name <- "finalLCAModelNClass_4_rep_5000_v18Jul2025_CES2024"
finalLCAModel

##### |> Get variable labels -----
dictionary <- read_xlsx("ces2024_datadictionary_20250126.xlsx", 
                        sheet = 2) %>% 
  select(VARNAME, VAR_LABEL, Year, Module) %>% 
  mutate(Module = case_when(
    is.na(Module) ~ "Not SQD",
    T ~ Module
  ))

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
    VARNAME == 'civicaction_advocacy' ~ 'Formed an advocacy group',
    VARNAME == 'civicaction_grant' ~ 'Received grant to implement community programs',
    VARNAME == 'civicaction_project' ~ 'Implemented CSO project',
    VARNAME == 'civicaction_follow' ~ 'Followed political figure on social media',
    VARNAME == 'civicaction_likeshare' ~ 'Liked or shared soc-pol posts on social media',
    VARNAME == 'civicaction_post' ~ 'Posted thoughts on soc-pol issues on social media',
    VARNAME == 'civicaction_discussonline' ~ 'Discussed soc-pol issues on the internet',
    VARNAME == 'civicaction_soughtnews' ~ 'Sought out news about political issues',
    VARNAME == 'civicaction_discussoffline' ~ 'Discussed soc-pol issues in person',
    T ~ VAR_LABEL
  ))

##### |> Create excel sheet of results -----
##### |>> Specify variable order -----
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

# Combine all axes into a single, ordered vector
ordered_variables <- c(axis1.a, axis1.b, axis2, axis3)

##### |>> Create excel table -----
probs_list <- finalLCAModel$probs
all_parts <- list()

for (var_name in ordered_variables) {
  if (!var_name %in% names(probs_list)) {
    warning(paste("Variable '", var_name, "' not found. Skipping."))
    next
  }
  
  # --- NEW: Look up the descriptive label ---
  var_label <- selectedVariable_Desc %>%
    filter(VARNAME == var_name) %>%
    pull(VAR_LABEL)
  # If no label is found, use a placeholder
  if (length(var_label) == 0) { var_label <- "No label found" }
  
  
  # Create the parts for this variable's table
  tech_header_df <- data.frame(V1 = var_name)
  label_header_df <- data.frame(V1 = var_label) # The new label row
  
  prob_matrix <- probs_list[[var_name]]
  data_df <- as.data.frame(prob_matrix) %>%
    mutate(across(everything(), ~sprintf("%.4f", .))) %>%
    mutate(Class = paste("class", 1:n(), ":"), .before = 1)
  colnames(data_df) <- c("Class", paste("Pr(", 1:(ncol(data_df)-1), ")", sep=""))
  
  # Add ALL parts to the list, including the new label header
  all_parts <- append(all_parts, list(tech_header_df, label_header_df, data_df, data.frame(V1="")))
}

final_df <- bind_rows(all_parts)


# 4. WRITE TO EXCEL WITH ENHANCED STYLING

wb <- createWorkbook()
addWorksheet(wb, "Conditional Probs")
writeData(wb, sheet = "Conditional Probs", x = final_df, colNames = FALSE)

# Save the workbook to a file
name <- str_c(rds_name, " - Results", ".xlsx")

saveWorkbook(wb, name, overwrite = TRUE)



