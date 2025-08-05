R Function Documentation
This document provides a concise overview and usage guide for the custom R functions used for data visualization and analysis. The examples are based on their implementation in Report Generation - Sample with Functions v4Jul2025.R.

graph_section()
Creates a faceted plot of horizontal stacked bar charts. This function is ideal for comparing multiple related survey questions (a "section") across different groups (e.g., Year Levels).

Purpose
To visualize and compare the distribution of responses for several variables at once. The resulting plot is faceted by a specified grouping variable, allowing for easy comparison between groups.

Usage

graph_section(
  dataWeighted_Used,
  selectedVariables_vec,
  selectedVariable_Desc_df,
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
  subset_variable = NULL,
  overall_include = FALSE,
  subset_variable_order = NULL
)

Key Arguments
dataWeighted_Used: The input data.frame containing the survey data.

selectedVariables_vec: A character vector of the column names for the variables to be plotted.

selectedVariable_Desc_df: A data.frame that maps variable names (VARNAME) to their desired labels for the graph (VAR_LABEL).

rank_by_value: A character vector specifying one or more response levels. The variables in the plot will be ordered based on the combined percentage of this/these level(s) in the first group defined by subset_variable_order.

value_levels: A character vector defining the order of the levels in the stacked bar chart (e.g., from "Very Important" to "Very Unimportant").

palette: A character vector of hex codes for the bar chart colors.

selected_Title: The main title for the graph.

subset_variable: The column name of the grouping variable used to create the facets. If NULL, a single "Overall" graph is produced without facets.

subset_variable_order: A character vector defining the display order of the facets.

Example
From "Example 1: Job Factors", this function plots several job_* variables, faceted by YearLevel. The variables are ranked by the percentage of "Very Important" responses among Freshmen.

graph <- graph_section(
  dataWeighted_Used = data_used,
  selectedVariables_vec = selectedVariables,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = c("Very Important"),
  value_levels = levels(value_levels_used$level),
  palette = palette,
  selected_Title = "Important Job Factors across Year Levels",
  subset_variable = "YearLevel",
  subset_variable_order = c("Freshmen", "Sophomores", "Juniors", "Seniors")
)

<br>

graph_single()
Creates a single horizontal stacked bar chart to visualize the response distribution for one variable across different groups.

Purpose
To compare how different groups or subsets responded to a single survey question. Each group is represented by a separate bar.

Usage
graph_single(
  dataWeighted_Used,
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
  subset_variable = NULL,
  overall_include = FALSE,
  subset_variable_order = NULL
)

Key Arguments
dataWeighted_Used: The input data.frame.

selectedVariable_name: The column name of the single variable to plot.

value_levels: A character vector defining the order of the levels within the stacked bars.

palette: A character vector of hex codes for the bar chart colors.

selected_Title: The main title for the graph.

subset_variable: The column name of the grouping variable. Each group will be a separate bar on the y-axis.

subset_variable_order: A character vector defining the order of the bars.

Example
From "Example 2: Overall College Satisfaction", this function creates a chart with four bars (one for each year level), each showing the satisfaction breakdown for that group.

graph <- graph_single(
  dataWeighted_Used = data_used,
  selectedVariable_name = "satisfyOverallCollege",
  value_levels = levels(value_levels_used$level),
  palette = palette,
  selected_Title = "Overall College Experience Satisfaction across Year Levels",
  subset_variable = "YearLevel",
  subset_variable_order = rev(c("Freshmen", "Sophomores", "Juniors", "Seniors"))
)

<br>

mice_impute_subset()
Processes mice imputed data objects to calculate survey-weighted statistics for specified subsets. It is a necessary pre-processing step for graph_sqd_section.

Purpose
To apply Rubin's Rules to combine estimates from multiple imputed datasets, generating a final, survey-weighted summary statistic for a variable across different groups.

Usage
mice_impute_subset(
  cluster_data = NULL,
  weights = "weightsDesignNonResponseRaked",
  miceObject = NULL,
  unweighted = F,
  boostrap = F, 
  subset_variable = "clusterID",
  subset_variable_order = NULL,
  uniqueIDMap = "..."
)

Key Arguments
cluster_data: A data.frame containing the uniqueID and the subset_variable column, used to link imputed data to the correct subset.

miceObject: The imputed object (as a list) from the mice package.

subset_variable: The column name of the grouping variable used for analysis.

weights: The column name for the survey weights.

Example
From "Example 3: Self-rated Traits", this function is run in a loop for each singly-imputed (SQD) variable. It takes the mice object and calculates the weighted mean for each YearLevel.

# Conceptual example from the loop in the script
imputed_statistic <- mice_impute_subset(
  cluster_data = sexism_classification, # Or any other data with subset info
  miceObject = selected_mice_object,
  subset_variable = "YearLevel"
)

<br>

graph_sqd_section()
Creates a faceted plot for singly-imputed (SQD) variables, with a structure similar to graph_section.

Purpose
To visualize and compare response distributions for multiple SQD variables that have been pre-processed using mice_impute_subset.

Usage
graph_sqd_section(
  imputed_statistics,
  selectedVariable_Desc_df,
  rank_by_value = "Selected",
  value_levels = NULL,
  guide_rows = 2,
  palette = NULL,
  selected_Title = NULL,
  geom_text = TRUE,
  facet_row_combined = 2,
  subset_variable,
  subset_variable_order = NULL
)

Key Arguments
imputed_statistics: Crucially, this is the combined data.frame of results generated by mice_impute_subset and any core (non-imputed) variables. It must contain columns for the subset group, variable name, response value (rowname), and frequency (results).

selectedVariable_Desc_df: A data.frame that maps variable names (VARNAME) to their desired labels (VAR_LABEL).

rank_by_value, value_levels, palette, selected_Title, subset_variable, subset_variable_order: These arguments function identically to their counterparts in graph_section.

Example
From "Example 3: Self-rated Traits", this function takes the combined results from the imputed and core variables (mean_imputed_all_vars_used) to generate the final plot.

graph <- graph_sqd_section(
  imputed_statistics = mean_imputed_all_vars_used,
  selectedVariable_Desc_df = selectedVariable_Desc,
  rank_by_value = "Excellent",
  value_levels = levels(value_levels_used$level),
  palette = palette,
  selected_Title = "Self-Rated Skills",
  subset_variable = "YearLevel",
  subset_variable_order = c("Freshmen", "Sophomores", "Juniors", "Seniors")
)
