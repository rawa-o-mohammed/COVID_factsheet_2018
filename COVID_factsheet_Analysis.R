rm(list = ls(all = T))
library(xlsx)
library(plyr)
library(dplyr)
library(koboquest) # manage kobo questionnaires
library(kobostandards) # check inputs for inconsistencies
library(hypegrammaR)
library(surveyweights)
library(expss)

source("postprocessing_functions.R")
source("covid_recoding.R")
######################load files ##########################################
# define how to aggregate or disaggregate the findings by or type "all" for not aggregating or not disaggregating
aggregate <- "all"
disaggregate <- "population_group"
dap_name <- "covid_analysis_2018"
name <- "2018_covid"
source("load_inputs.R")

data <- covid_recoding(data, loop)

weights <- function(data){
  data$weights
}

questionnaire <-
  load_questionnaire(
    data = data,
    questions = question,
    choices = choice,
    choices.label.column.to.use = "label"
  )

result <-
  from_analysisplan_map_to_output(
    data = data,
    analysisplan =  dap,
    weighting = weights,
    questionnaire = questionnaire,
    confidence_level = 0.9
  )

district_gov <-
  unique(data[, c("district", "governorate")])

summary <-
  bind_rows(lapply(result[[1]], function(x) {
    x$summary.statistic
  }))
write.csv(summary,
          sprintf("output/raw_results/raw_results_%s.csv", name),
          row.names = F)
summary <-
  read.csv(sprintf("output/raw_results/raw_results_%s.csv", name),
           stringsAsFactors = F)

summary <- correct.zeroes(summary)

summary <- summary %>% filter(dependent.var.value %in% c(NA, 1))
write.csv(
  summary,
  sprintf("output/raw_results/raw_results_%s_filtered.csv", name),
  row.names = F
)
if (all(is.na(summary$independent.var.value))) {
  summary$independent.var.value <- "all"
}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]

for (i in 1:length(groups)) {
  df <-
    pretty.output(
      summary = summary,
      independent.var.value = groups[i],
      analysisplan =  dap,
      lookup_table = lookup_table,
      district_gov = district_gov
    )
  df <- df[rowSums(is.na(df)) != ncol(df) - 2,]
  df <- df[, colSums(is.na(df)) != nrow(df) - 4]
  if (i == 1) {
    xlsx::write.xlsx(
      df,
      file = sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name),
      sheetName = groups[i],
      row.names = FALSE
    )
  } else {
    xlsx::write.xlsx(
      df,
      file = sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name),
      sheetName = groups[i],
      append = TRUE,
      row.names = FALSE
    )
  }
}