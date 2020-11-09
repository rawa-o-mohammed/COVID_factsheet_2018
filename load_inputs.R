question <-
  read.csv(
    "input/kobo/kobo_questions.csv",
    stringsAsFactors = F
  )
choice <-
  read.csv(
    "input/kobo/kobo_choices.csv",
    stringsAsFactors = F
  )

data <-
  read.csv(
    "input/dataset/2018_household level_dataset.csv",
    stringsAsFactors = F,
    na.strings = c("", "na", "NA", "NaN", "#N/A")
  )

loop <-
  read.csv(
    "input/dataset/2018_individual level_dataset.csv",
    stringsAsFactors = F,
    na.strings = c("", "na", "NA", "NaN", "#N/A")
  )

dap <-
  read.csv(
    sprintf("input/dap/dap_%s.csv", dap_name),
    stringsAsFactors = F
  )
dap$repeat.for.variable <- aggregate
dap$independent.variable <- disaggregate
lookup_table <-
  read.csv(
    "input/lookup_tables/lookup_table_names.csv",
    stringsAsFactors = F
  )
sampling_frame <-
  read.csv(
    "input/sampling_frame/sampling_frame.csv",
    stringsAsFactors = F
  )