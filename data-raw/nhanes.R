# Create NHANES dataset for the workshop
library(RNHANES)
library(here)
library(tidyverse)


# Search air pollution and health data
files <- nhanes_data_files()
variables <- nhanes_variables()
nhanes_search(files, "environmental")
nhanes_search(files, "Air Quality")

nhanes_search(files, "metal")


nhanes_search(variables, "", data_file_name == "AQQ_E")
nhanes_search(variables, "", data_file_name == "AQQ_F")

nhanes_search(files, "demo")
nhanes_search(variables, "age")

nhanes_search(files, "Medical Conditions")
nhanes_search(variables, "asthma")
nhanes_search(variables, "", data_file_name == "MCQ_E")

# Files for the workshop
# DEMO
# MCQ
# AQQ
# UHM
# Cycles: "2009-2010", "2007-2008"

# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/AQQ_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/MCQ_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/UHM_E.htm

data <- nhanes_load_data(rep(c("AQQ", "DEMO", "MCQ", "UHM"), each = 2), rep(c("2007-2008", "2009-2010"), 4))

nh2007 <- dplyr::inner_join(data[[3]], data[[1]], by = c("SEQN", "cycle")) |>
  dplyr::inner_join(data[[5]], by = c("SEQN", "cycle")) |>
  dplyr::inner_join(data[[7]], by = c("SEQN", "cycle"))

nh2009 <- dplyr::inner_join(data[[4]], data[[2]], by = c("SEQN", "cycle")) |>
  dplyr::inner_join(data[[6]], by = c("SEQN", "cycle")) |>
  dplyr::inner_join(data[[8]], by = c("SEQN", "cycle"))

nh2007 <- nh2007 |>
  dplyr::mutate(
    id = SEQN,
    gender = RIAGENDR,
    age_screening = RIDAGEYR, # 80 == >= 80,
    education = DMDEDUC2,
    education_child = DMDEDUC3,
    marital_status = DMDMARTL,
    creatinine = URXUCR,
    lead = URXUPB,
    barium = URXUBA,
    cadmium = URXUCD,
    asthma = MCQ010,
    heart_failure = MCQ160B,
    coronary_heart_disease = MCQ160C,
    heart_attack = MCQ160E,
    stroke = MCQ160F,
    chronic_bronchitis = MCQ160K,
    cancer = MCQ220,
    .keep = "none"
  ) |>
  dplyr::mutate(
    gender = factor(gender),
    education = factor(education),
    dplyr::across(heart_failure:cancer, \(x) x == 1)
  ) |>
  dplyr::filter(
    !is.na(creatinine),
    !is.na(lead),
    !is.na(barium),
    !is.na(cadmium)
  )


nh2009 <- nh2009 |>
  dplyr::mutate(
    id = SEQN,
    gender = RIAGENDR,
    age_screening = RIDAGEYR, # 80 == >= 80,
    education = DMDEDUC2,
    education_child = DMDEDUC3,
    marital_status = DMDMARTL,
    creatinine = URXUCR,
    lead = URXUPB,
    barium = URXUBA,
    cadmium = URXUCD,
    asthma = MCQ010,
    heart_failure = MCQ160B,
    coronary_heart_disease = MCQ160C,
    heart_attack = MCQ160E,
    stroke = MCQ160F,
    chronic_bronchitis = MCQ160K,
    cancer = MCQ220,
    .keep = "none"
  ) |>
  dplyr::mutate(
    gender = factor(gender),
    education = factor(education),
    dplyr::across(heart_failure:cancer, \(x) x == 1)
  ) |>
  dplyr::filter(
    !is.na(creatinine),
    !is.na(lead),
    !is.na(barium),
    !is.na(cadmium)
  )

save(nh2007, file = here("data/nh2007.RData"))
save(nh2009, file = here("data/nh2009.RData"))

## Codebook details from NHANES
# Gender
# tibble::tribble(
#   ~Code.or.Value, ~Value.Description, ~Count, ~Cumulative, ~Skip.to.Item,
#              "1",             "Male",  5096L,       5096L,            NA,
#              "2",           "Female",  5053L,      10149L,            NA,
#              ".",          "Missing",     0L,      10149L,            NA
#   )
#
# RIDRETH1 - Race/Ethnicity
# Race/Ethnicity
# tibble::tribble(
#   ~Code.or.Value,                    ~Value.Description, ~Count, ~Cumulative, ~Skip.to.Item,
#              "1",                    "Mexican American",  2157L,       2157L,            NA,
#              "2",                      "Other Hispanic",  1201L,       3358L,            NA,
#              "3",                  "Non-Hispanic White",  4115L,       7473L,            NA,
#              "4",                  "Non-Hispanic Black",  2211L,       9684L,            NA,
#              "5", "Other Race - Including Multi-Racial",   465L,      10149L,            NA,
#              ".",                             "Missing",     0L,      10149L,            NA
#   )
#
# DMDEDUC2 - Education Level - Adults 20+
#   tibble::tribble(
#     ~Code.or.Value,                                   ~Value.Description,
#     "1",                                "Less Than 9th Grade",
#     "2", "9-11th Grade (Includes 12th grade with no diploma)",
#     "3",                 "High School Grad/GED or Equivalent",
#     "4",                          "Some College or AA degree",
#     "5",                          "College Graduate or above",
#     "7",                                            "Refused",
#     "9",                                         "Don't Know",
#     ".",                                            "Missing"
#   )

#
# DMDMARTL - Marital Status
#
# tibble::tribble(
#   ~Code.or.Value,    ~Value.Description, ~Count, ~Cumulative, ~Skip.to.Item,
#              "1",             "Married",  3116L,       3116L,            NA,
#              "2",             "Widowed",   562L,       3678L,            NA,
#              "3",            "Divorced",   657L,       4335L,            NA,
#              "4",           "Separated",   203L,       4538L,            NA,
#              "5",       "Never married",   992L,       5530L,            NA,
#              "6", "Living with partner",   401L,       5931L,            NA,
#             "77",             "Refused",     4L,       5935L,            NA,
#             "99",          "Don't know",     0L,       5935L,            NA,
#              ".",             "Missing",  4214L,      10149L,            NA
#   )
#
#
# DMDEDUC3 - Education Level - Children/Youth 6-19
# URXUCR - Creatinine, urine (mg/dL)
# URXUPB = "Lead, urine (ug/L)",
# URXUBA - Barium, urine (ug/L)
# URXUCD - Cadmium, urine (ug/L),
# PAQ685 - Bad air quality change activities
#
# tibble::tribble(
#   ~Value,                              ~Value.Description, ~Count, ~Cumulative, ~Skip.to.Item,
#      "1",                                           "Yes",   777L,        777L,            NA,
#      "2",                                            "No",  5224L,       6001L,            NA,
#      "3", "SP never thought/not informed bad air quality",   544L,       6545L,            NA,
#      "7",                                       "Refused",     0L,       6545L,            NA,
#      "9",                                    "Don't know",     1L,       6546L,            NA,
#      ".",                                       "Missing",     0L,       6546L,            NA
#   )
#
#
# MCQ010 - Ever been told you have asthma
# MCQ160B - Ever told had congestive heart failure
# MCQ160C - Ever told you had coronary heart disease
# MCQ160E - Ever told you had heart attack
# MCQ160F - Ever told you had a stroke
# MCQ160K - Ever told you had chronic bronchitis
# MCQ160L - Ever told you had any liver condition
# MCQ220 - Ever told you had cancer or malignancy
