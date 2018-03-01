################################################################################
## Combine CADUCEUS-related data from MENDS2, MOSAIC, INSIGHT
################################################################################

## API tokens stored in .Renviron; some functions sourced from data_functions.r
source("data_functions.R")
  ## calls RCurl, glue, dplyr, stringr

library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

## Things we need from each database:
## - All CADUCEUS-specific variables (CADUCEUS form)
## - Demographic info: dob, enrollment date, gender
## - Enrollment info: MV, sepsis, shock at enrollment
## - Death info: did they die during hospitalization?
## - Daily info: mental status (CAM, RASS)
## These variables may be stored differently in the three databases.

## MENDS2 variable names
## Enrollment: id, dob, gender, enroll_time
## ICU type, MV, sepsis, shock at enrollment: Could not find these
##  One ICU reason = sepsis/septic shock; also hemorrhagic shock
## Death: death_time, hospdis_time
## Daily info: rass_actual_1/2, cam_1/2

## MOSAIC variable names
## Enrollment: id, dob, gender, enroll_dttm
## ICU type, MV, sepsis, shock: organ_fail_present (1 = MV, 3 = shock)
##  One ICU reason = sepsis/septic shock
## Death: death_dttm, hospdis_dttm
## Daily info: rass_actual_1/2, cam_1/2

## INSIGHT variable names
## Enrollment: id, dob, gender_birth + gender_identity, enroll_dttm
## ICU type, MV, sepsis, shock:
##  - resp_random = Yes + resp_present_1 (MV)
##  - shock_random
##  - sepsis: can't find anything (inc ICU admission reason)
## Death: death_dttm, hospdis_dttm
## Daily info: rass_actual_1/2, cam_1/2

## For each study, want:
## 1. one-rec-per-pt df with enrollment, admission variables
## 2. daily df with RASS, CAM, CADUCEUS variables

## -- Baseline, enrollment info for each study ---------------------------------
## Function which extracts the variables we want from a given database, names
## them consistently, and returns a final data.frame with one record per patient
export_oneobs <- function(rctoken, vnames){
  ## Only want data collected on Enrollment / day 1, which is noted slightly
  ## differently in each study. Use `unique_event_name` of the earliest event
  day1 <- get_events(rctoken) %>%
    arrange(day_offset) %>%
    slice(1) %>%
    pull(unique_event_name)
  
  df <- import_df(
    rctoken = rctoken,
    id_field = "id",
    fields = vnames,
    events = day1
  ) %>%
    dplyr::select(-redcap_event_name)
  
  ## Rename variables for consistency
  ## INSIGHT has both birth and identity questions for gender; use current
  ##  identity as final
  if("gender_birth" %in% vnames && "gender_identity" %in% vnames){
    df <- df %>%
      mutate(
        gender_identity = str_replace(gender_identity, " .*$", ""),
        gender = case_when(
          !is.na(gender_identity) & gender_identity == "Transgender" &
            !is.na(gender_birth) & gender_birth == "Male" ~ "Female",
          !is.na(gender_identity) & gender_identity == "Transgender" &
            !is.na(gender_birth) & gender_birth == "Female" ~ "Male",
          TRUE ~ gender_birth
        )
      )
  }

  ## All names are consistent except for date/times
  names(df) <- str_replace(names(df), "\\_time$", "_dttm")

  return(df %>% dplyr::select(id, dob, gender, ends_with("_dttm")))
}

## Extract raw data for each study and combine into a single df
oneobs_df <- map2_dfr(
  .x = paste0(c("MENDS2", "MOSAIC", "INSIGHT"), "_IH_TOKEN"),
  .y = list(
    c("id", "dob", "gender", "enroll_time", "death_time", "hospdis_time"),
    c("id", "dob", "gender", "enroll_dttm", "death_dttm", "hospdis_dttm"),
    c("id", "dob", "gender_birth", "gender_identity", "enroll_dttm",
      "death_dttm", "hospdis_dttm")
  ),
  .f = export_oneobs
) %>%
  mutate_at(
    vars(ends_with("_dttm")),
    ~ as.POSIXct(., format = "%Y-%m-%d %H:%M", tz = "UTC")
  ) %>%
  mutate(
    ## Calculate age at enrollment
    dob = as.Date(dob, format = "%Y-%m-%d"),
    enroll_date = as.Date(enroll_dttm),
    age = as.numeric(difftime(enroll_date, dob, units = "days")) / 365.25,
    
    ## Determine whether patient died in the hospital
    died_inhosp = factor(
      case_when(
        is.na(death_dttm) & is.na(hospdis_dttm)              ~ as.numeric(NA),
        is.na(death_dttm) & !is.na(hospdis_dttm)             ~ 0,
        !is.na(death_dttm) &
          (is.na(hospdis_dttm) | hospdis_dttm >= death_dttm) ~ 1,
        TRUE                                                 ~ 0
      ),
      levels = 0:1,
      labels = c("Survived hospitalization", "Died in hospital")
    )
  )

## -- RASS, CAM, CADUCEUS info for each study ----------------------------------
## Function which extracts the variables we want from a given database, names
## them consistently, and returns a final data.frame with >1 record per patient
export_daily <- function(rctoken, cad_fname){
  df <- import_df(
    rctoken = rctoken,
    id_field = "id",
    fields = c("id", "rass_actual_1", "rass_actual_2", "cam_1", "cam_2"),
    forms = cad_fname
  ) %>%
    ## Remove "form complete" variable
    dplyr::select(-matches("^caduceus\\_.+\\_complete$"))
  
  ## All names are consistent except for enzyme variables; should be two AChE,
  ## one BChE
  names(df) <- str_replace_all(
    names(df),
    c("^cad\\_ache$" = "cad_ache_ul",
      "^cad\\_ache_2" = "cad_ache_ughb",
      "^cad\\_bche$" = "cad_bche_ul")
  )

  return(df)
}

## Extract raw data for each study and combine into a single df
daily_df <- map2_dfr(
  .x = paste0(c("MENDS2", "MOSAIC", "INSIGHT"), "_IH_TOKEN"),
  .y = c("caduceus_2", "caduceus_2", "caduceus_log"),
  .f = export_daily
) %>%
  ## Data cleaning: different capitalizations in different studies
  mutate(
    cad_date = as.Date(cad_date, format = "%Y-%m-%d"),
    cad_no = ifelse(
      tolower(cad_no) == "patient/surrogate refused",
      "Patient/surrogate refused",
      cad_no
    )
  )

# ## skim for reasonableness
# skimr::skim(daily_df)

## Calculate mental status at each assessment, on each day
mental_df <- daily_df %>%
  dplyr::select(
    id, redcap_event_name, starts_with("rass"), starts_with("cam")
  ) %>%
  ## Reshape: eventual goal = one row per assessment, with vars for RASS, CAM
  gather(key = asmt_info, value = asmt_score, rass_actual_1:cam_2) %>%
  mutate(asmt_info = gsub("\\_actual", "", asmt_info)) %>%
  separate(asmt_info, into = c("asmt_type", "asmt_time"), sep = "_") %>%
  spread(key = asmt_type, value = asmt_score) %>%
  ## Data cleaning:
  ## - CAM is misspelled as "Postive" in some studies; UTA spelled differently
  ## - Change all "Not Dones" to NA
  ## - Make RASS numeric
  mutate_at(vars("cam", "rass"), ~ ifelse(toupper(.) == "NOT DONE", NA, .)) %>%
  mutate(
    cam = str_replace_all(
      cam,
      c("Postive" = "Positive",
        "Unable to [Aa]ssess" = "UTA")
    ),
    rass = as.numeric(rass)
  ) %>%
  ## Determine mental status at each time point:
  ## - RASS not missing & -4, -5, OR RASS missing & CAM UTA: coma; otherwise,
  ## - CAM not missing and Positive: delirious; otherwise,
  ## - CAM not missing and Negative: normal; otherwise,
  ## - Normal
  mutate(
    mental_status = case_when(
      !is.na(rass) & rass %in% c(-4, -5) |
        is.na(rass) & cam == "UTA"         ~ "Comatose",
      !is.na(cam) & cam == "Positive"      ~ "Delirious",
      !is.na(cam) & cam == "Negative"      ~ "Normal",
      TRUE                                 ~ as.character(NA)
    )
  )

## Summarize mental status by *day*
mental_day_df <- mental_df %>%
  group_by(id, redcap_event_name) %>%
  summarise(
    n_asmts = sum(!is.na(mental_status)),
    coma = ifelse(
      n_asmts == 0, NA,
      sum(mental_status == "Comatose", na.rm = TRUE) > 0
    ),
    delirium = ifelse(
      n_asmts == 0, NA,
      sum(mental_status == "Delirious", na.rm = TRUE) > 0
    ),
    normal = ifelse(
      n_asmts == 0, NA,
      sum(mental_status == "Normal", na.rm = TRUE) > 0
    )
  ) %>%
  ungroup()

## Merge daily info onto daily_df
daily_df <-
  left_join(daily_df, mental_day_df, by = c("id", "redcap_event_name")) %>%
  ## Calculate study day
  left_join(dplyr::select(oneobs_df, id, enroll_date), by = "id") %>%
  mutate(
    cad_day = as.numeric(difftime(cad_date, enroll_date, units = "days")) + 1
  ) %>%
  ## Keep only needed variables
  dplyr::select(id, cad_day, cad_yn:cad_bche_ul, coma:normal)

## Summarize mental status by *patient* (days of delirium, coma)
mental_pt <- mental_day_df %>%
  group_by(id) %>%
  summarise(
    n_days = sum(n_asmts > 0, na.rm = TRUE),
    days_del = ifelse(n_days == 0, NA, sum(delirium, na.rm = TRUE)),
    days_coma = ifelse(n_days == 0, NA, sum(coma, na.rm = TRUE))
  )

## Add patient info onto oneobs_df; keep only needed variables
oneobs_df <- left_join(oneobs_df, mental_pt, by = "id") %>%
  dplyr::select(id, gender, age, days_del, days_coma, died_inhosp)

## Remove unwanted datasets from workspace
rm("mental_day_df", "mental_df", "mental_pt")
