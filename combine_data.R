################################################################################
## Combine CADUCEUS-related data from MENDS2, MOSAIC, INSIGHT
################################################################################

## API tokens stored in .Renviron; some functions sourced from data_functions.r
source("data_functions.R")
  ## calls RCurl, glue, dplyr, stringr

library(purrr)

## Things we need from each database:
## - All CADUCEUS-specific variables (CADUCEUS form)
## - Demographic info: dob, enrollment date, gender
## - Enrollment info: MV, sepsis, shock at enrollment
## - Daily info: mental status (CAM, RASS)
## These variables may be stored differently in the three databases.

## MENDS2 variable names
## Enrollment: id, dob, gender, enroll_time
## ICU type, MV, sepsis, shock at enrollment: Could not find these
##  One ICU reason = sepsis/septic shock; also hemorrhagic shock
## Daily info: rass_actual_1/2, cam_1/2

## MOSAIC variable names
## Enrollment: id, dob, gender, enroll_dttm
## ICU type, MV, sepsis, shock: organ_fail_present (1 = MV, 3 = shock)
##  One ICU reason = sepsis/septic shock
## Daily info: rass_actual_1/2, cam_1/2

## INSIGHT variable names
## Enrollment: id, dob, gender_birth + gender_identity, enroll_dttm
## ICU type, MV, sepsis, shock:
##  - resp_random = Yes + resp_present_1 (MV)
##  - shock_random
##  - sepsis: can't find anything (inc ICU admission reason)
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

  ## All names are consistent except for enrollment date/time
  names(df) <- str_replace(names(df), "^enroll\\_time$", "enroll_dttm")

  return(df %>% dplyr::select(id, dob, enroll_dttm, gender))
}

## Extract raw data for each study and combine into a single df
oneobs_df <- map2_dfr(
  .x = paste0(c("MENDS2", "MOSAIC", "INSIGHT"), "_IH_TOKEN"),
  .y = list(
    c("id", "dob", "gender", "enroll_time"),
    c("id", "dob", "gender", "enroll_dttm"),
    c("id", "dob", "gender_birth", "gender_identity", "enroll_dttm")
  ),
  .f = export_oneobs
) %>%
  ## Calculate age at enrollment
  mutate(
    dob = as.Date(dob, format = "%Y-%m-%d"),
    enroll_dttm = as.POSIXct(enroll_dttm, format = "%Y-%m-%d %H:%M", tz = "UTC"),
    enroll_date = as.Date(enroll_dttm),
    age = as.numeric(difftime(enroll_date, dob, units = "days")) / 365.25
  ) %>%
  ## Remove identifiers
  dplyr::select(id, gender, age)

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
)

# ## skim for reasonableness
# skimr::skim(daily_df)
