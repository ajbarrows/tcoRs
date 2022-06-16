#' Pull Main Trial Enrollment Information
#'
#' @param rcon REDCap connection object exported from tcoRs::build_rcon()
#' @param fields A selection of pre-determined enrollment REDCap fields
#'
#' @return Data frame, one row per subject
#' @export
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' enrl <- get_maintrial_enrollment(rcon)
#' }
get_maintrial_enrollment <-
  function(rcon,
           fields = field_vectors$enrollment_fields) {
    events <- c("screening_arm_1", "baseline_2_arm_1", "week_16_arm_1")
    fields <- c(fields, "screen_id", "sl_ineligible")

    df <-
      tcoRs::download_rc_dataframe(rcon, fields = fields, events = events)

    df <- df %>%
      tcoRs::matrix_to_vector(
        "scrn_ineligible_reasons",
        field_vectors$scrn_inel_names,
        "sl_ineligible")
    df <- df %>%
      dplyr::group_by(.data$screen_id) %>%
      tidyr::fill(.data$ps2_date, .data$pevn_date_session_all, .direction = "updown") %>%
      dplyr::filter(.data$redcap_event_name == "screening_arm_1") %>%
      dplyr::mutate(baseline2_date = as.Date(.data$ps2_date)) %>%
      tcoRs::pjt_ste() %>%
      tcoRs::pi_prop() %>%
      dplyr::select(
        .data$screen_id,
        .data$sl_status,
        .data$scrn_ineligible_reasons,
        .data$screen_date,
        .data$baseline2_date,
        "week16_date" = .data$pevn_date_session_all,
        .data$project,
        .data$site,
        .data$pi_prop
      ) %>%
      dplyr::ungroup()
  }

#' Pull Main Trial Prescreens
#'
#' @param rcon REDCap Connection object exported from tcoRs::build_rcon()
#' @param site_name String name of study site, appended to return.
#' @param min_date Minimum prescreen date, fixed to 2020-10-01.
#' @param fields Vector of pre-determined REDCap prescreen eligibility fields.
#'
#' @return Data frame, one row per prescreen.
#' @export
#'
#' @examples
#' \dontrun{
#' rcon_uvm <- build_rcon("my_secret_token")
#' get_maintrial_prescreens <- (rcon = rcon_uvm, site_name = "uvm")
#' }
get_maintrial_prescreens <- function(rcon,
                                     site_name,
                                     min_date = as.Date("2020-10-01"),
                                     fields = field_vectors$ps_inel_names){
  "recruit_date" <- NULL
  "elig_project_none" <- NULL
  "screen_subjectid" <- NULL
  fields_download <-
    c("redcap_id",
      names(fields),
      "recruit_date",
      "elig_project_none",
      "screen_subjectid")

  df <- tcoRs::download_rc_dataframe(rcon, fields = fields_download)

  filter_str <- "-|_|copy|incomplete|empty"

  # recode ineligible = 1
  df %>%
   dplyr::mutate(date = as.Date(.data$recruit_date)) %>%
   plyr::rename(fields) %>%
   dplyr::mutate(
     site = site_name,
     age = ifelse(.data$age > 70 | .data$age < 21, 1, 0),
     daily_smoke = ifelse(.data$daily_smoke == 0, 1, 0),
     less_than_5cpd = ifelse(.data$num_cigs < 5, 1, 0),
     smoking_less_than_1year = ifelse(.data$five_per_day == 0, 1, 0),
     other_tobacco_use = ifelse(.data$other_tobacco > 9, 1, 0),
     unstable_opioid_txt = ifelse(.data$length_dose < 30, 1, 0),
     daily_ecig_user = ifelse(.data$ecig > 5, 1, 0)
   ) %>%
   dplyr::filter(
     !stringr::str_detect(.data$redcap_id, filter_str),
     !is.na(.data$recruit_date),
     as.Date(.data$recruit_date) >= min_date
   )
}
#
#' Summarize Main Trial Prescreens
#'
#' @param df Data Frame exported from tcoRs::get_maintrial_prescreens()
#'
#' @return List of data frames with (1) prsecreen tallies by site and (2) by exclusion criterion
#' @export
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' ps_vector <- summarize_maintrial_prescreens(ps_df)
#' }
summarize_maintrial_prescreens <- function(df) {
  n <- df %>%
    dplyr::mutate(
      eligible = dplyr::case_when(
        .data$elig_project_none == 1 ~ "ineligible",
        .data$elig_project_none == 0 ~ "eligible",
        is.na(.data$elig_project_none) ~ "ineligible"
      )
    ) %>%
    dplyr::group_by(.data$eligible, .data$site) %>%
    dplyr::count(.data$eligible) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = .data$site, values_from = .data$n
    ) %>%
    tibble::column_to_rownames(var = "eligible")

  ps_summary <- df %>%
    dplyr::select(
      .data$site,
      .data$age,
      .data$daily_smoke,
      .data$less_than_5cpd,
      .data$smoking_less_than_1year,
      .data$other_tobacco_use,
      .data$nic_rep,
      .data$quit_med,
      .data$quit_plans,
      .data$rolls_own,
      .data$pregnant,
      .data$other_research,
      .data$opiate_pain_meds,
      .data$unstable_opioid_txt,
      .data$daily_ecig_user
    ) %>%
    dplyr::group_by(.data$site) %>%
    dplyr::summarize(dplyr::across(tidyselect::everything(), sum, na.rm = TRUE)) %>%
    tidyr::pivot_longer(
      cols = -.data$site
    )
  list(n, ps_summary)
}


#' Summarize Maintrial Screenings
#'
#' @param df_enrl Data frame exported from tcoRs::get_maintrial_enrollment()
#' @param filter_proper Filter only study proper IDs. If `FALSE`, all IDs are counted.
#'
#' @return List of data frames with
#' 1. Screening tallies by site
#' 2. Exclusion criteria
#' @export
#'
#' @examples
#' \dontrun{
#' scrn_vector <- summarize_maintrial_screenings(df_enrl)
#' }
summarize_maintrial_screenings <- function(df_enrl, filter_proper = TRUE) {

  if (filter_proper) {
    df_enrl <- df_enrl %>% dplyr::filter(.data$pi_prop == "proper")
  }

  df_enrl <- df_enrl %>% dplyr::group_by(.data$project, .data$site)

  n <- df_enrl %>%
    dplyr::mutate(
      eligible = ifelse(is.na(.data$scrn_ineligible_reasons),
                        "eligible",
                        "ineligible")
      ) %>%
    dplyr::count(.data$eligible) %>%
    tidyr::pivot_wider(
      names_from = c("site", "eligible"),
      values_from = "n"
    )


  scrn_summary <- df_enrl %>%
    dplyr::group_by(.data$project, .data$site) %>%
    dplyr::select(.data$scrn_ineligible_reasons, .data$project, .data$site) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      scrn_ineligible_reasons = strsplit(.data$scrn_ineligible_reasons, ",")
      ) %>%
    tidyr::unnest(.data$scrn_ineligible_reasons) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$scrn_ineligible_reasons) %>%
    tidyr::pivot_wider(
      names_from = "site",
      values_from = "n"
    )

  list(n, scrn_summary)
}


#' Session Distribution
#'
#' @description returns a tally of sessions completed for each project
#' @param rcon REDCap connection object exported from `tcors::build_rcon()`
#'
#' @return data frame of session counts
#' @export
#'
#' @examples
#' \dontrun{
#' session_distribution(rcon)
#' }
session_distribution <- function(rcon) {
  fields <- c("screen_id", field_vectors$session_dates_fields)
  df <- download_rc_dataframe(rcon, fields = fields)

  rm_str <- "_arm_1"
  df %>%
    tidyr::unite(
      col = "date",
      tidyselect::contains("date"),
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      date = as.Date(.data$date),
      session = stringr::str_remove_all(.data$redcap_event_name, rm_str),
      session = factor(.data$session, levels = field_vectors$session_levels_fields)
      ) %>%
    dplyr::filter(!is.na(date)) %>%
    pjt_ste() %>%
    dplyr::group_by(.data$project) %>%
    dplyr::count(.data$session) %>%
    tidyr::pivot_wider(
      names_from = .data$project,
      values_from = .data$n
    )
}

#' Demographics
#'
#' @description get demographic information for the main trial
#'
#' @param rcon REDCap connection object exported from tcoRs::build_rcon()
#' @param cond data frame of conditions exported from tcoRs::get_maintrial_conditions()
#'
#' @return data frame of demographic values
#' @export
#'
#' @examples
#' \dontrun{
#' demographics(rcon, cond)
#' }
demographics <- function(rcon, cond) {
  fields <- c("screen_id", field_vectors$demographic_fields)

  df <- download_rc_dataframe(rcon, fields)

  screen_race_names <- list(
    "screen_race___1" = "Am. Indian or Alask. Native",
    "screen_race___2" = "Asian",
    "screen_race___3" = "Black or African-American",
    "screen_race___4" = "Native Hawaiian or Pacific Islander",
    "screen_race___5" = "White",
    "screen_race___6" = "Other race"
  )

  df %>%
    dplyr::filter(.data$redcap_event_name == "screening_arm_1") %>%
    dplyr::left_join(cond, by = "screen_id") %>%
    pjt_ste() %>%
    pi_prop() %>%
    dplyr::mutate(
      latino = ifelse(.data$screen_latino___0 == 1, "no","yes"),
      screen_sex = redcapAPI::redcapFactorFlip(.data$screen_sex)
      ) %>%
    matrix_to_vector(colname = "race", screen_race_names, "screen_race") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      race = ifelse(
        lengths(strsplit(.data$race, ",")) > 1,
        "More than one race",
        .data$race
        )
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(
      tidyselect::starts_with("redcap"),
      tidyselect::starts_with("screen_latino")
      ))
}

