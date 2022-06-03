
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
