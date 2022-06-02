
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
get_maintrial_enrollment <- function(rcon, fields = tcoRs:::field_vectors$enrollment_fields) {
  events <- c("screening_arm_1", "baseline_2_arm_1", "week_16_arm_1")
  fields <- c(fields, "screen_id")

  df <- tcoRs::download_rc_dataframe(rcon, fields = fields, events = events)
  df %>%
    dplyr::group_by(.data$screen_id) %>%
    tidyr::fill(.data$ps2_date, .data$pevn_date_session_all, .direction = "updown") %>%
    dplyr::filter(.data$redcap_event_name == "screening_arm_1") %>%
    dplyr::mutate(
      baseline2_date = as.Date(.data$ps2_date)
    ) %>%
    tcoRs::pjt_ste() %>%
    tcoRs::pi_prop() %>%
    dplyr::select(
      .data$screen_id, .data$sl_status, .data$screen_date, .data$baseline2_date,
      "week16_date" = .data$pevn_date_session_all,
      .data$project, .data$site, .data$pi_prop
    ) %>%
    dplyr::ungroup()
}
