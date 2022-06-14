#' Get Main Trial Adverse Events
#'
#' @param rcon REDCap connection object exported from tcoRS::build_rcon()
#' @param fields pre-determined vector of adverse event fields
#'
#' @return data frame of adverse events
#' @export
#'
#' @examples
#' \dontrun{
#' rcon <- build_rcon("my_secret_token")
#' ae_df <- get_maintrial_aes(rcon)
#' }
get_maintrial_aes <- function(rcon, fields = field_vectors$ae_fields) {
  fields <- c("screen_id", fields)
  events <- "baseline_1_arm_1"

  df <- tcoRs::download_rc_dataframe(rcon, fields = fields, events = events)

  df %>%
    dplyr::mutate(
      dplyr::across(
        where(is.redcapFactor),
        redcapAPI::redcapFactorFlip
      )
    )
}
#
#
#
#
#
# clean_ae <- function(ae_df) {
#
#   meddra_crosswalk <-
#     readxl::read_excel("../data/CTCAE_v5.0.xlsx") %>%
#     select(`MedDRA Code`, `MedDRA SOC`, `CTCAE Term`)
#
#   hardcode_list <- c(
#     "10012378",
#     "10002855",
#     "10018065"
#   )
#
#   ae_df %>%
#
#     mutate(ae_meddra = ifelse(ae_meddra1 == 0, ae_meddra2, ae_meddra1)) %>%
#     mutate(
#       ae_descrip = ifelse(ae_meddra == 10012378, "BDI Category Increase", ae_descrip),
#       ae_descrip = ifelse(ae_meddra == 10002855, "OASIS Score Increase", ae_descrip),
#       ae_descrip = ifelse(ae_meddra == 10018065, "CPD Increase", ae_descrip)
#     ) %>%
#     left_join(meddra_crosswalk, by = c("ae_meddra" = "MedDRA Code")) %>%
#     mutate(ae_descrip = ifelse(
#       stringr::str_detect(tolower(ae_descrip), "bdi|oasis|cpd"),
#       ae_descrip,
#       `CTCAE Term`
#     )
#     ) %>%
#     filter(!is.na(ae_descrip))
# }
