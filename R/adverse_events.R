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

#' Clean Main Trial Adverse Events
#'
#' @param ae_df Adverse Event data frame exported from tcoRs::get_maintrial_aes()
#'
#' @return data frame of clean adverse events
#' @export
#'
#' @examples
#' \dontrun{
#' rcon <- build_rcon("my_secret_token")
#' ae_df <- get_maintrial_aes(rcon)
#' clean_ae_df <- clean_ae(ae_df)
#' }
clean_ae <- function(ae_df) {
  hardcode_list <- c(
    "10012378",
    "10002855",
    "10018065"
  )
  names_list <- c(
    "BDI Category Increase",
    "OASIS Score Increase",
    "CPD Increase"
  )
  join_list <- c("ae_meddra" = "MedDRA.Code")
  det_string <- "bdi|oasis|cpd"

  filter_select <- "redcap"

  meddra <- tcoRs::ctcae5 %>%
    dplyr::select(.data$MedDRA.Code, .data$CTCAE.Term)

  ae_df %>%
    dplyr::mutate(ae_meddra = ifelse(.data$ae_meddra1 == 0, .data$ae_meddra2, .data$ae_meddra1)) %>%
    dplyr::mutate(
      ae_descrip = ifelse(.data$ae_meddra == hardcode_list[1], names_list[1], .data$ae_descrip),
      ae_descrip = ifelse(.data$ae_meddra == hardcode_list[2], names_list[2], .data$ae_descrip),
      ae_descrip = ifelse(.data$ae_meddra == hardcode_list[3], names_list[3], .data$ae_descrip)
    ) %>%
    dplyr::left_join(meddra, by = join_list) %>%
    dplyr::mutate(description = ifelse(
      stringr::str_detect(tolower(.data$ae_descrip), det_string),
      .data$ae_descrip,
      .data$CTCAE.Term
      )
    ) %>%
    dplyr::select(!tidyselect::starts_with(filter_select)) %>%
    dplyr::select(-c(.data$ae_common, .data$ae_meddra1, .data$ae_meddra2)) %>%
    dplyr::select(.data$screen_id, .data$description, .data$ae_descrip, .data$CTCAE.Term, tidyselect::everything())

}

summarize_ae <- function(ae_clean, cond, filter_proper = TRUE) {
  if (filter_proper) {
    ae_clean <- ae_clean %>%
      pi_prop() %>%
      dplyr::filter(.data$pi_prop == "proper")
  }

  ae_clean <- ae_clean %>%
    dplyr::left_join(cond, by = "screen_id") %>%
    dplyr::filter(!is.na(.data$ae_descrip))

  review_set <- ae_clean %>%
    dplyr::select(
      .data$screen_id,
      .data$description,
      .data$ae_descrip,
      .data$ae_meddra
      )

  list(ae_clean, review_set)
}




# count distinct subject-variable combinations
# then count distinct subject-description-variable combinations

count_ae <- function(ae_clean, var, by_condition = FALSE) {

  if (by_condition) {
    sub_var <- ae_clean %>%
      dplyr::distinct(.data$screen_id, .data$trt_grp, !!as.name(var)) %>%
      dplyr::group_by(.data$trt_grp, !!as.name(var)) %>%
      dplyr::count() %>%
      dplyr::filter(!is.na(!!as.name(var))) %>%
      tidyr::pivot_wider(
        names_from = c(.data$trt_grp,!!as.name(var)),
        values_from = "n",
        values_fill = 0
      )

    sub_descrip_var <- ae_clean %>%
      dplyr::distinct(.data$screen_id, .data$trt_grp, .data$description, !!as.name(var)) %>%
      dplyr::group_by(.data$description, .data$trt_grp, !!as.name(var)) %>%
      dplyr::count() %>%
      tidyr::drop_na() %>%
      tidyr::pivot_wider(
        names_from = c(.data$trt_grp,!!as.name(var)),
        values_from = "n",
        values_fill = 0
      )

  } else{
    sub_var <- ae_clean %>%
      dplyr::distinct(.data$screen_id, !!as.name(var)) %>%
      dplyr::group_by(!!as.name(var)) %>%
      dplyr::count() %>%
      dplyr::filter(!is.na(!!as.name(var))) %>%
      tidyr::pivot_wider(
        names_from = !!as.name(var),
        values_from = "n",
        values_fill = 0
      )

    sub_descrip_var <- ae_clean %>%
      dplyr::distinct(.data$screen_id, .data$description, !!as.name(var)) %>%
      dplyr::group_by(.data$description, !!as.name(var)) %>%
      dplyr::count() %>%
      tidyr::drop_na() %>%
      tidyr::pivot_wider(
        names_from = !!as.name(var),
        values_from = "n",
        values_fill = 0
      )
  }

  return(list(sub_var, sub_descrip_var))
}

