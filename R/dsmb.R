#' Get DSMB REDCap Measures
#'
#' @param rcon REDCap connection object exported from tcoRS::build_rcon()
#'
#' @return Data frame of longitudinal trial measures grouped into weeks
#' @export
#'
#' @examples
#' \dontrun{
#' rcon <- build_rcon("my_secret_token")
#' df <- get_dsmb_measures(rcon)
#' }
get_dsmb_measures <- function(rcon) {
  fields <- c(
    "screen_id",
    field_vectors$bdi_fields,
    field_vectors$oasis_fields,
    field_vectors$co_fields,
    field_vectors$resp_fields
  )

  df <- download_rc_dataframe(rcon, fields = fields)
  df <- week_bin_dsmb(df)

  df_vector <- odd_even_dsmb(df)
  odd <- df_vector[[1]]
  evn <- df_vector[[2]]

  df_glued <- rbind(odd, evn) %>% dplyr::arrange(.data$screen_id, .data$session)

  # CO
  CO <- df %>%
    dplyr::select(
      .data$screen_id,
      .data$session,
      .data$week_bin,
      tidyselect::all_of(field_vectors$co_fields)
      ) %>%
    tidyr::unite(
      col = "co",
      field_vectors$co_fields,
      na.rm = TRUE
    ) %>%
    dplyr::mutate(co = as.numeric(.data$co))


  # respiratory symptoms
  resp <- df_glued %>%
    dplyr::select(-c(.data$bdi_total, .data$oasis_total)) %>%
    dplyr::mutate(dplyr::across(where(is.redcapFactor), redcapAPI::redcapFactorFlip))


  # BDI + OASIS
  bdi_oasis <- df_glued %>%
    dplyr::select(
      .data$screen_id,
      .data$session,
      .data$bdi_total,
      .data$oasis_total
    )

  join_param <- c("screen_id", "session")

  CO %>%
    dplyr::left_join(resp, by = join_param) %>%
    dplyr::left_join(bdi_oasis, by = join_param) %>%
    dplyr::mutate(session = factor(.data$session, levels = field_vectors$session_levels_fields))
}


#' DSMB: Week Bin
#'
#' @param df REDCap Data Frame
#'
#' @return data frame with `session` in binned weeks appended
#' @export
#'
#' @examples
#' \dontrun{
#'df <- week_bin_dsmb(df)
#' }
week_bin_dsmb <- function(df) {

  rm_str <- "_arm_1"
  df <- df %>%
    dplyr::mutate(
      session = stringr::str_remove_all(.data$redcap_event_name, rm_str)
      )

  df$week_bin <- NA
  df$week_bin[df$session == "baseline_2"] <- "baseline"
  df$week_bin[df$session %in% c("week_1", "week_2", "week_3", "week_4")] <- "weeks_1-4"
  df$week_bin[df$session %in% c("week_5", "week_6", "week_7", "week_8")] <- "weeks_5-8"
  df$week_bin[df$session %in% c("week_9", "week_10", "week_11", "week_12")] <- "weeks_9-12"
  df$week_bin[df$session %in% c("week_13", "week_14", "week_15", "week_16")] <- "weeks_13-16"
  df$week_bin <- factor(df$week_bin, levels = c("baseline", "weeks_1-4", "weeks_5-8", "weeks_9-12", "weeks_13-16"))
  df
}

#' Odd Even DSMB
#'
#' @param df REDCap Data Frame
#'
#' @return vector of data frames, (1) odd weeks, (2) even weeks
#' @export
#'
#' @examples
#'\dontrun{
#' df_vector <- odd_evn_dsmb(df)
#'}
odd_even_dsmb <- function(df) {
  odd_str <- "podd"
  evn_str <- "pevn"

  rm_str <- "_arm_1"
  odd_weeks <- stringr::str_remove_all(field_vectors$odd_weeks_fields, rm_str)
  evn_weeks <- stringr::str_remove_all(field_vectors$even_weeks_fields, rm_str)

  odd <- df %>%
    dplyr::select(
      .data$screen_id,
      .data$session,
      tidyselect::contains(odd_str)
    ) %>%
    dplyr::filter(.data$session %in% odd_weeks)

  evn <- df %>%
    dplyr::select(
      .data$screen_id,
      .data$session,
      tidyselect::contains(evn_str)
    ) %>%
    dplyr::filter(.data$session %in% evn_weeks)


  names(evn) <- stringr::str_remove_all(names(evn), "pevn_|_all")
  names(odd) <- stringr::str_remove_all(names(odd), "podd_")

  list(odd, evn)
}
