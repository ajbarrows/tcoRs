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

#' Value Summary DSMB
#'
#' @description Summarize single-column quantity with week-bin variable
#' @param df data frame with value, week bin, condition
#' @param variable string, one of "co", "bdi", or "oasis"
#'
#' @return vector of data frames, one for each project
#' @importFrom stats median IQR
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tcoRs::get_dsmb_measures(rcon) %>%
#' dplyr::left_join(cond, by = "screen_id") %>%
#' tcoRs::pjt_ste() %>%
#' value_summary_dsmb("co")
#' }
value_summary_dsmb <- function(df, variable) {
  df_sub <- df %>%
    dplyr::group_by(.data$project, .data$trt_grp, .data$week_bin) %>%
    dplyr::summarize(
      dplyr::across(!!variable, list(mean = mean, median = median, IQR = IQR), na.rm = TRUE),
      .groups = "keep"
    ) %>%
    dplyr::filter(!is.na(.data$trt_grp) & !is.na(.data$week_bin)) %>%
    dplyr::ungroup()


  df_vector <- split(df_sub, f = df_sub$project)

  p1 <- pivot_week_summary(df_vector[[1]])
  p2 <- pivot_week_summary(df_vector[[2]])
  p3 <- pivot_week_summary(df_vector[[3]])

  list(p1, p2, p3)
}

#' Pivot Week Summary
#'
#' @param df Summarized data frame
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' pivot_week_summary(df_vector[[1]])
#' }
pivot_week_summary <- function(df) {
  df %>%
    dplyr::select(-.data$project) %>%
    tidyr::pivot_longer(
      -c(.data$trt_grp, .data$week_bin)
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$trt_grp
    )
}

#' Protocol Violations DSMB
#'
#' @param rcon REDCap connection object exported from tcoRs::build_rcon()
#' @param cond data frame of condition assignments from tcoRs::get_maintrial_conditions()
#'
#' @return data frame of protocol violations
#' @export
#'
#' @examples
#' \dontrun{
#' rcon <- build_rcon("my_secret_token")
#' cond <- get_maintrial_conditions(usr, pws)
#' violations <- protocol_violations(rcon, cond)
#' }
protocol_violations <- function(rcon, cond) {
  fields <- c("screen_id", field_vectors$nonmed_fields)
  df <- download_rc_dataframe(rcon, fields = fields)

  df %>%
    dplyr::filter(.data$redcap_event_name == "screening_arm_1") %>%
    dplyr::select(-tidyselect::starts_with("redcap")) %>%
    dplyr::left_join(cond, by = "screen_id")

}


#' Plot randomization progress
#'
#' @param enrl data frame exported from tcoRs::get_maintrial_enrollment()
#' @param goal numeric constant of project's randomization goal
#' @param min_date string enrollment start date, "yyyy-mm-dd"
#' @param end_date string projected enrollment end date, "yyyy-mm-dd"
#' @param proj string of either "Project 1", "Project 2", or "Project 3"
#' @param title string, title of plot
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' randomization_accrual_plot(
#' enrl,
#' goal = 212,
#' min_date = "2020-11-01",
#' end_date = "2023-06-01",
#' proj = "Project 1",
#' title = "Recruitment Accrual Graph: Project 1"
#'
#' )
#' }
randomization_accrual_plot <- function(enrl, goal, min_date, end_date, proj, title) {
  end_date <- as.Date(end_date)
  min_date <- as.Date(min_date)

  # create sequence of dates and enrollment per day based on
  # goal and date inputs
  x_seq <- seq(min_date, end_date, by = "month")
  y_seq <- seq(0, goal, length.out = length(x_seq))
  enrollment_goal <- data.frame(date = x_seq, target = y_seq, project = proj)

  colors <- c(
    "Target" = "#7CAE00",
    "Projected Accrual" = "#F8766D",
    "Actual Accrual" = "#00BFC4"
  )

  # sequence enrollment using dates
  randomized_progress <- enrl %>%
    dplyr::filter(
      .data$project == proj,
      !is.na(.data$baseline2_date)
      ) %>%
    dplyr::select("date" = .data$baseline2_date) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(randomized = seq_along(.data$date))


  ggplot2::ggplot(enrollment_goal, ggplot2::aes(x = .data$date, y = .data$target)) +
    ggplot2::geom_line(ggplot2::aes(color = "Projected Accrual"), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = goal, color = "Target"), linetype = "dotted") +
    ggplot2::geom_line(
      data = randomized_progress,
      ggplot2::aes(x = .data$date, y = .data$randomized, color = "Actual Accrual")) +
    ggplot2::geom_point(
      data = randomized_progress,
      ggplot2::aes(x = .data$date, y = .data$randomized, color = "Actual Accrual")) +
    ggplot2::annotate(
      "text",
      x = as.Date("2021-02-01"),
      y = goal - 10,
      label = paste("Target = ", goal)) +
    ggplot2::scale_color_manual(values=colors) +
    ggplot2::theme_classic(base_size = 15) +
    ggplot2::labs(x = "", y = "Subjects Enrolled", color = "",
         title = title)
}


#' Summarize EVALI symptoms
#'
#' @param df data frame exported from tcoRs::get_dsmb_measures()
#'
#' @return vector of data frames for all three project, EVALI severe symtpoms summarized
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_evali(redcap_values)
#' }
summarize_evali <- function(df) {
  resp_sub <- df %>%
    pi_prop() %>%
    dplyr::filter(pi_prop == "proper") %>%
    dplyr::select(
      .data$screen_id,
      .data$session,
      .data$project,
      .data$trt_grp,
      tidyselect::all_of(field_vectors$resp_values)
      ) %>%
    dplyr::filter(stringr::str_detect(.data$session, "week")) %>%
    tidyr::pivot_longer(
      -c(.data$screen_id, .data$session, .data$project, .data$trt_grp)
    ) %>%
    dplyr::filter(.data$value == "Severe") %>%
    dplyr::distinct(.data$screen_id, .data$name, .data$value, .keep_all = TRUE) %>%
    dplyr::group_by(.data$project, .data$name, .data$trt_grp) %>%
    dplyr::count()

  resp_vector <- split(resp_sub, f = resp_sub$project)

  p1 <- resp_vector[[1]] %>% tidyr::pivot_wider(
    names_from = .data$trt_grp,
    values_from = .data$n,
    values_fill = 0
  )

  p2 <- resp_vector[[2]] %>%  tidyr::pivot_wider(
    names_from = .data$trt_grp,
    values_from = .data$n,
    values_fill = 0
  )

  p3 <- resp_vector[[3]] %>% tidyr::pivot_wider(
    names_from = .data$trt_grp,
    values_from = .data$n,
    values_fill = 0
  )

  list(p1, p2, p3)
}


ivr_summary_dsmb <- function(ivr_sum) {
  ivr_sum <- ivr_sum[[1]]
  ivr_sum$week_bin <- NA
  ivr_sum$week_bin[ivr_sum$week == "week0"] <- "baseline"
  ivr_sum$week_bin[ivr_sum$week %in% c("week01", "week02", "week03", "week04")] <- "weeks_1-4"
  ivr_sum$week_bin[ivr_sum$week %in% c("week05", "week06", "week07", "week08")] <- "weeks_5-8"
  ivr_sum$week_bin[ivr_sum$week %in% c("week09", "week10", "week11", "week12")] <- "weeks_9-12"
  ivr_sum$week_bin[ivr_sum$week %in% c("week13", "week14", "week15", "week16")] <- "weeks_13-16"

  ivr_sum$week_bin <- factor(ivr_sum$week_bin, levels = c("baseline", "weeks_1-4", "weeks_5-8", "weeks_9-12", "weeks_13-16"))

  names(ivr_sum) <- stringr::str_remove_all(names(ivr_sum), "_mean")
  ivr_summary <- ivr_sum %>%
    dplyr::mutate(nonstudycigs = ifelse(week_bin == "baseline", cigs, nonstudycigs)) %>%
    dplyr::group_by(.data$project, .data$letter_code, .data$week_bin) %>%
    dplyr::summarize(
      dplyr::across(c(
             .data$studycigs,
             .data$nonstudycigs,
             .data$nstudypods,
             .data$nnonstudypods),
             list(mean = mean, median = median, IQR = IQR),
             na.rm = TRUE
    ),
    .groups = "keep"
    ) %>%
    tidyr::pivot_longer(
      -c(.data$project, .data$letter_code, .data$week_bin)
    ) %>%
    dplyr::filter(!is.na(.data$value) & !is.na(.data$letter_code))


  ivr_vector <- split(ivr_summary, f = ivr_summary$project)

  # ivr_vector[[1]] %>%
  #   tidyr::pivot_wider(
  #     names_from = "letter_code"
  #   )

  # TODO: add e-cigarettes, n, peel off proejcts
}



