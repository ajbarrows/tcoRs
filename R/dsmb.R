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
  bl <- df_vector[[3]]

  df_glued <- rbind(odd, evn) %>%
    dplyr::full_join(bl, by = c("screen_id", "session", "bdi_total", "oasis_total")) %>%
    dplyr::mutate(session = factor(.data$session, levels = field_vectors$session_levels_fields)) %>%
    dplyr::arrange(.data$screen_id, .data$session)

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

  out <- CO %>%
    dplyr::left_join(resp, by = join_param) %>%
    dplyr::left_join(bdi_oasis, by = join_param) %>%
    dplyr::mutate(session = factor(.data$session, levels = field_vectors$session_levels_fields)) %>%
    dplyr::filter(!is.na(.data$week_bin)) %>%
    pjt_ste() %>%
    pi_prop()
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
#' @return vector of data frames, (1) odd weeks, (2) even weeks, (3) baseline
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
  baseline <- "baseline_2"
  bl_str <- "ps2"

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

  bl <- df %>%
    dplyr::select(
      .data$screen_id,
      .data$session,
      tidyselect::contains(bl_str)
    ) %>%
    dplyr::filter(.data$session == baseline)

  names(evn) <- stringr::str_remove_all(names(evn), "pevn_|_all")
  names(odd) <- stringr::str_remove_all(names(odd), "podd_")
  names(bl) <- stringr::str_remove_all(names(bl), "ps2_")

  list(odd, evn, bl)
}

#' Value Summary DSMB
#'
#' @description Summarize single-column quantity with week-bin variable
#' @param df data frame with value, week bin, condition
#' @param variable string, one of "co", "bdi", or "oasis"
#'
#' @return data frame of values, data frame of subject counts
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
  df <- df %>%
    dplyr::filter(pi_prop == "proper") %>%
    dplyr::group_by(.data$project, .data$trt_grp, .data$week_bin)

  df_values <- df %>%
    dplyr::summarize(
      dplyr::across(!!variable, list(mean = mean, median = median, IQR = IQR), na.rm = TRUE),
      .groups = "keep"
    ) %>%
    dplyr::filter(!is.na(.data$trt_grp) & !is.na(.data$week_bin)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      -c("project", "trt_grp", "week_bin")
    ) %>%
    tidyr::pivot_wider(
      names_from = "trt_grp"
    )

    df_n <- df %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data$screen_id, .data$week_bin) %>%
      dplyr::count(.data$week_bin)

    list(df_values, df_n)
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
#' @return data frame of EVALI severe symtpoms summarized
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_evali(redcap_values)
#' }
summarize_evali <- function(df) {
  df %>%
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
}


#' IVR Weekbin DSMB
#'
#' @description  Impose week bins on IVR data for main trial DSMB summary
#' @param ivr_sum summarized IVR data frame from tcoRs::summarize_ivr()
#'
#' @return data frame with `week_bin` column
#' @export
#'
#' @examples
#' \dontrun{
#' ivr_weekbin_dsmb(ivr_sum)
#' }
ivr_weekbin_dsmb <- function(ivr_sum) {
  ivr_sum <- ivr_sum[[1]]
  ivr_sum$week_bin <- NA
  ivr_sum$week_bin[ivr_sum$week == "week0"] <- "baseline"
  ivr_sum$week_bin[ivr_sum$week %in% c("week01", "week02", "week03", "week04")] <- "weeks_1-4"
  ivr_sum$week_bin[ivr_sum$week %in% c("week05", "week06", "week07", "week08")] <- "weeks_5-8"
  ivr_sum$week_bin[ivr_sum$week %in% c("week09", "week10", "week11", "week12")] <- "weeks_9-12"
  ivr_sum$week_bin[ivr_sum$week %in% c("week13", "week14", "week15", "week16")] <- "weeks_13-16"

  ivr_sum$week_bin <- factor(ivr_sum$week_bin, levels = c("baseline", "weeks_1-4", "weeks_5-8", "weeks_9-12", "weeks_13-16"))
  return(ivr_sum)
}


#' IVR Use Summary DSMB
#'
#' @param ivr_sum summarized IVR data frame from tcoRs::summarize_ivr()
#'
#' @return vector of cig and ecig use
#' @export
#'
#' @examples
#' \dontrun{
#' ivr_summary_dsmb(ivr_sum)
#' }
ivr_summary_dsmb <- function(ivr_sum) {
  ivr_sum <- ivr_weekbin_dsmb(ivr_sum)

  names(ivr_sum) <- stringr::str_remove_all(names(ivr_sum), "_mean")
  ivr_summary <- ivr_sum %>%
    dplyr::filter(pi_prop == "proper") %>%
    dplyr::mutate(nonstudycigs = ifelse(.data$week_bin == "baseline", .data$cigs, .data$nonstudycigs)) %>%
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
    dplyr::filter(!is.na(.data$value) & !is.na(.data$letter_code)) %>%
    dplyr::arrange(.data$project, .data$letter_code, .data$week_bin, .data$name)

  ivr_summary <- ivr_summary %>% tidyr::pivot_wider(names_from = .data$letter_code)

  cig <- ivr_summary %>% dplyr::filter(stringr::str_detect(.data$name, "cigs"))
  ecig <- ivr_summary %>% dplyr::filter(stringr::str_detect(.data$name, "pods"))

  list(cig, ecig)
}

#' Number of Participants by Week Bin
#'
#' @param ivr_sum exported from tcoRs::summarize_ivr()
#'
#' @return tally of participants (1) by group, (2) by binned week
#' @export
#'
#' @examples
#' \dontrun{
#' ivr_count(ivr_sum)
#' }
ivr_count <- function(ivr_sum) {
  count_df <- ivr_sum %>%
    ivr_weekbin_dsmb() %>%
    dplyr::filter(.data$pi_prop == "proper")

  week_sum <- count_df %>%
    dplyr::group_by(.data$letter_code, .data$week_bin) %>%
    dplyr::distinct(.data$screen_id) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(
      names_from = "letter_code",
      values_from = "n"
    )

  group_sum <- count_df %>%
    dplyr::group_by(.data$letter_code) %>%
    dplyr::distinct(.data$screen_id) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(
      names_from = "letter_code",
      values_from = "n"
    )

  list(group_sum, week_sum)
}

#' IVR Ecig Days
#'
#' @details Number of days per week participants used ecigarettes
#' @param ivr_sum Summary IVR data frame
#'
#' @return Vector of average e-cig use-days per week, and special quantities
#' @export
#'
#' @examples
#' \dontrun{
#' ivr_ecig_days(ivr_sum)
#' }
ivr_ecig_days <- function(ivr_sum) {
  df <- ivr_sum %>%
    dplyr::filter(.data$pi_prop == "proper") %>%
    dplyr::select(
      .data$screen_id,
      .data$project,
      .data$pi_prop,
      .data$letter_code,
      .data$week_bin,
      .data$nstudypods_mean,
      .data$nnonstudypods_mean,
      .data$nstudypods_n,
      .data$nnonstudypods_n
    ) %>%
    dplyr::group_by(.data$project, .data$letter_code, .data$week_bin)

  avg_days <- df %>%
    dplyr::summarize(
      dplyr::across(c(
        .data$nstudypods_n,
        .data$nnonstudypods_n
      ), list(mean = mean, median = median, IQR = IQR),
      na.rm = TRUE
      ),
      .groups = "keep"
    ) %>%
    tidyr::pivot_longer(
      -c(.data$project, .data$letter_code, .data$week_bin)
    ) %>%
    tidyr::pivot_wider(
      names_from = "letter_code"
    ) %>%
    dplyr::filter(.data$week_bin != "baseline")

  df_special <- df %>%
    dplyr::mutate(
      zero_days = .data$nstudypods_n  == 0 | .data$nnonstudypods_n == 0,
      six_plus_days = .data$nstudypods_n >=6 | .data$nnonstudypods_n >= 6,
      six_plus_pods = sum(.data$nstudypods_mean, .data$nnonstudypods_mean, na.rm = TRUE) >= 6
    )
  zero <- df_special %>%
    dplyr::filter(.data$zero_days) %>%
    dplyr::count(name = "zero_days")

  six_plus_days <- df_special %>%
    dplyr::filter(.data$six_plus_days) %>%
    dplyr::count(name = "six_plus_days")

  six_plus_pods <- df_special %>%
    dplyr::filter(.data$six_plus_pods) %>%
    dplyr::count(name = "six_plus_pods")

  join_seq <- c("project", "letter_code", "week_bin")

  special <- zero %>%
    dplyr::left_join(six_plus_days, by = join_seq) %>%
    dplyr::left_join(six_plus_pods, by = join_seq) %>%
    tidyr::pivot_longer(
      -tidyselect::all_of(join_seq)
    ) %>%
    tidyr::pivot_wider(
      names_from = "letter_code"
    ) %>%
    dplyr::filter(.data$week_bin != "baseline")

  list(avg_days, special)

}



