#' Download Main Trial IVR Data
#'
#' @param token TeleSage Raw Data Download key
#'
#' @return Data frame of all surveys.
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' df <- load_ivr(token)
#' }
load_ivr <- function(token) {
  # load and clean IVR data externally
  user <- "tcorsstudy3"
  url <-
    paste('https://',
          user,
          ':',
          token,
          '@rd.telesage.com/tcorsstudy3/surveys.tsv',
          sep = '')
  # download from TeleSage's server

  df <- read.csv(url,
                 header = TRUE,
                 sep = "\t",
                 na.strings = "")

  # clean
  colnames(df) <- tolower(colnames(df))
  df$subjectid <- toupper(df$subjectid)
  df$subjectid <- gsub("-", "", df$subjectid)
  df$subjectid <- gsub("A", "-A", df$subjectid)
  df$subjectid <- gsub("B", "-B", df$subjectid)
  df$subjectid <- gsub("C", "-C", df$subjectid)

  df
}


#' Clean IVR Data
#'
#' @param ivr_raw Exported from tcoRs::load_ivr()
#' @param enrl Exported from tcoRs::get_maintrial_enrollment()
#'
#' @return Data frame: one row per subject per day.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' ivr_raw <- tcoRs::load_ivr(token = ivr_token)
#' enrl <- tcoRs::get_maintrial_enrollment(token = REDCap_token)
#' ivr_clean <- clean_ivr(ivr_raw, enrl)
#' }
clean_ivr <- function(ivr_raw, enrl) {
  # return one call per subject per day

  # subset
  ivr <- ivr_raw %>%
    dplyr::mutate(
      call_datetime = lubridate::mdy_hm(paste(.data$calldate, .data$calltime))) %>%
    dplyr::rename("screen_id" = .data$subjectid)%>%
    dplyr::filter(!is.na(.data$screen_id))

  # count number of non-missing values
  ivr$n_vals <- rowSums(!is.na(ivr))

  # if more than 1 call per day, take call with most data
  # if equally complete, keep most recent

  ivr <- ivr %>%
    dplyr::group_by(.data$screen_id, .data$daynumber) %>%
    dplyr::filter(.data$n_vals == max(.data$n_vals)) %>%
    dplyr::filter(.data$call_datetime == max(.data$call_datetime)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$screen_id, .data$daynumber, dplyr::everything()) %>%
    dplyr::arrange(.data$screen_id, .data$daynumber)

  # complete missing daynumber values

  ivr <- ivr %>%
    dplyr::group_by(.data$screen_id) %>%
    dplyr::filter(!is.na(.data$daynumber)) %>%
    dplyr::mutate(max_daynum = max(.data$daynumber, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(.data$screen_id, .data$daynumber, fill = list(NA)) %>%
    dplyr::group_by(.data$screen_id) %>%
    tidyr::fill(.data$max_daynum, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$daynumber > 0 & .data$daynumber <= .data$max_daynum) %>%
    dplyr::arrange(.data$screen_id, .data$daynumber) %>%
    dplyr::select(-.data$max_daynum)

  # make daynumberbl2 = date of baseline 2

  ivr <- enrl %>%
    dplyr::select(.data$screen_id, .data$baseline2_date) %>%
    dplyr::left_join(ivr, by = "screen_id") %>%
    dplyr::group_by(.data$screen_id) %>%
    dplyr::mutate(daynumbl2_verified = ifelse(
      as.Date(.data$call_datetime) ==.data$baseline2_date,
      .data$daynumber, NA
    )) %>%
    tidyr::fill(.data$daynumbl2_verified, .direction = "downup") %>%
    dplyr::ungroup()

  # sort out experimental phase, impose sequential weeks

  # note -- this sets daynumberEX for the date of Baseline 2 = 0
  ivr <- ivr %>% dplyr::mutate(daynumberEX = .data$daynumber - .data$daynumbl2_verified)

  # add binned weeks. programmatically, this looks inelegant, but it does make the naming explicit
  ivr$week <- NA
  ivr$week[ivr$daynumberEX < 2 & ivr$daynumber >= 2 & ivr$daynumber <= 8] <- 'week0'
  ivr$week[ivr$daynumberEX %in% 1:7] <- 'week01'
  ivr$week[ivr$daynumberEX %in% 8:14] <- 'week02'
  ivr$week[ivr$daynumberEX %in% 15:21] <- 'week03'
  ivr$week[ivr$daynumberEX %in% 22:28] <- 'week04'
  ivr$week[ivr$daynumberEX %in% 29:35] <- 'week05'
  ivr$week[ivr$daynumberEX %in% 36:42] <- 'week06'
  ivr$week[ivr$daynumberEX %in% 43:49] <- 'week07'
  ivr$week[ivr$daynumberEX %in% 50:56] <- 'week08'
  ivr$week[ivr$daynumberEX %in% 57:63] <- 'week09'
  ivr$week[ivr$daynumberEX %in% 64:70] <- 'week10'
  ivr$week[ivr$daynumberEX %in% 71:77] <- 'week11'
  ivr$week[ivr$daynumberEX %in% 78:84] <- 'week12'
  ivr$week[ivr$daynumberEX %in% 85:91] <- 'week13'
  ivr$week[ivr$daynumberEX %in% 92:98] <- 'week14'
  ivr$week[ivr$daynumberEX %in% 99:105] <- 'week15'
  ivr$week[ivr$daynumberEX %in% 106:112] <- 'week16'
  ivr$daynumberEX[ivr$daynumberEX < 1] <- NA

  ivr %>%
    dplyr::select(
      .data$screen_id,
      .data$daynumber,
      .data$daynumberEX,
      .data$week,
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      dplyr::across(c(.data$studycigs, .data$nonstudycigs), as.numeric)
    ) %>%
    dplyr::select(-.data$callerid) %>%
    pjt_ste() %>%
    pi_prop()
}

#' Summarize IVR Data Set
#'
#' Obtain weekly summary values for each subject from daily surveys.
#'
#' @param ivr_clean Exported from tcoRs::clean_ivr()
#' @param conditions Exported from tcoRs::get_maintrial_conditions()
#'
#' @return Data Frame -- one entry for each subject for each week.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' ivr_sum <- summarize_ivr(ivr_clean, conditions)
#' }
summarize_ivr <- function(ivr_clean, conditions) {
  vars_to_summarize <-
    c(
      "cigs",
      "studycigs",
      "nonstudycigs",
      "smokeless",
      "ecig",
      "pod",
      "npods",
      "studyecig",
      "studypod",
      "nstudypods",
      "nonstudyecig",
      "nonstudypod",
      "nnonstudypods",
      "nicrep",
      "irritability",
      "anxiety",
      "restlessness",
      "sad",
      "concentration",
      "sleep"
    )
  # join with conditions
  ivr_sum <- ivr_clean %>%
    dplyr::left_join(conditions, by = "screen_id")

  ivr_sum <- ivr_sum %>%
    dplyr::group_by(.data$screen_id, .data$week) %>%
    dplyr::mutate(any_zero = dplyr::case_when(
      any(.data$studycigs == 0 &
            .data$nonstudycigs == 0, na.rm = TRUE) ~ "yes"
    )) %>%
    dplyr::group_by(
      .data$screen_id,
      .data$week,
      .data$project,
      .data$site,
      .data$pi_prop,
      "letter_code" = .data$trt_grp,
      .data$menthol_status,
      .data$any_zero
    ) %>%
    dplyr::summarize(dplyr::across(
      tidyselect::all_of(vars_to_summarize),
      list(mean = mean, n = ~ sum(!is.na(.x)))
    ), .groups = "keep"
    ) %>%
    dplyr::filter(!is.na(.data$week))

  . <- NULL
  # organize data set
  ivr_sum <- ivr_sum %>%
    dplyr::arrange(.data$screen_id, .data$week) %>%
    dplyr::select(order(match(
      sub("_.*", "", names(.)), vars_to_summarize, nomatch = 0
    )))

  # primary outcome
  ivr_w16 <- ivr_sum %>% dplyr::filter(.data$week == "week16")


  list(ivr_sum, ivr_w16)
}



