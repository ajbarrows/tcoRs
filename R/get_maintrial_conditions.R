#' Get Main Trial Treatment Groups
#'
#' Get official dose conditions from randomization database. This database is hosted
#' at the University of Minnesota.
#'
#' @param username UMN Account Username
#' @param password UMN Account Password
#'
#' @return Data Frame with condition assignments
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' conditions <- get_maintrial_conditions(token = "my_secret_token")
#' }
get_maintrial_conditions <- function(username, password) {
  url <- "https://tcors4.cancer.umn.edu/tcors/account/super/allParticipantsReport.xlsx"

  full <- paste(
    'https://',
    username,
    ':',
    password,
    '@tcors4.cancer.umn.edu/tcors/account/super/allParticipantsReport.xlsx',
    sep = ""
  )

  df <- openxlsx::read.xlsx(full) %>%
    dplyr::select(
      "screen_id" = .data$Participant.Id,
      "menthol_status" = .data$Menthol.Status,
      "trt_grp" = .data$Randomization.Letter
    )

  df$overall_group <- NA
  df$overall_group[df$trt_grp %in% c("A3", "C3", "T3", "Q3")] <- "Cigarettes Only"
  df$overall_group[df$trt_grp %in% c("D3", "G3", "S3")] <- "E-cig Tobacco"
  df$overall_group[df$trt_grp %in% c("B3", "E3", "R3")] <- "E-cig Preferred"
  df
}
