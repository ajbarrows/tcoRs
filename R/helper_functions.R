#' Impose Project and Site
#'
#' @param df Data frame with `screen_id` variable of the form `X-A###`
#'
#' @return Data frame with columns appended for `project` and `site`
#' @export
#'
#' @examples
#' \dontrun{
#' df <- pjt_ste(df)
#' }
pjt_ste <- function(df){
  pjt <- substr(df$screen_id, 1, 1)
  ste <- substr(df$screen_id, 3, 3)

  df$project <- NA
  df$project[pjt == "X"] <- "Project 1"
  df$project[pjt == "Y"] <- "Project 2"
  df$project[pjt == "Z"] <- "Project 3"

  df$site <- NA
  df$site[ste == "A"] <- "uvm"
  df$site[ste == "B"] <- "brown"
  df$site[ste == "C"] <- "jhu"
  df$site <- factor(df$site, levels = c("uvm", "brown", "jhu"))

  df
}

#' Impose Pilot and Proper
#'
#' @param df Data frame with `screen_id` variable of the form `X-A###`
#'
#' @return Data frame with column appended `pi_prop` to designate study proper from pilot.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- pi_prop(df)
#' }
pi_prop <- function(df) {
  df$pi_prop <- ifelse(substr(df$screen_id, 4, 4) == 9, "pilot", "proper")
  df
}

