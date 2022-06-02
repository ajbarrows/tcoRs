#' Impose Project and Site
#'
#' @param df Data frame with `screen_id` variable of the form `X-A###`
#'
#' @return Data frame with columns appended for `project` and `site`
#' @export
#'
#' @examples
#' data("enrl_sample")
#' pjt_ste(enrl_sample)
#'
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
#'
#' data("enrl_sample")
#' pi_prop(enrl_sample)
#'
pi_prop <- function(df) {
  df$pi_prop <- ifelse(substr(df$screen_id, 4, 4) == 9, "pilot", "proper")
  df
}

#' Convert matrix data to character vector
#'
#' @param df Data frame containing (not necessarily exclusively) matrix data.
#' @param colname String of resulting column name
#' @param matrix_vars Named list of matrix variables.
#' @param select_string String to uniquely identify matrix variables to remove using dplyr::contains()
#'
#' @return Data frame with vectorized matrix variables.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- matrix_to_vector(df, "new_col", matrix_list, "matrix_string")
#' }
matrix_to_vector <- function(df, colname, matrix_vars, select_string) {

  rename_matrix <- function(df) {
    # make values names of columns
    w1 <- which(df[,1:ncol(df)] == 1, arr.ind = TRUE)
    w0 <- which(df[,1:ncol(df)] == 0, arr.ind = TRUE)

    mode(w1) <- "numeric"
    mode(w0) <- "numeric"
    # avoid zero-length error
    if(length(w1 > 0)) {
      df[w1] <- names(df)[w1[,"col"]]
    }
    df[w0] <- NA

    df
  }

  df_sub <- df %>%
    plyr::rename(matrix_vars) %>%
    dplyr::select(unlist(matrix_vars, use.names = FALSE)) %>%
    rename_matrix() %>%
    tidyr::unite(
      col = !!colname,
      sep = ",",
      na.rm = TRUE
    )
  df_sub[df_sub == ""] <- NA

  df %>%
    dplyr::select(-dplyr::contains(select_string)) %>%
    cbind(df_sub)
}


