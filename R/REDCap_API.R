#' Build REDCap API Connection
#'
#' @param token REDCap-generated unique API key.
#' @param url https://redcap.med.uvm.edu/api/
#'
#' @return REDCap Connection Object
#' @export
#'
#' @examples
#' \dontrun{
#' token <- read.table("my_secret_key.txt")
#' build_rcon(token)
#' }
build_rcon <-
  function(token, url = 'https://redcap.med.uvm.edu/api/') {
    redcapAPI::redcapConnection(url = url, token = token)
  }

#' Download REDCap data frame, a wrapper around `exportRecords` with helpful defaults.
#'
#' @param rcon REDCap API connection (see `build_rcon()`).
#' @param fields Character vector of field names.
#' @param events Character vector of event names.
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' rcon <- build_rcon(token)
#'download_rc_dataframe(rcon, fields = "screen_id", events = "screening_arm_1")
#'}
download_rc_dataframe <-
  function(rcon,
           fields = NULL,
           events = NULL) {
    df <-
      redcapAPI::exportRecords(
        rcon,
        fields = fields,
        labels = FALSE,
        survey = FALSE,
        dag = TRUE,
        events = events,
        form_complete_auto = FALSE,
        dates = FALSE,
        factors = TRUE
      )
  }
