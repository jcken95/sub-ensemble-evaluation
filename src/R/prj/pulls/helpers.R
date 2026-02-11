#' @name helpers
#' @section Version: 0.0.1
#'
#' @title Helper functions for pancast data pulls
#'
#' @description
#' Helper functions for pancast data pulls
#'
#' @seealso
#' * [helpers$connect_sgssdw()]
#'
#' @keywords developer
".__module__."


box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "cli",
    "DBI",
    "odbc"
  )
}


is_phe_machine <- function() {
  grepl("REDACTED", Sys.info()["nodename"])
}


is_directaccess_connected <- function() {
  tryCatch(
    system2("powershell", "Get-DAConnectionStatus", stdout = TRUE)[3] == "Status    : ConnectedRemotely",
    error = function(e) {
      cli::cli_abort("PowerShell not available - are you definitely working locally on a Windows laptop?")
    }
  )
}


#' Connect to the SGSSDW database
#'
#' This MUST be run in a local R session on a PHE laptop, since the user's
#' Windows credentials are used for authentication.
#'
#' @returns A DBIConnection object.
#'
#' @export
connect_sgssdw <- function() {
  if (!is_phe_machine()) {
    cli::cli_abort(c("x" = "You must connect to SGSSDW from an R session running locally on a PHE laptop!"))
  }

  if (!is_directaccess_connected()) {
    cli::cli_abort(c(
      "x" = "It looks like your laptop isn't connected to DirectAccess (\"Workplace Connection\")",
      " " = paste(
        "{.emph You can verify this in {.code Settings > Network & Internet > DirectAccess},",
        "which must show as {.val Connected}}"
      ),
      "",
      "i" = "This can usually be fixed by rebooting your laptop!"
    ))
  }

  DBI::dbConnect(
    odbc::odbc(),
    Driver = "REDACTED",
    Server = "REDACTED",
    Database = "REDACTED",
    Trusted_Connection = "yes"
  )
}
