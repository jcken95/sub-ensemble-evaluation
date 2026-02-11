#' @name user_check
#' @section Version: 0.0.2
#'
#' @title Narratives
#'
#' @description Functions for checking user inputs.
#'
".__module__."


box::use(box / deps_)
.on_load <- function(ns) {
  deps_$need(
    "rlang",
    "stringr"
  )
}



#' Check user is happy to continue
#'
#' @param lead_message A string printed before asking for the user's prompt.
#'                     Anything that can be no-quote-printed is also acceptable.
#' @param extra_fails String, possibly vector of strings, to be added to the
#'                    list of unacceptable responses that will halt the script.
#' @param extra_passes String, possibly vector of strings, to be added to the
#'                     list of acceptable responses to continue running script
#'
#' @returns Nothing; called for side effect.
#'          Will print an acknowledgement on pass or error on user stoppage.
#'
#' @examples
#'
#' # example usage 1: checking dates are correct in recent data pull:
#' library(lubridate)
#' print(summary_statistics) # printing a data frame may be best done separately
#' user_check(
#'   lead_message = paste( # A helpful message, possibly pasting together info
#'     "Please check these dates are sufficiently recent.",
#'     "Today's date is:",
#'     lubridate::today(tzone = "GMT")),
#'   extra_passes = "Yes this is fine!", # would now accept this string
#'   extra_fails = c("I'm afraid I can't do that.", "No way!")
#' )
#' # either of these strings will now stop the script if user types one in.
#'
#' # example usage 2: checking a diagnostic plot:
#' library(ggplot2)
#'
#' p <- mtcars |>
#'   ggplot(aes(x = wt, y = mpg)) +
#'   geom_point()
#'
#' print(p)
#'
#' user_check$user_check("Does the scatter plot look approximately linear?")
#'
#' @export
user_check <- function(
    lead_message = paste(
      "Please check whether this is tolerable",
      "before passing on the data."),
    extra_fails = NULL,
    extra_passes = NULL
    ) {
  # Message for user
  message(lead_message)

  # Main user input:
  user_ok <- readline(
    prompt = "Press [enter] to acknowledge & continue; enter 'no' into the console to halt the script: "
  ) |>
    clean_input()

  fail_states <- c(
    "0",
    "f",
    "n",
    "no",
    "bad",
    "halt",
    "help",
    "nope",
    "stop",
    "break",
    "cease",
    "false",
    "pause",
    "broken",
    clean_input(extra_fails))

  pass_states <- c(
    "", # so just pressing `enter` will work
    "1",
    "y",
    "t",
    "go",
    "yes",
    "yup",
    "true",
    "fine",
    "pass",
    "indeed",
    "please",
    "confirm",
    "correct",
    "proceed",
    "continue",
    "keep going",
    clean_input(extra_passes))

  if (user_ok %in% fail_states) {
    stop("User Termination") # as in terminated by the user, not ... you get it.
  } else if (user_ok %in% pass_states) {
    message("Acknowledged.")
  } else {
    warning(
      "Can't say I thought of that response.\n",
      "Maybe add it to the extra_fails|passes list, or into in user_check.R?\n",
      "Continuing for now."
    ) # could put a recursive user_check() here to be evil

  }
}

#' Common user input text cleaner
#' Bundles some common code into one internal function,
#' shouldn't need to be exported; unless you find a use case.
#' @param text String to be cleaned.
#' @returns A lower-case string without any obvious SQL injections or any other
#'          characters likely to break a pattern match.
#' @keywords internal
clean_input <- function(text) {
  as.character(text) |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("\\\\") |> # You never know who the user is.
    stringr::str_remove_all("[.,!?'|/]") |> # prevent SQL injection here!
    stringr::str_squish() # drop leading/trailing/excess internal white space.
}



#' Checks the disease will match the name in the file structure
#'
#' This function checks the disease name is one from a list of known disease that are analysed by IDM/pancasts
#' The disease name is converted to lower case by the function.
#'
#' @param disease String of the disease name (insensitive to upper or lower case)
#'
#' @returns String with the lowercase disease name.
#'
#' @export
disease_checker <- function(
    disease = c("covid", "influenza", "measles", "norovirus", "pertussis", "rsv")
    ) {

  disease <- tolower(disease)
  rlang::arg_match(disease)

  return(disease)
}
