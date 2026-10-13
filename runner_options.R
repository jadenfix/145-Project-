runner_usage <- "Usage: Rscript run_regime_core.R [--past-only]"

parse_runner_options <- function(args) {
  if (!is.character(args)) stop("Runner arguments must be character values", call. = FALSE)
  if (!length(args)) {
    return(list(past_only = FALSE, mode = "archived retrospective reproduction"))
  }
  if (identical(args, "--past-only")) {
    return(list(past_only = TRUE, mode = "past-only re-estimation"))
  }
  stop(
    paste0(
      "Unknown or duplicate argument(s): ", paste(args, collapse = " "),
      "\n", runner_usage
    ),
    call. = FALSE
  )
}
