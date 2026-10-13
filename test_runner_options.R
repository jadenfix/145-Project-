# Resolve repository files relative to this test, including Rscript's --file form.
full_args <- commandArgs()
file_arg <- grep('^--file=', full_args, value = TRUE)
if (length(file_arg)) {
  setwd(dirname(normalizePath(sub('^--file=', '', file_arg[[1L]]))))
} else if ('-f' %in% full_args) {
  setwd(dirname(normalizePath(full_args[[match('-f', full_args) + 1L]])))
}

source('runner_options.R')

archived <- parse_runner_options(character())
stopifnot(!archived$past_only, identical(archived$mode, "archived retrospective reproduction"))

past <- parse_runner_options('--past-only')
stopifnot(past$past_only, identical(past$mode, "past-only re-estimation"))

expect_rejected <- function(args) {
  message <- tryCatch({
    parse_runner_options(args)
    NA_character_
  }, error = function(error) conditionMessage(error))
  stopifnot(!is.na(message), grepl(runner_usage, message, fixed = TRUE))
}

expect_rejected('--past-onyl')
expect_rejected(c('--past-only', '--past-only'))
expect_rejected('unexpected.R')

# End-to-end regression: the real runner must reject a typo before loading
# packages, creating outputs, or selecting the archived future-input lane.
runner <- normalizePath('run_regime_core.R')
rscript <- file.path(R.home('bin'), 'Rscript')
output <- suppressWarnings(system2(rscript, c(runner, '--past-onyl'), stdout = TRUE, stderr = TRUE))
status <- attr(output, 'status')
if (is.null(status)) status <- 0L
stopifnot(status != 0L)
stopifnot(any(grepl('--past-onyl', output, fixed = TRUE)))
stopifnot(any(grepl(runner_usage, output, fixed = TRUE)))

cat('Observed typo rejection with exit status', status, '\n')
cat(paste(output, collapse = '\n'), '\n')
cat('Runner option parser and end-to-end typo regression checks passed\n')
