# Offline evaluation of check_prompt_quality() against labeled prompts. Calls the real LLM
# (~150 short calls, well under $1), so it lives outside tests/testthat/ and never runs
# with devtools::test(). Needs AZURE_OPENAI_API_KEY / AZURE_OPENAI_API_ENDPOINT (.Renviron).
#
# Run from the repo root:   Rscript tests/eval_prompt_quality.R [results.csv]
#
# Sources:
#   uiuc_testing_responses.csv   tester feedback form (not committed: contains netIDs). Optional.
#   tests/prompt_quality_tests.csv   older hand-written cases. Its "vague" labels follow the old
#     strict rubric, so they are reported by bucket, not scored as pass/fail.
suppressMessages(devtools::load_all(quiet = TRUE))
out_file <- commandArgs(TRUE)[1]
key <- list(key = "", switch_on = FALSE)  # -> Azure credentials from the environment

cols_for <- function(dataset) {
  f <- file.path("inst/app/www/demo_data", paste0(gsub(" ", "_", tolower(dataset)), ".csv"))
  if (!file.exists(f)) return(list(names = character(0), types = character(0)))
  d <- read.csv(f, nrows = 200)
  list(names = names(d), types = vapply(d, function(x) class(x)[1], character(1)))
}

cases <- list()
if (file.exists("uiuc_testing_responses.csv")) {
  u <- read.csv("uiuc_testing_responses.csv", stringsAsFactors = FALSE)
  flag <- grepl("should have", u[[7]])                       # tester wanted a flag
  flag[grepl("pickup line", u[[6]], ignore.case = TRUE)] <- TRUE            # label ruling
  flag[grepl("line graph of protein by diet", u[[6]], ignore.case = TRUE)] <- FALSE  # label ruling
  cases$uiuc <- data.frame(
    source = "uiuc", dataset = u[[5]], prompt = u[[6]], follow_up = FALSE,
    expected = ifelse(!flag, "ok", ifelse(u[[4]] == "Random", "off_topic", "vague")),
    stringsAsFactors = FALSE)
}
o <- read.csv("tests/prompt_quality_tests.csv", stringsAsFactors = FALSE)
cases$old <- data.frame(
  source = "old", dataset = sub(" \\(.*", "", o$dataset), prompt = o$prompt,
  follow_up = grepl("after", o$dataset), expected = o$expected_verdict, stringsAsFactors = FALSE)
cases <- do.call(rbind, cases)

cases$got <- NA_character_; cases$detail <- ""
for (i in seq_len(nrow(cases))) {
  cc <- cols_for(cases$dataset[i])
  r <- suppressMessages(check_prompt_quality(cases$prompt[i], key, cases$dataset[i], cc$names, cc$types,
                                             off_topic_only = cases$follow_up[i]))
  cases$got[i] <- if (r$verdict == "ok" && length(r$suggestions) > 0) "ok+note" else r$verdict
  cases$detail[i] <- paste(c(r$missing, r$suggestions), collapse = " | ")
  cat(sprintf("\r%d/%d", i, nrow(cases)))
}
cat("\n\n")

blocked <- cases$got %in% c("vague", "off_topic")
for (src in unique(cases$source)) {
  d <- cases[cases$source == src, ]; b <- blocked[cases$source == src]
  cat("==== ", src, " (", nrow(d), " prompts) ====\n", sep = "")
  print(table(expected = d$expected, got = d$got))
  cat(sprintf("should run, but BLOCKED (false positives): %d of %d\n", sum(d$expected == "ok" & b), sum(d$expected == "ok")))
  cat(sprintf("off-topic, but NOT blocked:               %d of %d\n", sum(d$expected == "off_topic" & d$got != "off_topic"), sum(d$expected == "off_topic")))
  v <- d$expected == "vague"
  cat(sprintf("labeled vague: %d blocked, %d ran with a note, %d ran clean\n\n", sum(v & b), sum(v & d$got == "ok+note"), sum(v & d$got == "ok")))
}
bad <- cases[(cases$expected == "ok" & blocked) | (cases$expected == "off_topic" & cases$got != "off_topic") |
             (cases$expected == "vague" & cases$got %in% c("ok", "off_topic")), ]
cat("==== prompts to look at ====\n")
for (i in seq_len(nrow(bad))) cat(sprintf("[%s | expected %s | got %s] %s\n      -> %s\n", bad$source[i], bad$expected[i], bad$got[i],
                                          gsub("\\s+", " ", bad$prompt[i]), bad$detail[i]))
if (!is.na(out_file)) write.csv(cases, out_file, row.names = FALSE)
