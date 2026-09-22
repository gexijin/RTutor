# Offline tests for the prompt gate. The LLM is mocked, so these cost nothing.
# The live accuracy check is tests/eval_prompt_quality.R.
# Run:  testthat::test_file("tests/testthat/test-prompt_quality.R")

fake_llm <- function(json) function(...) list(
  choices = data.frame(message.content = json, stringsAsFactors = FALSE),
  usage = list(prompt_tokens = 1, completion_tokens = 1))
check <- function(json, ...) {
  testthat::local_mocked_bindings(create_response = fake_llm(json))
  suppressMessages(check_prompt_quality("p", list(key = "", switch_on = FALSE), "ds", c("a", "b"), ...))
}
facts <- function(off = "false", type = '"scatterplot"', vars = '["a","b"]', need = "true", given = "true",
                  missing = "[]", sugg = "[]", pt = "true")
  sprintf('{"off_topic":%s,"plot_or_table":%s,"output_type":%s,"variables":%s,"roles_needed":%s,"roles_given":%s,"missing":%s,"suggestions":%s}',
          off, pt, type, vars, need, given, missing, sugg)

test_that("verdict is computed from the fact fields", {
  expect_equal(check(facts(off = "true"))$verdict, "off_topic")
  expect_equal(check(facts())$verdict, "ok")
  expect_equal(check(facts(type = "null"))$verdict, "vague")
  expect_equal(check(facts(vars = "[]"))$verdict, "vague")
  r <- check(facts(given = "false"))                       # roles missing, model gave no wording
  expect_equal(r$verdict, "vague"); expect_match(r$missing, "role")
  expect_equal(check(facts(given = "false", missing = '["Say which is x."]'))$missing, "Say which is x.")
})

test_that("hedged `missing` text cannot block when the facts are all present", {
  expect_equal(check(facts(missing = '["Clarify whether you mean a."]'))$verdict, "ok")
})

test_that("non plot/table prompts are never blocked for detail", {
  expect_equal(check(facts(pt = "false", type = "null", vars = "[]"))$verdict, "ok")
})

test_that("suggestions only accompany an ok verdict", {
  expect_equal(check(facts(sugg = '["Specify axis limits."]'))$suggestions, "Specify axis limits.")
  expect_length(check(facts(type = "null", sugg = '["x"]'))$suggestions, 0)
})

test_that("follow-ups are screened for off-topic only", {
  expect_equal(check('{"off_topic":false}', off_topic_only = TRUE)$verdict, "ok")
  expect_equal(check('{"off_topic":true}',  off_topic_only = TRUE)$verdict, "off_topic")
})

test_that("switch off disables detail blocking and tips but keeps off-topic", {
  testthat::local_mocked_bindings(vague_check_enabled = FALSE)
  r <- check(facts(type = "null", sugg = '["x"]'))
  expect_equal(r$verdict, "ok"); expect_length(r$suggestions, 0)
  expect_equal(check(facts(off = "true"))$verdict, "off_topic")
})

test_that("fails open on bad JSON or API error", {
  expect_equal(check("not json")$verdict, "ok")
  testthat::local_mocked_bindings(create_response = function(...) stop("down"))
  expect_equal(suppressMessages(check_prompt_quality("p", list(key = ""), "ds", "a"))$verdict, "ok")
})

# ---- submit flow in mod_03 -------------------------------------------------
run_flow <- function(verdicts, follow_up = FALSE, steps) {
  calls <- list(); i <- 0
  testthat::local_mocked_bindings(check_prompt_quality = function(..., off_topic_only = FALSE) {
    i <<- i + 1; calls[[i]] <<- off_topic_only
    v <- verdicts[[min(i, length(verdicts))]]
    list(verdict = v, missing = if (v == "vague") "Name the columns." else character(0),
         suggestions = character(0), usage = NULL)
  })
  cleared <- shiny::reactiveVal(0)
  shiny::testServer(mod_03_send_request_serv, args = list(
    chunk_selection = shiny::reactiveValues(), user_file = shiny::reactive(NULL),
    selected_dataset_name = shiny::reactive("mpg"), use_python = shiny::reactive(FALSE),
    quality_cleared = cleared, api_key = list(key = ""), current_data = shiny::reactive(mtcars),
    do_soft_reset = function() NULL, counter = shiny::reactiveValues(costs_total = 0),
    is_follow_up = shiny::reactive(follow_up)), {
    steps(session, cleared)
  })
  calls
}
submit <- function(session, text, n) session$setInputs(input_text = text, submit_button = n)

test_that("vague blocks once; the same prompt resubmitted runs without a second check", {
  calls <- run_flow(list("vague"), steps = function(session, cleared) {
    submit(session, "Make a bar graph of something", 1); expect_equal(shiny::isolate(cleared()), 0)
    submit(session, "Make a bar graph of something", 2); expect_equal(shiny::isolate(cleared()), 1)
  })
  expect_length(calls, 1)
})

test_that("off-topic never runs, however many times Submit is clicked", {
  run_flow(list("off_topic"), steps = function(session, cleared) {
    for (n in 1:3) submit(session, "How do I change a flat tire?", n)
    expect_equal(shiny::isolate(cleared()), 0)
  })
})

test_that("editing a warned prompt re-checks it for off-topic only, so it cannot be blocked for detail twice", {
  calls <- run_flow(list("vague", "ok"), steps = function(session, cleared) {
    submit(session, "Make a bar graph of something", 1)
    submit(session, "Make a bar graph of other thing", 2); expect_equal(shiny::isolate(cleared()), 1)
  })
  expect_equal(unlist(calls), c(FALSE, TRUE))
})

test_that("follow-up prompts are checked in off-topic-only mode", {
  calls <- run_flow(list("ok"), follow_up = TRUE, steps = function(session, cleared) {
    submit(session, "make the line red please", 1); expect_equal(shiny::isolate(cleared()), 1)
  })
  expect_true(calls[[1]])
})
