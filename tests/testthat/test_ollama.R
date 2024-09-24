test_that("Ollama connection returns true as long as Ollama is started",{
  skip_on_cran()
  expect_true(test_ollama_connection())
})

test_that("Ollama connection is returned",{
  skip_on_cran()
  expect_true(is.list(get_ollama_connection()))
})
