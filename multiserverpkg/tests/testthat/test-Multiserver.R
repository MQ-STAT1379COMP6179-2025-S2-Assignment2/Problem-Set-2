library(testthat)
library(devtools)
# Note - tests must use positive arival times

test_that("Multiserver returns a tibble with correct columns", {
  result <- Multiserver(
    Arrivals = c(1, 10, 20),
    ServiceTimes = c(5, 5, 5),
    NumServers = 2
  )
  expect_s3_class(result, "tbl_df")  # check if tibble
  expect_true(all(c("Arrivals", "ServiceBegins", "ChosenServer", "ServiceEnds") %in% colnames(result)))
})

test_that("Multiserver handles one customer correctly", {
  result <- Multiserver(
    Arrivals = 10,
    ServiceTimes = 5,
    NumServers = 1
  )
  expect_equal(result$ServiceBegins, 10)
  expect_equal(result$ServiceEnds, 15)
})




