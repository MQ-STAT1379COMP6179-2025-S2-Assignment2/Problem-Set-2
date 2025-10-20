#' Multi-server queue simulation
#'
#' Simulates a first-come, first-served queue with multiple servers.
#'
#' @param Arrivals Numeric vector of customer arrival times (seconds).
#' @param ServiceTimes Numeric vector of service durations (seconds).
#' @param NumServers Number of servers available (default 1).
#' @return A tibble with columns:
#' \describe{
#'   \item{Arrivals}{Customer arrival times}
#'   \item{ServiceBegins}{When the service starts for each customer}
#'   \item{ChosenServer}{Which server serves the customer}
#'   \item{ServiceEnds}{When the service ends for each customer}
#' }
#' @examples
#' multiserver(c(0,10,20), c(5,5,5), 2)
#' @export


Multiserver <- function(Arrivals, ServiceTimes, NumServers = 1) {
  if (any(Arrivals <= 0 | ServiceTimes <= 0) || NumServers <= 0){
    stop("Arrivals, ServiceTimes must be positive & NumServers must be positive" )
  }
  if (length(Arrivals) != length(ServiceTimes)){
    stop("Arrivals and ServiceTimes must have the same length")
  }
# Feed customers through a multiserver queue system to determine each
# customer's transition times.

NumCust <- length(Arrivals)  #  number of customer arrivals
# When each server is next available (this will be updated as the simulation proceeds):
AvailableFrom <- rep(0, NumServers)
# i.e., when the nth server will next be available

# These variables will be filled up as the simulation proceeds:
ChosenServer <- ServiceBegins <- ServiceEnds <- rep(0, NumCust)

# ChosenServer = which server the ith customer will use
# ServiceBegins = when the ith customer's service starts
# ServiceEnds = when the ith customer's service ends

# This loop calculates the queue system's "Transitions by Customer":
for (i in seq_along(Arrivals)){
  # go to next available server
  avail <-  min(AvailableFrom)
  ChosenServer[i] <- which.min(AvailableFrom)
  # service begins as soon as server & customer are both ready
  ServiceBegins[i] <- max(avail, Arrivals[i])
  ServiceEnds[i] <- ServiceBegins[i] + ServiceTimes[i]
  # server becomes available again after serving ith customer
  AvailableFrom[ChosenServer[i]] <- ServiceEnds[i]
}
  out <- tibble::tibble(Arrivals, ServiceBegins, ChosenServer, ServiceEnds)
  return(out)
}

library(usethis)
usethis::use_testthat()
usethis::use_test("Multiserver")
