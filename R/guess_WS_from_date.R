
estimate_version <- function(date, start_date, start_version, periods_per_year){

  nb_days_since_start <- (date - start_date) |>
    as.numeric()

  days_per_period <- 365/periods_per_year
  nb_periods_since_start <- nb_days_since_start / days_per_period

  floor(start_version + nb_periods_since_start)

}







#' Guess Wormbase release from date
#'
#' Note these estimates are approximatives, for exact dates please refer to the official
#' release schedule available at: [https://wormbase.org/about/release_schedule]
#'
#' @param date a Date object or an object that can be coerced into one.
#'
#'
#' @returns An estimate of the current production release of Wormbase at that date.
#' @export
#'
#' @examples
#' guess_WS_from_date("2018-01-01")
#' guess_WS_from_date("2028-04-10")
#' guess_WS_from_date("2008-04-01")
guess_WS_from_date <- function(date){

  date <- as.Date(date)



  # adapted from https://wormbase.org/about/release_schedule
  # 2021 to present: quarterly schedule. Count quarters since WS280 in May 2021
  # Mar 2011 (WS225) to Dec 2020 (WS279): bimonthly
  #    split with before/after 2016 to correct drift
  # From 2008 (WS187) to 2011 (WS225), monthly
  # older than 2008: too inconsistent, refer to official schedule
  eras <- data.frame(
    start_date = as.Date(c("2021-05-01", "2016-04-01", "2011-06-01", "2008-03-01")),
    start_version = c(280, 252, 225, 187),
    periods_per_year = c(4, 6, 6, 12)
  )



  era <- which(date > eras$start_date) |> utils::head(1)

  if(length(era) == 0L) stop("Too old, please refer to https://wormbase.org/about/release_schedule")

  estimated <- estimate_version(date,
                   eras$start_date[[era]],
                   eras$start_version[[era]],
                   eras$periods_per_year[[era]])

  paste0("WS", estimated)
}

guess_WS_from_date("2018-04-01")













