#' Extract Smooths from BBS model run
#'
#' @param indata dataframe of generated smooths. Each column is a year, each row is a predicted smooth
#' @param start_yr numeric year at which to start, if subset is required. Default is the last year available
#' @param end_yr numeric year at which to end, if subset is required. Default is the last year available
#'
#' @return dataframe with smooths generated for each year of the selected range
#' @export
#'
#' @examples
#' \dontrun{
#'smooths <- fit_smooths(indat3, start_yr = 1990, end_yr = 2020)
#'}
fit_smooths <- function(indata, start_yr = NA, end_yr = NA){

  #indata <- indat3

  # if a double - convert to dataframe

  allyr_seq <- colnames(indata) |>  as.numeric()

  # if years are to be filtered do this here:
  if(is.na(start_yr)) {
    start_year <- min(allyr_seq)
  } else {
    start_year <- start_yr
  }

  if(is.na(end_yr)) {
    end_year <- max(allyr_seq)
  } else {
    end_year <- end_yr
  }

  # create a list of year to use
  year_seq = allyr_seq
  year_seq = year_seq[year_seq >= start_year]
  year_seq = year_seq[year_seq <= end_year]

  # filter years of interest
  smooth_out <- indata |>
    dplyr::select(dplyr::all_of(as.character(year_seq)))

  return(smooth_out)

}
