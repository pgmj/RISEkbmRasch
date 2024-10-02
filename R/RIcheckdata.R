### RISE KBM Rasch analysis package, https://github.com/pgmj/RISEkbmRasch
### Created by magnus.p.johansson@ri.se ORCID: 0000-0003-1669-592X
### The contents of this file is licensed according to
### Creative Commons Attribution 4.0 International Public License
### https://creativecommons.org/licenses/by/4.0/

### See https://pgmj.github.io/raschrvignette/RaschRvign.html for vignette.

#' Check dataframe
#'
#' This functions checks for items with low number of responses in any category.
#'
#' For now, low number is set to < 3.
#'
#' @param data Font family for all plot text
#' @param n Lowest number of responses in a cell
#' @return Whether there are issues with the data or not (TRUE or FALSE)
#' @export
RIcheckdata <- function(data, n = 3) {

  if (min(as.matrix(data), na.rm = T) > 0) {
    stop("The lowest response category needs to coded as 0. Please recode your data.")
  }

  # count number of responses per cell (item & response category)
  allcells_n <- data %>%
    pivot_longer(everything()) %>%
    dplyr::count(name,value, .drop = FALSE) %>%
    pull(n)

  # check whether any cells have fewer than 3 responses and return logical TRUE/FALSE
  return(any(allcells_n < n))
}
