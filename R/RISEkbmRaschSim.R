### RISE KBM Rasch analysis package, https://github.com/pgmj/RISEkbmRasch
### Created by magnus.p.johansson@ri.se ORCID: 0000-0003-1669-592X
### The code in this file was written by nicklas.korsell@ri.se
### The contents of this file is licensed according to
### Creative Commons Attribution 4.0 International Public License
### https://creativecommons.org/licenses/by/4.0/

### See https://pgmj.github.io/raschrvignette/RaschRvign.html for vignette.

#' Simulate polytomous Rasch model data for a single item
#'
#' Documentation will be expanded on (and/or translated to English)
#' at a later point in time.
#'
#' @param x Vector with item threshold locations
#' @param thetas Vector of person thetas/scores
#' @export
SimPolyItem <- function(x, thetas) {
  k <- length(x) + 1 # Antal kategorier för denna item
  n <- length(thetas) # Antal respondenter

  # Tar diffen mellan varje theta och varje delta
  Y <- outer(thetas, x, "-")

  # Kumulativa summan (theta -delta1) + (theta - delta2) + ...
  cumsums <-
    t(
      rbind(
        rep(0, times = n), # Lägg på en rad med 0:or
        apply(X = Y, MARGIN = 1, FUN = cumsum)
      )
    )

  # Exponentiera
  expcumsums <- exp(cumsums)

  # Beräkna nämnaren i normeringen
  norms <- apply(X = expcumsums, MARGIN = 1, FUN = sum)

  # Gör själva normeringen
  z <- expcumsums / norms

  # Nu gör vi 1 simulering för denna item för alla individer
  vapply(
    X = 1:n,
    FUN = function(x) {
      sample(x = 0:(k - 1), size = 1, replace = TRUE, prob = z[x, ])
    },
    FUN.VALUE = 1
  )
}

#' Simulate polytomous Rasch model data for a set of items
#'
#' Documentation will be expanded on at a later point in time.
#'
#' @param deltaslist List object with item threshold locations
#' @param thetas Vector of person thetas/scores
#' @export
SimPartialScore <- function(deltaslist, thetavec) {
  # Gör om varje element i deltalist till en numerisk vector
  deltaslist <- lapply(X = deltaslist, FUN = unlist)

  # Anropa SimPolyItem för varje item
  sapply(
    X = deltaslist,
    FUN = SimPolyItem,
    thetas = thetavec
  )
}
