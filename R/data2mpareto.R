# This function is adapted from `graphicalExtremes::data2mpareto`
# Original grapihcalExtremes package by  Engelke S, Hitz A, Gnecco N, Hentschel M (2025).
# https://github.com/sebastian-engelke/graphicalExtremes
data2mpareto <- function (data, p, na.rm = FALSE) 
{
  if (na.rm) {
    naInRow <- apply(is.na(data), 1, any)
    data <- data[!naInRow, , drop = FALSE]
  }
  if (nrow(data) == 0) {
    return(data)
  }
  dataPar <- matrix(1/(1 - apply(data, 2, graphicalExtremes:::unif)), nrow(data), 
                    ncol(data))
  maxData <- apply(dataPar, 1, max, na.rm = TRUE) 
  maxRanks <- rank(maxData, ties.method = "first")
  qRank <- floor(length(maxRanks) * p)
  qMax <- maxData[maxRanks == qRank]
  idx <- suppressWarnings(maxRanks > qRank)
  dataPar <- dataPar[idx, , drop = FALSE]/qMax
  return(dataPar)
}