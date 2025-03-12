#' Mercato scontato
#'
#' Funzione che sconta il mercato.
#'
#' @param assets Lista contenente i processi stocastici dei prezzi dei titoli considerati.  Il primo titolo in assets deve essere il numeraire.
#' @return Lista contenente il mercato scontato.
#' @examples
#' S0 = list(2, c(4, 1), c(6, 2, 3, 1), c(8, 4, 6, 1, 6, 2, 4, 1/2))
#' S1 = list(4, c(8, 3), c(12, 2, 9, 4), c(16, 4, 12, 1, 12, 8, 8, 2))
#' X = c(8, 0, 22/3, 0, 20/3, 8/3, 13/3, 0)
#' assets = filtrazione(list("S0" = S0, "S1" = S1), blocchi = list(c(2,2), c(2,2,2,2)))
#' sconto(assets)
#' @export
sconto = function(assets) {
  out = assets
  for (i in 1:length(assets)) {
    for (j in 1:length(assets[[i]])) {out[[i]][[j]] = assets[[i]][[j]]/assets[[1]][[j]]}
  }
  return(out)
}
