#' Calcolo di misura martingala
#'
#' Funzione che calcola la misura martingala di un mercato.
#'
#' @param assets Lista contenente i processi stocastici dei prezzi dei titoli considerati.
#' @return Lista contenente la misura martingala (se esiste) e le probabilità condizionate dei blocchi.
#' @examples
#' #Esempio 1: Esiste ed è unica la misura martingala
#' S0 = list(2, c(4, 1), c(6, 2, 3, 1), c(8, 4, 6, 1, 6, 2, 4, 1/2))
#' S1 = list(4, c(8, 3), c(12, 2, 9, 4), c(16, 4, 12, 1, 12, 8, 8, 2))
#' assets = filtrazione(list("S0" = S0, "S1" = S1), blocchi = list(c(2, 2), c(2, 2, 2, 2)))
#' martingala(assets = assets)
#'
#' #Esempio 2: Esiste più di una misura martingala
#' S0 = list(1, c(1, 1), c(1, 1, 1, 1, 1))
#' S1 = list(2, c(3/2, 4), c(1, 3/2, 2, 2, 4))
#' assets = filtrazione(list("S0" = S0, "S1" = S1), blocchi = list(c(3, 2)))
#' martingala(assets = assets)
#'
#' #Esempio 3: Non esiste una misura martingala
#' S0 = list(1, c(1, 1), c(1, 1, 1, 1, 1))
#' S1 = list(3, c(2, 7), c(1, 3, 2, 2, 6))
#' S2 = list(3/2, c(1, 2), c(1, 2, 0, 1, 3))
#' assets = filtrazione(list("S0" = S0, "S1" = S1, "S2" = S2), list(2, c(3, 2)))
#' martingala(assets = assets)
#' @export
martingala = function(assets) {
  assets = stocastico::sconto(assets)
  times = length(assets[[1]])
  out = vector(mode = "list", length = times-1)
  out2 = vector(mode = "list", length = times)
  out2[[1]] = 1
  for (i in 2:times) {
    from = 1:length(assets[[1]][[i - 1]])
    for (block in from) {
      nomes = unlist(strsplit(names(assets[[1]][[i]]), split = "B"))
      nomes = as.numeric(nomes[seq(from = 1, by = 2, to = length(nomes))])
      A = matrix(NA, nrow = length(assets), ncol = length(which(nomes == block)))
      b = rep(NA, length(assets))
      for (j in 1:length(assets)) {
        A[j, ] = assets[[j]][[i]][nomes == block]
        b[j] = assets[[j]][[i - 1]][block]
      }
      out[[i - 1]][[block]] = unlist(lin.sys(A, b, nomi = paste0(i - 1, "x", which(nomes == block))))
      out2[[i]][[block]] = if (is.list(out[[i - 1]][[block]])) {
        list(lapply(out[[i - 1]][[block]], "*", unlist(out2[[i-1]])[[block]]))
      } else if (!is.character(out[[i - 1]][[block]]))
      {list(out[[i - 1]][[block]] * unlist(out2[[i-1]])[[block]])} else {return(c("Non esiste una misura martingala"))}
      out2[[i]][[block]] = unlist(out2[[i]][[block]])
    }
  }
  out = unlist(out)
  out2 = as.list(expand(unlist(out2[[length(out2)]])))
  names(out2) = paste0("M(ω",1:length(out2),")")
  is.number = unlist(lapply(out2, function(x) {if (length(free_symbols(x)) == 0) {is.numeric(as.numeric(x))} else {FALSE}} ))
  if (all(is.number)) {out2 = fractions(unlist(lapply(out2, function(x) as.numeric(x)))); out = fractions(out)}
  l = if (is.list(out)) {unlist(lapply(out, FUN = function(x) {if (is.numeric(x)) {x = as.numeric(x); x < 0 | x > 1} else {FALSE}}))} else {out < 0 | out > 1}
  if (any(l)) {out = c("Non esiste una misura martingala")} else {out = list("P(Bk+|Bk)" = out, "M" = out2)}
  return(out)
}
