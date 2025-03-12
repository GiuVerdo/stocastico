#' Derivati europei
#'
#' Funzione che calcola lea repliche ed i prezzi di non arbitraggio di un derivato europeo.
#'
#' @param X Variabile casuale del payoff del derivato europeo.
#' @param assets Lista contenente i processi stocastici dei prezzi dei titoli considerati.
#' @return Lista contenente i prezzi di non arbitraggio ed i portafogli di replica (se esistono).
#' @examples
#' #Esempio 1: Esiste ed è unica la misura martingala
#' S0 = list(2, c(4, 1), c(6, 2, 3, 1), c(8, 4, 6, 1, 6, 2, 4, 1/2))
#' S1 = list(4, c(8, 3), c(12, 2, 9, 4), c(16, 4, 12, 1, 12, 8, 8, 2))
#' X = c(8, 0, 22/3, 0, 20/3, 8/3, 13/3, 0)
#' assets = filtrazione(list("S0" = S0, "S1" = S1), blocchi = list(c(2,2), c(2,2,2,2)))
#' europa(X = X, assets = assets)
#'
#' #Esempio 2: Non esiste una misura martingala
#' S0 = list(1, c(1, 1), c(1, 1, 1, 1, 1))
#' S1 = list(3, c(2, 7), c(1, 3, 2, 2, 6))
#' S2 = list(3/2, c(1, 2), c(1, 2, 0, 1, 3))
#' X = c(0, 1, 2, 1, 3)
#' assets = filtrazione(list("S0" = S0, "S1" = S1, "S2" = S2), list(2, c(3, 2)))
#' europa(X = X, assets = assets)
#' @export
europa = function(X, assets) {
  if (is.character(stocastico::martingala(assets))) return(c("Non esiste un portafoglio di replica del derivato (non esiste una misura martingala)"))
  times = length(assets[[1]])
  d = length(assets)
  out = vector(mode = "list", length = times - 1)
  out2 = assets[[1]]
  out2[[times]] = X
  b = X
  for (i in times:2) {
    from = 1:length(assets[[1]][[i - 1]])
    old = matrix(NA, ncol = d, nrow = length(from))
    for (j in 1:d) old[, j] = assets[[j]][[i - 1]]
    b_b = c()
    for (block in from) {
      nomes = unlist(strsplit(names(assets[[1]][[i]]), split = "B"))
      nomes = as.numeric(nomes[seq(from = 1, by = 2, to = length(nomes))])
      A = matrix(NA, ncol = d, nrow = length(which(nomes == block)))
      for (j in 1:d) A[, j] = assets[[j]][[i]][nomes == block]
      out[[i - 1]][[block]] = unlist(lin.sys(A, b[nomes == block], nomi = paste0(i - 1, "α", 0:(d-1), "(",i-1,"B", block,")")))
      if (is.list(out[[i - 1]][[block]])) {
        o = mapply("*", out[[i - 1]][[block]], old[block, ])
        b_b = append(b_b, as.numeric(expand(Reduce("+", o))))
      } else if (!is.character(out[[i - 1]][[block]])) {
        o = out[[i - 1]][[block]] * old[block, ]
        b_b = append(b_b, sum(o))
      } else {
        return(c("Non esiste un portafoglio di replica del derivato"))
      }
    }
    b = b_b
    out2[[i - 1]] = b
  }
  out = unlist(out)
  if (!is.list(out)) out = fractions(out)
  out2 = fractions(unlist(out2))
  names(out2) = names(unlist(assets[[1]]))
  out = list("α" = out, "H" = out2)
  return(out)
}
