#' Preparazione della filtrazione
#'
#' Funzione che prepara la filtrazione alle funzioni di stocastico.
#' Con iBj si indica il j-esimo blocco di una partizione che nasce dal blocco numero i della partizione precedente.
#'
#' @param assets Lista contenente i processi stocastici considerati.
#' @param blocchi Lista che, per ogni tempo da 1 a N-1, contiene il numero di blocchi della partizione successiva che nascono da quel blocco.
#' @return Lista contenente la filtrazione.
#' @examples
#' S0 = list(1, c(1,2), c(1,2,3,4,5), c(1,2,3,4,5,6,7,8,9, 10, 11, 12))
#' S1 = list(2, c(3,4), c(5,6,7,8,9), c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
#' assets = list("S0" = S0, "S1" = S1)
#' blocchi = list(c(3, 2), c(2, 3, 2, 3, 2))
#' filtrazione(assets = assets, blocchi = blocchi)
#' @export
filtrazione = function(assets, blocchi) {
  k.Max = length(assets[[1]]) - 1
  names(assets[[1]][[1]]) = "0B1"
  names(assets[[1]][[2]]) = paste0("1B", 1:length(assets[[1]][[2]]))
  for (i in 2:k.Max) {
    names(assets[[1]][[i+1]]) = paste0(rep(1:length(blocchi[[i-1]]), times = blocchi[[i-1]]), "B", 1:sum(blocchi[[i-1]]))
    assets[[1]][[i+1]] = unlist(assets[[1]][[i+1]])
  }
  return(assets)
}
