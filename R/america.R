#' Derivati americani
#'
#' Funzione che calcola i prezzi di non arbitraggio, le repliche e le strategie di arresto di un derivato americano.
#' Si assume che il numeraire (S0) sia un processo deterministico e che il mercato sia completo.
#'
#' @param X Processo stocastico del payoff del derivato americano.
#' @param assets Lista contenente i processi stocastici dei prezzi dei titoli considerati.
#' @param errore Numero di decimali da considerare per l'approssimazione per vmin.
#' @param all_ports Paramentro per scegliere se costruire a prescindere tutti i portafogli o solo quelli razionali.
#' @return Lista contenente i prezzi di non arbitraggio, i portafogli di replica e le strategie di arresto vmin e vmax (se questi esistono).
#' @examples
#' assets = binomiale(S1 = 1, r = 0, u = 2, d = 1/2, N = 3)
#' X = list(2/3, c(5/3, 1/6), c(0, 2/3, 2/3, 0), c(0, 0, 5/3, 1/6, 5/3, 1/6, 1/6, 0))
#' america(X = X, assets = assets, errore = 8, all.ports = FALSE)
#' @export
america = function(X, assets, errore = 8, all.ports = FALSE) {
  P = stocastico::martingala(assets)
  if (!is.list(P)) return(c("Il mercato non è completo")) else if (is.list(P$M)) return(c("Il mercato non è completo")) else P = P$M
  X = stocastico::sconto(list(assets[[1]], X))[[2]]
  N = length(X)
  assets_s = stocastico::sconto(assets)
  probs = X
  probs[[N]] = P
  H = X
  W = X
  d = length(assets)
  alpha = c()
  b = X[[N]]
  for (i in (N-1):1) {
    alfs = c()
    for (j in 1:length(X[[i]])) {
      nomi_i =  unlist(strsplit(names(assets[[1]][[i+1]]), split = "B"))
      nomi_i = as.numeric(nomi_i[seq(1, length(nomi_i), by = 2)])
      probs[[i]][j] = sum(probs[[i+1]][j == nomi_i])
      probs[[i+1]][nomi_i == j] = probs[[i+1]][nomi_i == j]/probs[[i]][j]
      W[[i]][j] = sum(probs[[i+1]][nomi_i==j] * H[[i+1]][nomi_i == j])
      H[[i]][j] = max(X[[i]][j], W[[i]][j])
      A = matrix(NA, ncol = d, nrow = length(which(nomi_i == j)))
      for (l in 1:d) A[, l] = assets_s[[l]][[i+1]][nomi_i == j]
      b_b = b[which(nomi_i == j)]
      alfs = c(alfs, lin.sys(A, b_b, nomi = paste0(i, "α", 0:(d-1), "(",i-1,"B", j,")")))
    }
    alpha = c(alfs, alpha)
    b = H[[i]]
  }
  alpha = fractions(alpha)
  for (i in 1:length(X)) {names(X[[i]]) = names(assets[[1]][[i]])}
  port = stocastico::doob(H, P)$Z
  H = mapply("*", H, assets[[1]])
  H = lapply(H, fractions)
  vmin = X
  for (i in 1:N) {vmin[[i]] = rep(i, length(X[[i]]))}
  vmax = vmin
  for (i in 2:(N-1)) {
    nomipost = unlist(strsplit(names(assets[[1]][[i+1]]), split = "B"))
    nomipost = as.numeric(nomipost[seq(1, length(nomipost), 2)])
    nomiold = unlist(strsplit(names(assets[[1]][[i]]), split = "B"))
    nomiold = as.numeric(nomiold[seq(1, length(nomiold), 2)])
    for (j in 1:length(X[[i]])) {
      if (as.logical(vmax[[i]][j] != vmax[[i-1]][nomiold[j]])) {
        if (as.logical(X[[i]][j] > W[[i]][j])) {vmax[[i]][j] = i; vmax[[i+1]][nomipost == j] = i; port[[i]][j] = -1; port[[i+1]][nomipost == j] = -1}
      } else {vmax[[i+1]][nomipost == j] = vmax[[i]][j]; port[[i+1]][nomipost == j] = 0}
      if (as.logical(vmin[[i]][j] != vmin[[i-1]][nomiold[j]])) {
        if (as.logical(round(X[[i]][j], errore) >= round(W[[i]][j], errore))) {vmin[[i]][j] = i; vmin[[i+1]][nomipost == j] = i}
      } else {vmin[[i+1]][nomipost == j] = vmin[[i]][j]}
    }
  }
  vmin = vmin[[N]]-1
  vmax = vmax[[N]]-1
  port = rep(unlist(port[1:(N-1)]), each = length(assets))
  port = if (all.ports) {rep(TRUE, length(port))} else {ifelse(port == 0, TRUE, FALSE); port[1:d] = TRUE}
  if (as.logical(X[[1]] > W[[1]])) {vmax = rep(0, length(assets[[1]][[N]])); if(!all.ports) {port[1:d] = FALSE}}
  if (as.logical(round(X[[1]], errore) >= round(W[[1]], errore))) {vmin = rep(0, length(assets[[1]][[N]]))}
  alpha = if(any(port)) {alpha[port]} else {"Non si costruiscono repliche del derivato (non è razionale)"}
  out = list("α" = alpha, "H" = H, "vmin" = vmin, "vmax" = vmax)
  return(out)
}
