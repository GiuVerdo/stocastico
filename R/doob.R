#' Decomposizione di Doob
#'
#' Funzione che calcola la decomposizione di Doob di un processo stocastico X data una misura di probabilità P
#'
#' @param X Processo stocastico da decomporre.
#' @param P Misura di probabilità usata per la decomposizione.
#' @return Lista contenente le componenti Y e Z della decomposizione di Doob.
#' @examples
#' X = list(3, c(2, 2, 3), c(1, 2, 3, 4, 5, 6, 7))
#' X = filtrazione(list(X), blocchi = list(c(3, 2, 2)))[[1]]
#' P = c(1/4, 0, 1/4, 0, 1/4, 0, 1/4)
#' doob(X = X, P = P)
#' @export
doob = function(X, P) {
  N = length(X)
  probs = X
  probs[[N]] = P
  Z = X
  Z[[1]] = 0
  for (i in (N-1):1) {
    for (j in 1:length(X[[i]])) {
      nomi_i =  unlist(strsplit(names(X[[i+1]]), split = "B"))
      nomi_i = as.numeric(nomi_i[seq(1, length(nomi_i), by = 2)])
      probs[[i]][j] = sum(probs[[i+1]][j == nomi_i])
      probs[[i+1]][nomi_i == j] = probs[[i+1]][nomi_i == j]/probs[[i]][j]
    }
  }
  for (i in 2:N) {
    o = unlist(strsplit(names(X[[i]]), split = "B"))
    o = as.numeric(o[seq(1, length(o), by = 2)])
    Z[[i]] =  rep(aggregate(X[[i]] * probs[[i]], by = list(o), FUN = sum)$x - X[[i-1]] + Z[[i-1]], table(o))
    Z[[i]] = fractions(Z[[i]])
  }
  Y = mapply("-", X, Z)
  out = list("Y" = Y, "Z" = Z)
  return(out)
}
