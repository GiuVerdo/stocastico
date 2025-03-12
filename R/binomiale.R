#' Mercato binomiale
#'
#' Funzione che prepara un mercato binomiale con un titolo rischioso.
#'
#' @param S1 Valore al tempo 0 del titolo rischioso.
#' @param r Tasso di interesse periodale.
#' @param u Uptick.
#' @param d Downtick.
#' @param N Numero di periodi considerati per la filtrazione.
#' @return Lista contenente il mercato binomiale.
#' @examples
#' binomiale(S1 = 1, r = 1/2, u = 2, d = 1/2, N = 3)
#' @export
binomiale = function(S1, r, u, d, N) {
  S0 = list()
  x = list(S1)
  ud = c(u,d)
  blocchi = list()
  for (i in 2:N) {blocchi[[i-1]] = rep(2, 2^(i-1))}
  for (i in 1:(N+1)) {S0[[i]] = rep((1+r)^(i-1), 2^(i-1))}
  for (i in 1:N) {x[[i+1]] = rep(x[[i]],2) * rep(ud, each = 2^(i-1))}
  out = stocastico::filtrazione(list("S0" = S0, "S1" = x), blocchi)
  return(out)
}
