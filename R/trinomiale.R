#' Mercato trinomiale
#'
#' Funzione che prepara un mercato trinomiale con due titoli rischiosi.
#'
#' @param S1 Valore al tempo 0 del titolo rischioso 1.
#' @param S2 Valore al tempo 0 del titolo rischioso 2.
#' @param r Tasso di interesse periodale.
#' @param u Vettore degli uptick dei due titoli.
#' @param m Vettore dei mediumtick dei due titoli.
#' @param d Vettore dei downtick dei due titoli.
#' @param N Numero di periodi considerati per la filtrazione.
#' @return Lista contenente il mercato trinomiale.
#' @examples
#' trinomiale(S1 = 1, S2 = 1, r = 0.5, u = c(7/3, 22/9), m = c(1,1), d = c(1/2, 1/3), N = 2)
#' @export
trinomiale = function(S1, S2, r, u, m, d, N) {
  S0 = list()
  x = list(S1)
  y = list(S2)
  umd1 = c(u[1], m[1], d[1])
  umd2 = c(u[2], m[2], d[2])
  blocchi = list()
  for (i in 2:N) {blocchi[[i-1]] = rep(3, 3^(i-1))}
  for (i in 1:(N+1)) {S0[[i]] = rep((1+r)^(i-1), 3^(i-1))}#ok
  for (i in 1:N) {x[[i+1]] = rep(x[[i]],3) * rep(umd1, each = 3^(i-1))}
  for (i in 1:N) {y[[i+1]] = rep(y[[i]],3) * rep(umd2, each = 3^(i-1))}
  out = stocastico::filtrazione(list("S0" = S0, "S1" = x, "S2" = y), blocchi)
  return(out)
}
