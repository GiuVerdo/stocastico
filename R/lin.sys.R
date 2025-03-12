#' Sistemi lineari
#'
#' Funzione che risolve sistemi lineari.
#'
#' @param A Matrice dei coefficienti del sistema (dimensione n x p).
#' @param b Vettore dei termini noti (vettore di lunghezza n).
#' @param nomi Nomi da associare ai risultati (vettore di lunghezza p).
#' @return Soluzione del sistema.
#' @examples
#' #Esempio 1: Soluzione unica
#' A = matrix(c(1, 2, 3, 8, 2, 1, 1, 6, 4), ncol = 3)
#' b = c(3, 1, 5)
#' lin.sys(A = A, b = b)
#'
#' #Esempio 2: Infinite soluzioni
#' A = matrix(c(1, 1, 1, 1, 3/2, 2), ncol = 3, byrow = TRUE)
#' b = c(1, 3/2)
#' lin.sys(A = A, b = b)
#'
#' #Esempio 3: Nessuna soluzione
#' A = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3)
#' b = c(3, 1, 5)
#' lin.sys(A = A, b = b)
#' @export
lin.sys = function(A, b, nomi = NA) {
  Ab = cbind(A, b)
  rA = qr(A)$rank
  rAb = qr(Ab)$rank
  if (rA != rAb) {out = "Il sistema non ha soluzione"}
  else {
    if (any(is.na(nomi)) | length(nomi) != ncol(A)) {nomi = paste0("x", 1:ncol(A))}
    if (rA == nrow(A) && rA == ncol(A)) {out = solve(A, b)}
    else {
      triAb = gaussianElimination(Ab)
      susrows = rep(TRUE, nrow(triAb))
      for (i in 1:nrow(triAb)) {if (all(triAb[i,] == 0)) {susrows[i] = FALSE}}
      triAb = triAb[susrows,]
      dia = diag(TRUE, nrow = nrow(triAb), ncol = ncol(triAb))
      dia[,ncol(dia)] = TRUE
      if (any(triAb[!dia] != 0)) {
        symbs = list()
        modtriAb = triAb
        modtriAb[dia] = 0
        for (i in 1:ncol(A)) {symbs[[i]] = Symbol(nomi[i])}
        out = symbs
        for (i in 1:nrow(triAb)) {
          out[[i]] = 0
          for (j in 1:ncol(A)) {
            out[[i]] = symbs[[j]] * modtriAb[i,j] + out[[i]]
          }
          out[[i]] = expand(triAb[i,ncol(triAb)] - out[[i]])
        }
        for (i in (nrow(triAb)+1):ncol(A)) {out[[i]] = symbs[[i]]}
      }
      else {
        out = triAb[,ncol(triAb)]
      }
    }
  }
  if (!is.character(out)) {names(out) = nomi}
  return(out)
}
