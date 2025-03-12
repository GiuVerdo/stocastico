#' Lemma di Ito
#'
#' Funzione che determina l'equazione differenziale stocastica associata ad un processo stocastico generico Xt utilizzando il Lemma di Ito.
#'
#' @param Yt Funzione che determina il processo stocastico rispetto a cui si applica il lemma.
#' @param dXt Equazione differenziale associata al processo Xt di partenza.
#' @return Equazione differenziale stocastica di cui Yt è soluzione.
#' @examples
#' #Esempio 1: Moto Browniano
#' Yt = "mu*t + sigma*Xt"
#' dXt = "0*dt + 1*dBt"
#' Ito(Yt = Yt, dXt = dXt)
#'
#' #Esempio 2: Moto Browniano geometrico
#' Yt = "X0*exp(mu*t + sigma*Xt)"
#' dXt = "0*dt + 1*dBt"
#' Ito(Yt = Yt, dXt = dXt)
#'
#' #Esempio 3
#' Yt = "log(Xt)"
#' dXt = "mu*Xt*dt + sigma*Xt*dBt"
#' Ito(Yt = Yt, dXt = dXt)
#' @export
Ito = function(Yt = "Xt", dXt = "0*dt + 1*dBt") {
  a = paste0("(", as.character(as.expression(nlsDeriv(dXt, "dt"))), ")")
  b = paste0("(", as.character(as.expression(nlsDeriv(dXt, "dBt"))) ,")")
  DEYt = as.character(as.expression(nlsDeriv(Yt, "t")))
  DEYX = paste0("(", as.character(as.expression(nlsDeriv(Yt, "Xt"))), ")")
  DEYX2 = paste0("(", as.character(as.expression(nlsDeriv(DEYX, "Xt"))), ")")
  aDEYX = as.character(simplify(yac_symbol(paste(a, DEYX, sep = " * "))))
  bDEYX = as.character(simplify(yac_symbol(paste(b, DEYX, sep = " * "))))
  b2 = as.character(simplify(yac_symbol(paste0("(", b, ")^2"))))
  b2DEYX2 = as.character(simplify(yac_symbol(paste(1/2, b2, DEYX2, sep = " * "))))
  prima = as.character(simplify(yac_symbol(paste(DEYt, aDEYX, b2DEYX2, sep = " + "))))
  out = paste0("dY(t) = (", prima, ")*dt + ", bDEYX, "*dB(t)")
  return(out)
}
