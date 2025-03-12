#' Rappresentazione grafica della filtrazione
#'
#' Funzione che disegna il grafico della filtrazione inserita con i relativi valori dei processi stocastici.
#'
#' @param assets Lista contenente i processi stocastici considerati.
#' @param col1 Colore assegnato ai rettangoli contenenti i blocchi della partizione.
#' @param col2 Colore assegnato al testo dei valori dei processi stocastici.
#' @return Grafico della filtrazione.
#' @examples
#' S0 = list(1, c(1,2), c(1,2,3,4,5))
#' S1 = list(2, c(3,4), c(5,6,7,8,9))
#' assets = list("S0" = S0, "S1" = S1)
#' blocchi = list(c(3, 2), c(2, 3, 2, 3, 2))
#' assets = filtrazione(assets = assets, blocchi = blocchi)
#' plot_filtrazione(assets = assets, col1 = "lightblue", col2 = "red")
#' @export
plot_filtrazione = function(assets, col1 = "lightblue", col2 = "red") {
  k = list()
  for (i in 1:length(assets[[1]])) {
    a = lapply(assets, FUN = function(x) {x[[i]]})
    b = a[[1]]
    for (j in 2:length(assets)) {b = paste(b, a[[j]], sep = ", ")}
    k[[i]] = b
  }
  y = c()
  for (i in 1:(length(assets[[1]]) - 1)) {
    for (j in 1:length(assets[[1]][[i]])) {
      nomes = unlist(strsplit(names(assets[[1]][[i + 1]]), split = "B"))
      nomes = as.numeric(nomes[seq(from = 1, by = 2, to = length(nomes))])
      x = paste0("P", i, "_", names(assets[[1]][[i + 1]])[nomes == j])
      nom = paste0("P", i - 1, "_", names(assets[[1]][[i]][j]))
      l = 1:(2 * length(assets[[1]][[i + 1]][nomes == j]))
      p = l
      l = l %% 2
      p[l != 0] = nom
      p[l == 0] = x
      y = append(y, p)
    }
  }
  x = y
  lun = 1:length(y)
  divi = 1:length(y) %% 2
  for (i in lun) {
    n = unlist(strsplit(y[i], "_"))
    m = unlist(strsplit(n[1], "P"))[2]
    o = unlist(strsplit(n[2], "B"))[2]
    if (as.numeric(m) == length(assets[[1]]) - 1) {x[i] = paste0("ω", o)} else {x[i] = paste(m, o, sep = "B")}
  }
  x[x == "0B1"] = "Ω"
  g = make_graph(x, directed = TRUE)
  custom_layout = layout.reingold.tilford(g)
  custom_layout = -custom_layout[, 2:1]
  add_text = data.frame(x = custom_layout[, 1], y = custom_layout[, 2], testo = unlist(k))
  return(ggraph(g, layout = custom_layout) +
    geom_edge_link() +
    geom_node_point(color = col1, size = 12, shape = "square") +
    geom_node_text(aes(label = name), size = 4) +
    theme_void() +
    geom_text(data = add_text, mapping = aes(label = testo, x = x, y = y), vjust = -2, size = 4, color = col2))
}
