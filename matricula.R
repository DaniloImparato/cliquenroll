split_unlist <- function(...) unlist(strsplit(...))

cart_prod <- function(x) levels(interaction(x, sep=""))

accepts <- function(...) length(do.call(intersect, ...))==0

horarios <- function(cod) {
  cod <- split_unlist(cod, " ", perl=T)
  cod <- strsplit(cod, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl=T)
  
  cod <- lapply(cod, function(x) lapply(x, split_unlist, ""))
  
  unlist(lapply(cod, cart_prod))
}


matricula <- read.table("C:/Users/xdanilo/Downloads/_UF/matricula.txt",stringsAsFactors = F,sep="\t",header=T)
matricula[["horarios"]] <- lapply(matricula[,"horario"],horarios)
matricula[,"carga"] <- sapply(matricula[["horarios"]],function(x) length(x)*15)


acceptance <- data.frame(n = t(combn(matricula[,"nome"],2)), v = combn(matricula[,"horarios"],2,accepts))


library(igraph)

g <- graph_from_data_frame(subset(acceptance, v==T), directed = F, vertices = matricula)

V(g)$size <- V(g)$carga/5

plot(g)

