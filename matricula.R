split_unlist <- function(...) unlist(strsplit(...))

cart_prod <- function(x) levels(interaction(x, sep=""))

accepts <- function(...) length(do.call(intersect, ...))==0

append_comment <- function(df,comment) rbind(df,c("",comment))

horarios <- function(cod) {
  cod <- split_unlist(cod, " ", perl=T)
  cod <- strsplit(cod, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl=T)
  
  cod <- lapply(cod, function(x) lapply(x, split_unlist, ""))
  
  unlist(lapply(cod, cart_prod))
}

setwd("C:/R/cliquenroll/")

matricula <- read.table("matricula.tsv",stringsAsFactors = F,sep="\t",header=T)
recusados <- c("5T2","5T3")

matricula[["horarios"]] <- lapply(matricula[,"horario"],horarios)
matricula[,"carga"] <- sapply(matricula[["horarios"]],function(x) length(x)*15)

#recusando horarios
matricula <- matricula[!grepl(paste(recusados,collapse="|"),matricula[["horarios"]]),]

#formatando data.frame
matricula[,"id"] <- 1:nrow(matricula)
matricula <- matricula[,c("id","nome","carga","horario","horarios")]

#edgelist colisao de horarios
acceptance <- data.frame(
   n = t(combn(matricula[,"id"],2))
  ,v = combn(matricula[,"horarios"],2,accepts)
, stringsAsFactors = F)

#impedindo mesma disciplina em horarios diferentes
acceptance[with(acceptance, matricula[n.1,"nome"] == matricula[n.2,"nome"]),"v"] <- FALSE

library(igraph)

g <- graph_from_data_frame(subset(acceptance, v==T), directed = F, vertices = matricula)

V(g)$size <- V(g)$carga/5

plot(g)

g_cliques <- max_cliques(g)
carga_clique <- sapply(g_cliques, function(x) sum(x$carga))

g_cliques <- g_cliques[order(carga_clique, decreasing = T)]

sugestoes <- lapply(g_cliques, function(sugestao){
  df <- data.frame(nome = sugestao$nome, horario = sugestao$horario, stringsAsFactors = F)
  df <- append_comment(df,"----------")
  df <- append_comment(df,paste0(sum(sugestao$carga),"H"))
  df <- append_comment(df,"")
})

write.table(do.call(rbind,sugestoes),"sugestoes.tsv",row.names = F,col.names = T,quote=F,sep="\t")
