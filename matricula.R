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

horarios_ref <- setNames(1:18, as.vector(sapply(c("M","T","N"), paste0, 1:6)))

setwd("C:/R/cliquenroll/")

matricula <- read.table("matricula2.tsv",stringsAsFactors = F,sep="\t",header=T, encoding = "UTF-8")
recusados <- c("M")

matricula[["horarios"]] <- lapply(matricula[["horario"]],horarios)
matricula[["carga"]] <- sapply(matricula[["horarios"]],length) * 15

matricula[["teste"]] <- lapply(matricula[["horarios"]], function(x){
  tapply(x,substring(x,1,1), function(x) horarios_ref[substring(x,2)])
})

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
library(ggraph)

g <- graph_from_data_frame(subset(acceptance, v==T), directed = F, vertices = matricula)

#V(g)$size <- V(g)$carga/5

#plot(g)

ggraph(g) +
  geom_edge_link(colour="grey") +
  geom_node_point(aes(size=carga, color=nome)) +
  geom_node_text(aes(label=nome), size=V(g)$carga/20) +
  scale_size_continuous(range = c(4,8), guide = "none") +
  theme_void() + 
  theme(legend.position = 'none')

g_cliques <- cliques(g, min = 2, max = 7)
carga_clique <- sapply(g_cliques, function(x) sum(x$carga))

g_cliques <- g_cliques[order(carga_clique, decreasing = T)]

sugestoes <- lapply(g_cliques, function(sugestao){
  df <- data.frame(nome = sugestao$nome, horario = sugestao$horario, stringsAsFactors = F)
  df <- append_comment(df,"----------")
  df <- append_comment(df,paste0(sum(sugestao$carga),"H"))
  df <- append_comment(df,"")
})

write.table(do.call(rbind,sugestoes),"sugestoes.tsv",row.names = F,col.names = T,quote=F,sep="\t")
