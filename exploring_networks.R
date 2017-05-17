library(foreign)
library(dplyr)
library(reshape2)
library(igraph)
library(stringr)


friends = left_join(friends1, friends2, by = "ID_student")


setwd("~/ptu/data")
friends1 = read.table("friends1.txt",
                      header = TRUE,
                      sep="\t",
                      fill=FALSE, 
                      strip.white=TRUE)
friends1 = friends1[1:166,]
friends2 = read.table("friends2.txt",
                      header = TRUE,
                      sep="\t",
                      fill=FALSE,
                      strip.white=TRUE)
friends2 = friends2[1:166,]


f1 = na.omit(select(friends1, ID_student, f1))
colnames(f1)[2] = 'friend'
f2 = na.omit(select(friends1, ID_student, f2))
colnames(f2)[2] = 'friend'
f3 = na.omit(select(friends1, ID_student, f3))
colnames(f3)[2] = 'friend'
#f4 = na.omit(select(friends1, ID_student, f4))
#colnames(f4)[2] = 'friend'
#f5 = na.omit(select(friends1, ID_student, f5))
#colnames(f5)[2] = 'friend'

friends1 = rbind(f1,f2,f3)

#write.csv(friends1, file = 'edgelist1.csv', row.names = F)

f21 = na.omit(select(friends2, ID_student, f1))
colnames(f21)[2] = 'friend'
f22 = na.omit(select(friends2, ID_student, f2))
colnames(f22)[2] = 'friend'
f23 = na.omit(select(friends2, ID_student, f3))
colnames(f23)[2] = 'friend'
#f24 = na.omit(select(friends2, ID_student, f4))
#colnames(f24)[2] = 'friend'
#f25 = na.omit(select(friends2, ID_student, f5))
#colnames(f25)[2] = 'friend'

friends2 = rbind(f21,f22,f23)

#write.csv(friends2, file = 'edgelist2.csv', row.names = F)


g1 = simplify(graph.data.frame(friends1, directed = F))
g2 = simplify(graph.data.frame(friends2, directed = F))

g12 <- g1 + g2
g2 = g12 - g1

set.seed(1235)
myLayout <- layout.fruchterman.reingold(g12)

meta = read.csv('gender.csv', header=T)

par(mfrow = c(1, 2))
plot(g1,
     vertex.color = "yellow",
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.02,
     vertex.size = 3,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1",
     edge.curved = 0.2)
plot(g2,
     vertex.color = "yellow",
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.02,
     vertex.size = 3,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2",
     edge.curved = 0.2)

par(mfrow = c(1, 2))
plot(g1,
     vertex.color = ifelse(meta$sex == 1, "pink", "darkblue"),
     vertex.shape = ifelse(meta$sex == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.02,
     vertex.size = 3,
     vertex.label = NA,
     layout = myLayout,
     main = "Network wave 1")
plot(g2,
     vertex.color = ifelse(meta$sex == 1, "pink", "darkblue"),
     vertex.shape = ifelse(meta$sex == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.02,
     vertex.size = 3,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
