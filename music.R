library(foreign)
library(dplyr)
library(reshape2)
library(igraph)
library(stringr)

meta1 <- read.spss('wave1.sav', to.data.frame=T)
meta2 <- read.spss('wave2.sav', to.data.frame=T)

meta_control = read.spss('wavecontrol.sav', to.data.frame=T)

music1 <- select(meta1, ID_student, v80.1, v80.2, v80.3, v81.1, v81.2, v81.3, v81.4, v81.5)
music2 <- select(meta2, ID_student, v80.1, v80.2, v80.3, v81.1, v81.2, v81.3, v81.4, v81.5)

for (i in 2:dim(music1)[2]) {
  music1[,i] = tolower(music1[,i])
}
for (i in 2:dim(music2)[2]) {
  music2[,i] = tolower(music2[,i])
}

write.csv(music1, file="music1.csv", row.names = F)
write.csv(music2, file='music2.csv', row.names = F)


music1_clean <- read_csv("~/ptu/data/music1-clean.csv")
music2_clean <- read_csv("~/ptu/data/music2-clean.csv")

music11_clean <- filter(music1_clean, music2_clean$ID_student %in% music1_clean$ID_student)

library(reshape2)
geners1 = dcast(melt(music1_clean,id.vars = "ID_student"),ID_student ~value,fun.aggregate = length)
geners2 = dcast(melt(music2_clean,id.vars = "ID_student"),ID_student ~value,fun.aggregate = length)
geners1 = geners1[,-26]
geners2 = geners2[,-32]
library(polycor)

corrr<- hetcor(t(geners2[,-1]))
corrr
corrr <- corrr$correlations


library(corrgram)
corrgram(corrr)


library(psych)
# plotting
fa1<-fa(corrr, nfactors=5, rotate="varimax", fm="ml", scores="regression")

#factor space
factor.plot(fa1)
#factor structure
fa.diagram(fa1)

fa1$weights

geners2 = cbind(geners2, fa1$weights)

library(dplyr)
geners22 = dplyr::select(geners2, ID_student, ML1, ML2, ML3, ML4, ML5)

geners22$max <- apply(df, 1, function(x) max(x[x != 9]))


try = geners22 %>% group_by(ID_student) 


geners22$most_probable <- as.factor(unlist(
  lapply(
    1:nrow(geners22),
    function(x){
      names(which.max(fa1$weights[x,]))
    }
  )
)
)


