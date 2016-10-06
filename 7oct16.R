
# epidemiology of a shipwreck ----

View(titanic)
summary(titanic) 
summary(as.factor(titanic$Survived))
str(titanic) 
attributes(titanic) 
names(titanic)
head(titanic)

plot(titanic)

colSums(is.na(titanic))
colSums(!is.na(titanic))


#survival rates:
#  in general (survived/all)
#  among women (survived|women)
#  among menu  (survived|men)
#  among crew  (survived|crew)
#  among classes 1,2,3 (survived|class x)
#  among adults (survived|adults)
#  among children (survived|kids)

#2201 individuals total

#711 survived (.323)
#1490 died    (.677)

#325 1st class
#285 2nd class
#706 3rd class
#885 crew

#470 women
#1731 men

#2092 adults
#109 children

ti.fem  <- titanic[which(titanic$Sex=='Female'),]
View(ti.fem)
summary(ti.fem)
summary(as.factor(ti.fem$Survived))

#344 survived/470 (.732)
#126 died

#145 1st class
#106 2nd class
#196 3rd class
#23 crew

#425 women
#45 girls

ti.mal  <- titanic[which(titanic$Sex=='Male'),] 
summary(ti.mal)
summary(as.factor(ti.mal$Survived))

#367 survived/1731  (.212)
#1364 died

ti.kid  <- titanic[which(titanic$Age=='Child'),] 
summary(ti.kid)
summary(as.factor(ti.kid$Survived))

#57 survived/109 (.523)
#52 died

ti.adu  <- titanic[which(titanic$Age=='Adult'),] 
summary(as.factor(ti.adu$Survived))

#654 survived/2092 (.313)
#1438 died

w2.s <- w2.r[ which(!is.na(w2.r$JCog.z)), ]


oops <- lm(titanic$Survived ~ titanic$Class + titanic$Sex + titanic$Age)
summary(oops)