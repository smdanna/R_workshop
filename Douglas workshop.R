#### R workshop for Douglas lab ----
# sofia m. danna
# 5 oct 2016


# basic commands ----

names  <- c("merkel","the pope","kardashian","norbert","sofia")
cars <- c(3,1,91,1,0)
spicy  <- c(.1, .01, .5, .3, .85) 
dog  <- c(NA, 0, NA, NA, 0)
ppl <- data.frame(names, cars, spicy, dog)
ppl

ppl$cars
ppl[,2]

#kardashian loses a car
ppl[3,2] <- 90
ppl









# scoping out titanic data ----

View(titanic)
summary(titanic) 

titanic$Survived <- factor(titanic$Survived)
titanic$Survived.f <- factor(titanic$Survived, labels = c("died", "survived"))
titanic$Survived.n <- as.numeric(titanic$Survived)


titanic$Survived.f <- NULL
titanic$Survived.n <- NULL

str(titanic) 
attributes(titanic) 
names(titanic)
head(titanic)

plot(titanic)

# Look out for NAs, might have to create dataframe without them for some analyses
colSums(is.na(titanic))
colSums(!is.na(titanic))



ti.fem  <- titanic[which(titanic$Sex=='Female'),]
View(ti.fem)
summary(ti.fem)

ti.mal  <- titanic[which(titanic$Sex=='Male'),] 
summary(ti.mal)



ti.kid  <- titanic[which(titanic$Age=='Child'),] 
summary(ti.kid)

ti.adu  <- titanic[which(titanic$Age=='Adult'),] 
summary(as.factor(ti.adu$Survived))



ti.c1  <- titanic[which(titanic$Class=='1'),]
summary(ti.c1)

ti.c2  <- titanic[which(titanic$Class=='2'),] 
summary(ti.c2)

ti.c3  <- titanic[which(titanic$Class=='3'),] 
summary(ti.c3)

ti.cr  <- titanic[which(titanic$Class=='Crew'),] 
summary(ti.cr)


Overall .323   

group <- c('Female','Male','Adult','Child', '1st','2nd','3rd','Crew', 'Overall')
prop <- c(.732,.212,.313,.523,.625,.414,.252,.240,.323)
unadj <- data.frame(group, prop)

qplot(unadj$group, unadj$prop, geom="bar", stat="identity", fill = I("grey70")) + ggtitle("Proportion of survivors per group")


w2.s <- w2.r[ which(!is.na(w2.r$JCog.z)), ]









# multiple logistic regression ----

oops <- glm(titanic$Survived ~ titanic$Class + titanic$Sex + titanic$Age, family=binomial)

summary(oops)

logistic.regression.or.ci(oops)

  
  
  





# plotting ORs ----



grp <- c('Class2',  'Class3',  'Crew',    'Male',    'Child')
or <-  c(0.36128255,0.16901595,0.42414659,0.08891625,2.89082629)
l.b <- c(0.24604475,0.12075099,0.31159399,0.06752499,1.79187527)
u.b <- c(0.5304933, 0.2365727, 0.5773549, 0.1170841, 4.6637603)
ors <- data.frame(grp, or, l.b, u.b)

# define errorbar bounds
limits <- aes(ymax = u.b, ymin=l.b)

# plot to develop
titanic.plot <- ggplot(ors, aes(grp, or)) +
  geom_point() 
#  coord_trans(y = "log10") +
 # geom_errorbar(limits,width=0.1) +
#  theme_classic() +
#  geom_hline(yintercept=1) +
#  ggtitle("Odds ratios of survival on Titanic by demographic category") +
#  xlab("Group") +
#  ylab("Odds ratio")
titanic.plot
  



  
# final plot

# plot
titanic.plot <- ggplot(ors, aes(grp, or)) +
  geom_point() +
  coord_trans(y = "log10") +
 geom_errorbar(limits,width=0.1) +
#theme_classic() +
  geom_hline(yintercept=1) +
  ggtitle("Odds ratios of survival on Titanic by demographic category") +
  xlab("Group") +
  ylab("Odds ratio")
titanic.plot









# regression functions ----

#http://www.medicine.mcgill.ca/epidemiology/Joseph/courses/EPIB-621/logistic.regression.or.ci.txt

logistic.regression.or.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from a glm                   #
  #  (logistic model) command in R and provides not              #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for all coefficients and OR's.    #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  z.quantile <- qnorm(1-(1-level)/2)
  number.vars <- length(regress.out$coefficients)
  OR <- exp(regress.out$coefficients[-1])
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * z.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  OR.ci <- exp(slopes.ci)
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci, OR=OR, OR.ci = OR.ci)
  return(output)
}
