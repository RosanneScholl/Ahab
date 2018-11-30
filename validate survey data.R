library (polycor)
library (psych)
library (ggplot2)
install.packages("irr")
library (irr)

#constructed variables imported from csv include: elab_reason, pos_affect, neg_Affect, tasks, sharedvalues, selfreporthours
#see https://docs.google.com/spreadsheets/d/1KnA6JafmMPMRHd8gxlw6mlPgNGBLWT0e2W_RsIk97Uw/edit#gid=1168277328 for construction 
AhabSGexportcalcs_1_ <- read_excel("~/Downloads/AhabSGexportcalcs(1).xlsx")
View(AhabSGexportcalcs_5_)



#exploring reasons
#poorly specified pca- generates 22 dimensions 
reasons.pca <- prcomp(na.omit(AhabSGexportcalcs_1_[,c(47:68)]), center = TRUE)
summary (reasons.pca)
biplot(reasons.pca)

reason <- na.omit(AhabSGexportcalcs_5_[,c(47:55,58:68)])
reasonelab <- na.omit(reason[,c(2,4,6,8,10,11,17,19,20)])
reasonheur <- na.omit(reason[,c(1,3,5,7,9,13,14,15,16)])
psych::alpha(reason)
psych::alpha(reasonelab)
psych::alpha(reasonheur)

reasonfit <- factanal(reason,2,rotation="varimax")
print(reasonfit, sort=TRUE)

elab_reason_percent <- elab_reason *100

qplot(elab_reason_percent, geom="histogram", binwidth=5,fill=I("#3366FF"))+
  labs( x ="% elaborated reasons",
                                    y = "Frequency")+
  theme(text = element_text(size=20))

summary (elab_reason_percent)
sd(elab_reason_percent, na.rm=TRUE)
table(elab_reason_percent)
length (elab_reason_percent)


#exploring affect
#affects loads to 2 factors but does not explain much variance
affectfit <- factanal(affect,5,rotation="varimax")
print(affectfit, sort=TRUE)

negaffectplot <- qplot(neg_affect, geom="histogram", binwidth=.2,
      fill=I("#3366FF"))

negaffectplot<- negaffectplot +labs( x ="Mean Level across 5 Negative Affects",
                                   y = "Frequency")+
                       theme(text = element_text(size=20)
                       )
print (negaffectplot)

pos_affect <- as.numeric(pos_affect)
posaffectplot <- qplot(pos_affect, geom="histogram", binwidth=.2,
                       fill=I("#3366FF"))

posaffectplot<- posaffectplot +labs(  x ="Mean Level across 5 Positive Affects",
                                    y = "Frequency")+
  theme(text = element_text(size=20))
print(posaffectplot)
summary (neg_affect)
sd  (neg_affect, na.rm=TRUE)
summary (pos_affect)
sd  (pos_affect, na.rm=TRUE)




#exploring tasks
qplot(tasks, geom="histogram", binwidth=1,
      fill=I("#3366FF"),
      ylab="Frequency", xlab= "Number of tasks" )+
  theme(text = element_text(size=20))
summary (tasks)
sd (tasks, na.rm=TRUE)




#explorig shared values
qplot(sharedvalues, geom="histogram", binwidth=.25,
fill=I("#3366FF"),
ylab="Frequency", xlab= "Shared values with Mozilla" )+
  theme(text = element_text(size=20))

shares <-na.omit(AhabSGexportcalcs_5_[,c(123:127)])
alpha (shares, title="inter-item reliability for shared values items")
#items in shared values scale are all correlated at least weakly 
cor(shares)
#chronbachs alpha for interitem reliability is .62 (95% conf between .6 and .65)
psychpsych::alpha(shares)

