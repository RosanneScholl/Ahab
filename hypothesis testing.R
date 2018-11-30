library(readr)
wideMerged1_wideMerged <- read_csv("~/Downloads/wideMerged1 - wideMerged.csv")
View(wideMerged1_wideMerged)

library (ggplot2)
library (Hmisc)



################################################################
################reason elaboration##############################
##################################

#reason elab ~ duration
filterhours1 <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$hours<=30),]
pelab_reason<-filterhours1$elab_reason*100

ggplot(data=filterhours1, aes(y=pelab_reason, x=filterhours1$lnhours))+
  geom_point(data=filterhours1, aes(y=pelab_reason, x=filterhours1$lnhours),shape=21, colour="#3366FF", position=position_jitter(height =1.5)) +
  geom_smooth (method=lm)+
  xlim(-2.75,3.75)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
    label = c(0.1,1,  10,24,30))+
  labs(x ="Subsession hours",
       y = "% elaborated reasons")+
  theme(text = element_text(size=20)
  )

fit31 <- lm(pelab_reason ~ filterhours1$hours, data = filterhours1)
summary(fit31)


##################################
#reason elab ~ uri
qplot(wideMerged1_wideMerged$uri, geom="histogram")+xlim(-1,3000)


filteruri <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$uri<=3000),]
pelab_reason<-filteruri$elab_reason*100

ggplot(data=filteruri, aes(y=pelab_reason, x=filteruri$lnuri))+
  geom_point(data=filteruri, aes(y=pelab_reason, x=filteruri$lnuri),shape=21, colour="#3366FF", position=position_jitter(height =1.5)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(5),log(10),log(100),log(3000)),
                     label = c(0,5,10,100,3000))+
  labs(x ="URI count",
       y = "% elaborated reasons")+
  theme(text = element_text(size=20)
  )

fit32 <- lm(pelab_reason ~ filteruri$uri, data = filteruri)
summary(fit32)


##################################
#reason elab ~ search

qplot(wideMerged1_wideMerged$search, geom="histogram")+xlim(-1,100)


filtersearch <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$search<=100),]
pelab_reason<-filtersearch$elab_reason*100

ggplot(data=filtersearch, aes(y=pelab_reason, x=filtersearch$lnsearch))+
  geom_point(data=filtersearch, aes(y=pelab_reason, x=filtersearch$lnsearch),shape=21, colour="#3366FF", position=position_jitter(height =1.5, width=.1)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(2),log(3),log(4),log(5),log(10),log(100)),
                     label = c(0,2,3,4,5,10,100))+
  labs(x ="Search count",
       y = "% elaborated reasons")+
  theme(text = element_text(size=20)
  )

fit33 <- lm(pelab_reason ~ filtersearch$search, data = filtersearch)
summary(fit33)


################################################################
################ task variety ##################################
##################################


qplot(tasks/hours, geom="histogram", binwidth=.05,
      fill=I("#3366FF"),
      ylab="Frequency", xlab= "Task variety: number of tasks per hour" )+
  xlim(0,4)+
  theme(text = element_text(size=20))
summary (tasks/hours)
sd (tasks/hours, na.rm=TRUE)

#******************************************************
##################################
#tasks ~ hours
ggplot(data=filterhours1, aes(y=tasks, x=filterhours1$lnhours))+
  geom_point(data=filterhours1, aes(y=tasks, x=filterhours1$lnhours),shape=21, colour="#3366FF", position=position_jitter(height =.2)) +
  geom_smooth (method=lm)+
  xlim(-2.75,3.75)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
                     label = c(0.1,1,  10,24,30))+
  labs(x ="Subsession hours",
       y = "Task count")+
  theme(text = element_text(size=20)
  )
fit41t <- lm(tasks ~ filterhours1$hours, data = filterhours)
summary(fit41t)
##################################
#tasks ~ uri
ggplot(data=filteruri, aes(y=tasks, x=filteruri$lnuri))+
  geom_point(data=filteruri, aes(y=tasks, x=filteruri$lnuri),shape=21, colour="#3366FF", position=position_jitter(height =.2, width=0)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(5),log(10),log(100),log(3000)),
                     label = c(0,5,10,100,3000))+
  labs(x ="URI count",
       y = "Task count")+
  theme(text = element_text(size=20)
  )
fit42t <- lm(tasks ~ filteruri$uri, data = filteruri)
summary(fit42t)


##################################
#tasks ~ search

ggplot(data=filtersearch, aes(y=tasks, x=filtersearch$lnsearch))+
  geom_point(data=filtersearch, aes(y=tasks, x=filtersearch$lnsearch),shape=21, colour="#3366FF", position=position_jitter(height =0.2, width=.1)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(2),log(3),log(4),log(5),log(10),log(100)),
                     label = c(0,2,3,4,5,10,100))+
  labs(x ="Search count",
       y = "Task count")+
  theme(text = element_text(size=20)
  )

fit43t <- lm(tasks ~ filtersearch$search, data = filtersearch)
summary(fit43t)



##################################
#task variety ~ uri

qplot(wideMerged1_wideMerged$tasks, geom="histogram")+xlim(-1,50)
qplot(filteruri$tasks/hours, geom="histogram")
summary (filteruri$tasks/hours)

ggplot(data=filteruri, aes(y=tasks/hours, x=filteruri$lnuri))+
  geom_point(data=filteruri, aes(y=tasks/hours, x=filteruri$lnuri),shape=21, colour="#3366FF", position=position_jitter(height =0, width=0)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(5),log(10),log(100),log(3000)),
                     label = c(0,5,10,100,3000))+
  labs(x ="URI count",
       y = "Tasks per hour")+
  ylim(0,7)+
  theme(text = element_text(size=20)
  )
  fit42 <- lm(tasks/hours ~ filteruri$uri, data = filteruri)
  summary(fit42)

##################################
#task variety ~ search
qplot(wideMerged1_wideMerged$tasks, geom="histogram")+xlim(-1,50)
qplot(filtersearch$tasks/hours, geom="histogram")
summary (filtersearch$tasks/hours)

ggplot(data=filtersearch, aes(y=tasks/hours, x=filtersearch$lnsearch))+
  geom_point(data=filtersearch, aes(y=tasks/hours, x=filtersearch$lnsearch),shape=21, colour="#3366FF", position=position_jitter(height =0, width=.1)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(2),log(3),log(4),log(5),log(10),log(100)),
                     label = c(0,2,3,4,5,10,100))+
  labs(x ="Search count",
       y = "Tasks per hour")+
  ylim(0,8.25)+
  theme(text = element_text(size=20)
  )

fit43 <- lm(tasks ~ filtersearch$search, data = filtersearch)
summary(fit43)



################################################################
################ affect ########################################
##################################
qplot(pos_affect, geom="histogram")
qplot(neg_affect, geom="histogram")


##################################
#affect ~ duration

posaffect <-as.numeric(filterhours1$pos_affect)
negaffect <-as.numeric(filterhours1$neg_affect)

ggplot(data=filterhours1, aes(y=posaffect, x=filterhours1$lnhours))+
  geom_point(data=filterhours1, aes(y=posaffect, x=filterhours1$lnhours),shape=21, colour="#3366FF", position=position_jitter(height =.02)) +
  geom_smooth (method=lm)+
  xlim(-2.75,3.75)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
                     label = c(0.1,1,  10,24,30))+
  labs(x ="Subsession hours",
       y = "Positive Affect about Firefox")+
  theme(text = element_text(size=20)
  )

fit51p <- lm(posaffect ~ filterhours1$hours, data = filterhours1)
summary(fit51p)

ggplot(data=filterhours1, aes(y=negaffect, x=filterhours1$lnhours))+
  geom_point(data=filterhours1, aes(y=negaffect, x=filterhours1$lnhours),shape=21, colour="#3366FF", position=position_jitter(height =.02)) +
  geom_smooth (method=lm)+
  xlim(-2.75,3.75)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
                     label = c(0.1,1,  10,24,30))+
  labs(x ="Subsession hours",
       y = "Negative Affect about Firefox")+
  theme(text = element_text(size=20)
  )

fit51n <- lm(negaffect ~ filterhours1$hours, data = filterhours1)
summary(fit51n)

##################################
#affect ~ uri
posaffect <-as.numeric(filteruri$pos_affect)
negaffect <-as.numeric(filteruri$neg_affect)

ggplot(data=filteruri, aes(y=posaffect, x=filteruri$lnuri))+
  geom_point(data=filteruri, aes(y=posaffect, x=filteruri$lnuri),shape=21, colour="#3366FF", position=position_jitter(height =.02, width=0)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(5),log(10),log(100),log(3000)),
                     label = c(0,5,10,100,3000))+
  labs(x ="URI count",
       y = "Positive Affect about Firefox")+
  theme(text = element_text(size=20)
  )
fit52 <- lm(posaffect ~ filteruri$uri, data = filteruri)
summary(fit52)

ggplot(data=filteruri, aes(y=negaffect, x=filteruri$lnuri))+
  geom_point(data=filteruri, aes(y=negaffect, x=filteruri$lnuri),shape=21, colour="#3366FF", position=position_jitter(height =.02, width=0)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(5),log(10),log(100),log(3000)),
                     label = c(0,5,10,100,3000))+
  labs(x ="URI count",
       y = "Negative Affect about Firefox")+
  theme(text = element_text(size=20)
  )
fit52n <- lm(negaffect ~ filteruri$uri, data = filteruri)
summary(fit52n)
##################################
#affect ~ search
posaffect <-as.numeric(filtersearch$pos_affect)
negaffect <-as.numeric(filtersearch$neg_affect)

ggplot(data=filtersearch, aes(y=posaffect, x=filtersearch$lnsearch))+
  geom_point(data=filtersearch, aes(y=posaffect, x=filtersearch$lnsearch),shape=21, colour="#3366FF", position=position_jitter(height =.02, width=0.1)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(2),log(3),log(4),log(5),log(10),log(100)),
                     label = c(0,2,3,4,5,10,100))+
  labs(x ="Search count",
       y = "Positive Affect about Firefox")+
  theme(text = element_text(size=20)
  )
fit53p <- lm(posaffect ~ filtersearch$search, data = filtersearch)
summary(fit53p)

ggplot(data=filtersearch, aes(y=negaffect, x=filtersearch$lnsearch))+
  geom_point(data=filtersearch, aes(y=negaffect, x=filtersearch$lnsearch),shape=21, colour="#3366FF", position=position_jitter(height =.02, width=0.1)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(2),log(3),log(4),log(5),log(10),log(100)),
                     label = c(0,2,3,4,5,10,100))+
  labs(x ="Search count",
       y = "Negative Affect about Firefox")+
  theme(text = element_text(size=20)
  )
fit53n <- lm(negaffect ~ filtersearch$search, data = filtersearch)
summary(fit53n)


################################################################
################ shared values ########################################
##################################
qplot(sharedvalues, geom="histogram")

##################################
#shared ~hours
filterhours1 <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$hours<=30),]

ggplot(data=filterhours1, aes(y=sharedvalues, x=filterhours1$lnhours))+
  geom_point(data=filterhours1, aes(y=sharedvalues, x=filterhours1$lnhours),shape=21, colour="#3366FF", position=position_jitter(height =0.05)) +
  geom_smooth (method=lm)+
  xlim(-2.75,3.75)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
                     label = c(0.1,1,  10,24,30))+
  labs(x ="Subession hours",
       y = "Shared values with Mozilla")+
  theme(text = element_text(size=20)
  )

fit61 <- lm(sharedvalues ~ filterhours1$hours, data = filterhours1)
summary(fit61)

##################################
#shared ~uri

ggplot(data=filteruri, aes(y=sharedvalues, x=filteruri$lnuri))+
  geom_point(data=filteruri, aes(y=sharedvalues, x=filteruri$lnuri),shape=21, colour="#3366FF", position=position_jitter(height =.05, width=0)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(5),log(10),log(100),log(3000)),
                     label = c(0,5,10,100,3000))+
  labs(x ="URI count",
       y = "Shared values with Mozilla")+
  theme(text = element_text(size=20)
  )
fit62 <- lm(sharedvalues ~ filteruri$uri, data = filteruri)
summary(fit62)

##################################
#shared ~ search

ggplot(data=filtersearch, aes(y=sharedvalues, x=filtersearch$lnsearch))+
  geom_point(data=filtersearch, aes(y=sharedvalues, x=filtersearch$lnsearch),shape=21, colour="#3366FF", position=position_jitter(height =.05, width=0.1)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(2),log(3),log(4),log(5),log(10),log(100)),
                     label = c(0,2,3,4,5,10,100))+
  labs(x ="Search count",
       y = "Shared values with Mozilla")+
  theme(text = element_text(size=20)
  )
fit63 <- lm(sharedvalues ~ filtersearch$search, data = filtersearch)
summary(fit63)