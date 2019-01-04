


filterhours2 <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$hours<=30 & wideMerged1_wideMerged$selfreporthours<=24),]

#one additional average susbession hour over prior week is associated with 7 additional self-reported minutes yesterday
ggplot(data=filterhours2, aes(y=selfreporthours, x=filterhours2$lnhours))+
  geom_point(data=filterhours2, aes(y=selfreporthours, x=filterhours2$lnhours),shape=21, colour="#3366FF", position=position_jitter(height =0.3)) +
  geom_smooth (method=lm)+
  xlim(-2.75,3.75)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
                     label = c(0.1,1,  10,24,30))+
  labs(x ="Subsession hours",
       y = "Self-reported hours on the Internet yesterday")+
  theme(text = element_text(size=20)
  )

fit91 <- lm(selfreporthours ~ filterhours2$hours, data = filterhours2)
summary(fit91)

#same as above but with untransformed behavional hours
ggplot(data=filterhours2, aes(y=selfreporthours, x=filterhours2$hours))+
  geom_point(data=filterhours2, aes(y=selfreporthours, x=filterhours2$hours),shape=21, colour="#3366FF", position=position_jitter(height =0.3)) +
  geom_smooth (method=lm)+
  labs(x ="Subsession hours",
       y = "Self-reported hours on the Internet yesterday")+
  theme(text = element_text(size=20)
  )

#similar plot as above for telemetry hours yesterday (instead of hours averaged over last week)
#as with hours averaged over last week, hours yesterday is also associated with self report hours, but the association while equally significant is a little weaker
#1 additinla telemtry hour yesterday is associated with 5 additional self reported minutes yesterday
ggplot(data=filterhours2, aes(y=selfreporthours, x=log(filterhours2$hours1)))+
  geom_point(data=filterhours2, aes(y=selfreporthours, x=log(filterhours2$hours1)),shape=21, colour="#3366FF", position=position_jitter(height =0.3)) +
  geom_smooth (method=lm)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
                     label = c(0.1,1,  10,24,30))+
  labs(x ="Subsession hours yesterday",
       y = "Self-reported hours on the Internet yesterday")+
  theme(text = element_text(size=20)
  )
fit93 <- lm(selfreporthours ~ filterhours2$hours1, data = filterhours2)
summary(fit93)
