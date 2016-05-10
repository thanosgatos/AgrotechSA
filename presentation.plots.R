dfcast <- funggcast(ts.demand.AR50041, fcastcomp.AR50041)

arpres <- ggplot(dfcast, aes(x = date, y = observed))
arpres + geom_line() + 
  geom_line(aes(y = fitted), col = "blue") + 
  geom_line(aes(y = forecast), col = "red") + 
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.25) + 
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = 0.25) +
  xlab("Χρόνος") + ylab("Ζήτηση") + ggtitle("Πρόβλεψη Ζήτησης AR50041")



policyAR50041 <- as.data.frame(cbind(s2, ip2, order2, sales2))
date2 <- ymd(c("2014-4-1", "2014-5-1", "2014-6-1", "2014-7-1", "2014-8-1", 
               "2014-9-1", "2014-10-1", "2014-11-1", "2014-12-1", "2015-1-1", 
               "2015-2-1", "2015-3-1", "2015-4-1", "2015-5-1", "2015-6-1", 
               "2015-7-1"))

policyAR50041 <- cbind(date2, policyAR50041)
  
pplotAR50041 <- ggplot(policyAR50041, aes(x = date2, color))
pplotAR50041 + geom_line(aes(y = s2, color = "S")) + 
  geom_line(aes(y = ip2, color = "IP")) + 
  geom_line(aes(y = order2, color = "Order")) + 
  geom_line(aes(y = sales2, color = "Sales")) +
  labs(color = "Μεταβλητή") + ggtitle("Προτεινόμενη Πολιτική AR50041") + 
  xlab("Χρόνος") + ylab("Τεμάχια")


dfcast.L171436 <- funggcast(ts.demand.L171436, fcastcomp.L171436)

pres.L171436 <- ggplot(dfcast.L171436, aes(x = date, y = observed))
pres.L171436 + geom_line() + 
  geom_line(aes(y = fitted), col = "blue") + 
  geom_line(aes(y = forecast), col = "red") + 
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.25) + 
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = 0.25) +
  xlab("Χρόνος") + ylab("Ζήτηση") + ggtitle("Πρόβλεψη Ζήτησης L171436")





# p1a<-ggplot(data=pd,aes(x=date,y=observed)) 
# p1a<-p1a+geom_line(col='red')
# p1a<-p1a+geom_line(aes(y=fitted),col='blue')
# p1a<-p1a+geom_line(aes(y=forecast))+geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25)
# p1a<-p1a+scale_x_date(name='',breaks='1 year',minor_breaks='1 month',labels=date_format("%b-%y"),expand=c(0,0))
# p1a<-p1a+scale_y_continuous(name='Units of Y')
# p1a<-p1a+opts(axis.text.x=theme_text(size=10),title='Arima Fit to Simulated Data\n (black=forecast, blue=fitted, red=data, shadow=95% conf. interval)')