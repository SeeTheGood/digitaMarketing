data__boran=read.csv(file='Boran.csv',header = TRUE,sep = ';')

#Interaction linear model 
reg_lin1 <- lm(sales_Boran ~ price_Boran+price_Zeyma+price_Liebo+price_Cranb+display_prom+feature_prom+
                 display_prom_Liebo+feature_prom_Liebo+display_prom*feature_prom + display_prom_Liebo*feature_prom_Liebo
               + price_Boran*feature_prom + price_Boran*display_prom+as.factor(store),data=data__boran) 
(cor(predict(reg_lin1),data__boran$sales_Boran))^2
summary(reg_lin1)$r.squared

#Interaction exponential model 
data__boran$log_sales_Boran=log(data__boran$sales_Boran)
data__boran$log_price_Boran=log(data__boran$price_Boran)
data__boran$log_price_Cranb=log(data__boran$price_Cranb)
data__boran$log_price_Zeyma=log(data__boran$price_Zeyma)
data__boran$log_price_Liebo=log(data__boran$price_Liebo)
reg_expon1 = lm(log_sales_Boran ~ price_Boran+price_Zeyma+price_Liebo+price_Cranb+display_prom+feature_prom+
                  display_prom_Liebo+feature_prom_Liebo+display_prom*feature_prom + display_prom_Liebo*feature_prom_Liebo
                + price_Boran*feature_prom + price_Boran*display_prom+as.factor(store),data=data__boran) 
(cor(exp(predict(reg_expon1)), data__boran$sales_Boran))^2
summary(reg_expon1)$r.squared


#Interaction multiplicative model 
reg_mult <- lm(log_sales_Boran ~log_price_Boran+log_price_Cranb+log_price_Zeyma+log_price_Liebo+display_prom+
                 feature_prom+display_prom_Liebo+feature_prom_Liebo+display_prom*feature_prom + 
                 display_prom_Liebo*feature_prom_Liebo + log_price_Boran*feature_prom + log_price_Boran*display_prom
               +as.factor(store),data=data__boran) 
(cor(exp(predict(reg_mult)), data__boran$sales_Boran))^2
summary(reg_mult)$r.squared

plot(data__boran$week, data__boran$sales_Boran, type="b", col="darkgreen",xlab='Week',ylab="Sales")

Predicted_sales_linear = predict(reg_lin1)
lines(x = data__boran$week, y = Predicted_sales_linear, col="red")

dev.off() 
Predicted_sales_exp = exp(predict(reg_expon1))
lines(x = data__boran$week, y = Predicted_sales_exp, col="grey") 
help(darkgreen)

dev.off() 
Predicted_sales_mult = exp(predict(reg_mult))
lines(x = data__boran$week, y = Predicted_sales_mult, col="blue")     
dev.off()      


# Multiplicative model 1
reg_mult1 <- lm(log_sales_Boran ~log_price_Boran+log_price_Cranb+log_price_Zeyma+log_price_Liebo+display_prom+
                  feature_prom+display_prom_Liebo+feature_prom_Liebo+display_prom*feature_prom + 
                  display_prom_Liebo*feature_prom_Liebo + log_price_Boran*feature_prom + log_price_Boran*display_prom
                +as.factor(store),data=data__boran) 
summary(reg_mult1)



#Revised multiplicative model 2
reg_mult2 <- lm(log_sales_Boran ~log_price_Boran+log_price_Cranb+log_price_Zeyma+log_price_Liebo+display_prom+
                  feature_prom+display_prom_Liebo+display_prom*feature_prom + 
                  display_prom_Liebo*feature_prom_Liebo + log_price_Boran*display_prom+as.factor(store),data=data__boran) 
summary(reg_mult2)


#Revised multiplicative model 3 
reg_mult3 <- lm(log_sales_Boran ~log_price_Boran+log_price_Zeyma+display_prom+
                  feature_prom+display_prom_Liebo+display_prom*feature_prom + 
                  display_prom_Liebo*feature_prom_Liebo+as.factor(store),data=data__boran) 
summary(reg_mult3)
<- 
  #Revised multiplicative model 4
  reg_mult4 <- lm(log_sales_Boran ~log_price_Boran+log_price_Zeyma+log_price_Liebo+display_prom+
                    feature_prom+display_prom_Liebo+display_prom*feature_prom + 
                    display_prom_Liebo*feature_prom_Liebo+log_price_Boran*display_prom+as.factor(store),data=data__boran) 
summary(reg_mult4)
