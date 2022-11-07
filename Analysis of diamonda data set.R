## Analysis of diamond data set in R

library(ggplot2)
View(diamonds)
attach(diamonds)


library(dplyr+)
## For data manipulation
diamonds%>%filter(cut=="Ideal" & x>4 & y>4 &z>4)->data_cut
View(data_cut)


dim(diamonds)
colnames(diamonds)

ggplot(data=diamonds,aes(x=carat,y=price,col=3))+geom_point()

table(cut)





library(ggplot2)

## For visualization of the variables
## for scatter plot
ggplot(data=diamonds,aes(x=carat,y=price,col=cut))+geom_point()

## For histogram, checking the distribution of the price
ggplot(data=diamonds,aes(x=x),fill=3)+geom_histogram(color = 'turquoise4')

## For Box plot
ggplot(data=diamonds,aes(x=x,fill=cut))+geom_boxplot()
       

## Bar plot of cut variable.
ggplot(data=diamonds,aes(x=cut,fill=cut))+geom_bar()


## From the bar plot it is shown that ideal cut of diamond has maximum frequency and fair cut of diamons has low frequency.










library(caTools)

## Fitting  the linear regression
sample.split(price,SplitRatio = 0.65)->split_1


as.data.frame(subset(diamonds,split_1==T))->train


as.data.frame(subset(diamonds,split_1==F))->test


## Fitting the model

lm(price~carat,data=train)->model_1
predict(model_1,data=test)->pred_price
cbind(Actual=test$price,pred_price)->final
as.data.frame(final)->final

final$Actual-final$pred_price->error
cbind(Actual=test$price,pred_price,error,square_residual=error^2)->final
as.data.frame(final)->final
View(final)

sqrt(mean(final$square_residual))->res_mode_1
res_mode_1




########## Multiple linear regression

## Fitting the model

lm(price~x+y+z,data=train)->model_2
model_2
predict(model_2,data=test)->pred_price_2
error=test$price-pred_price_2
cbind(Actual=test$price,pred_price_2,error=test$price-pred_price_2,Square_res=error^2)->final_2

as.data.frame(final_2)->final_2
View(final_2)


## RMSE for accuracy checking of the MLR model.

sqrt(mean(final_2$Square_res))->res_mod_2


res_mod_2
res_mode_1




## Here we can say that multiple regression model perform better than linear regression 
#model predict the price of the diamons correctly.

