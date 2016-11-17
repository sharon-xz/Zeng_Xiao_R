##Prof G - Nice work.

library("ggplot2")
library("grid") #Loading packages

# Bring Diamonds data into memory
data(diamonds)


#Question 2
ggplot(diamonds,aes(carat,price)) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price') +labs(title='Diamonds-Weight to Price by Color') 
#geom_points() is to modify the display of points
#xlab(),ylab() and labs() is to change the names of axis and title.

#Question 3
ggplot(diamonds,aes(log(carat),log(price))) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price') +labs(title='Diamonds-Weight to Price(Linear)') 
#Taking the log of both variables                    

#Question 4
model1 <- lm(log(price)~log(carat), data = diamonds) #Creating a linear model
summary(model1)  
ggplot(diamonds,aes(log(carat),residuals((model1)))) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price Residuals') +labs(title='Diamonds-Weight to Price by Color') 
#residuals() is to get the residual of a linear model.

#Question 5
require("grid")
model1 <- lm(log(price)~log(carat), data = diamonds)
ggplot(diamonds,aes(log(carat),residuals((model1)))) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price Residuals') +labs(title='Diamonds-Weight to Price by Color') +theme(legend.position = 'top')
#Drawing the large graph first and moving the legend position to the top

graph1<-ggplot(diamonds,aes(price, colour = factor(color))) +geom_histogram(binwidth = 30) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none')
#Creating the density graph of price with binwidth 30.
graph2<-ggplot(diamonds,aes(carat, colour = factor(color))) +geom_histogram(binwidth = 0.1) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none')
#Creating the density graph of carat with binwidth 0.1.

vp1 <- viewport(width = 0.4, height = 0.2, x = 0.8, y = 0.7) #top-right viewport
vp2 <- viewport(width = 0.5, height = 0.3, x = 0.25, y = 0.15) #bottom-left viewport


print(graph2, vp = vp1) #printing graph2 to the top-right viewport 
print(graph1, vp = vp2)  #printing graph1 to the bottom-left viewport 
