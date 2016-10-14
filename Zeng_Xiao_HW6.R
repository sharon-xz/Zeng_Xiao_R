library("ggplot2")
library("grid")

# Bring Diamonds data into memory
data(diamonds)


#Question 2
ggplot(diamonds,aes(carat,price)) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price') +labs(title='Diamonds-Weight to Price by Color') 


#Question 3
ggplot(diamonds,aes(log(carat),log(price))) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price') +labs(title='Diamonds-Weight to Price(Linear)') 
                    

#Question 4
model1 <- lm(log(price)~log(carat), data = diamonds)
summary(model1)
ggplot(diamonds,aes(log(carat),residuals((model1)))) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price Residuals') +labs(title='Diamonds-Weight to Price by Color') 


#Question 5
require("grid")
model1 <- lm(log(price)~log(carat), data = diamonds)
ggplot(diamonds,aes(log(carat),residuals((model1)))) + geom_point(aes(colour = factor(color))) + xlab('Weight') + ylab('Price Residuals') +labs(title='Diamonds-Weight to Price by Color') +theme(legend.position = 'top')


graph1<-ggplot(diamonds,aes(price, colour = factor(color))) +geom_histogram(binwidth = 30) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none')
graph2<-ggplot(diamonds,aes(carat, colour = factor(color))) +geom_histogram(binwidth = 0.1) + theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position='none')

vp1 <- viewport(width = 0.4, height = 0.2, x = 0.8, y = 0.7)
vp2 <- viewport(width = 0.5, height = 0.3, x = 0.25, y = 0.15)


print(graph2, vp = vp1)
print(graph1, vp = vp2)