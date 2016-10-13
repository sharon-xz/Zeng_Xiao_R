
#Homework 5 
#Xiao Zeng


#Question 1

#all methods and attributes associates with a dataframe
data(diamonds)
str(diamonds)
summary(diamonds)
attributes(diamonds)

ncol(diamonds)  #Number of columns in the dataframe

#Question 2
nrow(diamonds)  #Number of rows in the dataframe


#Question 3
colnames(diamonds)  #column names 
cat(names(diamonds),sep="\n") #printing names of the columns (one per line) to the console

#Question 4
sapply(diamonds, class)  #determines the type of each column

#Question 5

for (i in (1:ncol(diamonds))){
  j <- diamonds[,i]  #every column
  if (is.numeric(j)==TRUE){  #if it is numeric
      print(names(diamonds)[i])   #labeling the column name
      print(mean(j))}     #Calculating the mean of each column
}


#Question 6
library(plyr)   #import the count() function
for (i in (1:ncol(diamonds))){
  j <- diamonds[,i]   #for each column
  
  if (is.numeric(j)==FALSE){  #for non-numeric columns
    print(count(diamonds, names(diamonds)[i]))   #counting the frequency and returns a table
    }
}

#Question 7
#Getting the number if NAs in each column
na_col <- 0
for (i in (1:ncol(diamonds))){
  j <- diamonds[,i]  #for every column
  for (value in j){
    if (is.na(value) == TRUE ){
      na_col = na_col +1   #counting the number of NAs
    }
  }
  print("Number of NAs in each column")
  print(names(diamonds)[i])
  print(na_col)   #printing results

}

#Getting the percentage of rows containing NA in any column

na_total <- 0
na_row <- 0
for (i in (1:nrow(diamonds))){
  j <- diamonds[i,]
  for (value in j){
    if (is.na(value) == TRUE ){
      na_row = 1
    }
  }
  if (na_row ==1){  #be sure that each row that has NA in it only be counted once
      na_total = na_total + 1
      na_row = 0
  }
  
}
percentage <- 0.0
percentage <- na_total/nrow(diamonds)
print("Percentage of rows containing NA in any column:")
print(percentage) #printing results



#Question 8

f <- function(dataframe){
  #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
  #and their corresponding Pearson correlation coefficient in the second column.
  #Parameters: A dataframe
  #Returns: A dataframe
  
  name_pairs <- cbind()  #Initiation
  correlations <- cbind()
  
  for (i in (1:ncol(dataframe))){
    name1 = names(diamonds)[i]  #getting the first column name
    
    for (j in (i+1:ncol(dataframe))){  #only getting the columns after the first one to avoid repetition
      if  (is.na(names(diamonds)[j]) == FALSE){
          if (names(diamonds)[j] != names(diamonds)[i]){
              name2 =names(diamonds)[j]}   #getting the second column name
              name_pairs <- rbind(name_pairs,paste(name1,name2,sep = "-", collapse = NULL))  #paste them in the "name1-name2" format
              
              if (is.numeric(dataframe[,name1]) == TRUE){  #only calculates the correlation for numeric columns
                if (is.numeric(dataframe[,name2]) == TRUE){
                  correlations<- rbind(correlations, cor(dataframe[,name1],dataframe[,name2], method="pearson"))}#calculating the pearson correlations
                else{
                  correlations<- rbind(correlations, NA)  #for non-numeric columns, the correletion is NA to avoid errors
                }}
              else{
                correlations<- rbind(correlations, NA)
              }
      
      }
    }

  }
  
  output = data.frame(name_pairs,correlations) #combining two columns
  return(output)
}
  

print(f(diamonds))




