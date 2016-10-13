require("grid")

#Question 1
data(diamonds)
str(diamonds)
summary(diamonds)
ncol(diamonds)
attr(diamonds, "attributes",exact = FALSE)
#Question 2
nrow(diamonds)


#Question 3
colnames(diamonds)
cat(names(diamonds),sep="\n")

#Question 4
sapply(diamonds, class)

#Question 5

for (i in (1:length(diamonds))){
  j = diamonds[i]
  print(j)
  if (is.numeric(j)==TRUE){
     print(mean(j),class(diamonds)[i]) }
}


#Question 6
for (i in diamonds){

}

