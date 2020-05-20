require(readr)
require(arules)
require(arulesViz)
require(splitstackshape) 
require(plyr)
require(dplyr)
require(tidyverse)
require(RColorBrewer)

#Stroring the datasets
mydata<-read.csv("http://www.utdallas.edu/~sxg180154/order_products__prior.csv", nrows=20000)
products<-read.csv("http://www.utdallas.edu/~sxg180154/products.csv")

#Selection of the first two columns
mydata<-mydata[,1:2] 

#Joining the data
joineddata<-left_join(products,mydata) %>% group_by(order_id) %>% summarise(product_names=as.vector(list(product_name)))
fItems <- eclat (joineddata$product_names, parameter = list(supp = 0.01, maxlen = 10))

# Frequent itemsets for products in orders dataset. You have to output product names and not just product id: 
inspect(fItems)

mydata<-merge(mydata,products,by="product_id") #merging
mydata<-arrange(mydata, order_id) # ascending order

dt <- split(mydata$product_name, mydata$order_id)
dt2 = as(dt,"transactions")

frequency <- eclat(dt2,parameter = list(supp = 0.01, maxlen = 10))
itemfreq <- itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,topN=20,type="absolute") #plotting the itemfreuency of products using itemFrequencyPlot()


#Association rules for products in orders dataset. You have to output product names and not just product id
rules = apriori(dt2, parameter=list(support=0.001, confidence=0.8, maxlen=10))#Finding the association rules

options(digits=2)
inspect(head(rules,5))
rules<-sort(rules, by="lift", decreasing=TRUE)#Sorting rules using lift
inspect(head(rules,5))
summary(rules)


#Frequent itemsets for departments in orders dataset (i.e which departments have highest number of orders). You have to output department names and not just department id
#Storing the datasets
mydata<-read.csv("http://www.utdallas.edu/~sxg180154/order_products__prior.csv", nrows=20000)
departments<-read.csv("http://www.utdallas.edu/~sxg180154/departments.csv")

#Selection of the first two columns
mydata<-mydata[,1:2] 
departments<-merge(products,departments,by="department_id")

#Joining the data
mydata<-merge(mydata,departments,by="product_id") 
mydata<-arrange(mydata, department_id) # ascending order

dt <- split(mydata$department, mydata$order_id)
dt2 = as(dt,"transactions")

frequency <- eclat(dt2,parameter = list(supp = 0.01, maxlen = 10))
itemfreq <- itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,topN=20,type="absolute") #plotting the itemfreuency of departments using itemFrequencyPlot()

#Association rules for departments in orders dataset (e.g. frozen -> groceries). You have to output department names and not just department id:
rules = apriori(dt2, parameter=list(support=0.001, confidence=0.8,maxlen=10))#Finding the association rules

options(digits=2)
inspect(head(rules,5))
rules<-sort(rules, by="lift", decreasing=TRUE)#Sorting rules using lift
inspect(head(rules,5))
summary(rules)