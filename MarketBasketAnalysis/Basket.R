getwd()
setwd("C:/Users/Shruti Sankhe/Documents/data science")

library(arules)

install.packages("data.table")

library(data.table)
library(dplyr)

#1.
basket<-read.csv("groceries.csv",sep="\n",header=FALSE,stringsAsFactors = FALSE)
head(basket)
k<-nrow(basket)
trans_id<-1:k

basket<-cbind(basket,trans_id)
str(basket)
z<-strsplit(basket$V1,split=",")

head(z)

basket<-data.frame(trans_id = rep(basket$trans_id, sapply(z, length)), V1 = unlist(z))
colnames(basket)[2]<-"Item"

head(basket)


#2.

translist<-split(basket$Item,basket$trans_id,"transactions")

str(translist)

head(translist)

unique(basket$Item)


rul=apriori(translist,parameter = list(support=0.005,confidence=0.5,maxlen=10,minlen=2))


inspect(rul)


#3. Find the top 20 "sold items" that occur in the dataset.

ctr<-0
p<-function(.data) .data %>% do( mutate (. , Count=ctr+1))
bas<-basket %>% p
head(bas)

total<-bas %>% group_by(bas$Item) %>% summarise(No_of_sold_items=sum(Count)) %>% arrange(desc(No_of_sold_items))

  
top_20<-bas %>% group_by(bas$Item) %>% summarise(No_of_sold_items=sum(Count)) %>% arrange(desc(No_of_sold_items)) %>% filter(No_of_sold_items>600)

colnames(top_20)[1]<-"Item"

View(top_20)


head(total)

top_20<-top_20[,-4]
colnames(total)[1]<-"Item"
#4.

top_20_sales<- sum(top_20$No_of_sold_items)
top_20_sales
total_sales<-sum(total$No_of_sold_items)

top20contr<-(top_20_sales/total_sales)*100

top20contr

#support(rul,translist)

#5.
install.packages("arulesViz")
library(arulesViz)
png(file="rulesplot.png")
plot(rul,jitter=0)
dev.off()


#6.
inspect(subrules)


#Piechart
sales<-c()
for(i in 1:nrow(top_20)){
sales[i]<-(top_20$No_of_sold_items[i]/top_20_sales)*100  
}
top_20

sales

top_20<-cbind(top_20,sales)
str(total)
png(file="pie.png")
pie(top_20$sales,labels=top_20$Item,main="Percentage sales",col=rainbow(length(top_20$Item)))
dev.off()
#Graph
png(file="probar.png")
barplot(top_20$No_of_sold_items,names.arg=top_20$Item,col=rainbow(length(top_20$Item)),xlab="items",
        ylab="NO of sold items")
dev.off()
#Onions,root,Whole milk,Root vegetables,citrus fruit,frozen vegetables and  tropical fruit should be racked near to the other vegetables.
#Curd,yogurt,baking powder should be racked near whole milk
#



