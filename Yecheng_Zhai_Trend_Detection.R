rm(list=ls())
library(tm)
library(topicmodels)
library(wordcloud)

docs<-Corpus(DirSource(c( "fb2011","fb2012","fb2013","fb2014","fb2015" )))
dim(docs)
length(docs)

mystopwords <- c("http","www","com","https","can","will","html","just","know","now","one","day","time","year","got","made","don","today","will","really","ever","may","make","use","also","get","week","minutes")
dtm <- DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=c(stopwords("english"),mystopwords)))
dim(dtm)
inspect(dtm[1:10,1:8])
1-1/60
1-2/60
dtm <- removeSparseTerms(dtm,0.97)
dim(dtm)

#freq = colSums( as.matrix(dtm) ) # the num of time each terms appear in all docs
#freq.sorted = sort( freq, decreasing=TRUE )
#freq.sorted[1:100] # most frequent words appeared in the corpus

# Term frequency
freq = colSums( as.matrix(dtm) )
wordcloud(names(freq), freq, max.words=50, colors=brewer.pal(6,"Dark2"))


# Plot word cloud for a specific topic
tid <- 10
freq <- terms[tid, ] 
wordcloud(names(freq), freq, max.words=30, colors=brewer.pal(6,"Dark2"))

library(text2vec)
ingredients = readLines("ingredients.txt", warn = FALSE)

ingredients <-tolower(ingredients)

dtm <- as.matrix(dtm)
colnames(dtm)[which(colnames(dtm) %in% ingredients)]
num_of_ingred<- sum(colnames(dtm) %in% ingredients) # there are 547 ingredients that are often mentioned in fb posts
freq_matrix <- dtm[,which(colnames(dtm) %in% ingredients)]
rownames(freq_matrix) <- substr(rownames(freq_matrix),7,12)
rownames(dtm) <- substr(rownames(dtm),7,12)

#test cauliflower & vegetable noodle
barplot(freq_matrix[,colnames(freq_matrix)=="cauliflower"], xlab = rownames(freq_matrix), main = "cauliflower")
barplot(dtm[,colnames(dtm)=="zucchini"], main = "zucchini")

#calculate the means of absolute increase rates in every month for each of the 547 ingredients
rate <- matrix (NA, 108,num_of_ingred)
month_var <-rep(NA, num_of_ingred) # variance of the change rates in year 2015 for rach ingredient
YOY_mean <- rep(NA, num_of_ingred) # variance of the change rates on year-on-year-basis in year 2015 for rach ingredient

for (i in 2:60){
  for (m in 1:num_of_ingred){
    rate[i-1,m] <- freq_matrix[i,m]/freq_matrix[i-1,m]-1
    month_var[m] <- var(rate[48:59,m][is.finite(rate[48:59,m])], na.rm = T) 
  }
}
for (i in 2:49){
  for (m in 1:num_of_ingred){
    rate[i+59,m] <- freq_matrix[i+11,m]/freq_matrix[i-1,m]-1
    YOY_mean[m] <- mean(rate[97:108,m][is.finite(rate[97:108,m])], na.omit = T)
  }
}

x1 <- which(month_var>=sort(month_var, decreasing = TRUE)[70], arr.ind = T)
#the code above get the location of ingredients which has the largest variance in terms of monthly increase rate in year 2015
x2 <- which(YOY_mean>= sort(YOY_mean[is.finite(YOY_mean)], decreasing = TRUE)[70],arr.ind = T)
##the code above get the location of ingredients which has the largest mean in terms of monthly increase rate on Year-On-Year basis in year 2015

length(x1)
length(x2)
x <- subset(x1, x1 %in% x2)
#here I find out the location of ingredients which have not only large variance on monthly increase rate, but also the large variance on increase rate on Year-On-Year basis in year 2015
length(x)
colnames(freq_matrix)[x]
#here are the names of the 17 ingredients which have a higher trend of being mentioned on fb, so they may have high market potential. 
par(mfrow=c(4,5)) 
for (i in 1:length(x)){
  barplot(freq_matrix[,x[i]], main = colnames(freq_matrix)[x[i]])
}
# bar plot of the number of posts containing these ingredients each month
