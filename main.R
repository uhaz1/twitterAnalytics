

library(tm)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(ggplot2)
library(readr)
library(plyr)
library(magrittr)
library(purrrlyr)

# get sample
uniqR = 
uniqR <- uniqR[,-c(8,9,10)]

### get text documents
text <- as.character(tw$text)
## carry out text data cleaning-gsub
some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tw$text)
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
some_txt<-gsub("@\\w+","",some_txt)
some_txt<-gsub("[[:punct:]]"," ",some_txt)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)
some_txt=as.character(some_txt)
class(text)
print(text[1])
length(text)
### create corpus
corpus <- VCorpus(VectorSource(text))
class(corpus)

### inspect corpus
print(corpus)
inspect(corpus[1:2])
#meta(corpus[[2]],"id")
meta(corpus[[1]])

class(corpus[[1]])
corpus[[1]]
corpus[[1]][[1]]     # print document 1
inspect(corpus[[1]]) # character rep of document 1 in tm

lapply(corpus[1:2],as.character)

#### TRANSFORMATIONS  ####

# done using tm_map()
# modify docs - 
# eg. stemming , 
#     stopwords removal

inspect(corpus[[1]])

corpus2 <- corpus
# remove extra whitespace
corpus <- tm_map(corpus,stripWhitespace)
inspect(corpus[[1]])

# convert to lowercase
corpus <- tm_map(corpus,content_transformer(tolower))
inspect(corpus[[1]])

# remove stopwords
# stopwords - words that are frequent but provide little information./174
# Some common English stop words include "I", "she'll", "the"
corpus <- tm_map(corpus,removeWords, stopwords("english"))
inspect(corpus[[1]])

# Add "word1" and "word" to the list: new_stops
# new_stops <- c("word1", "word2", stopwords("english"))
# corpus <- tm_map(corpus,removeWords, new_stops)

# stemming
# stemming- reduce words to word to their word stem, base or root form
# eg. cats, catlike, and catty -> cat; fishing, fished, and fisher -> fish
corpus <- tm_map(corpus,stemDocument)
inspect(corpus[[1]])


##### FILTERS #####


#### TERM DOCUMENT MATRIX  ####

#create term document matrix
dtm <- DocumentTermMatrix(corpus)
# inspect term document matrix - sample
inspect(dtm)


# terms that occurs at least 5 times
findFreqTerms(dtm,5)

#  associations with at least 0.8 correlation with a word
findAssocs(dtm, "credit",0.5)

# remove sparse terms
# terms document matrices tend to get big. To reduce matrix size dramatically,
#   - remove sparse terms (terms occuring in a very few docs) 
inspect(removeSparseTerms(dtm,0.997))
inspect(dtm)

removedsparse <- removeSparseTerms(dtm,0.997)

freq_up = colSums(as.matrix(removedsparse))

#word cloud
bag = as.matrix(freq_up)
bag = sort(rowSums(bag), decreasing = T)
bag.df = data.frame(word = names(bag), freq = bag)

set.seed(111)
str(bag)

wordcloud(words = bag.df$word, freq = bag.df$freq, min.freq = 5,max.words=1500, random.order=FALSE, rot.per=0.3,colors=brewer.pal(8, "Dark2"),scale = c(5,0.5))


##################################
####### CLUSTERING -Kmeans #######
##################################

# Determine number of clusters
wss <- 1:14

for (i in 1:15) {
  wss[i] <- sum(kmeans(M,centers=i)$withinss)
}

# ss vs cluster count
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

Cluster <- kmeans(M, 9, nstart = 20)
Cluster$cluster
Cluster$centers
length(Cluster$cluster)

df2$cluster <- Cluster$cluster
head(df2)
uniqR$cluster <- Cluster$cluster

clust.id = 9
freq2 = colSums(df2[df2$cluster==clust.id,-c(1215)])
print(sort(freq2, decreasing = T))


##################################
####### TOPIC MODELS #######
##################################

K <- 9
ap_lda1 <- LDA(dtm, k = K, method = "Gibbs", 
               control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))
ap_lda1

term <- terms(ap_lda1, 15) ##top 10 topics
head(term)
#topics <- topics(ap_lda1, 15) ##top 10 topics
#head(t(topics))

topics <- posterior(ap_lda1)
head(topics[['terms']])
head(topics[['topics']])

#distHellinger(topics[['terms']])

library(tidytext)

ap_topics <- tidy(ap_lda1, matrix = "beta")
#per-topic-per-word probabilities
ap_topics


ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ap_documents <- tidy(ap_lda1, matrix = "gamma")
ap_documents
#LDA also models each document as a mixture of topics
#50% words from topic1

findAssocs(dtm, "benefit",0.2)
findAssocs(dtm, "govern",0.1)


library(LDAvis)

topics <- posterior(ap_lda1)
head(topics['topics'])
head(topics['terms'])

help(createJSON, package = "LDAvis")

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(ap_lda1))


