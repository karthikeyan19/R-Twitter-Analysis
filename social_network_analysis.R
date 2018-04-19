library(twitteR)


tweets <- searchTwitter("#csk", n = 250)

df <- do.call("rbind", lapply(tweets, as.data.frame))

require(tm)

myCorpus <- Corpus(VectorSource(df$text))


removeRT <- function(x) gsub("RT @[a-z,A-Z]*: ", "", x)
xx <- tm_map(xx, removeURL)

myCorpus<-tm_map(myCorpus, removePunctuation)

removeLanguage <- function(x) gsub("[^[:graph:]]", "", x)
xx <- tm_map(xx, removeURL)

ccm<-tm_map(ccm, function(x) iconv(enc2utf8(x), "UTF-8",sub = ""))

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
xx <- tm_map(xx, removeURL)

removeNum <- function(x) gsub("[[:digit:]]",
                              "", x)
removeNum <- function(x) gsub("[[:alnum:]]",
                              "", x)

xx <- tm_map(xx, removeURL)

myCorpus<-tm_map(myCorpus, tolower)

myCorpus<-tm_map(myCorpus, removeWords, stopwords("english"))
                 
myCorpus<-tm_map(myCorpus, stemDocument)
inspect(myCorpus[1:3]) 






strwrap(myCorpus[[1]])

myDtm <- TermDocumentMatrix(mcc, control = list(minWordLength = 1))

inspect(myDtm[1:10,20:30])


findFreqTerms(myDtm, lowfreq=10)

findAssocs(myDtm, 'csk', 0.1)

findAssocs(myDtm, 'yellow', 0.1)

findAssocs(myDtm, 'tamil', 0.1)

findAssocs(myDtm, 'msdhoni', 0.1)

findAssocs(myDtm, 'pune', 0.1)

require(wordcloud)

m <- as.matrix(myDtm)

v <- sort(rowSums(m), decreasing=TRUE)

myNames <- names(v)

d <- data.frame(word=myNames, freq=v)

wordcloud(d$word, d$freq, colors = c("tomato","black", "blue"), scale = c(6, 0.5), random.color = TRUE, rot.per = 0.5, 
              min.freq = 2, font = 2, family = "serif")
library(igraph)

mat<-m[1:10,]
mat[mat>=1] <- 1
 mat <- mat %*% t(mat)
 g <- graph.adjacency(mat, weighted=T, mode = "undirected")
 V(g)$degree <- degree(g)
 V(g)$label <- V(g)$name
 layout1 <- layout.fruchterman.reingold(g)
 plot(g, layout=layout1)


