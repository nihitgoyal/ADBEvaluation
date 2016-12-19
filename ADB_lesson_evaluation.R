#Load the text mining package
library(tm)

#Read PVRs into R
pvr <- read.csv("input\\ADB_Comprehensive_PVR.csv",
               header = TRUE,
               quote="\"",
               stringsAsFactors= TRUE,
               strip.white = TRUE)

#Extract contents of the lessons column
lessons <- pvr[["LESSONS"]]

corpus <- Corpus(VectorSource(lessons))
writeLines(as.character(corpus[[10]]))

#Preprocess contents of lessons for topics modelling
corpus <- tm_map(corpus, content_transformer(tolower))

writeLines(as.character(corpus[[10]]))

corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[[:cntrl:]]", replacement = " ")
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[^[:alnum:]]", replacement = " ")
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(gsub), 
                 pattern = "[[:space:]]{1,}(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})[[:space:]]{1,}", 
                 replacement = " ") #remove roman numerals

corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

additionalStopWords <- c("project", "report", "pcr", "lesson", "topic", "adb", "para", "text", "year", "document", "amp")
corpus <- tm_map(corpus, removeWords, additionalStopWords)

dtm <- DocumentTermMatrix(corpus)
rownames(dtm) <- pvr[["Report Title"]]

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each PVR
dtm <- dtm[rowTotals > 0, ] #Remove all PVRs without words

frequency <- colSums(as.matrix(dtm))

length(frequency)
order <- order(frequency, decreasing = TRUE)
frequency[order]
write.csv(frequency[order], "frequencies.csv")

library(topicmodels)
library(ldatuning)

#Set parameters for Gibbs sampling
burnin <- 100
iter <- 1000
thin <- 50
nstart <- 5
seed <- sample(1:1000000, 5) 
best <- TRUE

t1 <- Sys.time()

#result <- FindTopicsNumber(
#  dtm,
#  topics = seq(from = 10, to = 20, by = 2),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin),
#  mc.cores = 1,
#  verbose = TRUE
#)

#FindTopicsNumber_plot(result)

#t2 <- Sys.time()
#t2 - t1

best.model <- lapply(seq(90,360, by=45), function(k){LDA(dtm, k)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
best.model.logLik.df <- data.frame(topics=seq(90,360, by=45), LL=as.numeric(as.matrix(best.model.logLik)))

t2 <- Sys.time()
t2 - t1

library(ggplot2)
ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
  xlab("Number of topics") + ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw() #+ 
#  opts(axis.title.x = theme_text(vjust = -0.25, size = 14)) + 
#  opts(axis.title.y = theme_text(size = 14, angle=90))

best.model.logLik.df[which.max(best.model.logLik.df$LL),]
#k <- 100

ldaOutput <- LDA(dtm, k, method="Gibbs",
                 control = list(nstart = nstart, seed = seed, best = best, burnin = burnin,
                                iter = iter, thin = thin))

 
ldaOutput.topics <- as.matrix(topics(ldaOutput))
write.csv(ldaOutput.topics, file = paste("LDAGibbs", k, "topics_by_pvr.csv"))
 
ldaOutput.terms <- as.matrix(terms(ldaOutput, 20))
write.csv(ldaOutput.terms, file = paste("LDAGibbs", k, "terms_by_topics.csv"))
 
topicProbabilities <- as.data.frame(ldaOutput@gamma)
write.csv(topicProbabilities, file = paste("LDAGibbs", k, "topic_probabilities.csv"))