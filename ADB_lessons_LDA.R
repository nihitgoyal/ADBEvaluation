pvr <- read.csv("input\\ADB_Comprehensive_PVR.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE)

#Extract contents of the Project Title column
lessons <- pvr[["LESSONS"]]

stopwords_regex = paste(stopwords("SMART"), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
lessons = stringr::str_replace_all(lessons, stopwords_regex, '')

library(tm)

corpus <- Corpus(VectorSource(lessons))

#Preprocess contents of titles for topics modelling
corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[[:cntrl:]]", replacement = " ")
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[^[:alnum:]]", replacement = " ")
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(gsub), 
                 pattern = "[[:space:]]{1,}(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})[[:space:]]{1,}", 
                 replacement = " ") #remove roman numerals

corpus <- tm_map(corpus, removeWords, stopwords("SMART"))

additionalStopWords <- c("adb", "amp", "completion report", "document", "lesson", "lessons", "para", "pcr",
                         "program", "project", "project completion report", "projects", "report", "text",
                         "topic", "topics", "xarr", "year") 

corpus <- tm_map(corpus, removeWords, additionalStopWords)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^\\s+|\\s+$", replacement = "")

#Temporarily removing stemming for ease of interpretation
corpus <- tm_map(corpus, stemDocument)

write(sapply(1:length(corpus), function(i){corpus[[i]]$content}), "pvr_lessons_stem.txt")

dtm <- DocumentTermMatrix(corpus)
rownames(dtm) <- pvr[["ID"]]

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each PCR
dtm <- dtm[rowTotals > 0, ] #Remove all PVRs without words

frequency <- colSums(as.matrix(dtm))

length(frequency)
order <- order(frequency, decreasing = TRUE)
frequency[order]
write.csv(frequency[order], "output\\frequencies_stem.csv")

library(ldatuning)

#Set parameters for Gibbs sampling

t1 <- Sys.time()
  
result <- FindTopicsNumber(dtm, topics = seq(from = 20, to = 55, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs", mc.cores = 1, verbose = TRUE)

t2 <- Sys.time()
t2 - t1

FindTopicsNumber_plot(result)

#Setting K manually based on graph
# k <- 26
# 
# t1 <- Sys.time()
# 
# ldaOutput <- LDA(dtm, k, method="Gibbs",
#                  #control = list(nstart = nstart, seed = seed, best = best, burnin = burnin,
#                  #              iter = iter, thin = thin)
#                  )
# 
# t2 <- Sys.time()
# t2 - t1
# 
# ldaOutput.topics <- as.matrix(topics(ldaOutput))
# write.csv(ldaOutput.topics, file = paste("output\\lesson_topics_by_pvr.csv"))
# 
# ldaOutput.terms <- as.matrix(terms(ldaOutput, 20))
# write.csv(ldaOutput.terms, file = paste("output\\lesson_terms_by_topics.csv"))
# 
# topicProbabilities <- as.data.frame(ldaOutput@gamma)
# topicProbabilities$ID <- ldaOutput@documents
# write.csv(topicProbabilities, file = paste("output\\lesson_topic_probabilities.csv"))
# 
# mergedPVR <- merge(pvr, topicProbabilities, by="ID")
# write.csv(mergedPVR, file = paste("output\\PVR_with_topics.csv"))
# 
# #Now run using LDA and LDAVis
# #Change the PCR variables below to the pre-processed ones... 
# ldaInput <- setNames(sapply(1:length(corpus), function(i){corpus[[i]]$content}), pvr[["ID"]])
# ldaInput <- sapply(ldaInput, function(x) paste(x, collapse = " "))
# 
# # tokenize on space and output as a list:
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# ldaInput <- trim(ldaInput)
# 
# document.list <- strsplit(ldaInput, "[[:space:]]+")
# 
# # compute the table of terms:
# term.table <- table(unlist(document.list))
# term.table <- sort(term.table, decreasing = TRUE)
# vocab <- names(term.table) 
# 
# # now put the documents into the format required by the lda package:
# get.terms <- function(x) {
# 	index <- match(x, vocab)
# 	index <- index[!is.na(index)]
# 	rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
# }
# documents <- lapply(document.list, get.terms)
# 
# # Compute some statistics related to the data set:
# D <- length(documents) # number of documents
# W <- length(vocab) # number of terms in the vocab
# document.length <- sapply(documents, function(x) sum(x[2, ])) # number of tokens per document 
# N <- sum(document.length) # total number of tokens in the data 
# term.frequency <- as.integer(term.table) # frequencies of terms in the corpus
# 
# # Fit the model:
# library(lda)
# 
# # MCMC and model tuning parameters:
# #K <- 20
# burnin <- 100
# iter <- 1000
# alpha <- 0.02
# eta <- 0.02
# set.seed(sample(1:1000000))
# 
# t1 <- Sys.time()
# 
# fit <- lda.collapsed.gibbs.sampler(documents = documents, K = k, vocab = vocab,
# 	num.iterations = iter, alpha = alpha, eta = eta, initial = NULL, burnin = burnin, 
# 	compute.log.likelihood = TRUE)
# 
# t2 <- Sys.time()
# t2 - t1
# 
# theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
# phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))) 
# ldaVis <- list(phi = phi, theta = theta, doc.length = document.length, vocab = vocab, 
# 	term.frequency = term.frequency)
# 
# library(LDAvis)
# 
# # create the JSON object to feed the visualization:
# json <- createJSON(phi = ldaVis$phi, 
#                    theta = ldaVis$theta, 
#                    doc.length = ldaVis$doc.length, 
#                    vocab = ldaVis$vocab, 
#                    term.frequency = ldaVis$term.frequency)
# 
# serVis(json, out.dir = 'vis', open.browser = FALSE)