pvr <- read.csv("input\\ADB_Comprehensive_PVR.csv",
                header = TRUE,
                quote="\"",
                stringsAsFactors= TRUE,
                strip.white = TRUE)

#Extract contents of the Project Title column
lessons <- pvr[["LESSONS"]]

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
#corpus <- tm_map(corpus, stemDocument)

write(sapply(1:length(corpus), function(i){corpus[[i]]$content}), "pvr_lessons.txt")

dtm <- DocumentTermMatrix(corpus)
rownames(dtm) <- pvr[["ID"]]

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each PCR
dtm <- dtm[rowTotals > 0, ] #Remove all PVRs without words

frequency <- colSums(as.matrix(dtm))

length(frequency)
order <- order(frequency, decreasing = TRUE)
frequency[order]
write.csv(frequency[order], "output\\frequencies.csv")

library(ldatuning)

#Set parameters for Gibbs sampling
burnin <- 100
iter <- 1000
thin <- 50
nstart <- 5
seed <- sample(1:1000000, 5) 
best <- TRUE

t1 <- Sys.time()
  
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 5, to = 205, by = 20),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin),
  mc.cores = 1,
  verbose = TRUE
)

t2 <- Sys.time()
t2 - t1

FindTopicsNumber_plot(result)

#Maximum Log Likelihood

library(topicmodels)

t1 <- Sys.time()

best.model <- lapply(seq(5,205, by=20), function(k){LDA(dtm, k, method="Gibbs",
                        control = list(nstart = nstart, seed = seed, best = best,
                        burnin = burnin, iter = iter, thin = thin))})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
best.model.logLik.df <- data.frame(topics=seq(5,205, by=20), LL=as.numeric(as.matrix(best.model.logLik)))

t2 <- Sys.time()
t2 - t1

library(ggplot2)

ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
  xlab("Number of topics") + ylab("Log likelihood of the model") + geom_line() + theme_bw()

best.model.logLik.df[which.max(best.model.logLik.df$LL),]

k <- best.model.logLik.df[which.max(best.model.logLik.df$LL), 1]

#Run with topicmodels package

t1 <- Sys.time()

ldaOutput <- LDA(dtm, k, method="Gibbs",
                 control = list(nstart = nstart, seed = seed, best = best, burnin = burnin,
                                iter = iter, thin = thin))

t2 <- Sys.time()
t2 - t1

ldaOutput.topics <- as.matrix(topics(ldaOutput))
write.csv(ldaOutput.topics, file = paste("output\\title_topics_by_pcr.csv"))

ldaOutput.terms <- as.matrix(terms(ldaOutput, 20))
write.csv(ldaOutput.terms, file = paste("output\\title_terms_by_topics.csv"))

topicProbabilities <- as.data.frame(ldaOutput@gamma)
topicProbabilities$ID <- ldaOutput@documents
write.csv(topicProbabilities, file = paste("output\\title_topic_probabilities.csv"))

mergedPVR <- merge(pvr, topicProbabilities, by="ID")
write.csv(mergedPVR, file = paste("output\\PVR_with_topics.csv"))

#Now run using LDA and LDAVis
#Change the PCR variables below to the pre-processed ones... 
ldaInput <- setNames(sapply(1:length(corpus), function(i){corpus[[i]]$content}), pvr[["ID"]])
ldaInput <- sapply(ldaInput, function(x) paste(x, collapse = " "))

# tokenize on space and output as a list:
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ldaInput <- trim(ldaInput)

document.list <- strsplit(ldaInput, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(document.list))
term.table <- sort(term.table, decreasing = TRUE)
vocab <- names(term.table) 

#del <- names(term.table) %in% ""
#term.table <- term.table[!del]

#vocab <- vocab[!del]

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
	index <- match(x, vocab)
	index <- index[!is.na(index)]
	rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(document.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents) # number of documents
W <- length(vocab) # number of terms in the vocab
document.length <- sapply(documents, function(x) sum(x[2, ])) # number of tokens per document 
N <- sum(document.length) # total number of tokens in the data 
term.frequency <- as.integer(term.table) # frequencies of terms in the corpus

# Fit the model:
library(lda)

# MCMC and model tuning parameters:
#K <- 20
alpha <- 0.02
eta <- 0.02
set.seed(sample(1:1000000))

t1 <- Sys.time()

fit <- lda.collapsed.gibbs.sampler(documents = documents, K = k, vocab = vocab,
	num.iterations = iter, alpha = alpha, eta = eta, initial = NULL, burnin = burnin, 
	compute.log.likelihood = TRUE)

t2 <- Sys.time()
t2 - t1

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))) 
ldaVis <- list(phi = phi, theta = theta, doc.length = document.length, vocab = vocab, 
	term.frequency = term.frequency)

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = ldaVis$phi, 
                   theta = ldaVis$theta, 
                   doc.length = ldaVis$doc.length, 
                   vocab = ldaVis$vocab, 
                   term.frequency = ldaVis$term.frequency)

serVis(json, out.dir = 'vis', open.browser = FALSE)

#Try STM
library(stm)

pvr$year <- as.numeric( format(as.Date(pvr$Report.Approval.Date., "%d-%m-%y"), "%Y"))

stmInput <- textProcessor(pvr$LESSONS, metadata = pvr, customstopwords = additionalStopWords)
stmData <- prepDocuments( stmInput$documents, stmInput$vocab, stmInput$meta, lower.thresh = 1)

t1 <- Sys.time()

stmSelectK <- searchK(stmData$documents, stmData$vocab, K = seq(5, 205, by = 20),
                      prevalence =~  I..CHOOSE.SECTOR + Choose.COUNTRIES + s(year) + Choose.Validation.of.Project.Program.Completion.Report.rating,
                      data = stmData$meta, init.type = "Spectral")

t2 <- Sys.time()
t2 - t1

plot.searchK(stmSelectK)

#Assigning K manually based on the graphs from previous operations. Should do this in code ideally...
k <- 12

#t1 <- Sys.time()

#Use this when not using "Spectral" init
#stmSelect <- selectModel(stmData$documents, stmData$vocab, K = k,
#                         prevalence =~  I..CHOOSE.SECTOR, max.em.its = 200, 
#                         data = stmData$meta, runs = 20, seed = sample(1:100000))
#t2 <- Sys.time()
#t2 - t1
#plotModels(stmSelect)

stmOutput <- stm(stmData$documents, stmData$vocab, K = k,
                 prevalence =~  I..CHOOSE.SECTOR + Choose.COUNTRIES + s(year) + Choose.Validation.of.Project.Program.Completion.Report.rating,
                 max.em.its = 500, data = stmData$meta, init.type = "Spectral")  

cloud(stmOutput)

labelTopics(stmOutput)

plot.STM(stmOutput, type="summary")

stmEffect <- estimateEffect(seq(1:k) ~ I..CHOOSE.SECTOR + Choose.COUNTRIES + s(year) + Choose.Validation.of.Project.Program.Completion.Report.rating,
                            stmOutput, metadata=stmData$meta, uncertainty = "None")

plot.estimateEffect(stmEffect, covariate = "year", model = stmOutput, method = "continuous", ci.level = 0,
                    topics = seq(1:k))

plot.estimateEffect(stmEffect, covariate = "year", model = stmOutput, method = "continuous", ci.level = 0,
                    topics = seq(1:k))

stmTopicCorr <- topicCorr(stmOutput)
plot.topicCorr(stmTopicCorr)