df <- read.csv("input\\all_country.csv",
               header = TRUE,
               quote="\"",
               stringsAsFactors= TRUE,
               strip.white = TRUE)

data(df[["LESSONS"]], package = "LDAvisData")

# read in some stopwords: 
library(tm)
stop_words <- stopwords("SMART")

Encoding(evaluations) <- "latin1"

evaluations <- df[["LESSONS"]]

evaluations <- gsub("'", "", evaluations) # remove apostrophes
evaluations <- gsub("[[:punct:]]", " ", evaluations) # replace punctuation with space
evaluations <- gsub("[[:cntrl:]]", " ", evaluations) # replace control characters with space
evaluations <- gsub("[^[:alnum:]]", " ", evaluations) #remove non-printable characters
evaluations <- gsub("^[[:space:]]+", "", evaluations) # remove whitespace at beginning of documents
evaluations <- gsub("[[:space:]]+$", "", evaluations) # remove whitespace at end of documents
evaluations <- tolower(evaluations) # force to lowercase 
evaluations <- gsub("[[:digit:]]+", "", evaluations) #remove numbers
evaluations <- gsub("[[:space:]]{1,}(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})[[:space:]]{1,}", " ", evaluations) #remove roman numerals

 # tokenize on space and output as a list:
doc.list <- strsplit(evaluations, "[[:space:]]+") 

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
del <- names(term.table) == ""
term.table <- term.table[!del]
vocab <- names(term.table) 

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
	index <- match(x, vocab)
	index <- index[!is.na(index)]
	rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents) # number of documents
W <- length(vocab) # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ])) # number of tokens per document 
N <- sum(doc.length) # total number of tokens in the data 
term.frequency <- as.integer(term.table) # frequencies of terms in the corpus

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
	num.iterations = G, alpha = alpha,                                                                                                          				      eta = eta, initial = NULL, burnin = 0,                                                                      				      compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))) 
ADBevaluations <- list(phi = phi, theta = theta, doc.length = doc.length, vocab = vocab, 
	term.frequency = term.frequency)

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = ADBevaluations$phi, 
                   theta = ADBevaluations$theta, 
                   doc.length = ADBevaluations$doc.length, 
                   vocab = ADBevaluations$vocab, 
                   term.frequency = ADBevaluations$term.frequency)

serVis(json, out.dir = 'vis', open.browser = FALSE)