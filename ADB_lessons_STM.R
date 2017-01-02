library(stm)

pvr <- read.csv("input\\ADB_Comprehensive_PVR.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE)

#Extract contents of the Project Title column
lessons <- pvr[["LESSONS"]]

#Try STM
pvr$year <- as.numeric( format(as.Date(pvr$Report.Approval.Date., "%d-%m-%y"), "%Y"))


stmInput <- textProcessor(pvr$LESSONS, metadata = pvr, customstopwords = additionalStopWords)
stmData <- prepDocuments( stmInput$documents, stmInput$vocab, stmInput$meta, lower.thresh = 1)

t1 <- Sys.time()

stmSelectK <- searchK(stmData$documents, stmData$vocab, K = seq(5, 105, by = 5),
                      prevalence =~  I..CHOOSE.SECTOR + Choose.COUNTRIES,
                      #+ s(year) + Choose.Validation.of.Project.Program.Completion.Report.rating,
                      data = stmData$meta, init.type = "Spectral")

t2 <- Sys.time()
t2 - t1

plot.searchK(stmSelectK)

#t1 <- Sys.time()

#stmSelectK <- searchK(stmData$documents, stmData$vocab, K = seq(2, 25, by = 1),
#                      prevalence =~  I..CHOOSE.SECTOR + Choose.COUNTRIES,
#                      #+ s(year) + Choose.Validation.of.Project.Program.Completion.Report.rating,
#                      data = stmData$meta, init.type = "Spectral")

#t2 <- Sys.time()
#t2 - t1

#plot.searchK(stmSelectK)

#Assigning K manually based on the graphs from previous operations. Should do this in code ideally...
#k <- 5

#t1 <- Sys.time()

#Use this when not using "Spectral" init
#stmSelect <- selectModel(stmData$documents, stmData$vocab, K = k,
#                         prevalence =~  I..CHOOSE.SECTOR, max.em.its = 200, 
#                         data = stmData$meta, runs = 20, seed = sample(1:100000))
#t2 <- Sys.time()
#t2 - t1
#plotModels(stmSelect)

#stmOutput <- stm(stmData$documents, stmData$vocab, K = k,
#                 prevalence =~  I..CHOOSE.SECTOR + Choose.COUNTRIES,
#                 max.em.its = 500, data = stmData$meta, init.type = "Spectral")  

#cloud(stmOutput)

#labelTopics(stmOutput)

#plot.STM(stmOutput, type="summary", n = 10)
#plot.STM(stmOutput, type="hist")
#plot.STM(stmOutput, type="perspectives", topics = c(2,4))


#stmEffect <- estimateEffect(seq(1:k) ~ I..CHOOSE.SECTOR + Choose.COUNTRIES,
#                            stmOutput, metadata=stmData$meta, uncertainty = "None")

#plot.estimateEffect(stmEffect, covariate = "I..CHOOSE.SECTOR", model = stmOutput, topics = seq(1:k))

#plot.estimateEffect(stmEffect, covariate = "Choose.COUNTRIES", model = stmOutput, topics = seq(1:k))

#stmTopicCorr <- topicCorr(stmOutput)
#plot.topicCorr(stmTopicCorr)