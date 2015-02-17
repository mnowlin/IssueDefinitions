# DETERMINE ISSUE DIMENSIONS 

install.packages("tm", dependencies=TRUE, repos='http://cran.us.r-project.org')
library(tm)

install.packages("SnowballC", dependencies=TRUE, repos='http://cran.us.r-project.org')
library(SnowballC)

install.packages("topicmodels", dependencies=TRUE, repos='http://cran.us.r-project.org')
library(topicmodels)

######## load statements
statements <- Corpus(DirSource("Witness Statements Complete txt"))
summary(statements)

#### pre-processing 
statements <- tm_map(statements, content_transformer
(tolower))
statements <- tm_map(statements, removeNumbers)
statements <- tm_map(statements, removePunctuation)
stan_stopwords <-c(stopwords("english"))
statements <- tm_map(statements, removeWords, stan_stopwords)
statements <- tm_map(statements, removeWords,
                     c("nuclear","waste","wastes","spent","fuel",
"chairman","committee","thank","testimony","question","questions",
 "record","available","statement","look","dont","talk","congress",
"people","believe","will","one","can","get","think","like","say","now","just","make","want","get","know","problem","point","much", "time","way","state"))

statements <- tm_map(statements, stemDocument)

#### create document term matrix 
stateDTM <- DocumentTermMatrix(statements)

dim(stateDTM)
stateDTM

stateDTM <- removeSparseTerms(stateDTM, .99)
stateDTM
dim(stateDTM)

##############################################################
#### LDA with 7 topics; won't run							##
## SEED <- 12345											##
## k <- 7													##
## lda7 <- LDA(stateDTM, k, method="Gibbs", control=list(seed=SEED))									##
## Export results to .csv									##
## state.dat <- cbind(topics(lda7),posterior(lda7)$topics)	##
## write.csv(state.dat, "nw.statements.complete.csv")		##	
##############################################################