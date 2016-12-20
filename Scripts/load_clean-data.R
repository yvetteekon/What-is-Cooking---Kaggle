# install package
#install.packages("jsonlite")

# load libraries
packages <- c("jsonlite", "tm", "SnowballC", "magrittr", "dplyr")
lapply(packages, require, character.only = T)

# load data
train <- fromJSON("train.json")
test <- fromJSON("test.json")
head(train, 6); head(test, 6)
names(train); names(test)

# combine data
test$cuisine <- NA
combi <- rbind(train, test)
dim(combi)
class(combi)

# convert vector to corpus
corpus <- Corpus(VectorSource(combi$ingredients))

# inspect corpus
inspect(corpus[1])
viewCorpus <- function(d, n){
  d %>% extract2(n) %>% as.character() %>% writeLines()
}
viewCorpus(corpus, 12)

# text pre-processing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# copy of corpus for dictionary
#corpusCopy <- corpus

# stemming
corpus <- tm_map(corpus, stemDocument)

# stem completion
#corpus <- tm_map(corpus, stemCompletion, dictionary = corpusCopy)

# clean corpus
corpus <- tm_map(corpus, PlainTextDocument)

# create matrix
dtm <- DocumentTermMatrix(corpus, control = list(wordLength = c(1, Inf)))
tdm <- as.TermDocumentMatrix(dtm)
dtm

# create dataframe
freq <- sort(colSums(as.matrix(dtm)), decreasing = T)
words <- names(freq)
df <- data.frame(words = words, freq = freq)
row.names(df) <- NULL
head(df, 20)
tail(df, 20)

# remove sparse terms
dtms <- removeSparseTerms(dtm, 0.99)
tdms <- removeSparseTerms(tdm, 0.99)

# modify dataframe
freq <- sort(colSums(as.matrix(dtms)), decreasing = T)
ingredients <- names(freq)
df <- data.frame(words = ingredients, freq = freq)
row.names(df) <- NULL
head(df, 20)
tail(df, 20)

# export data
save(train, file = "train.RData")
save(train, file = "test.RData")
save(combi, file = "combi.RData")
save(dtm, file = "dtm.RData")
save(tdm, file = "tdm.RData")
save(df, file = "df.RData")
save(dtms, file = "dtms.RData")
save(tdms, file = "tdms.RData")

