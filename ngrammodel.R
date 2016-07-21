
#Load the required packages
library(tm)
library(RWeka)
library(ggplot2)
library(tau)
library(SnowballC)
library(slam)
library(data.table)
library(stringr)

# Getting the Data 

setwd("E:/DataScience/Capstone/Project")
file.info(list.files(path=".", pattern = ".txt$"))
Data_source <- DirSource("E:/DataScience/Capstone/Project", encoding = "UTF-8", mode = "text")
Data_source
con1 <- file(Data_source$filelist[1], "r")
blogs <- readLines(con1, skipNul = TRUE)
close(con1)
con2 <- file(Data_source$filelist[2], "r")
news <- readLines(con2, skipNul = TRUE)
close(con2)
con3 <- file(Data_source$filelist[3], "r")
twitter <- readLines(con3, skipNul = TRUE)
close(con3)

# Build a sizeable sample from all the data files
set.seed(352)
create_sample <- function(x,sample_decimal) {
  sample_size <- as.integer(length(x)*sample_decimal)
  sample_data <- sample(x, size = sample_size, replace = FALSE)
  return(sample_data)
}
sample_from_blogs <- create_sample(blogs, 0.04)
sample_from_news <- create_sample(news, 0.4)
sample_from_twitter <- create_sample(twitter, 0.02)
combined_sample <- c(sample_from_blogs, sample_from_news, sample_from_twitter)

combined_sample <- iconv(combined_sample, "latin1", "UTF-8", sub = '')
combined_sample <- gsub("@[^\\s]+", " ", combined_sample)
combined_sample <- gsub("@[!?.]+", " ", combined_sample)

rm(sample_from_news, sample_from_blogs, sample_from_twitter)

data_corpus <- VCorpus(VectorSource(combined_sample))

# Cleaning the Data
corpus_copy <- data_corpus
#toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
#data_corpus <- tm_map(data_corpus, toSpace, "\\W")
data_corpus <- tm_map(data_corpus, content_transformer(stripWhitespace))
data_corpus <- tm_map(data_corpus, content_transformer(removeNumbers))
data_corpus <- tm_map(data_corpus, content_transformer(removePunctuation))
data_corpus <- tm_map(data_corpus, content_transformer(tolower))
data_corpus <- tm_map(data_corpus, removeWords, stopwords("en"))
data_corpus <- tm_map(data_corpus, stemDocument)
#data_corpus <- tm_map(data_corpus, PlainTextDocument)

# Term document matrix creation and building the 2,3,4 data grams
trmdocmxgram2 <- TermDocumentMatrix(data_corpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))))
trmdocmxgram3 <- TermDocumentMatrix(data_corpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))))
trmdocmxgram4 <- TermDocumentMatrix(data_corpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))))

# aggregation of the 2,3,4 grams
freq2gram <- slam::row_sums(trmdocmxgram2, na.rm = TRUE)
freq3gram <- slam::row_sums(trmdocmxgram3, na.rm = TRUE)
freq4gram <- slam::row_sums(trmdocmxgram4, na.rm = TRUE)

#sorting the frequency grams
freq2 <- sort(freq2gram, decreasing = TRUE)
freq3 <- sort(freq3gram, decreasing = TRUE)
freq4 <- sort(freq4gram, decreasing = TRUE)

# converting into a data table
wordfreq2 <- data.table(terms=names(freq2), freq=freq2)
wordfreq3 <- data.table(terms=names(freq3), freq=freq3)
wordfreq4 <- data.table(terms=names(freq4), freq=freq4)

write.table(wordfreq2, file = "freq2file", col.names = TRUE)
write.table(wordfreq3, file = "freq3file", col.names = TRUE)
write.table(wordfreq4, file = "freq4file", col.names = TRUE)

freq2 <- read.table(file = "freq2file", header = TRUE, stringsAsFactors = FALSE)
freq3 <- read.table(file = "freq3file", header = TRUE, stringsAsFactors = FALSE)
freq4 <- read.table(file = "freq4file", header = TRUE, stringsAsFactors = FALSE)

#save the tables to files 
save(freq2, file = "freq2.RData")
save(freq3, file = "freq3.RData")
save(freq4, file = "freq4.RData")
