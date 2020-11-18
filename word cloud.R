library(tm)
library(RColorBrewer)
library(wordcloud2)

setwd("C:/Users/Shashank/Desktop/project")
getwd()
readLines("calories.txt")
text_ <- readLines("calories.txt")

# Creating a corpus
docs_ <- Corpus(VectorSource(text_))

# Data cleaning
docs_ <- docs_ %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_ <- tm_map(docs_, content_transformer(tolower))
docs_ <- tm_map(docs_,removeWords,c(stopwords("english"),"calorie","can","people","may"))

# Creating a document terms matrix
dtm <- TermDocumentMatrix(docs_)
mat <- as.matrix(dtm)
words <- sort(rowSums(mat))
df <- data.frame(word = names(words), freq = words)
df <- df[order(df$freq, decreasing=T),]
head(df)

# Creating word cloud
wordcloud2(data = df,color = "random-dark",backgroundColor = "black")