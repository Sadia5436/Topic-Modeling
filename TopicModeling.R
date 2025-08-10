install.packages("hunspell")
install.packages("dplyr")


library(dplyr)
library(stringr)
library(tidytext)

library(SnowballC)
library(hunspell)
library(dplyr)

contractions <- c(
  "can't" = "cannot",
  "won't" = "will not",
  "don't" = "do not",
  "didn't" = "did not",
  "isn't" = "is not",
  "aren't" = "are not",
  "wasn't" = "was not",
  "weren't" = "were not",
  "hasn't" = "has not",
  "haven't" = "have not",
  "he's" = "he is",
  "she's" = "she is",
  "it’s" = "it is",
  "they’re" = "they are",
  "you’re" = "you are",
  "i'm" = "i am",
  "we're" = "we are",
  "that's" = "that is",
  "what's" = "what is",
  "who's" = "who is",
  "you’ll"="you will"
)



data <- read.csv("C:/Users/Ahmed/Downloads/data.csv", stringsAsFactors = FALSE)
data




data <- data %>%
  filter(!is.na(name) & name != "") %>%  
  mutate(
    
    name = str_to_lower(name),
    
    name = str_replace_all(name, contractions),
    
    name = str_remove_all(name, "[[:punct:]]"),
    
    name = str_squish(name),
    
    name = str_remove_all(name, "[^a-z\\s]"),
    
    name = str_remove_all(name, "[0-9]"),
    
    name = str_remove_all(name, "<[^>]+>"),
    
    name = str_remove_all(name, "[\U00010000-\U0010ffff]")
    
  )

data

data_tokens <- data %>%
  unnest_tokens(word, name)  
data_tokens


data_tokens <- data_tokens %>%
  anti_join(stop_words)  
data_tokens




correct_spelling <- function(word) {
  suggestions <- hunspell_suggest(word)
  if (length(suggestions) > 0 && length(suggestions[[1]]) > 0) {
    return(suggestions[[1]][1])  
  } else {
    return(word)  
  }
}


data_tokens <- data_tokens %>%
  mutate(word = sapply(word, correct_spelling))  

data_tokens


write.csv(data_tokens, "C:/Users/Ahmed/Downloads/preprocessed_dataset.csv", row.names = FALSE)




library(tm)
# Create a corpus
corpus <- Corpus(VectorSource(data_tokens$word))

# Create a DTM
dtm <- DocumentTermMatrix(corpus)




inspect(dtm[1:5, 1:10])


dim(dtm)
#non-zero entiers
sum(dtm > 0)



# Calculate TF-IDF
dtm_tfidf <- weightTfIdf(dtm)
dtm_tfidf
# Inspect the first 5 documents and 10 terms for TF-IDF values
inspect(dtm_tfidf[1:5, 1:10])



# Check for empty documents (rows with all zeros)
empty_docs <- apply(dtm, 1, sum) == 0
sum(empty_docs)  # How many empty documents are there?







install.packages("topicmodels")

library(topicmodels)

# Filter out rows with no non-zero entries
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# Remove empty documents
#dtm_clean <- dtm[!empty_docs, ]

lda_model <- LDA(dtm, k = 5)

# View the topics
print(lda_model)

#Examine the Topics


# Display the top 10 terms for each of the 3 topics
terms(lda_model, 10)


# Get the topic proportions for each document
topic_proportions <- posterior(lda_model)$topics
head(topic_proportions)




# Get the most probable words for each topic
top_terms <- terms(lda_model, 10)
top_terms




# Get the topic proportions for each document
doc_topics <- posterior(lda_model)$topics
head(doc_topics)







# Install and load the necessary visualization libraries
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)



# Get the top terms for each topic
top_terms <- terms(lda_model, 10)  # Top 10 terms for each topic

top_terms_df <- as.data.frame(top_terms) %>%
  rownames_to_column(var = "topic") %>%
  gather(key = "rank", value = "term", -topic) %>%
  mutate(rank = as.numeric(gsub("[^0-9]", "", rank)))  # Removing non-numeric characters

# Check cleaned data
head(top_terms_df)


top_terms_df <- top_terms_df %>% filter(!is.na(rank))

summary(top_terms_df)
head(top_terms_df)


ggplot(top_terms_df, aes(x = reorder(term, rank), y = rank, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Terms for Each Topic", x = "Term", y = "Rank") +
  theme_minimal()
















