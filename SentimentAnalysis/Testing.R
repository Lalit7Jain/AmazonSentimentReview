library(dplyr)
library(tidytext)

myCorpus <- Corpus(VectorSource(myCorpus)) #converts the relevant part of your file into a corpus

myCorpus = tm_map(myCorpus, PlainTextDocument) # an intermediate preprocessing step

myCorpus = tm_map(myCorpus, tolower) # converts all text to lower case

myCorpus = tm_map(myCorpus, removePunctuation) #removes punctuation

myCorpus = tm_map(myCorpus, removeWords, stopwords("english")) #removes common words like "a", "the" etc

myCorpus = tm_map(myCorpus, stemDocument) # removes the last few letters of similar words such as get, getting, gets

myCorpus = tm_map(myCorpus, removeNumbers)

dtm = DocumentTermMatrix(myCorpus) #turns the corpus into a document term matrix


amazonreview_td <- tidy(dtm)

amazonreview_sentiments <- amazonreview_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

amazonreview_sentiments

### We can find the most negative documents:
library(tidyr)

amazonreview_sentiments %>%
  count(document, sentiment, wt = count) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)

## Or visualize which words contributed to positive and negative sentiment:
library(ggplot2)

amazonreview_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 2) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#### 

reviews.sent <- dfm(myCorpus)
reviews.sent[,1:5]




# loading packages
#library(ROAuth)
library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)


conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

reviews.read <- read.csv("reviews/ReviewSentiment.csv")  %>%
  # converting some symbols
  dmap_at('Review', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(label == "Positive", 1, 0)) %>% select(Review,sentiment)



# data splitting on train and test
set.seed(2322)
smp_size <- floor(0.80 * nrow(reviews.read))

train_ind <- sample(seq_len(nrow(reviews.read)), size = smp_size)
amazon_train <- reviews.read[train_ind,]
amazon_test <- reviews.read[-train_ind,]





##### doc2vec #####
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer


it_train <- itoken(amazon_train$Review, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   progressbar = TRUE)

it_test <- itoken(amazon_test$Review,
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  progressbar = TRUE)



# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)

# define tf-idf model
tfidf <- TfIdf$new()

# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)


# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = amazon_train[['sentiment']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))


plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))


preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
glmnet:::auc(as.numeric(amazon_test$sentiment), preds)
