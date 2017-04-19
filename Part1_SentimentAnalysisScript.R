

library(dplyr)
library(rvest)

## Loading the page to get the review page
#url <- "https://www.amazon.com/All-New-Amazon-Echo-Dot-Add-Alexa-To-Any-Room/dp/B01DFKC2SO/ref=redir_mobile_desktop?_encoding=UTF8&ref_=ods_gw_ha_d_3pack"
url <- "https://www.amazon.com/Amazon-Echo-Bluetooth-Speaker-with-WiFi-Alexa/dp/B00X4WHP5E/ref=s9u_simh_gw_i2?_encoding=UTF8&fpl=fresh&pd_rd_i=B00X4WHP5E&pd_rd_r=F5K20DTJEQHV3WG9XZP0&pd_rd_w=piZZ2&pd_rd_wg=kS3ou&pf_rd_m=ATVPDKIKX0DER&pf_rd_s=&pf_rd_r=W6VQZ4FB0Q96E1NGPKVR&pf_rd_t=36701&pf_rd_p=781f4767-b4d4-466b-8c26-2639359664eb&pf_rd_i=desktop"

main.page <- read_html(x = url)
review.page <- main.page %>% html_nodes(xpath = '//*[@id="revF"]/div/a') %>% xml_attr("href")
if(length(review.page) == 0) {
  review.page <- main.page %>% html_nodes(xpath = '//*[@id="reviews-medley-footer"]/div[1]/a') %>% xml_attr("href")
  review.page <- paste("https://www.amazon.com",review.page,sep="")
}


## Scraping the review page and writing it to the Review folder

XPATH_REVIEW_SECTION_2 = '//div[@data-hook="review"]'
XPATH_REVIEW_SECTION_3 = '//span[@data-hook="review-body"]//text()'

page = 1000
p = 1
for (i in 1:page){
  newpage <- paste(review.page,"&pageNumber=",i,sep = "")
  readreviewpage <- read_html(x=newpage)
  reviews <- readreviewpage %>% html_nodes(xpath = XPATH_REVIEW_SECTION_2) %>% 
    html_nodes(xpath = XPATH_REVIEW_SECTION_3) %>% html_text()  
  
    if(length(reviews) > 0){
        for (j in 1:length(reviews)){
          r <- as.character(reviews[j])
          filename <- paste("reviews/Review",p,".txt",sep = "")
          write(r,file = filename)
          p = p+1
        }
    }
}


library("tm")
library("SnowballC")

# # Loading the corpus
#
# reviewcorpus  <-Corpus(DirSource("reviews/"))
# summary(reviewcorpus)
# reviewcorpus <- tm_map(reviewcorpus, removeNumbers)
# reviewcorpus <- tm_map(reviewcorpus, removePunctuation)
# reviewcorpus <- tm_map(reviewcorpus , stripWhitespace)
# reviewcorpus <- tm_map(reviewcorpus, tolower)
# reviewcorpus <- tm_map(reviewcorpus, removeWords, stopwords("english")) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords
# reviewcorpus <- tm_map(reviewcorpus, stemDocument, language = "english")
# adtm <-DocumentTermMatrix(reviewcorpus, control=list(wordLengths=c(3,Inf)))

###


pos <- scan('positive-words.txt', what='character', comment.char=';')
neg <- scan('negative-words.txt', what='character', comment.char=';')
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

## Loading the corpus
require(quanteda)

reviewsInput <- quanteda::textfile("reviews/*.txt")
str(reviewsInput)
myCorpus <- corpus(reviewsInput)

## checking a review from the corpus
texts(myCorpus)[1]

## making a dictionary of positive and negative words
myDict <- dictionary(list(positive = pos.words,negative = neg.words))

## Quanteda will create Document Frequency Matrix by function dfm(). This function
## essentially does this by series of operation including tokenizing, lowercasing, indexing
reviews.sent <- dfm(myCorpus, stem = TRUE, removePunct = TRUE,dictionary = myDict,toLower = TRUE,removeNumbers = TRUE)
reviews.sent

###
require(tidytext)
tidy(reviews.sent)

## Making a dataframe
sentiment.df <- as.data.frame(as.matrix(reviews.sent))
sentiment.df$document <- rownames(sentiment.df)

## Adding all the reviews
sentiment.df$Review <- NULL
for(i in 1:length(texts(myCorpus))){
  sentiment.df[i,"Review"] <- texts(myCorpus)[i]
}

## Removing reviews which does not have positive or negative count
sentiment.df <- sentiment.df[which(sentiment.df$posit != 0 | sentiment.df$negat != 0),]

## labelling the document
library(dplyr)
sentiment.df <- sentiment.df %>% mutate(label = if_else(posit>negat,"Positive","Negative"))


## Visualizing individual reviews sentiment
library(reshape2)
dat_l <- melt(sentiment.df[,1:3], id.vars = c("document"))
library(ggplot2)
p <- ggplot(data = dat_l, aes(x = document, y = value, group = variable, fill = variable))
p <- p + geom_bar(stat = "identity", width = 0.5, position = "dodge")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 90))
png(filename="reviews/ReviewSentiment.png")
plot(p)
dev.off()


## computing and visualizing overall sentiment

overall.sentiment <- data.frame(Positive = sum(sentiment.df$posit)/nrow(sentiment.df),
                                Negative = sum(sentiment.df$negat/nrow(sentiment.df)),
                                Review = "Overall Review")
write.csv(overall.sentiment,"reviews/OverallSentiment.csv")

dat_l2 <- melt(overall.sentiment, id.vars = c("Review"))
p2 <- ggplot(data = dat_l2, aes(x = Review, y = value, group = variable, fill = variable))
p2 <- p2 + geom_bar(stat = "identity", width = 0.5, position = "dodge")
p2 <- p2 + theme_bw()
png(filename="reviews/OverallSentiment.png")
plot(p2)
dev.off()


# Saving the file
sentiment.df <- select(sentiment.df,Review,label)
write.csv(sentiment.df,"reviews/ReviewSentiment.csv")


# Saving the file
sentiment.df_2 <- select(sentiment.df[1:3000,],Review,label)
write.csv(sentiment.df_2,"reviews/ReviewSentiment_2.csv")

