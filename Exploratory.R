# Exploratory

# libraries

library(quanteda)
library(ngram)
library(dplyr)
library(ggplot2)

# Check if data file exists, if not, create

ifelse(!file.exists("AllText.Rda"),
       {
        pblog<-"C:/Coursera/Capstone/final/en_US.blogs.txt"
        pnews<-"C:/Coursera/Capstone/final/en_US.news.txt"
        ptweet<-"C:/Coursera/Capstone/final/en_US.twitter.txt"

        # load text into data frames, combine into one df

        blogs<-as.data.frame(readLines(pblog, encoding="UTF-8"))
        names(blogs)<-c("Text")
        blogs$Text<-as.character(blogs$Text)
        blogs$Source<-c("Blogs")
        blogwords<-wordcount(blogs$Text)
        blogdocs<-nrow(blogs)

        news<-as.data.frame(readLines(pnews, encoding="UTF-8"))
        names(news)<-c("Text")
        news$Text<-as.character(news$Text)
        news$Source<-c("News")
        newswords<-wordcount(news$Text)
        newsdocs<-nrow(news)
        
        tweets<-as.data.frame(readLines(ptweet, encoding="UTF-8"))
        names(tweets)<-c("Text")
        tweets$Text<-as.character(tweets$Text)
        tweets$Source<-c("Tweets")
        tweetwords<-wordcount(tweets$Text)
        tweetdocs<-nrow(tweets)

        AllText<-as.data.frame(rbind(blogs,tweets,news))
        
        # store word and document counts for each text type in counts df
        
        counts<-as.data.frame(cbind(tweetwords, tweetdocs, newswords, newsdocs, blogwords, blogdocs))
        names(counts)<-c("tweetwords", "tweetdocs", "newswords", "newsdocs", "blogwords", "blogdocs")
        counts$tweetwordsperdoc<-counts$tweetwords/counts$tweetdocs
        counts$newswordsperdoc<-counts$newswords/counts$newsdocs
        counts$blogwordsperdoc<-counts$blogwords/counts$blogdocs
        
        # Save dfs just created to disk so we won't need to make them again
        
        save(AllText, file = "AllText.Rda")
        
        save(counts, file = "counts.Rda")
        
        # remove some data objects to manage memory
        
        rm(blogdocs, blogs, blogwords, counts, news, newsdocs, newswords, pblog, pnews, ptweet, tweetdocs, tweets, tweetwords)
        gc(T)
        
        },

load(file = "AllText.Rda"))

load(file = "counts.Rda")

# Create corpus of all text data

TxtCorpAll <- corpus(AllText$Text)

# add document variable for text source (tweet, blog, or news)

docvars(TxtCorpAll, "Source")<-AllText$Source

# store a count of total documents in big corpus

corpdocs<-ndoc(TxtCorpAll)

# store a count of total features (unique words) in big corpus

corpfeatures<-nfeature(dfm(TxtCorpAll))

# remove the big data frame from memory

rm(AllText)

# recover some system memory

gc(T)

# Sample big corpus for analysis

set.seed(123)

sampsize<-corpdocs*.05

TxtCorp<-corpus_sample(TxtCorpAll, sampsize)

# drop big corpus from memory

rm(TxtCorpAll)

gc(T)

# Create document feature matrix (dfm) of uni-grams

Mydfm1 <- dfm(TxtCorp, tolower = T, stem = F, remove_punct = T, verbose=F,
         remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 1)

# Display top features evaluate frequencies

TF1<-topfeatures(Mydfm1, n=50)

tbl1<-textstat_frequency(Mydfm1)

tbl1$cumfreq<-cumsum(tbl1$frequency)

tbl1$pcttoken<-tbl1$cumfreq/sum(tbl1$frequency)

tbl1$ndx<-seq.int(nrow(tbl1))

tbl1$pctdoc<-tbl1$ndx/max(as.numeric(tbl1$ndx))

# Create dfm of bi-grams

Mydfm2 <- dfm(TxtCorp, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 2)

# Display top features evaluate frequencies of bi-grams

TF2<-topfeatures(Mydfm2, n=50)

tbl2<-textstat_frequency(Mydfm2)

tbl2$cumfreq<-cumsum(tbl2$frequency)

tbl2$pcttoken<-tbl2$cumfreq/sum(tbl2$frequency)

tbl2$ndx<-seq.int(nrow(tbl2))

tbl2$pctdoc<-tbl2$ndx/max(as.numeric(tbl2$ndx))

View(tbl2)

# Create dfm of tri-grams

Mydfm3 <- dfm(TxtCorp, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 3)

# Display top features evaluate frequencies of tri-grams

TF3<-topfeatures(Mydfm3, n=50)

TF3

tbl3<-textstat_frequency(Mydfm3)

tbl3$cumfreq<-cumsum(tbl3$frequency)

tbl3$pcttoken<-tbl3$cumfreq/sum(tbl3$frequency)

tbl3$ndx<-seq.int(nrow(tbl3))

tbl3$pctdoc<-tbl3$ndx/max(as.numeric(tbl3$ndx))

View(tbl3)

# Remove the dfms to conserve memory

rm(Mydfm1, Mydfm2, Mydfm3, TxtCorp)

gc(T)

# Make some plots

plot1<-ggplot(tbl1, aes(x=pctdoc, y=pcttoken))+geom_line(col="blue")+
  labs(x="Percentage of Unique Unigrams", y="Percentage of Total Unigrams", title="Distribution of Unigrams")

plot1

plot2<-ggplot(tbl2, aes(x=pctdoc, y=pcttoken))+geom_line(col="blue")+
  labs(x="Percentage of Unique Bigrams", y="Percentage of Total Bigrams", title="Distribution of Bigrams")

plot2

plot3<-ggplot(tbl3, aes(x=pctdoc, y=pcttoken))+geom_line(col="blue")+
  labs(x="Percentage of Unique Trigrams", y="Percentage of Total Trigrams", title="Distribution of Trigrams")

plot3
