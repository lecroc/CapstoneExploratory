# Exploratory

# some usefull functions

sizeme<-function(x) {
  print(object.size(x), units="auto")
}

# libraries

if("dplyr" %in% rownames(installed.packages()) == FALSE)
{install.packages("dplyr")}

if("quanteda" %in% rownames(installed.packages()) == FALSE)
{install.packages("quanteda")}

if("ngram" %in% rownames(installed.packages()) == FALSE)
{install.packages("ngram")}

if("ggplot2" %in% rownames(installed.packages()) == FALSE)
{install.packages("ggplot2")}

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

save(corpdocs, file="corpdocs.Rda")

# store a count of total features (unique words) in big corpus

corpfeatures<-nfeature(dfm(TxtCorpAll))

save(corpfeatures, file="corpfeatures.Rda")

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

# Create dfm of tri-grams

Mydfm3 <- dfm(TxtCorp, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 3)

# Display top features evaluate frequencies of tri-grams

TF3<-topfeatures(Mydfm3, n=50)

tbl3<-textstat_frequency(Mydfm3)

tbl3$cumfreq<-cumsum(tbl3$frequency)

tbl3$pcttoken<-tbl3$cumfreq/sum(tbl3$frequency)

tbl3$ndx<-seq.int(nrow(tbl3))

tbl3$pctdoc<-tbl3$ndx/max(as.numeric(tbl3$ndx))

# Remove the dfms to conserve memory

rm(Mydfm1, Mydfm2, Mydfm3, TxtCorp)

gc(T)

save(tbl1, file="tbl1.Rda")
save(tbl2, file="tbl2.Rda")
save(tbl3, file="tbl3.Rda")

# Make some plots

# define intercepts where we reach 50 and 90 percent of total n-grams

y1a<-tbl1 %>%
  filter(pcttoken>=.5)
yint1a<-min(y1a$pctdoc)

y1b<-tbl1 %>%
  filter(pcttoken>=.9)
yint1b<-min(y1b$pctdoc)

y2a<-tbl2 %>%
  filter(pcttoken>=.5)
yint2a<-min(y2a$pctdoc)

y2b<-tbl2 %>%
  filter(pcttoken>=.9)
yint2b<-min(y2b$pctdoc)

y3a<-tbl3 %>%
  filter(pcttoken>=.5)
yint3a<-min(y3a$pctdoc)

y3b<-tbl3 %>%
  filter(pcttoken>=.9)
yint3b<-min(y3b$pctdoc)

rm(y1a, y1b, y2a, y2b, y3a, y3b)

gc(T)

# Plot n-gram frequencies vs. total n-grams

plot1<-ggplot(tbl1, aes(x=pctdoc, y=pcttoken))+geom_line(col="blue", size=1.75)+
  labs(x="Percentage of Unique Unigrams", y="Percentage of Total Unigrams", title="Distribution of Unigrams")+
  geom_hline(yintercept=c(.5, .9), col="red")+geom_vline(xintercept=c(yint1a, yint1b), col="red")

plot1

dev.copy(png, "plot1.png")
dev.off()

plot2<-ggplot(tbl2, aes(x=pctdoc, y=pcttoken))+geom_line(col="blue", size=1.75)+
  labs(x="Percentage of Unique Bigrams", y="Percentage of Total Bigrams", title="Distribution of Bigrams")+
  geom_hline(yintercept=c(.5, .9), col="red")+geom_vline(xintercept=c(yint2a, yint2b), col="red")

plot2

dev.copy(png, "plot2.png")
dev.off()

plot3<-ggplot(tbl3, aes(x=pctdoc, y=pcttoken))+geom_line(col="blue", size=1.75)+
  labs(x="Percentage of Unique Trigrams", y="Percentage of Total Trigrams", title="Distribution of Trigrams")+
  geom_hline(yintercept=c(.5, .9), col="red")+geom_vline(xintercept=c(yint3a, yint3b), col="red")

plot3

dev.copy(png, "plot3.png")
dev.off()

rm(plot1, plot2, plot3)

gc()

# Plot the frequencies of the 20 most common n-grams

# Create some tables to plot with

p1<-tbl1[1:20,]
p1<-arrange(p1, desc(frequency))

p2<-tbl2[1:20,]
p2<-arrange(p2, desc(frequency))

p3<-tbl3[1:20,]
p3<-arrange(p3, desc(frequency))

pl1<-ggplot(aes(x=feature, y=frequency),data=p1)+geom_bar(fill="blue", stat="identity")+
  labs(x="Unigram", y="Frequency", title="Top 20 Unigrams")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  scale_x_discrete(limits=p1$feature)

pl1

dev.copy(png, "Columns1.png")
dev.off()

pl2<-ggplot(aes(x=feature, y=frequency),data=p2)+geom_bar(fill="blue", stat="identity")+
  labs(x="Bigram", y="Frequency", title="Top 20 Bigrams")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  scale_x_discrete(limits=p2$feature)

pl2

dev.copy(png, "Columns2.png")
dev.off()

pl3<-ggplot(aes(x=feature, y=frequency),data=p3)+geom_bar(fill="blue", stat="identity")+
  labs(x="Trigram", y="Frequency", title="Top 20 Trigrams")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  scale_x_discrete(limits=p3$feature)

pl3

dev.copy(png, "Columns3.png")
dev.off()
