---
title: "Exploratory Analysis of Text Prediction Data"
author: "Charlie LeCrone"
date: "December 31, 2017"
output:
  html_document:
    fig_caption: yes
    fig_height: 6
    fig_width: 8
    keep_md: yes
  word_document: default
---



## Synopsis

This document will detail the exploratory analysis I've done with the text data I am using to build a predictive text model.  I will provide some basic statistics on the large data set provided.  Then I will provide some estimated frequencies and distributions of the words / tokens and phrases / n-grams projected from a random sample of the larger data set.  Finally, I'll provide a summary of my take on the data and how I plan to proceed in developing a text prediction algorithm. 



## Data Overview

The data set includes three large text files.

The en_us.blogs.txt file is a collection of text from blogs.  It includes:

- 899,288 documents
- 37,334,131 words
- 41.52 words per document

The en_us.news.txt file is a collection of text from news outlets.  It includes:

- 77,259 documents
- 2,643,969 words
- 34.22 words per document

The en_us.tweets file is a collection of text from tweets.  It includes:

- 2,360,148 documents
- 30,373,543 words
- 12.87 words per document

I read all three of these files into a single data frame, then used the quanteda package to create a single large corpus of all the text data.  The large corpus contains:

- 3,336,695 documents
- 738,533 unique words

738,533 unique words comprise a total language of 70,351,643 words.  This indicates that there are a small set of words that are used frequently.  This information should be useful as I begin to develop a model.

## Frequency Analysis



To conserve system resources, used the quanteda corpus_sample() function to randomly sample 5% of the larger corpus.  With this sample I analyzed the frequency distribution of 1, 2, and 3 grams to get an idea of how many n-grams I would need to cover the majority of the language in the corpus.

### Unigrams

The plot below shows the top 20 unigrams:

![](Columns1.png)


This next plot shows what percentage of unique unigrams I need to cover 50 and 90 percent of the total unigrams in the sample.  We can see that we only need 0.11% of the unique unigrams to cover 50$ of the language and only 6.1% to cover 90%.

![](plot1.png)

### Bi-grams

The plot below shows the top 20 bigrams:

![](Columns2.png)

Now we will plot what percentage of unique bigrams I need to cover 50 and 90 percent of the total bigrams in the sample.  With bigrams we need 2.71% of the unique bigrams to cover 50% of the language and 70.95% to cover 90%.

![](plot2.png)

### Trigrams

This plot shows the top 20 trigrams:

![](Columns3.png)

With trigrams, we see that there is a much higher percentage of low frequency occurances.  It takes 32.72% of the unique trigrams to cover 50% of the total and 86.54% of the uniques to cover 90%.  

![](plot3.png)

## Next Steps

To me, the object of this project is to provide a utility that will aid the user by filling in the most common parts of the language as they are typing on their mobile device.  This means that the app should be as proficient as possible at providing support for common phrases and not worrry too much about those less frequently used.  It seems that as n gets larger the coverage of the most frequent n-grams gets smaller.  I plan to explore the appropriate frequency cut-offs for 3, 4, and 5 grams to get the most common phrases predicted accurately.

### Model and Application Development

As I move forward with developing a text prediciton model, I will be exploring the following areas further:

#### Stop Words
So far, I have left the stop words in the corpus as I think they need to be included for accurate prediction.

#### n-grams
I will be exploring adding 4, 5, and 6 grams into the model

#### Coverage
To manage the size of the data behind the algorithm, I will look at how many of the infrequent n-grams I can drop from the data set.  I plan treat these improbable n-grams with the same smoothing procedure I'll use to account for terms not in the corpus.

#### Source data
It seems that the blog data is the most conversational of all the data sets.  It might make sense to oversample or only use this data set vs. the news and twitter data.

#### Shiny App
I am not an HTML expert.  I plan to adapt the shiny app that I created for the data products course for this project.  My main focus will be on making the model as accurate and efficient as possible.  If I have extra time after that is accomplished, I'll spend some time trying to make it look pretty as well.

#### Code
All of the code to perform my analysis and generate this document can be found at [this](https://github.com/lecroc/CapstoneExploratory) github repo.





