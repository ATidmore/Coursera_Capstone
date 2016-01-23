#require(tm)
#require(RWeka)
require(quanteda, quietly = TRUE)
require(stringi, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
set.seed(2016)


setwd("~/Training/CourseraDataScience/CAPSTONE/texts")

## 1. Load files

con <- file("en_US.news.txt", "rb")
news <- readLines(con)
close(con)

twtr_con <- file("en_US.twitter.txt", encoding = "UTF-8")
twtr <- readLines(twtr_con)
close(twtr_con)

blog_con <- file("en_US.blogs.txt", encoding = "UTF-8")
blog <- readLines(twtr_con)
close(blog_con)

text.summary <- do.call(rbind, lapply(list(twtr,news,blog), 
                      function(x) c(objectSize.Mb=c(round(object.size(x)/1024/1024,2))
                                ,length=length(x)
                                ,Longestline=max(nchar(x))
                                )
                        )
                )

text.summary <- data.frame(text.summary)
rownames(text.summary) <- c("twitter", "news", "blogs")

setwd("~/Training/CourseraDataScience/CAPSTONE")
save(text.summary, file = "text.summary.RData")

# #Quiz 1 Q4: What is the percentage of the number of lines with "love" vs. those with "hate"
# length(grep("love", twtr)) / length(grep("hate", twtr)) #4.1086
# 
# #Quiz 1 Q5: There is one tweet that has the word "biostats" in it. Says what?
# twtr[grep("biostats", twtr)] #"i know how you feel.. i have biostats on tuesday and i have yet to study =/"
# # answer: They haven't studied for their biostats exam
# 
# #Q6: How many times does the exact phrase "A computer once beat me at chess, but it was no match for me at kickboxing" appear in the Twitter text?
# length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", twtr))
# #answer: 3

# create sample, light cleansing of data
twtr_ss <- round(length(twtr)*.05,0)
twtr_sample <- quanteda::sample(twtr, size=twtr_ss, replace=FALSE) 
twtr_sample <- gsub("#", " ", twtr_sample)

news_ss <- round(length(news)*0.15,0)
news_sample <- quanteda::sample(news, size=news_ss, replace=FALSE)
news_sample <- gsub("[^[:alnum:]///' ]", "", news_sample)

blog_ss <- round(length(blog)*0.10,0)
blog_sample <- quanteda::sample(blog, size=blog_ss, replace=FALSE)
blog_sample <- gsub("[^[:alnum:]///' ]", "", blog_sample)


#create Corpus
twtr_corpus <- quanteda::corpus(twtr_sample)
blog_corpus <- quanteda::corpus(blog_sample)
news_corpus <- quanteda::corpus(news_sample)

quanteda::docvars(twtr_corpus, "Source") <- "Twitter"
quanteda::docvars(blog_corpus, "Source") <- "Blog"
quanteda::docvars(news_corpus, "Source") <- "News"

myCorpus <- twtr_corpus + blog_corpus + news_corpus

setwd("~/Training/CourseraDataScience/CAPSTONE")
save(myCorpus, file = "myCorpus.RData")
#load(file = "myCorpus.RData")

#Writes summary to a DF for easy reference
myCorpus.summ <- data.frame(summary(myCorpus, n = twtr_ss + news_ss + blog_ss, verbose = FALSE))

#Clean up
rm(twtr, news, blog, twtr_corpus, twtr_sample, blog_corpus, blog_sample, news_corpus, news_sample)

gc() #garbage collector

## 1. Counts of Corpus (words, sentences, types)
myCorpus.summ.out <- summarize(group_by(myCorpus.summ, Source), 
                               tot_types = sum(Types),
                               tot_tokens = sum(Tokens),
                               tot_sentences = sum(Sentences),
                               tot_docs = n()
)
myCorpus.summ.out


## 2. Create DFMs and Summaries

dfm.1 <- quanteda::dfm(myCorpus, ngrams = 1)
dfm.1.summ <- data.frame(docfreq(dfm.1))
names(dfm.1.summ) <- "freq"
dfm.1.summ$word <- rownames(dfm.1.summ)
dfm.1.summ <- arrange(dfm.1.summ, desc(freq))

dfm.2 <- quanteda::dfm(myCorpus, ngrams = 2, concat = " ")
dfm.2.summ <- data.frame(docfreq(dfm.2))
names(dfm.2.summ) <- "freq"
dfm.2.summ$word <- rownames(dfm.2.summ)
dfm.2.summ <- arrange(dfm.2.summ, desc(freq))

dfm.3 <- quanteda::dfm(myCorpus, ngrams = 3, concat = " ")
dfm.3.summ <- data.frame(docfreq(dfm.3))
names(dfm.3.summ) <- "freq"
dfm.3.summ$word <- rownames(dfm.3.summ)
dfm.3.summ <- arrange(dfm.3.summ, desc(freq))

dfm.4 <- quanteda::dfm(myCorpus, ngrams = 4, concat = " ")
dfm.4.summ <- data.frame(docfreq(dfm.4))
names(dfm.4.summ) <- "freq"
dfm.4.summ$word <- rownames(dfm.4.summ)
dfm.4.summ <- arrange(dfm.4.summ, desc(freq))

## 3. Create Pct of Total, removing frequencies of "1"

dfm.1.summ <- dfm.1.summ[which(dfm.1.summ$freq>1) ,]
dfm.1.summ <- mutate(dfm.1.summ
        , word = tolower(word)
        , pct.of.total = freq / sum(freq)
       #,cumu.pot = cumsum(pct.of.total)
       )

dfm.2.summ <- dfm.2.summ[which(dfm.2.summ$freq>1) ,]
dfm.2.summ <- mutate(dfm.2.summ
                     , word = tolower(word)
                     , pct.of.total = freq / sum(freq)
                    #,cumu.pot = cumsum(pct.of.total)
)

dfm.3.summ <- dfm.3.summ[which(dfm.3.summ$freq>1) ,]
dfm.3.summ <- mutate(dfm.3.summ
                     , word = tolower(word)
                     , pct.of.total = freq / sum(freq)
                     #,cumu.pot = cumsum(pct.of.total)
)

dfm.4.summ <- dfm.4.summ[which(dfm.4.summ$freq>1) ,]
dfm.4.summ <- mutate(dfm.4.summ
                     , word = tolower(word)
                     , pct.of.total = freq / sum(freq)
                     #,cumu.pot = cumsum(pct.of.total)
)


#load(file = "EDA.RData")

save(c("myCorpus.summ","myCorpus.summ.out"), file = "myCorpus.summ.out.RData")

save(list = c("dfm.1.summ", "dfm.2.summ", "dfm.3.summ", "dfm.4.summ"), file = "DFM.EDA.summ.RData")



rm(dfm.1, dfm.2, dfm.3, dfm.4, myCorpus)

gc()
