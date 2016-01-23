load(file = "DFM.EDA.summ.RData")
library(hashr, quietly = T)
library(tidyr, quietly = T)
library(dplyr, quietly = T)

#split bi-gram
dfm.2.summ <- dfm.2.summ %>% separate(word, c("input", "predicted"), sep = " ", remove = F)

## join w/ unigram on input, get unigram freq
dfm.2.summ <- merge(dfm.2.summ, dfm.1.summ[ ,c("freq", "word")], by.x = "input", by.y = "word", all.x = TRUE)

# calculate conditional probabilty of "predicted given input"
dfm.2.summ <- mutate(dfm.2.summ, mle = freq.x / freq.y)
dfm.2.summ <- rename(dfm.2.summ, freq.input = freq.y)
dfm.2.summ <- rename(dfm.2.summ, freq.bigram = freq.x)


## do the same for tri and quad...

dfm.3.summ <- dfm.3.summ %>% separate(word, c("input1", "input2", "predicted"), sep = " ", remove = F)
dfm.3.summ <- mutate(dfm.3.summ, input = paste(input1, input2))
dfm.3.summ <- merge(dfm.3.summ, dfm.2.summ[ ,c("freq.x", "word")], by.x = "input", by.y = "word", all.x = TRUE)
dfm.3.summ <- mutate(dfm.3.summ, mle = freq / freq.x)
dfm.3.summ <- rename(dfm.3.summ, freq.input = freq.x)
dfm.3.summ <- rename(dfm.3.summ, freq.trigram = freq)
dfm.3.summ <- dfm.3.summ[ ,c("input", "freq.trigram","freq.input", "word", "predicted", "pct.of.total", "mle")]



dfm.4.summ <- dfm.4.summ %>% separate(word, c("input1", "input2","input3", "predicted"), sep = " ", remove = F)
dfm.4.summ <- mutate(dfm.4.summ, input = paste(input1, input2, input3))
dfm.4.summ <- merge(dfm.4.summ, dfm.3.summ[ ,c("freq.trigram", "word")], by.x = "input", by.y = "word", all.x = TRUE)
dfm.4.summ <- mutate(dfm.4.summ, mle = freq / freq.trigram)
dfm.4.summ <- rename(dfm.4.summ, freq.input = freq.trigram)
dfm.4.summ <- rename(dfm.4.summ, freq.quadgram = freq)
dfm.4.summ <- dfm.4.summ[ ,c("input", "freq.quadgram","freq.input", "word", "predicted", "pct.of.total", "mle")]

save(list = c("dfm.1.summ", "dfm.2.summ", "dfm.3.summ", "dfm.4.summ"), file = "DFM.Mod.summ.RData")

#add hashed input, select only necessary columns, add rank?

head(dfm.1.pred)

dfm.1.pred <- dfm.1.summ %>% mutate(rn = row_number(desc(pct.of.total)))
dfm.1.pred <- select(dfm.1.pred, predicted = word, mle = pct.of.total, rn)

dfm.2.pred <- transmute(dfm.2.summ
                        , input = hash(input)
                        , predicted = predicted
                        , mle = mle)

dfm.2.pred <- dfm.2.pred %>% group_by(input) %>% mutate(rn = row_number(desc(mle)))

dfm.3.pred <- transmute(dfm.3.summ
                        , input = hash(input)
                        , predicted = predicted
                        , mle = mle)

dfm.3.pred <- dfm.3.pred %>% group_by(input) %>% mutate(rn = row_number(desc(mle)))

dfm.4.pred <- transmute(dfm.4.summ
                        , input = hash(input)
                        , predicted = predicted
                        , mle = mle)

dfm.4.pred <- dfm.4.pred %>% group_by(input) %>% mutate(rn = row_number(desc(mle)))

save(list = c("dfm.1.pred", "dfm.2.pred", "dfm.3.pred", "dfm.4.pred"), file = "DFM.Pred.RData")
