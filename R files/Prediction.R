setwd("C:/Users/AustinT/Documents/Training/CourseraDataScience/CAPSTONE/")
load(file = "DFM.Pred.RData")

library(dplyr, quietly = T)
library(stringr, quietly = T)
library(hashr, quietly = T)


f.predict.uni <- function() {
        uni.results <<- mutate(Ngram = "1gram Results", dfm.1.pred[which(dfm.1.pred$rn <= 5) ,c("predicted","mle")])   
}

f.predict.bi <- function(input) {
        bi.results <<- mutate(Ngram = "2gram Results", dfm.2.pred[which(dfm.2.pred$input == hash(input) & dfm.2.pred$rn <= 5) ,c("predicted","mle")])  
}

f.predict.tri <- function(input) {
        tri.results <<- mutate(Ngram = "3gram Results", dfm.3.pred[which(dfm.3.pred$input == hash(input) & dfm.3.pred$rn <= 5) ,c("predicted","mle")])  
}

f.predict.quad <- function(input) {
        quad.results <<- mutate(Ngram = "4gram Results", dfm.4.pred[which(dfm.4.pred$input == hash(input) & dfm.4.pred$rn <= 5) ,c("predicted","mle")]) 
}

f.predict <- function(input){
        
        #cleanse input
                input <- stringr::str_trim(input)
                input <- gsub("[^[:alnum:]///' ]", "", input)
                input <- tolower(input)
      
        #calculate input length
              
                input.last3 <- stringr::word(input, start = -3, end = -1)
                input.last2 <- stringr::word(input, start = -2, end = -1)
                input.last1 <- stringr::word(input, start = -1, end = -1)
     
        #based on length of subset of input, call the function to return the predicted values
        if (!is.na(input.last3)) {
                f.predict.quad(input.last3)
                f.predict.tri(input.last2)
                f.predict.bi(input.last1)
                f.predict.uni()
                
                results.app <<- rbind(quad.results[ ,c("Ngram", "predicted", "mle")]
                                      , tri.results[ ,c("Ngram", "predicted", "mle")]
                                      , bi.results[ ,c("Ngram", "predicted", "mle")]
                                      , uni.results[ ,c("Ngram", "predicted", "mle")])
                
                results.app <<- results.app %>% arrange(desc(mle))
                
        } else  if (!is.na(input.last2)) {
                
                f.predict.tri(input.last2)
                f.predict.bi(input.last1)
                f.predict.uni()
                results.app <<- rbind(tri.results[ ,c("Ngram", "predicted", "mle")]
                                      , bi.results[ ,c("Ngram", "predicted", "mle")]
                                      , uni.results[ ,c("Ngram", "predicted", "mle")])
                
                results.app <<- results.app %>% arrange(desc(mle))
                
        } else if (!is.na(input.last1)) {
                
                f.predict.bi(input.last1)
                f.predict.uni()
                results.app <<- rbind(bi.results[ ,c("Ngram", "predicted", "mle")]
                                      , uni.results[ ,c("Ngram", "predicted", "mle")])
                
                results.app <<- results.app %>% arrange(desc(mle))
                
        } else {
                
                f.predict.uni()
                results.app <<- uni.results[ ,c("Ngram", "predicted", "mle")]
                
                results.app <<- results.app %>% arrange(desc(mle))
        }
                


}


save(list = c("f.predict","f.predict.uni","f.predict.bi", "f.predict.tri","f.predict.quad"), file = "Prediction.RData")



