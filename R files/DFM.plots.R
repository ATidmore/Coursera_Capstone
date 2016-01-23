setwd("~/Training/CourseraDataScience/CAPSTONE")
load(file = "DFM.summ.RData")


df1.plot <- ggplot(dfm.1.summ[1:20 ,], aes(x=reorder(word,-pct.of.total, sum), y = pct.of.total)) + xlab("Word") + ggtitle("Unigram Frequency")
df1.plot <- df1.plot + geom_bar(stat = "identity", fill = "steelblue")
df1.plot


df2.plot <- ggplot(dfm.2.summ[1:20 ,], aes(x=reorder(word,-pct.of.total, sum), y = pct.of.total)) + xlab("Word") + ggtitle("Bigram Frequency")
df2.plot <- df2.plot + geom_bar(stat = "identity", fill = "chartreuse3")+theme(axis.text.x=element_text(angle=45))
df2.plot

df3.plot <- ggplot(dfm.3.summ[1:20 ,], aes(x=reorder(word,-pct.of.total, sum), y = pct.of.total)) + xlab("Word") + ggtitle("Trigram Frequency")
df3.plot <- df3.plot + geom_bar(stat = "identity", fill = "goldenrod2")+theme(axis.text.x=element_text(angle=45))
df3.plot

df4.plot <- ggplot(dfm.4.summ[1:20 ,], aes(x=reorder(word,-pct.of.total, sum), y = pct.of.total)) + xlab("Word") + ggtitle("Quad-gram Frequency")
df4.plot <- df4.plot + geom_bar(stat = "identity", fill = "firebrick")+theme(axis.text.x=element_text(angle=45))
df4.plot

#Coverage

length(which(dfm.1.summ$cumu.pot <= 0.50)) #223
length(which(dfm.1.summ$cumu.pot <= 0.90)) #9 795

length(which(dfm.2.summ$cumu.pot <= 0.50)) #39 397
length(which(dfm.2.summ$cumu.pot <= 0.90)) #1 726 935

length(which(dfm.3.summ$cumu.pot <= 0.50)) #1 646 509
length(which(dfm.3.summ$cumu.pot <= 0.90)) #5 271 774

length(which(dfm.4.summ$cumu.pot <= 0.50)) #3 540 502
length(which(dfm.4.summ$cumu.pot <= 0.90)) #7 013 493


