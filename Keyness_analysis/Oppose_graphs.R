###Hi, this is the keyness/oppose code
###it is written in R.
###The environment to be loaded (test and training set) is attached.

#We start. Make it run line by line or simply follow comments  

library(stylo)

#Upload Environment. It has to be in R's working directory. Set it with a command similar to the following one:
#setwd("/home/rsimone/Musil - TSZ Project/")
#load the corpus:
load("Corpus.RData")

###Musil's authored texts keyness values

#set secondary(training) set:
custom.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
names(custom.txt.collection) = c("blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "ritter_1", "ritter_2", "ritter_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3")

#set primary(test) set:
custom.txt.collection2 = list(musil_1, musil_2, musil_3) 
names(custom.txt.collection2) = c("musil_1", "musil_2", "musil_3")

#calculate Craig's zetas (moving window of 5000 words, cutoff set by default to 0.1)
authored_musil_1 = oppose(gui = FALSE, path = NULL,
       primary.corpus = custom.txt.collection2,
       secondary.corpus = custom.txt.collection, text.slice.length = 5000, 
       text.slice.overlap = 4900,
       oppose.method = "craig.zeta")

#calculate Eder's zetas
authored_musil_2 = oppose(gui = FALSE, path = NULL,
       primary.corpus = custom.txt.collection2,
       secondary.corpus = custom.txt.collection, text.slice.length = 5000, 
       text.slice.overlap = 4900,
       oppose.method = "eder.zeta")

###preferred words

authored_musil_tmp <- c(authored_musil_1$words.preferred.scores, authored_musil_2$words.preferred.scores)
#combines results of the two oppose functions

authored_musil_pref = tapply(authored_musil_tmp, names(authored_musil_tmp), prod)
#multiplies values obtained by oppose with same names

exclude_names = table(names(authored_musil_tmp))
#helps to exclude names that are retrieved only once

authored_musil_pref = authored_musil_pref[-which(exclude_names<2)]
#exclude names that are retrieved only once. I.e., their product = 0

authored_musil_pref = authored_musil_pref/max(authored_musil_pref)
#normalize values to 0/1

###avoided words

authored_musil_tmp <- c(authored_musil_1$words.avoided.scores, authored_musil_2$words.avoided.scores)
#combines results of the two oppose functions

authored_musil_avoid = tapply(authored_musil_tmp, names(authored_musil_tmp), prod)
#multiplies values obtained by oppose with same names

exclude_names = table(names(authored_musil_tmp))
#helps to exclude names that are retrieved only once

authored_musil_avoid = authored_musil_avoid[-which(exclude_names<2)]
#exclude names that are retrieved only once. I.e., their product = 0

authored_musil_avoid = authored_musil_avoid/max(authored_musil_avoid)
#normalize values to 0/1

authored_musil_pref = sort(authored_musil_pref, decreasing = TRUE)
authored_musil_avoid = sort(-authored_musil_avoid)


###Oppose Graph modified
###taken from https://github.com/computationalstylistics/stylo/blob/master/R/oppose.R
###I just increased the plotting area, so not to cut words in the graph...

if(length(names(authored_musil_pref)) > 70) {
        preferred.words.for.plotting = names(authored_musil_pref)[1:70]
        preferred.indices.for.plotting = 1:70
        preferred.scores.for.plotting = authored_musil_pref[1:70]
} else {
        preferred.words.for.plotting = names(authored_musil_pref)
        preferred.indices.for.plotting = 1:length(authored_musil_pref)
        preferred.scores.for.plotting = authored_musil_pref
}
# the same procedure applied to the avoided words
if(length(names(authored_musil_avoid)) > 70) {
        avoided.words.for.plotting = names(authored_musil_avoid)[1:70]
        avoided.indices.for.plotting = 1:70
        avoided.scores.for.plotting = authored_musil_avoid[1:70]
} else {
        avoided.words.for.plotting = names(authored_musil_avoid)
        avoided.indices.for.plotting = 1:length(authored_musil_avoid)
        avoided.scores.for.plotting = authored_musil_avoid
}




plot.current.task = function(){
        ####Simone modified HERE
        plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(-1.5,1.5), type="n", xlab="Rank of the item", ylab="Score")
        text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7, srt=90, adj=c(0,0))
        text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7, srt=90, adj=c(1,0))
        abline(h=0, lty=2)      
        mtext("Preferred", side = 4, at = 0.5, las = 3)
        mtext("Avoided", side = 4, at = -0.5)
        title(main = graph.title)
}

graph.title = "Musil vs. Training Set (Eder's and Craig's Zeta, 5,000W moving window)"
plot.current.task()

png(filename = "Musil_oppose.png", 
        width=10,height=7,res=1200, units="in")
plot.current.task()
dev.off()






###Ritter
###this part is just copy-paste of what done before, just changing the training/test sets to focus on Ritter

custom.txt.collection = list(musil_1, musil_2, musil_3, blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
names(custom.txt.collection) = c("musil_1", "musil_2", "musil_3", "blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3")

custom.txt.collection2 = list(ritter_1, ritter_2, ritter_3) 
names(custom.txt.collection2) = c("ritter_1", "ritter_2", "ritter_3")

authored_ritter_1 = oppose(gui = FALSE, path = NULL,
       primary.corpus = custom.txt.collection2,
       secondary.corpus = custom.txt.collection, text.slice.length = 5000, 
       text.slice.overlap = 4900,
       oppose.method = "craig.zeta")

authored_ritter_2 = oppose(gui = FALSE, path = NULL,
       primary.corpus = custom.txt.collection2,
       secondary.corpus = custom.txt.collection, text.slice.length = 5000, 
       text.slice.overlap = 4900,
       oppose.method = "eder.zeta")


###preferred words

authored_ritter_tmp <- c(authored_ritter_1$words.preferred.scores, authored_ritter_2$words.preferred.scores)
#combines results of the two oppose functions

authored_ritter_pref = tapply(authored_ritter_tmp, names(authored_ritter_tmp), prod)
#multiplies values obtained by oppose with same names

exclude_names = table(names(authored_ritter_tmp))
#helps to exclude names that are retrieved only once

authored_ritter_pref = authored_ritter_pref[-which(exclude_names<2)]
#exclude names that are retrieved only once. I.e., their product = 0

authored_ritter_pref = authored_ritter_pref/max(authored_ritter_pref)
#normalize values to 0/1

###avoided words

authored_ritter_tmp <- c(authored_ritter_1$words.avoided.scores, authored_ritter_2$words.avoided.scores)
#combines results of the two oppose functions

authored_ritter_avoid = tapply(authored_ritter_tmp, names(authored_ritter_tmp), prod)
#multiplies values obtained by oppose with same names

exclude_names = table(names(authored_ritter_tmp))
#helps to exclude names that are retrieved only once

authored_ritter_avoid = authored_ritter_avoid[-which(exclude_names<2)]
#exclude names that are retrieved only once. I.e., their product = 0

authored_ritter_avoid = authored_ritter_avoid/max(authored_ritter_avoid)
#normalize values to 0/1

authored_ritter_pref = sort(authored_ritter_pref, decreasing = TRUE)
authored_ritter_avoid = sort(-authored_ritter_avoid)



###Oppose Graph modified

if(length(names(authored_ritter_pref)) > 70) {
        preferred.words.for.plotting = names(authored_ritter_pref)[1:70]
        preferred.indices.for.plotting = 1:70
        preferred.scores.for.plotting = authored_ritter_pref[1:70]
} else {
        preferred.words.for.plotting = names(authored_ritter_pref)
        preferred.indices.for.plotting = 1:length(authored_ritter_pref)
        preferred.scores.for.plotting = authored_ritter_pref
}
# the same procedure applied to the avoided words
if(length(names(authored_ritter_avoid)) > 70) {
        avoided.words.for.plotting = names(authored_ritter_avoid)[1:70]
        avoided.indices.for.plotting = 1:70
        avoided.scores.for.plotting = authored_ritter_avoid[1:70]
} else {
        avoided.words.for.plotting = names(authored_ritter_avoid)
        avoided.indices.for.plotting = 1:length(authored_ritter_avoid)
        avoided.scores.for.plotting = authored_ritter_avoid
}




plot.current.task = function(){
        ####Simone modified HERE
        plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(-1.5,1.5), type="n", xlab="Rank of the item", ylab="Score")
        text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7, srt=90, adj=c(0,0))
        text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7, srt=90, adj=c(1,0))
        abline(h=0, lty=2)      
        mtext("Preferred", side = 4, at = 0.5, las = 3)
        mtext("Avoided", side = 4, at = -0.5)
        title(main = graph.title)
}

graph.title = "Ritter vs. Training Set (Eder's and Craig's Zeta, 5,000W moving window)"
plot.current.task()

png(filename = "Ritter_oppose.png", 
        width=10,height=7,res=1200, units="in")
plot.current.task()
dev.off()






###Inside Test Set

Ritter_selection = texts_Ritter_attr
#it's the selection of Ritter's TSZ attributed texts...

Musil_selection = texts_Musil_attr

Ritter_test_set = character()
for(i in Ritter_selection){
  Ritter_test_set = c(Ritter_test_set, TestSet[[i]])
}

Musil_test_set = character()
for(i in Musil_selection){
  Musil_test_set = c(Musil_test_set, TestSet[[i]])
}


Ritter.txt.collection = list(Ritter_test_set, Ritter_test_set)
Musil.txt.collection = list(Musil_test_set, Musil_test_set)
names(Ritter.txt.collection) = c("Ritter_1", "Ritter_2")
names(Musil.txt.collection) = c("Musil_1", "Musil_2")
### Note: the texts are repeated twice
### this is because Stylo forces you to have at least two texts in each set (I have to understand why...)
#word frequencies are simply duplicated, and results should be the same as with one text

TSZ_1 = oppose(gui = FALSE, path = NULL,
       primary.corpus = Ritter.txt.collection,
       secondary.corpus = Musil.txt.collection, text.slice.length = 5000, 
       text.slice.overlap = 4900,
       oppose.method = "craig.zeta")

TSZ_2 = oppose(gui = FALSE, path = NULL,
       primary.corpus = Ritter.txt.collection,
       secondary.corpus = Musil.txt.collection, text.slice.length = 5000, 
       text.slice.overlap = 4900,
       oppose.method = "eder.zeta")

TSZ_ritter_tmp <- c(TSZ_1$words.preferred.scores,TSZ_2$words.preferred.scores)

exclude_names = table(names(TSZ_ritter_tmp))

TSZ_ritter = tapply(TSZ_ritter_tmp, names(TSZ_ritter_tmp), prod)

TSZ_ritter = TSZ_ritter[-which(exclude_names<2)]

TSZ_ritter = TSZ_ritter/max(TSZ_ritter)


TSZ_musil_tmp <- c(TSZ_1$words.avoided.scores,TSZ_2$words.avoided.scores)
#here Musil's preferred words are calculated as Ritter's avoided words

exclude_names = table(names(TSZ_musil_tmp))

TSZ_musil = tapply(TSZ_musil_tmp, names(TSZ_musil_tmp), prod)

TSZ_musil = TSZ_musil[-which(exclude_names<2)]

TSZ_musil = TSZ_musil/max(TSZ_musil)


TSZ_ritter = sort(-TSZ_ritter)
TSZ_musil = sort(TSZ_musil, decreasing = TRUE)

###Oppose Graph modified

if(length(names(TSZ_musil)) > 70) {
        preferred.words.for.plotting = names(TSZ_musil)[1:70]
        preferred.indices.for.plotting = 1:70
        preferred.scores.for.plotting = TSZ_musil[1:70]
} else {
        preferred.words.for.plotting = names(TSZ_musil)
        preferred.indices.for.plotting = 1:length(TSZ_musil)
        preferred.scores.for.plotting = TSZ_musil
}
# the same procedure applied to the avoided words
if(length(names(TSZ_ritter)) > 70) {
        avoided.words.for.plotting = names(TSZ_ritter)[1:70]
        avoided.indices.for.plotting = 1:70
        avoided.scores.for.plotting = TSZ_ritter[1:70]
} else {
        avoided.words.for.plotting = names(TSZ_ritter)
        avoided.indices.for.plotting = 1:length(TSZ_ritter)
        avoided.scores.for.plotting = TSZ_ritter
}




plot.current.task_TSZ = function(){
        ####Simone modified HERE
        plot(preferred.indices.for.plotting, preferred.scores.for.plotting, ylim=c(-1.5,1.5), type="n", xlab="Rank of the item", ylab="Score")
        text(preferred.indices.for.plotting, preferred.scores.for.plotting, as.character(preferred.words.for.plotting), cex=0.7, srt=90, adj=c(0,0))
        text(avoided.indices.for.plotting, avoided.scores.for.plotting, as.character(avoided.words.for.plotting), cex=0.7, srt=90, adj=c(1,0))
        abline(h=0, lty=2)      
        mtext("Musil", side = 4, at = 0.5, las = 3)
        mtext("Ritter", side = 4, at = -0.5)
        title(main = graph.title)
}

graph.title = "Musil (14 texts) vs. Ritter (10 texts) in TSZ (Eder's and Craig's Zeta, 5,000W moving window)"
plot.current.task_TSZ()

png(filename = "TSZ_oppose.png", 
        width=10,height=7,res=1200, units="in")
plot.current.task_TSZ()
dev.off()
