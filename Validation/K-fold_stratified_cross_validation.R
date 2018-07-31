#####
## Full cross-validation process (using cv.folds in Stylo)
## Results are put into matrix format
## tested: 8 approaches, with or without merging training and test set, with 3 different test/training sets compositions
## 
## with corpus in external folders
#####


library(stylo)

resultsCV = matrix(nrow = 8, ncol = 6)
rownames(resultsCV) = c("Canberra", "Cosine", "CosineDelta", "Delta", "Eder", "SVM", "NSC", "KNN")
colnames(resultsCV) = c("1FALSE", "1TRUE", "2FALSE", "2TRUE", "3FALSE", "3TRUE")

my.cosine.distance = function(x){
    # z-scoring the input matrix of frequencies
    x = scale(x)
    # computing cosine dissimilarity
    y = as.dist( x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) )
    # then, turning it into cosine similarity
    z = 1 - y
    # getting the results
    return(z)
}


####COSINE DELTA

######
dist_measure = "my.cosine.distance"
all_scores = "FALSE"
results.styloCosineDeltaALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["CosineDelta", "1FALSE"] = mean(results.styloCosineDeltaALLfalse1$cross.validation.summary)

######
dist_measure = "my.cosine.distance"
all_scores = "FALSE"
results.styloCosineDeltaALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["CosineDelta", "2FALSE"] = mean(results.styloCosineDeltaALLfalse2$cross.validation.summary)

######
dist_measure = "my.cosine.distance"
all_scores = "FALSE"
results.styloCosineDeltaALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["CosineDelta", "3FALSE"] = mean(results.styloCosineDeltaALLfalse3$cross.validation.summary)

######
dist_measure = "my.cosine.distance"
all_scores = "TRUE"
results.styloCosineDeltaALLtrue1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["CosineDelta", "1TRUE"] = mean(results.styloCosineDeltaALLtrue1$cross.validation.summary)

######
dist_measure = "my.cosine.distance"
all_scores = "TRUE"
results.styloCosineDeltaALLtrue2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["CosineDelta", "2TRUE"] = mean(results.styloCosineDeltaALLtrue2$cross.validation.summary)

######
dist_measure = "my.cosine.distance"
all_scores = "TRUE"
results.styloCosineDeltaALLtrue3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["CosineDelta", "3TRUE"] = mean(results.styloCosineDeltaALLtrue3$cross.validation.summary)



####COSINE

######
dist_measure = "dist.cosine"
all_scores = "FALSE"
results.styloCosineALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Cosine", "1FALSE"] = mean(results.styloCosineALLfalse1$cross.validation.summary)

######
dist_measure = "dist.cosine"
all_scores = "FALSE"
results.styloCosineALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Cosine", "2FALSE"] = mean(results.styloCosineALLfalse2$cross.validation.summary)

######
dist_measure = "dist.cosine"
all_scores = "FALSE"
results.styloCosineALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Cosine", "3FALSE"] = mean(results.styloCosineALLfalse3$cross.validation.summary)

######
dist_measure = "dist.cosine"
all_scores = "TRUE"
results.styloCosineALLtrue1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Cosine", "1TRUE"] = mean(results.styloCosineALLtrue1$cross.validation.summary)

######
dist_measure = "dist.cosine"
all_scores = "TRUE"
results.styloCosineALLtrue2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Cosine", "2TRUE"] = mean(results.styloCosineALLtrue2$cross.validation.summary)

######
dist_measure = "dist.cosine"
all_scores = "TRUE"
results.styloCosineALLtrue3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Cosine", "3TRUE"] = mean(results.styloCosineALLtrue3$cross.validation.summary)



####CANBERRA

######
dist_measure = "dist.canberra"
all_scores = "FALSE"
results.styloCanberraALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Canberra", "1FALSE"] = mean(results.styloCanberraALLfalse1$cross.validation.summary)

######
dist_measure = "dist.canberra"
all_scores = "FALSE"
results.styloCanberraALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Canberra", "2FALSE"] = mean(results.styloCanberraALLfalse2$cross.validation.summary)

######
dist_measure = "dist.canberra"
all_scores = "FALSE"
results.styloCanberraALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Canberra", "3FALSE"] = mean(results.styloCanberraALLfalse3$cross.validation.summary)

######
dist_measure = "dist.canberra"
all_scores = "TRUE"
results.styloCanberraALLtrue1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Canberra", "1TRUE"] = mean(results.styloCanberraALLtrue1$cross.validation.summary)

######
dist_measure = "dist.canberra"
all_scores = "TRUE"
results.styloCanberraALLtrue2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Canberra", "2TRUE"] = mean(results.styloCanberraALLtrue2$cross.validation.summary)

######
dist_measure = "dist.canberra"
all_scores = "TRUE"
results.styloCanberraALLtrue3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Canberra", "3TRUE"] = mean(results.styloCanberraALLtrue3$cross.validation.summary)



####EDER

######
dist_measure = "dist.eder"
all_scores = "FALSE"
results.styloEderALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Eder", "1FALSE"] = mean(results.styloEderALLfalse1$cross.validation.summary)

######
dist_measure = "dist.eder"
all_scores = "FALSE"
results.styloEderALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Eder", "2FALSE"] = mean(results.styloEderALLfalse2$cross.validation.summary)

######
dist_measure = "dist.eder"
all_scores = "FALSE"
results.styloEderALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Eder", "3FALSE"] = mean(results.styloEderALLfalse3$cross.validation.summary)

######
dist_measure = "dist.eder"
all_scores = "TRUE"
results.styloEderALLtrue1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Eder", "1TRUE"] = mean(results.styloEderALLtrue1$cross.validation.summary)

######
dist_measure = "dist.eder"
all_scores = "TRUE"
results.styloEderALLtrue2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Eder", "2TRUE"] = mean(results.styloEderALLtrue2$cross.validation.summary)

######
dist_measure = "dist.eder"
all_scores = "TRUE"
results.styloEderALLtrue3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Eder", "3TRUE"] = mean(results.styloEderALLtrue3$cross.validation.summary)



####BURROWS

######
dist_measure = "dist.delta"
all_scores = "FALSE"
results.styloDeltaALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Delta", "1FALSE"] = mean(results.styloDeltaALLfalse1$cross.validation.summary)

######
dist_measure = "dist.delta"
all_scores = "FALSE"
results.styloDeltaALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Delta", "2FALSE"] = mean(results.styloDeltaALLfalse2$cross.validation.summary)

######
dist_measure = "dist.delta"
all_scores = "FALSE"
results.styloDeltaALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Delta", "3FALSE"] = mean(results.styloDeltaALLfalse3$cross.validation.summary)

######
dist_measure = "dist.delta"
all_scores = "TRUE"
results.styloDeltaALLtrue1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["Delta", "1TRUE"] = mean(results.styloDeltaALLtrue1$cross.validation.summary)

######
dist_measure = "dist.delta"
all_scores = "TRUE"
results.styloDeltaALLtrue2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["Delta", "2TRUE"] = mean(results.styloDeltaALLtrue2$cross.validation.summary)

######
dist_measure = "dist.delta"
all_scores = "TRUE"
results.styloDeltaALLtrue3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="delta", distance.measure=dist_measure, reference.wordlist.of.all.samples=all_scores, z.scores.of.all.samples=all_scores, mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["Delta", "3TRUE"] = mean(results.styloDeltaALLtrue3$cross.validation.summary)



####SVM

######
results.styloSVMALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="svm", svm.kernel="linear", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["SVM", "1FALSE"] = mean(results.styloSVMALLfalse1$cross.validation.summary)

######
results.styloSVMALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="svm", svm.kernel="linear", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["SVM", "2FALSE"] = mean(results.styloSVMALLfalse2$cross.validation.summary)

######
results.styloSVMALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="svm", svm.kernel="linear", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["SVM", "3FALSE"] = mean(results.styloSVMALLfalse3$cross.validation.summary)



####NSC

######
results.styloNSCALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="nsc", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["NSC", "1FALSE"] = mean(results.styloNSCALLfalse1$cross.validation.summary)

######
results.styloNSCALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="nsc", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["NSC", "2FALSE"] = mean(results.styloNSCALLfalse2$cross.validation.summary)

######
results.styloNSCALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="nsc", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["NSC", "3FALSE"] = mean(results.styloNSCALLfalse3$cross.validation.summary)



####KNN

######
results.styloKNNALLfalse1 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="knn", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_1/", training.corpus.dir = "secondary_set_1/", cv.folds=20)
resultsCV["KNN", "1FALSE"] = mean(results.styloKNNALLfalse1$cross.validation.summary)

######
results.styloKNNALLfalse2 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="knn", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_2/", training.corpus.dir = "secondary_set_2/", cv.folds=20)
resultsCV["KNN", "2FALSE"] = mean(results.styloKNNALLfalse2$cross.validation.summary)

######
results.styloKNNALLfalse3 <- classify(gui = FALSE, corpus.lang="German", analyzed.features="w", ngram.size=1,classification.method="knn", mfw.min=10, mfw.max=2000, mfw.incr=10, test.corpus.dir = "primary_set_3/", training.corpus.dir = "secondary_set_3/", cv.folds=20)
resultsCV["KNN", "3FALSE"] = mean(results.styloKNNALLfalse3$cross.validation.summary)



#########
###calculate differences
#########

resultsALLtrue = cbind(resultsCV[,"1TRUE"], resultsCV[,"2TRUE"], resultsCV[,"3TRUE"])
resultsALLfalse = cbind(resultsCV[,"1FALSE"], resultsCV[,"2FALSE"], resultsCV[,"3FALSE"])
resultsCV = cbind(resultsCV, rowMeans(resultsALLfalse))
resultsCV = cbind(resultsCV, rowMeans(resultsALLtrue))
resultsCV = t(resultsCV)
rownames(resultsCV)[7] = "mean_FALSE"
rownames(resultsCV)[8] = "mean_TRUE"

meansDiff = (rowMeans(resultsALLfalse)[1:5] - rowMeans(resultsALLtrue)[1:5])
FinalMeansDiff = mean(c(meansDiff[1], meansDiff[3:5]))

MeanSD = mean(c(sd(resultsALLfalse["Canberra",]), sd(resultsALLfalse["Delta",]), sd(resultsALLfalse["Eder",]), sd(resultsALLfalse["CosineDelta",]), sd(resultsALLtrue["Canberra",]), sd(resultsALLtrue["Delta",]), sd(resultsALLtrue["Eder",]), sd(resultsALLtrue["CosineDelta",])))

MeanRange = mean(c(diff(range(resultsALLfalse["Canberra",])), diff(range(resultsALLfalse["Delta",])), diff(range(resultsALLfalse["Eder",])), diff(range(resultsALLfalse["CosineDelta",])), diff(range(resultsALLtrue["Canberra",])), diff(range(resultsALLtrue["Delta",])), diff(range(resultsALLtrue["Eder",])), diff(range(resultsALLtrue["CosineDelta",]))))

save.image("ResultsStratifiedCrossValidation10-2000.RData")
