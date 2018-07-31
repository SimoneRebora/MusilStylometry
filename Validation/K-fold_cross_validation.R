### Code for K-fold cross validation
### actually, it is a 21-fold CV (leave-one-out strategy, with 21 texts)
### results are calculated for each text in the training set
### with 8 different methods (5 distance measures and 3 machine learning approaches)
### Everything is based on the "classify" function in Stylo

distance_measures <- c("dist.delta", "dist.eder", "dist.canberra", "dist.cosine.delta", "dist.cosine")
load("Corpus.RData")
library(stylo)

dist.cosine.delta <- function(x){
  # z-scoring the input matrix of frequencies
  x = scale(x)
  # computing cosine dissimilarity
  y = as.dist( x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) )
  # then, turning it into cosine similarity
  z = 1 - y
  # getting the results
  return(z)
}

calculate_distance <- function(chosen_distance){
  results.evaluation <- vector(mode = "numeric", length = 21)
  for(i in 1:21)
  {
  	custom.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
  	names(custom.txt.collection) = c("blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "musil_1", "musil_2", "musil_3", "ritter_1", "ritter_2", "ritter_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3")
  	dummy.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
  	names(dummy.txt.collection) = c("blei_11", "blei_21", "blei_31", "dellegrazie_11", "dellegrazie_21", "dellegrazie_31", "kafka_11", "kafka_21", "kafka_31", "musil_11", "musil_21", "musil_31", "ritter_11", "ritter_21", "ritter_31", "salus_11", "salus_21", "salus_31", "zweig_11", "zweig_21", "zweig_31")
  	results.stylo <- classify(gui = FALSE, corpus.lang="German", classification.method="delta", distance.measure=chosen_distance, mfw.min=10, mfw.max=2000, mfw.incr=10, training.corpus = custom.txt.collection[-i], test.corpus = c(custom.txt.collection[i], dummy.txt.collection[i]))
  	results.evaluation[i] = results.stylo$overall.success.rate[1]
  	cat("\n#############################iteration", i, "\n\n")
  }
  return(results.evaluation)
}


###start process

results.evaluationDelta <- calculate_distance(distance_measures[1])
results.evaluationEder <- calculate_distance(distance_measures[2])
results.evaluationCanberra <- calculate_distance(distance_measures[3])
results.evaluationCosineDelta <- calculate_distance(distance_measures[4])
results.evaluationCosine <- calculate_distance(distance_measures[5])


###with SVM
results.evaluation <- vector(mode = "numeric", length = 21)
for(i in 1:21)
{
	custom.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
	names(custom.txt.collection) = c("blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "musil_1", "musil_2", "musil_3", "ritter_1", "ritter_2", "ritter_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3")
	dummy.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
	names(dummy.txt.collection) = c("blei_11", "blei_21", "blei_31", "dellegrazie_11", "dellegrazie_21", "dellegrazie_31", "kafka_11", "kafka_21", "kafka_31", "musil_11", "musil_21", "musil_31", "ritter_11", "ritter_21", "ritter_31", "salus_11", "salus_21", "salus_31", "zweig_11", "zweig_21", "zweig_31")
	results.stylo <- classify(gui = FALSE, corpus.lang="German", classification.method="svm", svm.kernel="linear", mfw.min=20, mfw.max=2000, mfw.incr=20, training.corpus = custom.txt.collection[-i], test.corpus = c(custom.txt.collection[i], dummy.txt.collection[i]))
	results.evaluation[i] = results.stylo$overall.success.rate[1]
	cat("\n#############################iteration", i, "\n\n")
}
results.evaluationSVM <- results.evaluation

###with NSC
for(i in 1:21)
{
	custom.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
	names(custom.txt.collection) = c("blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "musil_1", "musil_2", "musil_3", "ritter_1", "ritter_2", "ritter_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3")
	dummy.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
	names(dummy.txt.collection) = c("blei_11", "blei_21", "blei_31", "dellegrazie_11", "dellegrazie_21", "dellegrazie_31", "kafka_11", "kafka_21", "kafka_31", "musil_11", "musil_21", "musil_31", "ritter_11", "ritter_21", "ritter_31", "salus_11", "salus_21", "salus_31", "zweig_11", "zweig_21", "zweig_31")
	results.stylo <- classify(gui = FALSE, corpus.lang="German", classification.method="nsc", mfw.min=20, mfw.max=2000, mfw.incr=20, training.corpus = custom.txt.collection[-i], test.corpus = c(custom.txt.collection[i], dummy.txt.collection[i]))
	results.evaluation[i] = results.stylo$overall.success.rate[1]
	cat("\n#############################iteration", i, "\n\n")
}
results.evaluationNSC <- results.evaluation

###with knn
for(i in 1:21)
{
  custom.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
  names(custom.txt.collection) = c("blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "musil_1", "musil_2", "musil_3", "ritter_1", "ritter_2", "ritter_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3")
  dummy.txt.collection = list(blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
  names(dummy.txt.collection) = c("blei_11", "blei_21", "blei_31", "dellegrazie_11", "dellegrazie_21", "dellegrazie_31", "kafka_11", "kafka_21", "kafka_31", "musil_11", "musil_21", "musil_31", "ritter_11", "ritter_21", "ritter_31", "salus_11", "salus_21", "salus_31", "zweig_11", "zweig_21", "zweig_31")
  results.stylo <- classify(gui = FALSE, corpus.lang="German", classification.method="knn", mfw.min=20, mfw.max=2000, mfw.incr=20, training.corpus = custom.txt.collection[-i], test.corpus = c(custom.txt.collection[i], dummy.txt.collection[i]))
  results.evaluation[i] = results.stylo$overall.success.rate[1]
  cat("\n#############################iteration", i, "\n\n")
}
results.evaluationKNN <- results.evaluation

results.evaluationDelta[22] <- mean(results.evaluationDelta)
results.evaluationEder[22] <- mean(results.evaluationEder)
results.evaluationCosine[22] <- mean(results.evaluationCosine)
results.evaluationCosineDelta[22] <- mean(results.evaluationCosineDelta)
results.evaluationCanberra[22] <- mean(results.evaluationCanberra)
results.evaluationSVM[22] <- mean(results.evaluationSVM)
results.evaluationNSC[22] <- mean(results.evaluationNSC)
results.evaluationKNN[22] <- mean(results.evaluationKNN)

classify_final_df <- data.frame(results.evaluationDelta, results.evaluationEder, results.evaluationCanberra, results.evaluationCosine, results.evaluationCosineDelta, results.evaluationSVM, results.evaluationKNN, results.evaluationNSC)
rownames(classify_final_df) <- c("blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "musil_1", "musil_2", "musil_3", "ritter_1", "ritter_2", "ritter_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3", "mean_values")
write.csv(classify_final_df, file = "Classify_results_SUPER10-2000.csv")
save.image("ResultsCrossValidation10-2000.RData")



