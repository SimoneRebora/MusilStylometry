load("Corpus.RData")
library(stylo)


###define functions

dist.cosine.delta = function(x){
    # z-scoring the input matrix of frequencies
    x = scale(x)
    # computing cosine dissimilarity
    y = as.dist( x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) )
    # then, turning it into cosine similarity
    z = 1 - y
    # getting the results
    return(z)
}

###define distance calculation main iterative function

distance_calculation = function(custom.txt.collection, chosen_distance_measure, chunk_length){
	results.combinatory <- vector(mode = "numeric", length = length(custom.txt.collection))
	for (n in 1:10)
	{
		if (n == 1){
			results.stylo <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=(chunk_length*n), mfw.max=(chunk_length*n), mfw.incr=0, distance.measure=chosen_distance_measure, write.pdf.file = FALSE, parsed.corpus = custom.txt.collection)
			results.combinatory <- results.combinatory + (results.stylo$distance.table[1,]/(max(results.stylo$distance.table[1,])))
		}
		if (n > 1){
			results.stylo <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=(chunk_length*n), mfw.max=(chunk_length*n), mfw.incr=0, distance.measure=chosen_distance_measure, write.pdf.file = FALSE, parsed.corpus = custom.txt.collection, use.existing.freq.tables = TRUE)
			results.combinatory <- results.combinatory + (results.stylo$distance.table[1,]/(max(results.stylo$distance.table[1,])))
		}
		cat("\n#############################iteration", n, "\n\n")
	}
	results.combinatory <- results.combinatory/10
	return(results.combinatory)
}

#######
###main process begins
#####


###define validation vectors
musil_full <- c(musil_1, musil_2, musil_3)
musil_full <- musil_full[1:20000]
musil_testset <- list()
musil_trainingset <- list()

ritter_full <- c(ritter_1, ritter_2, ritter_3)
ritter_full <- ritter_full[1:20000]
ritter_testset <- list()
ritter_trainingset <- list()

for(validation in 1:10){
	random_sel <- sample(1:length(musil_full), 2000, replace=FALSE)
	musil_testset[[validation]] <- musil_full[random_sel]
	ritter_testset[[validation]] <- ritter_full[random_sel]
	musil_full <- musil_full[-random_sel]
	ritter_full <- ritter_full[-random_sel]
	musil_trainingset[[validation]] <- musil_full
	ritter_trainingset[[validation]] <- ritter_full
	if(validation==1){next}
	for(i in 1:(validation-1)){
		musil_trainingset[[validation]] <- c(musil_trainingset[[validation]], musil_testset[[i]])
		ritter_trainingset[[validation]] <- c(ritter_trainingset[[validation]], ritter_testset[[i]])
	}
}


############


####iteration begins
final_results <- matrix(0, nrow = 28, ncol = 160)
methods_combination <- expand.grid(c(20,50,100,200), c("dist.delta", "dist.eder", "dist.canberra", "dist.cosine.delta"), stringsAsFactors = FALSE)
validation_method <- 1

for(validation in 1:10){
	for(method in 1:16){
		for(text in 1:28){
			TestSetCombin <- c(TestSet[[text]], musil_testset[[validation]], ritter_testset[[validation]])
			TrainingSet_Musil <- musil_trainingset[[validation]]
			TrainingSet_Ritter <- ritter_trainingset[[validation]]
			custom.txt.collection <- list(TestSetCombin, TrainingSet_Musil, TrainingSet_Ritter)
			names(custom.txt.collection) <- c("TestSet", "Musil", "Ritter")
			stylo_results <- distance_calculation(custom.txt.collection = custom.txt.collection, chosen_distance_measure = methods_combination$Var2[method], chunk_length = methods_combination$Var1[method])
			final_results[text, validation_method] <- stylo_results[3] - stylo_results[2]
			print(validation_method)
			print(text)
		}
		validation_method <- validation_method + 1
	}
}

save.image("Simplified_design_results.RData")
