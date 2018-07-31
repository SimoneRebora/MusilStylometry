### call environment

load("Corpus.RData")
library(stylo)

### set variables
## available distance measures: "dist.delta" "dist.eder" "dist.canberra" "dist.cosine" "dist.cosine.delta"  

chosen_distance_measure = "dist.delta"
output_file_name = "Delta.RData"
one_tenth_iterations = TRUE

######
## Definition of main functions

### define Cosine Delta distance

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

### define distance calculation main iterative function

distance_calculation = function(chosen_distance_measure, chunk_length, iter){
	TestSetCombin <- c(TestSet[[combinations[1,iter]]],TestSet[[combinations[2,iter]]],TestSet[[combinations[3,iter]]],TestSet[[combinations[4,iter]]],TestSet[[combinations[5,iter]]],TestSet[[combinations[6,iter]]])
	custom.txt.collection = list(TestSetCombin, blei_1, blei_2, blei_3, dellegrazie_1, dellegrazie_2, dellegrazie_3, kafka_1, kafka_2, kafka_3, musil_1, musil_2, musil_3, ritter_1, ritter_2, ritter_3, salus_1, salus_2, salus_3, zweig_1, zweig_2, zweig_3)
	names(custom.txt.collection) = c(paste("Test", combinations[1,iter], combinations[2,iter], combinations[3,iter], combinations[4,iter], combinations[5,iter], combinations[6,iter], ""), "blei_1", "blei_2", "blei_3", "dellegrazie_1", "dellegrazie_2", "dellegrazie_3", "kafka_1", "kafka_2", "kafka_3", "musil_1", "musil_2", "musil_3", "ritter_1", "ritter_2", "ritter_3", "salus_1", "salus_2", "salus_3", "zweig_1", "zweig_2", "zweig_3")
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
		results.combinatory <- results.combinatory/10
		cat("\n#############################iteration", n, "\n\n")
	}
	return(results.combinatory)
}

### define matches check main iteration
matches_check <- function(results.combinatory){
	# iteration for the mean values on a specific text in TestSet
	# it checks "i" iterations on "n" texts
	combined.distances <- list()
	matches <- list()
	for(n in 1:28)
	{
		combined.distances[[n]] <- vector(mode = "numeric", length = length((results.combinatory[[1]])))
		matches[[n]] <-0
		for(i in 1:total_iterations) 
		{
			text.to.catch <- paste("", n, "")
			if(grepl(text.to.catch, names((results.combinatory[[i]])[1])) == "TRUE")
			{
				combined.distances[[n]] <- combined.distances[[n]] + (results.combinatory[[i]])
				matches[[n]] <- matches[[n]] + 1
			}
		}
	}
	results_list <- list(combined.distances, matches)
	return(results_list)
}

###define total number of iterations

total_iterations = dim(combinations)[2]

#############
### start process
#############
### full procedure with all combinations
#############

if (one_tenth_iterations == FALSE){

	###
	#SUPER
	##

	results.combinatorySUPER <- list()
	for(i in 1:total_iterations){
		results.combinatorySUPER[[i]] <- distance_calculation(chosen_distance_measure, 200, i)
	}

	matches_tmp <- matches_check(results.combinatorySUPER)

	combined.distancesSUPER <- matches_tmp[[1]]
	matchesSUPER <- matches_tmp[[2]]
	
	###
	#EXTRA
	##
	
	results.combinatoryEXTRA <- list()
	for(i in 1:total_iterations){
		results.combinatoryEXTRA[[i]] <- distance_calculation(chosen_distance_measure, 100, i)
	}

	matches_tmp <- matches_check(results.combinatoryEXTRA)

	combined.distancesEXTRA <- matches_tmp[[1]]
	matchesEXTRA <- matches_tmp[[2]]
	
	###
	#LONG
	##

	results.combinatoryLONG <- list()
	for(i in 1:total_iterations){
		results.combinatoryLONG[[i]] <- distance_calculation(chosen_distance_measure, 50, i)
	}

	matches_tmp <- matches_check(results.combinatoryLONG)

	combined.distancesLONG <- matches_tmp[[1]]
	matchesLONG <- matches_tmp[[2]]
	
	###
	#MED
	##
	
	results.combinatoryMED <- list()
	for(i in 1:total_iterations){
		results.combinatoryMED[[i]] <- distance_calculation(chosen_distance_measure, 20, i)
	}

	matches_tmp <- matches_check(results.combinatoryMED)

	combined.distancesMED <- matches_tmp[[1]]
	matchesMED <- matches_tmp[[2]]

}


###########
####reduced procedure with one-tenth combinations
###########

if (one_tenth_iterations == TRUE){

	###
	#SUPER
	##

	results.combinatorySUPER <- list()
	for(i in 1:trunc(total_iterations/10)){
		results.combinatorySUPER[[i]] <- distance_calculation(chosen_distance_measure, 200, i*10)
	}

	matches_tmp <- matches_check(results.combinatorySUPER)

	combined.distancesSUPER <- matches_tmp[[1]]
	matchesSUPER <- matches_tmp[[2]]
	
	###
	#EXTRA
	##
	
	results.combinatoryEXTRA <- list()
	for(i in 1:total_iterations){
		results.combinatoryEXTRA[[i]] <- distance_calculation(chosen_distance_measure, 100, i*10)
	}

	matches_tmp <- matches_check(results.combinatoryEXTRA)

	combined.distancesEXTRA <- matches_tmp[[1]]
	matchesEXTRA <- matches_tmp[[2]]
	
	###
	#LONG
	##

	results.combinatoryLONG <- list()
	for(i in 1:total_iterations){
		results.combinatoryLONG[[i]] <- distance_calculation(chosen_distance_measure, 50, i*10)
	}

	matches_tmp <- matches_check(results.combinatoryLONG)

	combined.distancesLONG <- matches_tmp[[1]]
	matchesLONG <- matches_tmp[[2]]
	
	###
	#MED
	##
	
	results.combinatoryMED <- list()
	for(i in 1:total_iterations){
		results.combinatoryMED[[i]] <- distance_calculation(chosen_distance_measure, 20, i*10)
	}

	matches_tmp <- matches_check(results.combinatoryMED)

	combined.distancesMED <- matches_tmp[[1]]
	matchesMED <- matches_tmp[[2]]

}

save.image(output_file_name)
