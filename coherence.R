source('analysisFunctions.R')
options(stringsAsFactors = FALSE)


## Parameters of Model and Umass measure ##
nwords = 20
ntopics = 30
start = 1
nslices = 100
eta = 0.1

## File path of -mult.dat and -seq.dat
filePath = paste("models/topics", as.character(ntopics), '/', sep = '')


#### Functions for measuring umass coherence ####
#### Must have model loaded with XFGdtmres.r ####


## Loads corpus given a filepath which must be the path before file -mult.dat
load_corpus = function(filePath) {
	con  = file(paste(filePath, '-mult.dat', sep = ''), open = "r")
	while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
		a = unlist(strsplit(oneLine, " "))
		if (length(a) >= 2) {
			a = a[2:length(a)]
			a = unlist(strsplit(unlist(a), ':'))
			d[[count]] = matrix(as.integer(a), nrow = 2)
			count = count + 1
		}
		if (count %% 1000 == 0) {print(count)}
	}
	close(con)
	return(d)
}


## Creates a word co-occurence matrix given a vector of word ids
co_occurences = function(sd, ids) {
	tabl = matrix(0, nrow = length(ids), ncol = length(ids))
	for (x in 1:length(sd)) {
		for (n in sd[[x]][1,]) {
			if (n %in% ids) {
				n_i = which(n == ids)
				tabl[n_i,n_i] = tabl[n_i,n_i] + 1
				for (m in sd[[x]][1,]) {
					if (m != n & m %in% ids) {
						m_i = which(m == ids)
						tabl[n_i,m_i] = tabl[n_i,m_i] + 1
					}
				}
			}
		}
		if (x %% 500 == 0) {print(x)}
	}
	return(tabl)
}

## Returns vector of unique ids of top nwords for each topic at time time
coherence_ids = function(time, nwords) {
	topicTerms = matrix(0, nrow = ntopics, ncol = nwords)

	for (topic in 1:ntopics) {
		termsRanked = sapply(1:length(vocab), function(termId) {return(tl[[topic]][termId, time])})
		t1 		= sort(termsRanked, decreasing = TRUE)
		termIds = as.integer(names(t1[1:nwords]))
		topicTerms[topic,] =  topicTerms[topic,] + termIds
	}
	return(unique(c(topicTerms)))
}

## Calculates Umass coherence measure on a vector of word ids from co-occurence matrix
umass_topic = function(tabl, toCheck) {
	measure = 0
	for (i in 1:(length(toCheck) - 1)) {
		for (j in (i + 1):length(toCheck)) {
			i_index = toCheck[i]
			j_index = toCheck[j]
			if (tabl[i_index,i_index] != 0) {
				measure = measure + log((tabl[i_index,j_index] + eta)/tabl[i_index,i_index])
			}
		}
	}
	return(measure)
}




## Load corpus and time slices
d = load_corpus(filePath)
slices = read.table(paste(filePath, "-seq.dat", sep = ""), sep = "\n")

## Calculate Umass for all chained LDA models
current = 1
chainedMeasure = list()
for (time in start:(start + nslices) {
	
	## Subset the corpus
	next1 = current + slices[time] - 1
	sd = d[current:next1]
	current = next1 + 1

	## Make word co-occurence matrix
	ids = coherence_ids(nwords)
	tabl = co_occurences(sd, ids)
	print(paste('Co-occurences done for slice', as.character(time)))

	## Calculate Umass measure for each topic
	topicsMeasure = c()
	for (topic in 1:ntopics) {
		termsRanked = sapply(1:length(vocab), function(termId) {return(tl[[topic]][termId, time])})
		t1 		= sort(termsRanked, decreasing = TRUE)
		termIds = as.integer(names(t1[1:nwords]))	

		toCheck = sapply(1:20, function(x) {return(which(termIds[x] == ids))})
		
		measure = umass_topic(tabl, toCheck)
		topicsMeasure = c(topicsMeasure, measure)
		
	}

	chainedMeasure[[time]] = topicsMeasure
	print('Calc Complete, mean: ')
	print(mean(topicsMeasure))
}
	