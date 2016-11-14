source('analysisFunctions.R')
options(stringsAsFactors = FALSE)

## RESULTS LOADER

## PARAMETERS

## xaxis labels
start 	= '2009-11-22'
end 		= '2016-08-06'
granularity = '1 week'
axisLab 	= seq(as.POSIXct(start), as.POSIXct(end), by = granularity)

## Number of time slices and number of topics
ntimes  	= 350
ntopics		= 30

## Input path and output path
filePath 	= paste('models/topics', as.character(ntopics), '/', sep = '')
outputPath 	= 'output/'


## Creates vec of topic ids for loading results
topics 	= sapply(0:(ntopics - 1), function(x) {
			if (x < 100) {
				if (x < 10) {return(paste('00', toString(x), sep = ''))
				} else {return(paste('0', toString(x), sep = ''))}
			} else {return(toString(x))}
		})
	
## Load vocab
vocab 	= read.delim(paste(filePath, "vocabulary.dat", sep = ''), sep = '\n', header = FALSE)[,1]

## Load the results into list of length ntopics of word/time matrices
## tl[[topicNumber]] returns a matrix of size V x ntimes
tl 		= lapply(topics, function(topic) {
			d = scan(paste(filePath, outputPath, "lda-seq/topic-", toString(topic), "-var-e-log-prob.dat", sep=''))
			f = matrix(d, ncol=ntimes, byrow = TRUE)
			rownames(f) <- 1:length(vocab)
			return(f)
			}
		)
		

	
## Load the topic document proportions
## Gives a matrix of size Documents x topics, documents are ordered by time
a = scan(paste(filePath, outputPath, "lda-seq/gam.dat", sep = ""))
b = matrix(a, ncol = ntopics, byrow = TRUE)
rs = rowSums(b)
gamma = b / rs
rownames(gamma) = 1:dim(gamma)[1]

## Load the time slices
times = read.table(paste(filePath, "-seq.dat", sep = ""), sep = "\n")
times = c(times)[[1]]
times = times[2:length(times)]
	

