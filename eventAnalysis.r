source('analysisFunctions.R')
options(stringsAsFactors = FALSE)

#### Functions for event detection ####
#### Must have model loaded using XFGdtmres.r ####

## Compares detected events with known events
checkEvents = function(events, dates) {
	
	df = data.frame(word = character(), topics = character(), detected = logical(), heat = character())
	
	for (i in 1:dim(dates)[1]) {
	
		detected = FALSE
		topics = c()
		heat = c()
	
		## Determine which events to check against known dates of events
		word = dates$word[i]
		datesCheck = which(word == events$word)
		
		if (dates$end[i] == '') {
			start = dates$start[i]
			end = dates$start[i]
		} else {
			start = dates$start[i]
			end = dates$end[i]
		}
		
		for (x in datesCheck) {
		
			if (is.na(events$topic[x])) {
				next
			}
		
			detected1 = dateIntersection(events$start[[x]], events$end[[x]], start, end)
			
			## Save topics in which event was detected correctly and determine the topic temperature
			if (detected1 & !(events$topic[x] %in% topics)) {
				topics = c(topics, events$topic[x])
				heat = c(heat, mean(topicProp(as.integer(events$topic[x]))[dateToSlice(start):dateToSlice(end)]) - 1/ntopics)
			}
			
			detected = detected | detected1
		}
		df = rbind(df, data.frame(word, topics = paste(topics, collapse = ','), detected, heat = paste(heat, collapse = ',')))
	}
	
	print(paste(sum(as.integer(df$detected)), 'events detected out of', length(df$detected)))
	return(df)
}

## Helper function to check if two date ranges intersect
dateIntersection = function(a, b, x, y) {
	a = as.Date(a)
	b = as.Date(b) + 7
	x = as.Date(x)
	y = as.Date(y) + 7
	
	case1 = x <= a & a <= y
	case2 = x <= b & b <= y
	case3 = a <= x & y <= b
	
	return(case1 | case2 | case3)
}


## Takes as input a vector of words to check for events and an event detection limit v
eventDf = function(wordVec, v, threshold = 0.001) {
	
	eventsAll = data.frame(word = character(), topic = integer(), start = character(), end = character())
	
	for (x in 1:length(wordVec)) {
		
		if (!(wordVec[x] %in% vocab)) {
			eventsAll = rbind(eventsAll, data.frame(word = wordVec[x], topic = NA, start = NA, end = NA))
			next
		}
		
		topics = threshTopics(wordVec[x], threshold)
	
		for (topic in topics) {
			events = eventDetect(wordVec[x], topic, v, threshold)
			eventsAll = rbind(eventsAll, events)
		}	
	}
	
	eventsAll$start = sapply(eventsAll$start, function(x) {strftime(sliceToDate(as.integer(x)), '%Y-%m-%d')})
	eventsAll$end = sapply(eventsAll$end, function(x) {strftime(sliceToDate(as.integer(x)), '%Y-%m-%d')})
	return(eventsAll)
}


## Detects an event in the input words timeline
eventDetect = function(word, topic, v, threshold = 0.001) {
	times = c()
	evo = c()
	event = FALSE
	
	termId = which(vocab == word)
	events = data.frame(word = character(), topic = integer(), start = integer(), end = integer())
	
	for (time in 1:ntimes) {
		p = exp(tl[[topic]][termId, time])
		evo = c(evo, p)
		
		if (time == 1) {
			next
		}
		
		## Upper control limit
		contr = mean(evo) + v * sd(evo)
		
		eventCrit = (p > contr) & (p > threshold)
		
		if (!event & eventCrit) {
			START = time
			event = TRUE
		}
		
		if (event & !eventCrit) {
			END = time - 1
			event = FALSE
			events = rbind(events, data.frame(word, topic, start = START, end = END))
		}
	}

	if (event & eventCrit) {
		events = rbind(events, data.frame(word, topic, start = START, end = ntimes))
	}
	
	return(events)
}


## Determines which topics to test for events, threshold should be bigger when using termScore or substring
threshTopics = function(term, threshold = 0.001, termScore = FALSE, substr = FALSE) {
	termId = which(term == vocab)
	top = c()
	for (topic in 1:ntopics) {
		top = c(top, sum(termEvo(term, topic, termScore, substr)))
	}
	return(which(threshold < top))
}


## File path of event files
filePath = 'events/'

## Load words to test and data frame of words and event dates
wordVec = scan(paste(filePath, 'eventVec.dat', sep = ''), what = character())
dateVec = read.csv(paste(filePath, 'eventDate.dat', sep = ''))

## Detect events and check if they are correct and what the topic proportion was
events = eventDf(wordVec, 3, 0.001)
check = checkEvents(events, dateVec)

