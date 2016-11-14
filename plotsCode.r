options(stringsAsFactors = FALSE)
source('analysisFunctions.R')


#### Code for plots
#### Must have relevant model loaded with XFGdtmres.r ####


## MtGox plots for 50 topic model

title = 'MtGox Word Evolution 22/11/2009 - 06/08/2016'
xl = 'Time'

par(mfrow = c(2,1), oma = c(0, 0, 2, 0))
stuffToPlot = termEvoP('gox', 23, TRUE)
stuffToPlot1 = termEvoP('gox', 38, TRUE)

plot(axisLab, stuffToPlot, type = 'l', col = 'blue', main = 'Topic 23', cex.lab = 1.5, cex.axis = 1.5, xlab = xl, ylab = 'p(w = mtgox | t = 23)')
plot(axisLab, stuffToPlot1, type = 'l', col ='red', main = 'Topic 38', cex.lab = 1.5, cex.axis = 1.5, xlab = xl, ylab = 'p(w = mtgox | t = 38)')

mtext(title, outer = TRUE, cex = 1.5)


## Mining hardware for 50 topic model comparison plots

title = 'Mining Hardware Word Evolution 22/11/2009 - 06/08/2016'
xl = 'Time'

par(mfrow = c(2,2), oma = c(0, 0, 2, 0))
stuffToPlot = termEvoP('cpu', 50)
stuffToPlot1 = termEvoP('gpu', 50)
stuffToPlot2 = termEvoP('asics', 50)
stuffToPlot3 = termEvoP('antminer', 50)

plot(axisLab, stuffToPlot, type = 'l', cex.lab = 1.5, cex.axis = 1.5, col = 'blue', main = 'CPU', xlab = xl, ylab = 'p(w = CPU | k = 50)')
plot(axisLab, stuffToPlot1, type = 'l', cex.lab = 1.5, cex.axis = 1.5, col ='red', main = 'GPU', xlab = xl, ylab = 'p(w = GPU | k = 50)')
plot(axisLab, stuffToPlot2, type = 'l', cex.lab = 1.5, cex.axis = 1.5, col = 'blue', main = 'Asics', xlab = xl, ylab = 'p(w = Asics | k = 50)')
plot(axisLab, stuffToPlot3, type = 'l', cex.lab = 1.5, cex.axis = 1.5, col = 'green', main = 'Antminer', xlab = xl, ylab = 'p(w = Antminer | k = 50)')

mtext(title, outer = TRUE, cex = 1.5)


## Topic heat plot for 30 topic model

## Run event detection and prepare output for plot
source('eventAnalysis.R')

## Remove non-detected events and order by heat
clean = check[check$detected == 1,]
cleanO = clean[order(-as.double(clean$heat)),]

par(mar=c(8,5,2,1),oma = c(0, 0, 2, 0))
plot(cleanO$heat, col = 'blue', cex.lab = 1.5, cex.axis = 1.5, type = 'h', xaxt = 'n', xlab = '', ylab = 'p(k = Event Topic | t = Time of Event)')
axis(1, at = 1:length(cleanO$label), labels = cleanO$label, cex.axis = 1.7, las = 3)

mtext('Event Topic Temperature', outer = TRUE, cex = 2)


## Event vs Model plots

## Results from running event detection on 10, 30 and 50 topic models respectively
nevents10 = c(2,0,1,14,0,1,0,0,0,1)
nevents30 = c(0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,22,0,0,0,0,0,0,1)
nevents50 = c(0,0,1,0,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,3,0,0,0,0,0,0,0,19,0,0,0,0,0,0,0,0,0,0,0,0)

par(mfrow = c(1,3))
layout(matrix(c(1,1,1,1,2,2,2,2,0,0,3,3,3,3,0,0), 2, 8, byrow = TRUE))
barplot(nevents10, names.arg = 1:10, col = 'green', cex.lab = 1.5, cex.axis = 1.5, xlab = 'Topic Number', ylab = 'Number of Events', main = '10 Topic Model')
barplot(nevents30, names.arg = 1:30, col = 'blue', cex.lab = 1.5, cex.axis = 1.5, xlab = 'Topic Number', ylab = 'Number of Events', main = '30 Topic Model')
barplot(nevents50, names.arg = 1:50, col = 'red', cex.lab = 1.5, cex.axis = 1.5, xlab = 'Topic Number', ylab = 'Number of Events', main = '50 Topic Model')


