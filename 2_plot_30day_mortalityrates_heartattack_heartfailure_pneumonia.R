plot_30day_mortalityrates_heartattack_heartfailure_pneumonia = function(){
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	# heart attack
	outcome[, 11] = as.numeric(outcome[, 11])
	# heart failure
	outcome[, 17] = as.numeric(outcome[, 17])
	# pneumonia
	outcome[, 23] = as.numeric(outcome[, 23])

	# plot 3 histograms vertically on the same plot window
	par(mfrow = c(3,1))
	print(range(outcome[,11], na.rm=T))
	hist_min = min(min(range(outcome[,11], na.rm=T)), min(range(outcome[,17], na.rm=T)), min(range(outcome[,23], na.rm=T)))
	hist_max = max(max(range(outcome[,11], na.rm=T)), max(range(outcome[,17], na.rm=T)), max(range(outcome[,23], na.rm=T)))
	print(hist_min)
	print(hist_max)
	print(paste("Median for heart attack:", median(outcome[,11], na.rm=T)))
	print(paste("Median for heart failure:", median(outcome[,17], na.rm=T)))
	print(paste("Median for pneumonia:", median(outcome[,23], na.rm=T)))
	hist(outcome[, 11], main="Heart Attack", xlab="30-day Death Rate", xlim=c(hist_min, hist_max))
	abline(v=median(outcome[,11], na.rm=T))
	hist(outcome[, 17], main="Heart Failure", xlab="30-day Death Rate", xlim=c(hist_min, hist_max))
	abline(v=median(outcome[,17], na.rm=T))
	hist(outcome[, 23], main="Pneumonia", xlab="30-day Death Rate", xlim=c(hist_min, hist_max))
	abline(v=median(outcome[,23], na.rm=T))
	
}