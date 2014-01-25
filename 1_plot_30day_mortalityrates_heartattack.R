plot_30day_mortalityrates_heartattack = function(){
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	# show first rows of the dataset
	print(head(outcome))
	# number of columns in the dataset
	print(paste("Number of columns in the dataset:", ncol(outcome)))
	print("Names of those columns:")
	print(names(outcome))
	# number of rows in the dataset
	print(paste("Number of rows in the dataset:", nrow(outcome)))

	# column 11 contains the 30-day death rates from heart attack
	# when we read the data we said it only contained characters, now converting the data to numerics
	outcome[, 11] = as.numeric(outcome[, 11])
	# show the histogram
	hist(outcome[, 11], main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate")
}