plot_30day_deathrates_by_state = function(){
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	# heart attack
	outcome[, 11] = as.numeric(outcome[, 11])
	
}