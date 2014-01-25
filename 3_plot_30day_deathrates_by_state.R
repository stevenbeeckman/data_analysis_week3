plot_30day_deathrates_by_state = function(){
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	# heart attack
	outcome[, 11] = as.numeric(outcome[, 11])
	
	# table() counts the number observations 
	states_with_more_than_20_hospitals = (table(outcome$State) > 20)
	states_with_more_than_20_hospitals
	outcome2 = subset(outcome, states_with_more_than_20_hospitals)
	
	# basic boxplot of the death rates by state
	DF = data.frame(death = outcome2[, 11], state = outcome2$State)
	print(summary(DF))

	oind = order(as.numeric(by(DF$death, DF$state, median)))
	print(oind)
	DF$state = ordered(DF$state, levels=levels(DF$state)[oind])
	# print(DF)
	par(las=2)
	boxplot(death ~ state, data=DF, ylab = "30-day Death Rate", main = "Heart Attack 30-day Death Rate by State")
}