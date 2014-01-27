rankhospital = function(state, outcome, num = "best"){
	## read outcome data
	outcomes = read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## check that state and outcome are valid
	validate(state, outcome, num)

	## return hospital name in that state with lowest 30-day death rate
	findHospitalWithDeathRateRanking(state, outcome, num)
}

validate = function(state, outcome, num){
	if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
		stop("invalid outcome")
	}else if(!(state %in% outcomes$State)){
		stop("invalid state")
	}
}

findHospitalWithDeathRateRanking = function(state, outcome, num){
	stateOutcomesWithNA = subset(outcomes, State == state)
	stateOutcomes = subset(stateOutcomesWithNA, complete.cases(stateOutcomesWithNA))

	if(outcome == "heart attack"){
		orderedStatesIndex = order(stateOutcomes[,11])
		orderedStateOutcomes = stateOutcomes[orderedStatesIndex,]
		if(num == "best"){
			num = 1
		}else if(num == "worst"){
			num = length(orderedStateOutcomes[,11])
		}else if(num > length(orderedStateOutcomes[,11])){
			NA
		}
		print(paste(orderedStateOutcomes[num, "Hospital.Name"], orderedStateOutcomes[num,11]))
	}else if(outcome == "heart failure"){
		orderedStatesIndex = order(stateOutcomes[,17])
		orderedStateOutcomes = stateOutcomes[orderedStatesIndex,]
		if(num == "best"){
			num = 1
		}else if(num == "worst"){
			num = length(orderedStateOutcomes[,17])
		}else if(num > length(orderedStateOutcomes[,17])){
			NA
		}
		print(paste(orderedStateOutcomes[num, "Hospital.Name"], orderedStateOutcomes[num,17]))
	}else if(outcome == "pneumonia"){
		orderedStatesIndex = order(stateOutcomes[,23])
		orderedStateOutcomes = stateOutcomes[orderedStatesIndex,]
		if(num == "best"){
			num = 1
		}else if(num == "worst"){
			num = length(orderedStateOutcomes[,23])
		}else if(num > length(orderedStateOutcomes[,23])){
			NA
		}
		print(paste(orderedStateOutcomes[num, "Hospital.Name"], orderedStateOutcomes[num,23]))
	}

}