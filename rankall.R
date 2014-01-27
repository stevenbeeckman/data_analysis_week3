rankall = function(outcome, num = "best"){
	## read outcome data
	outcomes = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## check that state and outcome are valid
	validate(outcome, num)
	## for each state, find the hospital of the given rank
	states = sort(unique(outcomes$State))
	# print(states)
	hospitals = sapply(states, findHospitalWithDeathRateRanking, outcome, num)
	# print(str(hospitals))
	# print(hospitals)
	result = data.frame(hospital = hospitals, state = states, stringsAsFactors = F)
	# str(result)
	result
	## return a data frame with the hospital names and the (abbreviated) state name
}

validate = function(outcome, num){
	if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
		stop("invalid outcome")
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
		orderedStateOutcomes[num, "Hospital.Name"]
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
		orderedStateOutcomes[num, "Hospital.Name"]
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
		orderedStateOutcomes[num, "Hospital.Name"]
	}

}