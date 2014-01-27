best = function(state, outcome){
	## read outcome data
	outcomes = read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## check that state and outcome are valid
	validate(state, outcome)

	## return hospital name in that state with lowest 30-day death rate
	findHospitalWithLowest30dayDeathRate(state, outcome)
}

validate = function(state, outcome){
	if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
		stop("invalid outcome")
	}else if(!(state %in% outcomes$State)){
		stop("invalid state")
	}
}

findHospitalWithLowest30dayDeathRate = function(state, outcome){
	stateOutcomesWithNA = subset(outcomes, State == state)
	## print(str(stateOutcomesWithNA))
	if(outcome == "heart attack"){
		stateOutcomes = subset(stateOutcomesWithNA, complete.cases(stateOutcomesWithNA))
		# print(str(stateOutcomes))
		minimalOutcomeStates = subset(stateOutcomes, stateOutcomes[,11] == min(stateOutcomes[,11]), select=Hospital.Name)
		orderedMinimalOutcomeStates = minimalOutcomeStates[order(minimalOutcomeStates),]
		minimalState = orderedMinimalOutcomeStates[1]
		minimalState
	}else if(outcome == "heart failure"){
		stateOutcomes = subset(stateOutcomesWithNA, complete.cases(stateOutcomesWithNA))
		# print(str(stateOutcomes))
		minimalOutcomeStates = subset(stateOutcomes, stateOutcomes[,17] == min(stateOutcomes[,17]), select=Hospital.Name)
		orderedMinimalOutcomeStates = minimalOutcomeStates[order(minimalOutcomeStates),]
		minimalState = orderedMinimalOutcomeStates[1]
		minimalState
	}else if(outcome == "pneumonia"){
		stateOutcomes = subset(stateOutcomesWithNA, complete.cases(stateOutcomesWithNA))
		# print(str(stateOutcomes))
		minimalOutcomeStates = subset(stateOutcomes, stateOutcomes[,23] == min(stateOutcomes[,23]), select=Hospital.Name)
		orderedMinimalOutcomeStates = minimalOutcomeStates[order(minimalOutcomeStates),]
		minimalState = orderedMinimalOutcomeStates[1]
		minimalState
	}
}