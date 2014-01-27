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
	stateOutcomes = subset(outcomes, State == state)
	print(str(stateOutcomes))
	if(outcome == "heart attack"){
		stateOutcomes[,11] = as.numeric(stateOutcomes[,11])
		print(dim(stateOutcomes[,11]))
		apply(stateOutcomes[,11], 2, min)
	}else if(outcome == "heart failure"){
		stateOutcomes[,17] = as.numeric(stateOutcomes[,17])
		print(dim(stateOutcomes[,11]))
		apply(stateOutcomes[,17], 2, min)
	}else if(outcome == "pneumonia"){
		stateOutcomes[,23] = as.numeric(stateOutcomes[,23])
		print(dim(stateOutcomes[,11]))
		apply(stateOutcomes[,23], 2, min)
	}
}