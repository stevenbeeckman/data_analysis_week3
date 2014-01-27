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
	
}