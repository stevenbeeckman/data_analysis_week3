plot_30day_deathrates_number_of_patients = function() {
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospital = read.csv("hospital-data.csv", colClasses = "character")

	# join the two datasets together to math the Hospital.Ownership variable to the death rate data
	outcome.hospital = merge(outcome, hospital, by = "Provider.Number")

	death = as.numeric(outcome.hospital[, 11])
	npatient = as.numeric(outcome.hospital[, 15])
	owner = factor(outcome.hospital$Hospital.Ownership)

	library(lattice) # needed for xyplot
	xyplot(death ~ npatient | owner, panel = function(x,y, ...){ panel.xyplot(x, y) 
		panel.lmline(x, y)}, main="Heart Attack 30-day Death Rate by Ownership", xlab = "Number of Patients Seen", ylab = "30-day Death Rate")
	# p2 = panel.lmline(death, npatient)
	# p1 + p2
}