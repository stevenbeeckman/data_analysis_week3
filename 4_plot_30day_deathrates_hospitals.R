plot_30day_deathrates_number_of_patients = function{
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospital = read.csv("hospital-data.csv", colClasses = "character")

	# join the two datasets together to math the Hospital.Ownership variable to the death rate data
	outcome.hospital = merge(outcome, hospital, by = "Provider.Number")
}