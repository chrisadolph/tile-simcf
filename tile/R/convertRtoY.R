"convertRtoY" <-
function(data,usr) {
  # Convert data from t-axis to x-axis units
  if (length(na.omit(usr[3:4]))==2)
    data <- (data - usr[7])/(usr[8] - usr[7]) * (usr[4] - usr[3]) + usr[3]
  data
}

