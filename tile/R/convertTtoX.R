"convertTtoX" <-
function(data,usr) {
  # Convert data from t-axis to x-axis units
  if (length(na.omit(usr[1:2]))==2)
    data <- (data - usr[5])/(usr[6] - usr[5]) * (usr[2] - usr[1]) + usr[1]
  data
}

