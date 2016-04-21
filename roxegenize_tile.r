#This file is for replication purposes but converts tile and simcf
#documentation to roxygen compatiable
#the same should also be done for simcf
library(Rd2roxygen)
formatR::usage(Rd2roxygen)
Rd2roxygen('tile/')
rab('tile/')
