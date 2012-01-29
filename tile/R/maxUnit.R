# Create a simple representation of the maximum of a list of grid units
# Units must be simple; e.g., lines, not strwidth

maxUnit <- function(...,units) {
    unit(max(as.numeric(as.list(unit.c(...)))),
             units)
}
