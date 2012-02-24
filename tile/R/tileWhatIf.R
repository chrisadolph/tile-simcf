tileWhatIf <- function(extrapolate) {
    require(WhatIf)
    print("Running whatif")
    wi <- WhatIf::whatif(formula=extrapolate$formula, data=extrapolate$data, cfact=extrapolate$cfact)
    print("Whatif finished; returning to tile")
    extrapolate$control <- !wi$in.hull
    extrapolate
}
