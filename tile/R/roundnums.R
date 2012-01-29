"roundnums" <-
function(bounds,cuts) {

  range <- bounds[2]-bounds[1]
  space <- range/(cuts-1)
  seqa <- seq(bounds[1],bounds[2],space)
  seqa <- zapsmall(seqa)
  seqa
}

