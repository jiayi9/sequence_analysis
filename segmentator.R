
segmentator = function(x){
  N = length(x)
  CURSOR = 1
  INDEX = 1
  for(i in 2:N){
    if(x[i] == x[i-1]) {
      INDEX = c(INDEX, CURSOR)
    } else {
      CURSOR = CURSOR + 1
      INDEX = c(INDEX, CURSOR)
    }
  }
  INDEX
}
