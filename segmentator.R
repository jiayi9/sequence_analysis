
x_segmentator = function(x){
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

interval_segmentator = function(x, MAX_INTERVAL_TIME = 60*60, RETURN = 'SEG', TYPE = 'time'){
  N = length(x)
  if(TYPE == 'time'){
    x = lubridate::ymd_hms(x)
    DIFF = as.numeric(as.duration(x[2:N] - x[1:(N-1)]))
  } else {
    DIFF = diff(x)
  }
  FIRSTS = which(DIFF > MAX_INTERVAL_TIME)
  if(length(FIRSTS) == 0) {
    L = 1:N
    L2 = rep(1,N)
  } else {
    m = length(FIRSTS)
    L = list()
    for(i in 0:m){
      if(i == 0){
        INDEX = 1:(FIRSTS[1]-1)
      } else if( i == m) {
        INDEX = FIRSTS[m]:N
      } else {
        INDEX = FIRSTS[i]:(FIRSTS[i+1]-1)
      }
      L = rlist::list.append(L, INDEX)
    }
    LEN = length(L)
    L2 = unlist(lapply(1:LEN, function(i) {rep(i, length(L[[i]]))}))
  }
  print('Return: SEG: segmentation vector, others: INDEX in a list')
  print(paste('You have chosen Returning', RETURN))
  if(RETURN == 'SEG') R = L2 else R = L
  return(R)
}

GET_LAST_BATCH_INDEX = function(x, MAX = 3600){
  tmp = interval_segmentator(x, MAX_INTERVAL_TIME = MAX, RETURN = '', TYPE = 'time')
  INDEX = tmp[[length(tmp)]]
  INDEX
}
