# Key cycle detection function
findInterval = function(x, Timestamp = NA){
  N = length(x)

  SearchIndexStart = 2
  SearchIndexEnd = N - 18 + 1
  SearchIndexSequence = SearchIndexStart:SearchIndexEnd

  L = lapply(SearchIndexSequence, function(i) {
    
    START = i
    END = i + 14 -1
    SEQ = x[START:END]
    
    MAX = max(SEQ)
    MIN = min(SEQ)
    MEAN = mean(SEQ)
    
    Leading_1 = x[START - 1]
    Trailing_3 = x[(END+1):(END+3)]
    
    x_all = x[(START-1):(END+3)]
    
    if(any(
      is.null(END), is.null(Leading_1), is.null(MEAN), is.null(MAX), is.null(MIN), is.null(Trailing_3), is.null(x_all)
    )) 
      return(data.frame(NULL))
    
    if( all(!is.na(x_all)) & MAX - MIN < 4 & Leading_1 - MEAN < 30 & Leading_1 - MEAN > 20 & all(Trailing_3 - MEAN > 10) & all(Trailing_3 - MEAN < 20)){        
      if(is.na(Timestamp[1])){
        data.frame(t(x_all), MeanCT = round(mean(x_all),2))
      } else {
        data.frame(TimeFirstCT = Timestamp[i-1], t(x_all), MeanCT = round(mean(x_all),2))
      }
    } else {
      data.frame(NULL)
    }
    
  })
  
  D = do.call(rbind, L)
  
  D
}
