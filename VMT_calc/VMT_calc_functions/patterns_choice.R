patterns_choice <- function(num){
  
  if(num == 1){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_01_patterns.csv"}
  else if(num == 2){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_02_patterns.csv"}
  else if(num == 3){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_03_patterns.csv"}
  else if(num == 4){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_04_patterns.csv"}
  else if(num == 5){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_05_patterns.csv"}
  else if(num == 6){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_06_patterns.csv"}
  else if(num == 7){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_07_patterns.csv"}
  else if(num == 8){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_08_patterns.csv"}
  else if(num == 9){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_09_patterns.csv"}
  else if(num == 10){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_10_patterns.csv"}
  else if(num == 11){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_11_patterns.csv"}
  else if(num == 12){patterns_text <- "S:/Restricted Data Library/Safegraph/y=2018/m_12_patterns.csv"}
  else{patterns_text <-"error"}
  
  return(patterns_text)
  
}