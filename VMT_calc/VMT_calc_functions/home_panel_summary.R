### m_hps from "home_panel_summary"

home_panel_summary <- function(num){

  if(num == 1){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_1.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 2){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_2.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 3){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_3.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 4){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_4.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 5){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_5.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 6){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_6.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 7){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_7.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 8){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_8.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 9){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_9.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 10){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_10.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 11){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_11.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 12){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraph/home_panel_summary/m_12.csv", header=TRUE, stringsAsFactors = FALSE)}

  m_hps <- filter(m_hps, state == "ca")
  m_hps <- select(m_hps, c("census_block_group", "number_devices_residing"))
  colnames(m_hps) <- c("block_id", "number_devices_residing")

  return(m_hps)
  
}