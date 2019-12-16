### m_hps from "home_panel_summary"

home_panel_summary <- function(num){

  if(num == 1){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_01.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 2){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_02.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 3){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_03.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 4){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_04.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 5){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_05.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 6){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_06.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 7){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_07.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 8){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_08.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 9){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_09.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 10){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_10.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 11){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_11.csv", header=TRUE, stringsAsFactors = FALSE)}
  else if(num == 12){m_hps <- read.csv("C:/Users/Derek/Desktop/safegraphplaces/home_panel_summary/m_12.csv", header=TRUE, stringsAsFactors = FALSE)}

  m_hps <- filter(m_hps, state == "ca")
  m_hps <- dplyr::select(m_hps, c("census_block_group", "number_devices_residing"))
  colnames(m_hps) <- c("block_id", "number_devices_residing")

  return(m_hps)
  
}