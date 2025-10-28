library(tidyverse) # All the tidy things
library(jsonlite)  # Converting json data into data frames
library(magrittr)  # Extracting items from list objects using piping grammar
library(httr)      # Interacting with HTTP verbs
library(xml2)
library(sf)


zip_file = "https://results.votehamiltoncountyohio.gov//OH/Hamilton/123661/358351/reports/detailxml.zip"
#unzip(zipfile = zip_file, exdir = getwd())
temp <- tempfile()

getXMLFile = function() {
  download.file(zip_file,temp)
  data <- read_xml(unz(temp, "detail.xml"))
  return(data)
}

prepXML <- function(xml_file){
  
  ## Get all Contest nodes
  contests <- xml_file %>% xml_find_all(".//Contest")
  
  # Function to extract vote data for each contest and candidate
  parse_contest_votes <- function(contest_node) {
    contest_name <- xml_attr(contest_node, "text")
    
    # Get all candidate (Choice) nodes
    choices <- contest_node %>% xml_find_all(".//Choice")
    
    # Iterate through each candidate
    map_df(choices, function(choice) {
      candidate_name <- xml_attr(choice, "text")
      
      # Get all VoteType nodes (Early, Election)
      vote_types <- choice %>% xml_find_all(".//VoteType")
      
      # Iterate through each VoteType and precinct data
      map_df(vote_types, function(vote_type) {
        vote_type_name <- xml_attr(vote_type, "name")
        
        # Extract precincts
        precincts <- vote_type %>% xml_find_all(".//Precinct")
        
        # For each precinct, extract the votes and precinct name
        map_df(precincts, function(precinct) {
          precinct_name <- xml_attr(precinct, "name")
          votes <- xml_attr(precinct, "votes") %>% as.numeric()
          
          tibble(
            contest = contest_name,
            candidate = candidate_name,
            vote_type = vote_type_name,
            precinct = precinct_name,
            votes = votes
          )
        })
      })
    })
  }
  
  # Apply the function to all contests
  election_data <- contests %>% map_df(parse_contest_votes)
  return(election_data)
}


GetTurnoutFromXML <- function(turnout.data){
  # Create a data frame
  turnout.df <- data.frame(
    name = xml_attr(turnout.data, "name"),
    totalVoters = as.integer(xml_attr(turnout.data, "totalVoters")),
    ballotsCast = as.integer(xml_attr(turnout.data, "ballotsCast")),
    voterTurnout = as.numeric(xml_attr(turnout.data, "voterTurnout")),
    percentReporting = as.numeric(xml_attr(turnout.data, "percentReporting")),
    stringsAsFactors = FALSE
  )
  return(turnout.df)
}

# all we need to do to get updated data

data = prepXML( getXMLFile() )


wide_data <- data %>%
  pivot_wider(names_from = candidate, values_from = contest)

#yaa4ayay codee

#' gets the information from a specific election
#' @param data your dataset (obtained from prepXML)
#' @param election the election you are looking for
    #' @note you can search for "ISSUE 2" like "ISSUE 2", "issue 2" "Issue", all work
#' @return a dataset with each precinct and their votes for their candidates, as well as total votes!
#' @examples (they work properly but you store the base data in a variable)
    #' issueTwoData = GetElectionInfo( prepXML( getXMLFile() ) , "Issue 2" )
    #' mayoralData = GetElectionInfo( prepXML( getXMLFile() ) , "Mayor" )
GetElectionInfo = function(data,election) {
  
  return(
    data %>% 
      filter( str_detect( tolower(data$contest) , tolower(election) ) ) %>% 
      group_by(candidate,precinct) %>% 
      summarise(across( votes ,sum)) %>% 
      pivot_wider(
        names_from = candidate,
        values_from = votes
      ) %>% 
      mutate(TOTAL = rowSums(across(where(is.numeric)), na.rm=TRUE))
    
  )
  
}

issue2Data = GetElectionInfo(data,"IsSuE 2")
mayorData = GetElectionInfo(data,"mayor")

# map example
# ISSUE 2 Proposed Constitutional Amendment TO FUND PUBLIC INFRASTRUCTURE CAPITAL
# IMPROVEMENTS BY PERMITTING THE ISSUANCE OF GENERAL OBLIGATION BONDS

map2020 <- st_zm(st_read("data/maps/PRECINCT_052219.shp"))

combinedData = map2020 %>% 
  left_join(issue2Data,c("PRECINCT" = "precinct"))
  
combinedData %>%
  mutate(YESPROP = YES/(YES+NO)) %>% 
  ggplot(aes(fill=YESPROP)) +
  geom_sf() +
  scale_fill_viridis_c(
    option = "viridis",
    direction = 1,       # 1 = normal, -1 reverses it
    na.value = "grey90"
  ) +
  labs(
    title = "Proportion of voters that voted FOR (the most recent election's) Issue 2"
  )
 

unlink(temp)
