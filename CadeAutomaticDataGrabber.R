library(tidyverse) # All the tidy things
library(jsonlite)  # Converting json data into data frames
library(magrittr)  # Extracting items from list objects using piping grammar
library(httr)      # Interacting with HTTP verbs
library(xml2)
library(sf)


zip_file = "https://results.votehamiltoncountyohio.gov//OH/Hamilton/123661/358351/reports/detailxml.zip"
#unzip(zipfile = zip_file, exdir = getwd())
temp <- tempfile()

getXMLFile = function(file = zip_file) {
  download.file(file,temp)
  data <- read_xml(unz(temp, "detail.xml"))
  return(data)
}

readXMLFile = function(file) {
  data <- read_xml(file)
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
  turnout.data = turnout.data %>% 
    xml_find_all(".//VoterTurnout")
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
      mutate(TOTAL = rowSums(across(where(is.numeric)), na.rm=TRUE)) %>% 
      mutate(REGISTERED.VOTERS = floor( acs_interp_cincy$pop_totalE ))
    
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


turnout.df <- function(turnout.data){
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
a = turnout.df(data)

dataXML = getXMLFile()
data = prepXML(dataXML)

turnout_data = read_xml(dataXML) %>%
  xml_find_all("//Precincts/Precinct")
prec.turnout <- turnout.df(turnout_data)
NNN = nrow(prec.turnout)

## What do we want?

# we are able to get current electionvotes
# can we get the amount of registered voters? YES!

precinct_nodes <- xml_find_all(dataXML, "//VoterTurnout//Precinct")
votingNumberData <- map_dfr(precinct_nodes, ~ as.list(xml_attrs(.x))) %>% 
  rename()

# precinct reporting:
    # 0 = ?
    # 1 =  Nothing
    # 2 = ?
    # 3 = ?
    # 4 = complete

#### Map Election Results ####

##### define important stuff #####
results.df <- function(my.race, my.dem, my.rep, election_data) {
  ## Take the total election results and sum up votes for Presidential candidates
  vote_sums <- election_data  %>%
    filter(contest == my.race) %>% 
    group_by(candidate, vote_type) %>%
    summarize(total = sum(votes, na.rm = TRUE),.groups="keep") %>% 
    pivot_wider(names_from = vote_type, values_from = total) %>% 
    mutate(Overall = rowSums(across(everything()))) %>% 
    ungroup()
  
  column_sums <- vote_sums %>%
    select(where(is.numeric)) %>%
    summarize(across(everything(), ~ sum(.x, na.rm = TRUE)))
  vote_sums <- vote_sums %>%
    add_row(candidate = "Total", !!!column_sums)
  
  ## Pull out the Democratic and Republican candidates and calculate their percentages
  dem.row = vote_sums %>% filter(candidate == my.dem)
  rep.row = vote_sums %>% filter(candidate == my.rep)
  tot.row = vote_sums %>% filter(candidate == 'Total')
  
  output = rbind(round(100*dem.row[2:4]/tot.row[2:4],1),round(100*rep.row[2:4]/tot.row[2:4],1))
  rownames(output) = c(my.dem,my.rep)
  
  return(output)
}
precinct.results.df <- function(my.race, my.dem, my.rep, election.data) {
  ## Get the precinct-by-precinct election results for Democrat and Republican in head-to-head race
  early <- election_data  %>%
    filter(contest == my.race,vote_type=='Early') %>% 
    group_by(precinct) %>%
    summarize(early = sum(votes, na.rm = TRUE),.groups="keep") 
  
  election <- election_data  %>%
    filter(contest == my.race,vote_type %in% c("Election","Election Day")) %>% 
    group_by(precinct) %>%
    summarize(election = sum(votes, na.rm = TRUE),.groups="keep") 
  
  overall <- election_data  %>%
    filter(contest == my.race) %>% 
    group_by(precinct) %>%
    summarize(overall = sum(votes, na.rm = TRUE),.groups="keep")   
  
  dem.early <- election_data  %>%
    filter(contest == my.race,candidate==my.dem,vote_type=='Early') %>% 
    group_by(precinct) %>%
    summarize(dem.early = sum(votes, na.rm = TRUE),.groups="keep") 
  
  dem.election <- election_data  %>%
    filter(contest == my.race,candidate==my.dem,vote_type %in% c("Election","Election Day")) %>% 
    group_by(precinct) %>%
    summarize(dem.election = sum(votes, na.rm = TRUE),.groups="keep") 
  
  dem.overall <- election_data  %>%
    filter(contest == my.race,candidate==my.dem) %>% 
    group_by(precinct) %>%
    summarize(dem.overall = sum(votes, na.rm = TRUE),.groups="keep") 
  
  rep.early <- election_data  %>%
    filter(contest == my.race,candidate==my.rep,vote_type=='Early') %>% 
    group_by(precinct) %>%
    summarize(rep.early = sum(votes, na.rm = TRUE),.groups="keep") 
  
  rep.election <- election_data  %>%
    filter(contest == my.race,candidate==my.rep,vote_type%in% c("Election","Election Day")) %>% 
    group_by(precinct) %>%
    summarize(rep.election = sum(votes, na.rm = TRUE),.groups="keep") 
  
  rep.overall <- election_data  %>%
    filter(contest == my.race,candidate==my.rep) %>% 
    group_by(precinct) %>%
    summarize(rep.overall = sum(votes, na.rm = TRUE),.groups="keep")   
  
  output <-
    left_join(early,election,by='precinct') %>% 
    left_join(overall,by="precinct") %>% 
    left_join(dem.early,by="precinct") %>% 
    left_join(dem.election,by="precinct") %>% 
    left_join(dem.overall,by="precinct") %>% 
    left_join(rep.early,by="precinct") %>% 
    left_join(rep.election,by="precinct") %>% 
    left_join(rep.overall,by="precinct")
  
  return(output)
}


# Load the XML file and parse into a dataframe
election.data.2018 <- prepXML(readXMLFile("09 - xml stuff/old elections/detail2018.xml"))
election.data.2019 <- prepXML(readXMLFile("09 - xml stuff/old elections/detail2019.xml"))
election.data.2020 <- prepXML(readXMLFile("09 - xml stuff/old elections/detail2020.xml"))
election.data.2022 <- prepXML(readXMLFile("09 - xml stuff/old elections/detail2022.xml"))
election.data.2023 <- prepXML(readXMLFile("09 - xml stuff/old elections/detail2023.xml"))

# Load in the current precinct map and precinct Name Equivalency files
raw.map <- st_make_valid(st_zm(st_read("data/maps/precincts_2024.shp")))
name.equivalents <-readRDS("09 - xml stuff/Precinct_name_id_equiv2024.rds")

# Make sure map includes all of the possible names/labels for precincts
map <- left_join(raw.map,name.equivalents,by = c("PRECINCT" = "PRC_ID"))

map2019 <- st_make_valid(st_zm(st_read("data/maps/precincts_2019.shp"))) %>% 
  st_transform(., crs = st_crs(map))
map2020 <- st_make_valid(st_zm(st_read("data/maps/precincts_2020.shp"))) %>% 
  st_transform(., crs = st_crs(map))
map2023 <- st_make_valid(st_zm(st_read("data/maps/precincts_2023.shp"))) %>% 
  st_transform(., crs = st_crs(map))

##### example: 2020 presidential #####
my.year = "2020"
my.race = "For President and Vice President"
my.dem = "Joseph R. Biden / Kamala D. Harris"
my.rep = "Donald J. Trump / Michael R. Pence"
election_data = election.data.2020
my.map = map2020
my.name = "Biden.v.Trump.2020"

overall.results <- results.df(my.race,my.dem,my.rep,election_data)
prec.results <- precinct.results.df(my.race,my.dem,my.rep,election_data)
mapANDresults <- left_join(my.map,prec.results,by=c("PRECINCT"="precinct"))


# why wasn't this working? interpolate_pw doesn't exist! its st_interpolate_aw
interpolated.results <- st_interpolate_aw(
  x = st_make_valid(select(mapANDresults,where(is.numeric), geometry)),
  to = st_make_valid(map),
  extensive = TRUE
)

interpolated.results %>% 
  mutate(dem.prop = dem.overall/overall) %>%
  ggplot(aes(fill=dem.prop)) +
  geom_sf()+
  labs(title = paste(my.race,my.year,sep = ", "),
       subtitle = paste(my.dem,' (', overall.results$Overall[1],'%) vs ',my.rep,' (',overall.results$Overall[2],'%)',sep=''),
       fill = "", 
       caption = "")+
  scale_fill_gradientn(colours=brewer.pal(n=10,name="RdBu"),na.value = "transparent",
                       breaks=c(0,.25,0.5,.75,1),labels=c("100% Rep","","50%/50%","","100% Dem"),
                       limits=c(0,1))+
  theme_void()

##### silverstein #####

my.year = "2023"
my.race = "For Judge of Hamilton County Municipal Court (District 4) Full Term Commencing 1-3-2024"
my.dem = "Samantha Silverstein"
my.rep = "Curt Kissinger"
election_data = election.data.2023
my.map = map2023
my.name = "Silverstein.v.Kissinger.2023"

overall.results <- results.df(my.race,my.dem,my.rep,election_data)
prec.results <- precinct.results.df(my.race,my.dem,my.rep,election_data)
mapANDresults <- left_join(my.map,prec.results,by=c("NAME"="precinct"))


# why wasn't this working? interpolate_pw doesn't exist! its st_interpolate_aw
interpolated.results <- st_interpolate_aw(
  x = st_make_valid(select(mapANDresults,where(is.numeric), geometry)),
  to = st_make_valid(map),
  extensive = TRUE
) %>% 
  filter(if_any(where(is.numeric), ~ !is.na(.)))


interpolated.results %>% 
  mutate(dem.prop = dem.overall/overall) %>%
  ggplot(aes(fill=dem.prop)) +
  geom_sf()+
  labs(title = paste(my.race,my.year,sep = ", "),
       subtitle = paste(my.dem,' (', overall.results$Overall[1],'%) vs ',my.rep,' (',overall.results$Overall[2],'%)',sep=''),
       fill = "", 
       caption = "")+
  scale_fill_gradientn(colours=brewer.pal(n=10,name="RdBu"),na.value = "transparent",
                       breaks=c(0,.25,0.5,.75,1),labels=c("100% Rep","","50%/50%","","100% Dem"),
                       limits=c(0,1))+
  theme_void()

##### berky #####

my.year = "2019"
my.race = "For Judge of Hamilton County Municipal Court  District 4 (Full term commencing 01-05-2020)"
my.dem = "John Kennedy"
my.rep = "Josh Berkowitz"
election_data = election.data.2019
my.map = map2019
my.name = "Kennedy.v.Berkowitz.2019"

overall.results <- results.df(my.race,my.dem,my.rep,election_data)
prec.results <- precinct.results.df(my.race,my.dem,my.rep,election_data)
mapANDresults <- left_join(my.map,prec.results,by=c("PRECINCT"="precinct"))


# why wasn't this working? interpolate_pw doesn't exist! its st_interpolate_aw
interpolated.results <- st_interpolate_aw(
  x = st_make_valid(select(mapANDresults,where(is.numeric), geometry)),
  to = st_make_valid(map),
  extensive = TRUE
) %>% 
  filter(if_any(where(is.numeric), ~ !is.na(.)))


interpolated.results %>% 
  mutate(dem.prop = dem.overall/overall) %>%
  ggplot(aes(fill=dem.prop)) +
  geom_sf()+
  labs(title = paste(my.race,my.year,sep = ", "),
       subtitle = paste(my.dem,' (', overall.results$Overall[1],'%) vs ',my.rep,' (',overall.results$Overall[2],'%)',sep=''),
       fill = "", 
       caption = "")+
  scale_fill_gradientn(colours=brewer.pal(n=10,name="RdBu"),na.value = "transparent",
                       breaks=c(0,.25,0.5,.75,1),labels=c("100% Rep","","50%/50%","","100% Dem"),
                       limits=c(0,1))+
  theme_void()

##### but we can make this a function :D #####

generateRaceMap = function(my.year,
                           my.race,
                           my.dem,
                           my.rep,
                           election_data,
                           my.map,
                           my.name) {
  
  overall.results <- results.df(my.race,my.dem,my.rep,election_data)
  prec.results <- precinct.results.df(my.race,my.dem,my.rep,election_data)
  
  print( names(my.map) )
  if ("NAME" %in% names(my.map)) {
    my.map <- my.map %>% rename(precinct = NAME)
  } else if ("PRECINCT" %in% names(my.map)) {
    my.map <- my.map %>% rename(precinct = PRECINCT)
    
  }
  
  mapANDresults <- left_join(my.map,prec.results,by="precinct")
  
  
  # why wasn't this working? interpolate_pw doesn't exist! its st_interpolate_aw
  interpolated.results <- st_interpolate_aw(
    x = st_make_valid(select(mapANDresults,where(is.numeric), geometry)),
    to = st_make_valid(map),
    extensive = TRUE
  ) #%>% 
    #filter(if_any(where(is.numeric), ~ !is.na(.)))
  
  
  interpolated.results %>% 
    mutate(dem.prop = dem.overall/overall) %>%
    ggplot(aes(fill=dem.prop)) +
    geom_sf()+
    labs(title = paste(my.race,my.year,sep = ", "),
         subtitle = paste(my.dem,' (', overall.results$Overall[1],'%) vs ',my.rep,' (',overall.results$Overall[2],'%)',sep=''),
         fill = "", 
         caption = "")+
    scale_fill_gradientn(colours=brewer.pal(n=10,name="RdBu"),na.value = "transparent",
                         breaks=c(0,.25,0.5,.75,1),labels=c("100% Rep","","50%/50%","","100% Dem"),
                         limits=c(0,1))+
    theme_void()
}

# let's try it!

generateRaceMap(
  my.year = "2020",
  my.race = "For President and Vice President",
  my.dem = "Joseph R. Biden / Kamala D. Harris",
  my.rep = "Donald J. Trump / Michael R. Pence",
  election_data = election.data.2020,
  my.map = map2020,
  my.name = "Biden.v.Trump.2020"
)

generateRaceMap(
  my.year = "2023",
  my.race = "For Judge of Hamilton County Municipal Court (District 4) Full Term Commencing 1-3-2024",
  my.dem = "Samantha Silverstein",
  my.rep = "Curt Kissinger",
  election_data = election.data.2023,
  my.map = map2023,
  my.name = "Silverstein.v.Kissinger.2023"
)

generateRaceMap(
  my.year = "2019",
  my.race = "For Judge of Hamilton County Municipal Court  District 4 (Full term commencing 01-05-2020)",
  my.dem = "John Kennedy",
  my.rep = "Josh Berkowitz",
  election_data = election.data.2019,
  my.map = map2019,
  my.name = "Kennedy.v.Berkowitz.2019"
)
