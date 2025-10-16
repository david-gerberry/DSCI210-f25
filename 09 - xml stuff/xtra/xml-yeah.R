library(xml2)
library(dplyr)

# Load the XML file
xml_file <- read_xml("detail 10.xml")

# Extract precinct-level data
precincts <- xml_file %>%
  xml_find_all("//Precincts/Precinct")

# Create a data frame
precinct_df <- data.frame(
  name = xml_attr(precincts, "name"),
  totalVoters = as.integer(xml_attr(precincts, "totalVoters")),
  ballotsCast = as.integer(xml_attr(precincts, "ballotsCast")),
  voterTurnout = as.numeric(xml_attr(precincts, "voterTurnout")),
  percentReporting = as.numeric(xml_attr(precincts, "percentReporting")),
  stringsAsFactors = FALSE
)

# View the data frame
print(precinct_df)
