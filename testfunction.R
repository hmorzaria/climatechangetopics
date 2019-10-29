get_species <- function(species_name){
  
  name.species <- paste("I like",species_name)
  
  return(name.species)
}

myurl <- "http://13.65.45.125:6002/species/"

species <- "penguin"

# Convert the list to JSON for posting
json.body <- toJSON(species)

payload <- get_species("penguin")

url.service <- paste(myurl,species,sep="")

raw.response <- GET(url.service)

raw.response <- POST(url.service, body = payload)

# Read in the response as character
json.response <- readBin(raw.response$content, "character")

print(json.response)
# Convert the character, now JSON, response back to a list
list.response <- fromJSON(json.response)
