library( tidyverse )
library( purrr )
library( lubridate )

# Generate a set of sporadic observations from a known underlying process.

# Network Parameters
# n_ports 			- Number of potential sites where seizure can be observed
# n_products 		- Number of products traded in the network
# n_source			- Potential number of source products for each port
# n_dest				- Potential number of destination products for each port

n_ports <- 12
n_products <- 12
n_source <- 5
n_dest <- 5


# Shipment Parameters
# p_shipment		- Probability that a shipment happens on a given day for each source port/product.
# p_ship_detect 	- Probability that a shipment is detected at source. 
# p_dest_detect 	- Probability that a shipment is detected at destination.

p_shipment <- 0.5
p_source_detect <- 0.05
p_dest_detect <- 0.05

# Randomly generate names for ports and species
name_components <- read_csv( "data/name_components.csv", show_col_types=FALSE )

# Function to generate a name from name component dataset
generate_name <- function( components, entity_type ) {

	# Empty name and components
	name <- ""
	prefix <- ""
	suffix <- ""

	# Species have a 30% chance of a prefix, but always a suffix.
	# Ports have a 50% chance of having a suffix. Species always.
	p_prefix <- ifelse( entity_type == "species", 70, 100 ) 
	p_suffix <- ifelse( entity_type == "port", 50, 100 ) 

	# Combine "prefix", "main", and "suffix"

	# Random prefix, if required.
	if( sample( 1:100, 1 ) <= p_prefix ) 
		prefix <- 
			components %>%
			filter( ( type==entity_type ) & ( component=="prefix" ) ) %>%
			sample_n( 1 ) %>%
			pull( value )

	# Random main
	main <- 
		components %>%
		filter( ( type==entity_type ) & ( component=="main" ) ) %>%
		sample_n( 1 ) %>%
		pull( value )

	# Random suffix, if required.
	if( sample( 1:100, 1 ) <= p_suffix ) 
		suffix <- 
			components %>%
			filter( ( type==entity_type ) & ( component=="suffix" ) ) %>%
			sample_n( 1 ) %>%
			pull( value )


	# Combine components.
	name <- str_trim( paste( prefix, main, suffix ) )

	# Return the generated name
	return( name )

}

# Generate the list of species
product_list <- c() 
for( i in 1:n_products ) 
	product_list <- c( product_list, generate_name( name_components, "species" ) )

# Randomly create a trade network.
# Each port is randomly assigned:
#  - up to n_source products for which it is a source (products may be shipped from here). 
#  - up to n_dest products for which it is a dest (products may be shipped to here).

# Assume that every source port is willing to create shipments to every
# destination port that desires that product.

# The network structure will be a dataframe in the following form:
# source port, destination port, product, p_shipment

# Generate the data frames of ports.
# One contains all ports and the products they ship, and the probability of a shipment.
# The other is a list of ports that accept certain products.
# Note that each pairing between the data frames will be a unique "trade opportunity" that can occur.

# Empty data frames.
port_source_tbl<- NULL
port_dest_tbl<- NULL

for( i in 1:n_ports ) {

	name <- generate_name( name_components, "port" ) 
	n_source_actual <- sample( 1:n_source, 1 )
	n_dest_actual <- sample( 1:n_dest, 1 )

	# Choose products created here
	source_product_list <-
		sample( product_list, n_source_actual )

	# Choose products accepted here.
	# (Only choose from products not produced here.
	dest_product_list <-
		sample( product_list[ !product_list %in% source_product_list ], n_dest_actual )

	# Generate table entries produced here
	for( source_product in source_product_list ) {
		port_source_tbl <- 
			bind_rows( port_source_tbl, tibble( name=name, product=source_product, probability=p_shipment ) )
	}

	# Generate table entries consumed here
	for( dest_product in dest_product_list ) {
		port_dest_tbl <- 
			bind_rows( port_dest_tbl, tibble_row( name, product=dest_product ) )
	}

}

# Check that each source product has a destination, and vice-versa. If not,
# drop un-reciprocated products from the tables.
port_source_tbl <-
	port_source_tbl %>%
	filter( product %in% port_dest_tbl$product )

port_dest_tbl <-
	port_dest_tbl %>%
	filter( product %in% port_source_tbl$product )


# Sample each source port at each day to create a time series of events, in the
# following form: date, port_source, port_destination, product, quantity,
# detected_source, detected_destination

# Function to take a single source port entry row and generate a shipment
# event, as described in the previous comment.
generate_event <- function( name, product, probability, event_date, port_dest_tbl ) {

	# A row to store the event if it occurs
	event_row <- NULL

	event_product <- product
	event_probability <- 0.5

	# If the shipment occurred, work out the details.
	if( rbinom( 1, size=1, prob=event_probability )==1 ) {

		# Filter the destination tibble only to those that accept this product, then randomly choose one.
		event_destination <-
			port_dest_tbl %>%
			filter( product == event_product ) %>%
			sample_n( 1 ) %>%
			pull( name )

		# Calculate detection occurrence. Detection is only possible at destination if not detected at source.
		detected_destination <- 0
		detected_source <- rbinom( 1, size=1, prob=p_source_detect )
		if( detected_source==0 ) {
			detected_destination <- rbinom( 1, size=1, prob=p_dest_detect )
		}

		# Construct the result row
		event_row <-
			tibble_row( date=event_date, 
						  	port_source=name,
							port_destination=event_destination,
							product=product,
							quantity=sample( 1:10, 1 ),		# Fixed for now. Should be defined in product tbl.
							detected_source=detected_source,
							detected_destination=detected_destination )

	}

	return( event_row )

}

# The dates of interest
date_list <-
	seq( ymd( "1900-01-01" ), ymd("1900-12-31"), by="day" )

# Step through the dates and generate a tibble of trade events
event_list <- NULL
for( event_date in date_list ) {

	event_date <- as.Date( event_date )

	message( paste0( "Generating events for: ", event_date ) )

	# Generate the day's events
	daily_event <-
		pmap( port_source_tbl, generate_event, event_date=event_date, port_dest_tbl=port_dest_tbl ) %>%
		bind_rows

	# Add the day's events to the overall list
	event_list <- 
		event_list %>%
		bind_rows( daily_event )

}

# Save the event list
dir.create( "work", showWarnings = FALSE )
saveRDS( event_list, "work/event_list.rds" )
