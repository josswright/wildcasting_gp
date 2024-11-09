suppressPackageStartupMessages({

library( tidyverse )
library( purrr )
library( lubridate )

library( extraDistr ) # Some useful extra probability distributions

#library ( simts )	# Time series simulation

library( cli ) # For message colouring and progress bars

})

# Generate a set of sporadic observations for trade between randomised ports,
# for various products, from a known underlying process.

# Network Parameters
# n_ports 			- Number of potential sites where seizure can be observed
# n_products 		- Number of products traded in the network
# n_source			- Potential number of source products for each port
# n_dest				- Potential number of destination products for each port

n_ports <- 		12
n_products <- 	12
n_source <- 	5
n_dest <- 		5

# Random parameters for generating ports and shipments

# p_detect_min 	- Define range of detection probabilities. (runif())
# p_detect_max 	
p_detect_min 		<- 0.01
p_detect_max 		<- 0.1

# p_gen_size_scale 	- Sigma parameter for per-port randomisation of product size parameters (rnbinom)
# p_gen_prob_alpha - Alpha parameter for per-port randomisation of product probability parameters (rbeta)
# p_gen_prob_beta - Beta parameter for per-port randomisation of product probability parameters (rbeta)
p_gen_size_alpha <- 1
p_gen_size_beta <- 1
p_gen_prob_alpha 	<- 2 
p_gen_prob_beta 	<- 2

# Dates of interest. (Irrelevant, really, but it's intuitive to work with dates.)
date_list <-
	seq( ymd( "1900-01-01" ), ymd("1900-12-31"), by="day" )

# Attempt to clear the console. (There is no good way to do this cross-platform
# for all environments, it seems, but this is fairly robust.)
if (Sys.info()["sysname"] == "Windows") {
    # For Windows
    shell("cls", intern = TRUE)
} else {
    # For Mac and Linux
    cat("\033[2J\033[1;1H")
}

# Report start of processing
msg_width <- 56
value_width <- 8
message( col_blue( "Wildcasting Trade Network Simulator" ) )
message( paste0( {col_green(symbol$play)},  "  Generating trade network and events." ) )
message( paste0( {col_green(symbol$play)},  "  Requested parameters:" ) )
message( paste( str_pad( paste( "  ", {col_yellow(symbol$bullet)},  "Ports:" ), msg_width, side="right" ), str_pad( n_ports, value_width, side="left" ) ) )
message( paste( str_pad( paste( "  ", {col_yellow(symbol$bullet)},  "Products:" ), msg_width, side="right" ), str_pad( n_products, value_width, side="left" ) ) )
message( paste( str_pad( paste( "  ", {col_yellow(symbol$bullet)},  "Port production limit:" ), msg_width, side="right" ), str_pad( n_source, value_width, side="left" ) ) )
message( paste( str_pad( paste( "  ", {col_yellow(symbol$bullet)},  "Port consumption limit:" ), msg_width, side="right" ), str_pad( n_dest, value_width, side="left" ) ) )
message()

# Randomly generate names for ports and products
name_components <- read_csv( "data/name_components.csv", show_col_types=FALSE )

# Function to generate a name from name component dataset
generate_name <- function( components, entity_type ) {

	# Empty name and components
	name <- ""
	prefix <- ""
	suffix <- ""

	# Species have a 30% chance of a prefix, but always a suffix.
	# Ports have a 50% chance of having a suffix. Species always have a suffix.
	p_prefix <- ifelse( entity_type == "product", 0.7, 1 ) 
	p_suffix <- ifelse( entity_type == "port", 0.5, 1 ) 

	# Combine "prefix", "main", and "suffix"

	# Random prefix, if required.
	if( rbinom( 1, size=1, prob=p_prefix ) == 1 ) {
		prefix <- 
			components %>%
			filter( ( type==entity_type ) & ( component=="prefix" ) ) %>%
			sample_n( 1 ) %>%
			pull( value )
	}

	# Random main
	main <- 
		components %>%
		filter( ( type==entity_type ) & ( component=="main" ) ) %>%
		sample_n( 1 ) %>%
		pull( value )

	# Random suffix, if required.
	if( rbinom( 1, size=1, prob=p_suffix ) == 1 ) {
		suffix <- 
			components %>%
			filter( ( type==entity_type ) & ( component=="suffix" ) ) %>%
			sample_n( 1 ) %>%
			pull( value )
	}

	# Combine components.
	name <- str_trim( paste( prefix, main, suffix ) )

	# Return the generated name
	return( name )

}

# Generate the list of products
product_list <- c() 
for( i in 1:n_products ) 
	product_list <- c( product_list, generate_name( name_components, "product" ) )

message( paste0( {col_green(symbol$tick)},  "  Generated ", length( product_list ), " products: " ) )
print( product_list )
message( )

# Randomly create a trade network.
# Each port is randomly assigned:
#  - up to n_source products for which it is a source (products may be shipped from here). 
#  - up to n_dest products for which it is a dest (products may be shipped to here).

# Assume that every source port is willing to create shipments to every
# destination port that desires that product.

# The network structure will be a dataframe in the following form:
# source port, destination port, product

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

	# Create source detection probability. (Chance that a product shipment will be detected at this port.)
	source_product_detection <- runif( 1, p_detect_min, p_detect_max )

	# Generate table entries produced here
	for( source_product in source_product_list ) {

		# Generate random production parameters for this product.
		p_gen_size <- rbeta( 1, p_gen_size_alpha, p_gen_size_beta ) / 10 
		p_gen_prob <- rbeta( 1, p_gen_prob_alpha, p_gen_prob_beta ) / 10 

		port_source_tbl <- 
			bind_rows( port_source_tbl, tibble( name=name, product=source_product, p_size=p_gen_size, p_prob=p_gen_prob, p_detect=source_product_detection ) )
	}

	# Generate table entries consumed here
	for( dest_product in dest_product_list ) {

		# Create destination detection probability. (Chance that a product arrival will be detected at this port.)
		dest_product_detection <- runif( 1, p_detect_min, p_detect_max )

		port_dest_tbl <- 
			bind_rows( port_dest_tbl, tibble_row( name, product=dest_product, p_detect=dest_product_detection ) )
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

# Report ports
message( paste0( {col_green(symbol$tick)},  "  Generated ", n_ports, " ports: " ) )
port_list <-
	bind_rows( port_source_tbl, port_dest_tbl ) %>%
	pull( name ) %>%
	unique

print( port_list )
message()

# Sample each source port at each day to create a time series of events, in the
# following form: date, port_source, port_destination, product, quantity,
# detected_source, detected_destination

# Function to take a single source port entry row and generate a shipment
# event, as described in the previous comment.
generate_event <- function( name, product, p_size, p_prob, p_detect, event_date, port_dest_tbl ) {

	# A row to store the event if it occurs
	event_row <- NULL

	event_name <- name
	event_product <- product

	# Number of shipments
	event_shipment_count <- rnbinom( 1, size=p_size, prob=p_prob )

	# If the shipment occurred, work out the details.
	if( event_shipment_count > 0 ) {

		# Filter the destination tibble only to those that accept this product, then randomly choose one.
		event_destination <-
			port_dest_tbl %>%
			filter( product == event_product ) %>%
			sample_n( 1 ) %>%
			select( name, p_detect )

		# Calculate detection occurrence. Detection is only possible at destination if not detected at source.
		detected_destination <- 0
		detected_source <- rbinom( 1, size=1, prob=p_detect )

		if( detected_source==0 ) {
			detected_destination <- rbinom( 1, size=1, prob=event_destination$p_detect )
		}

		# Construct the result row
		event_row <-
			tibble_row( date=event_date, 
						  	port_source=name,
							port_destination=event_destination$name,
							product=product,
							quantity=event_shipment_count,	
							detected_source=detected_source,
							detected_destination=detected_destination )

	}

	return( event_row )

}

# Function to generate events for a single day
generate_day_events <- function( event_date, port_source_tbl, port_dest_tbl ) {

	event_date <- as.Date( event_date )

	# Generate the day's events
	day_events <-
		pmap( port_source_tbl, generate_event, 
			  	event_date=event_date, port_dest_tbl=port_dest_tbl ) %>%
		bind_rows

	return( day_events )

}

# Progress bar updating wrapper around the generate_day_events function
generate_day_events_progressive <- 
	function( event_date, port_source_tbl, port_dest_tbl, progress_bar ) {

	day_events <- generate_day_events( event_date, port_source_tbl, port_dest_tbl )

	# Update reporting variables
	pb_date <- as.Date( event_date )
	#pb_total_events <- pb_total_events + nrow( day_events )

	cli_progress_update( id=progress_bar, force=TRUE )

	return( day_events )

}

# Suitably garish progress bar format
options( cli.spinner = "moon",
		  	cli.progress_bar_style = "fillsquares",
		  	cli.progress_show_after = 0 )

progress_format <- 
	paste0(	col_green("{pb_spin}"), "Generating events for ", 
			 	col_blue( "{pb_date}" ), ": ", 
				col_green("["), col_yellow("{pb_bar}"), col_green("]"),
				" [{pb_current}/{pb_total}] ",
				"ETA:", 
				col_yellow( "{pb_eta}" )
	)

# Progress bar
event_pb <- 
	cli_progress_bar( format = progress_format,
							clear = FALSE,
							total = length( date_list ) )

# Step through the dates and generate a tibble of trade events
event_tbl <-
	map( 	date_list, 
		 	generate_day_events_progressive, port_source_tbl, port_dest_tbl,
			progress_bar=event_pb ) %>%
	list_rbind

# Close the progress bar to avoid polluting the environment
cli_progress_done()

# Report completion.
message( paste0( {col_green(symbol$tick)},  "  Generated ", nrow( event_tbl ), " events." ) )

# Save the port list, event list, and parameters
dir.create( "work", showWarnings = FALSE )

wildcast_network <- 
	list(	ports 		=	port_list,
			products		=	product_list,
			dates 		= 	date_list, 
		  	port_source = 	port_source_tbl,
		  	port_dest 	= 	port_dest_tbl,
		  	events 		= 	event_tbl )

saveRDS( wildcast_network, "work/wildcast_network.rds" )
