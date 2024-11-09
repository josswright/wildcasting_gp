suppressPackageStartupMessages({

library( tidyverse )
library( purrr )
library( lubridate )

library( showtext )

library( scales ) # Improve scales for output plot

library( gganimate ) # Animate time series plots

library( grimoire ) # devtools::install_github( "weirddatascience/grimoire" )

library( cli ) # For message colouring and progress bars

})

# Load the generated network data.
# (From test_data_generate.r)
wildcast_network <- readRDS( "work/wildcast_network.rds" )

# Extract the list components to individual elements
event_tbl <- wildcast_network$events
date_list <- wildcast_network$dates
port_list <- wildcast_network$ports

# Fonts for plotting
font_add_google( name="Jost", family="main" )
showtext_auto()

# Create example time series plots of shipments.

# Function for plotting only integer y-axis values.
integer_breaks <- function( n = 5, ...) {
    breaks_floor <- function(x) {
        breaks <- floor( pretty( x, n, ...) )
        names( breaks ) <- attr( breaks, "labels" )
        return( breaks )
    }
    return( breaks_floor )
}

# Tally shipment counts (daily)
# Use complete to ensure that there is an entry for each day for each port.
shipment_events_port_source <-
	event_tbl %>%
	group_by( date, port_source ) %>%
	tally() %>%
	ungroup %>%
	complete( date=date_list ) %>%
	complete( date, port_source, fill=list( n=0 ) ) %>%
	filter( !is.na( port_source ) ) 

# Plot the data for all shipments
shipment_events_port_source_plot <-
	ggplot( shipment_events_port_source ) +
	geom_line( aes( x=date, y=n, colour=port_source ) ) +
	scale_y_continuous( breaks=integer_breaks(), limits=c(0,NA) ) +	# Ensure integers on the y axis and include 0
	facet_wrap( vars( port_source ), ncol=1, scales="fixed" ) + # Facet per port
	labs( x="Date", y="Shipment count", colour="Source Port" ) + 
	ggtitle( "Generated Shipment Time Series" ) +
	theme_weird() + 
	theme( axis.title.y = element_text( angle=90 ) )

# Plot the data for only observed shipments at destination ports
shipment_events_port_source_observed <-
	event_tbl %>%
	filter( detected_destination == 1 ) %>%
	group_by( date, port_source ) %>%
	tally() %>%
	ungroup %>%
	complete( date=date_list ) %>%
	complete( date, port_source, fill=list( n=0 ) ) %>%
	filter( !is.na( port_source ) ) 

shipment_events_port_source_observed_plot <-
	ggplot( shipment_events_port_source_observed ) +
	geom_line( aes( x=date, y=n, colour=port_source ) ) +
	scale_y_continuous( breaks=integer_breaks(), limits=c(0,NA) ) +	# Ensure integers on the y axis and include 0
	facet_wrap( vars( port_source ), ncol=1, scales="fixed" ) + # Facet per port
	labs( x="Date", y="Shipment count", colour="Source Port" ) + 
	ggtitle( "Generated Shipment Time Series" ) +
	theme_weird() + 
	theme( axis.title.y = element_text( angle=90 ) )

# Combined version of the two datasets
shipment_events_port_source_combined <-
	shipment_events_port_source %>%
	left_join( shipment_events_port_source_observed, by=c( "date", "port_source" ) ) %>% 
	rename( n = n.x, n_observed = n.y ) %>%
	replace_na( list( n_observed=0 ) ) %>% # Replace missing observed values with 0
	pivot_longer( cols=c( n, n_observed ), names_to="event_type" ) # Pivot to longer table for plotting

# Plot the two series against each other
shipment_events_port_source_combined_plot <-
	ggplot( shipment_events_port_source_combined ) +
	geom_line( aes( x=date, y=value, colour=event_type ) ) +
	scale_colour_manual( values=c( weird_colours[["midnight blue"]], weird_colours[["carcosa yellow"]] ),
							  	breaks=c( "n", "n_observed" ),
								labels=c( "Shipment", "Observed Shipment" ) ) +
	scale_y_continuous( breaks=integer_breaks(), limits=c(0,NA) ) +	# Ensure integers on the y axis and include 0
	facet_wrap( vars( port_source ), ncol=1, scales="fixed" ) + # Facet per port
	labs( x="Date", y="Shipment Count", colour="Source Port" ) + 
	ggtitle( "Generated Shipment Time Series" ) +
	theme_weird() + 
	theme( axis.title.y = element_text( angle=90 ) )

# Save plots to output directory
dir.create( "output", showWarnings = FALSE )
ggsave( shipment_events_port_source_plot, file="output/shipment_events_port_source_plot.pdf", width=16, height=9 )
ggsave( shipment_events_port_source_observed_plot, file="output/shipment_events_port_source_observed_plot.pdf", width=16, height=9 )
ggsave( shipment_events_port_source_combined_plot, file="output/shipment_events_port_source_combined_plot.pdf", width=16, height=9 )

# Animated version of time series events for single port

# Choose a port and restrict to only that time series
shipment_events_port_source_single <-
	shipment_events_port_source %>%
	filter( port_source == head( port_list, 1 ) )

# Example time series animation
shipment_events_port_source_animation <-
	ggplot( shipment_events_port_source_single ) +
	geom_line( aes( x=date, y=n ), colour=weird_colours[["carcosa yellow"]] ) +
	scale_y_continuous( breaks=integer_breaks(), limits=c(0,NA) ) +	# Ensure integers on the y axis and include 0
	labs( x="Date", y="Shipment count", colour="Source Port" ) + 
	ggtitle( "Generated Shipment Time Series" ) +
	theme_weird() + 
	theme( axis.title.y = element_text( angle=90 ) ) +
	transition_reveal( date )

animate( shipment_events_port_source_animation, 
		  	width = 1200,
		  	height = 720,
		  	renderer = gifski_renderer( loop = FALSE ) )



