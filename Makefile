# Include configuration variable from .env file
include .env
export $(shell sed 's/=.*//' .env)

# Define R interpeter for executing .R files
R_INTERPRETER = /usr/local/bin/Rscript

# Define project directory
PROJECT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SRC_DIR := $(PROJECT_DIR)/src
0_DATA_DIR := $(SRC_DIR)/0_data
1_EPI_DIR := $(SRC_DIR)/1_epidemic_overview
3_RT_DIR := $(SRC_DIR)/3_calculate_rt

# [CONTAINER MANGEMENT]
# Pull & tag container from GitHub package registry
#docker pull docker.pkg.github.com/epiforecasts/epinow2/epinow2:latest
#docker tag docker.pkg.github.com/epiforecasts/epinow2/epinow2:latest epinow2
# Build container with additional dependencies
build:
	#docker build . -t epinow2
	docker pull docker.pkg.github.com/epiforecasts/epinow2/epinow2:latest
	docker tag docker.pkg.github.com/epiforecasts/epinow2/epinow2:latest epinow2


# [CONTAINER MANGEMENT]
# Start Docker container for development (include volume mounts to data dirs)
up:
	docker run -d -p 8787:8787 \
	--mount type=bind,source=$(PROJECT_DIR),target=/home/epinow2 \
	--mount type=bind,source=$(DATA_DIR),target=/home/epinow2/data \
	--mount type=bind,source=$(OUTPUT_DIR),target=/home/epinow2/output \
	--mount type=bind,source=$(MOB_DIR),target=/home/epinow2/mobility_external \
	 --name epinow2 -e USER=epinow2 -e PASSWORD=epinow2 epinow2

# [CONTAINER MANGEMENT]
# Stop and remove Docker container
down:
	docker stop epinow2
	docker rm epinow2

# [STEP 0: DATA]
# Adjust manually extracted line list data to known epi weeks & admin 2 areas
process_line_list:
	$(R_INTERPRETER) $(0_DATA_DIR)/process_line_list.R

# Make spatial reference of combined region and district areas
standardise_spatial_reference:
	$(R_INTERPRETER) $(0_DATA_DIR)/standardise_spatial_reference.R

# Combine Municipal Assembly districts into single districts
adjust_municipal_areas:
	$(R_INTERPRETER) $(0_DATA_DIR)/adjust_municipal_areas.R

# Spatially Reference and Produce Standard Indicator from Google Mobility
process_google_mobility:
	$(R_INTERPRETER) $(0_DATA_DIR)/process_google_mobility.R

# Spatially Reference Vodafone Mobility
process_vodafone_mobility:
	$(R_INTERPRETER) $(0_DATA_DIR)/process_vodafone_mobility.R

# Produce Standard Indicator from Vodafone Mobility
vodafone_mobility_indicator:
	$(R_INTERPRETER) $(0_DATA_DIR)/vodafone_mobility_indicator.R

# Produce standardised mobility indicator from NPI data
interventions_to_index:
	$(R_INTERPRETER) $(0_DATA_DIR)/interventions_to_index.R

# [STEP 1: EPIDEMIC OVERVIEW]
# Plot overview of epidemic
plot_epidemic:
	$(R_INTERPRETER) $(1_EPI_DIR)/plot_epidemic.R

# [STEP 3: RT ESTIMATES]
# Prepare (filter) data for Rt estimation
prepare_rt_data:
	$(R_INTERPRETER) $(3_RT_DIR)/prepare_rt_data.R
