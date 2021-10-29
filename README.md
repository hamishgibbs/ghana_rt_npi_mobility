## Ghana NPI Analysis

Repository for the analysis of the impact of Non-pharmaceutical Interventions in Ghana.

*Estimating the relationship between mobility, non-pharmaceutical interventions, and COVID-19 transmission in Ghana*

#### Usage

This analysis is conducted in the [EpiNow2](https://github.com/epiforecasts/EpiNow2/wiki/Docker) Docker image, based on the [rocker/verse](https://hub.docker.com/r/rocker/verse) Docker image.

To start developing in this container, follow the instructing in the [EpiNow2 Wiki](https://github.com/epiforecasts/EpiNow2/wiki/Docker).

Start the docker container for development:

```
docker run -d -p 8787:8787 --name epinow2 -e USER=epinow2 -e PASSWORD=epinow2 epinow2
```

You can mount a volume with the `--mount` argument to specify the location of input datasets.

The `Makefile` includes convenience commands for interacting with the Docker container.

To download the latest EpiNow2 image (requires authentication with GitHub Packages):

```
make build
```

To run a container based on the EpiNow2 image (remember to alter volume mounts in the `Makefile`):

```
make up
```

To stop and remove the Docker container (tagged as `epinow2`):

```
make down
```

#### Environment Variables

Environment variables are stored in a project configuration `.env` file stroed in the project root. This file is not tracked in version control.

Environment variables:

`MOB_DIR`: Directory where mobility data is located  
`DATA_DIR`: Directory where remaining data is located  
`OUTPUT_DIR`: Directory for output of figures  

#### Data Requirements

**Line List Data**

Line list data is provided by Ghana Health Service. Terms of data use do not permit redistribution of the data. Original data was provided in 15 district-specific line lists.

Initial data cleaning was conducted manually. Key data was extracted to standard format `.csv` files (`region`, `district`, `epi_week`, `specimen_date`). Lookups were produced to transform coded (`district` and `epi_week`) values to standard format.

Coded value lookup files are included in version control:

`config/cases/spatial_lookups` - Lookup files assigning original administrative districts to known administrative level 2 areas.
`config/cases/epi_week_lookups` - Lookup files assigning epi weeks to standard format.

Original (manually extracted) line list files are cleaned/standardised using the `make` command:

```{shell}
make clean_cases
```

#### Analysis

This analysis is organised in sections which correspond to the sections of the publication.

#### Code Dependencies

Some of the analysis relies on the custom plotting library (not available from CRAN):

[`hamishgibbs/ggutils`](https://github.com/hamishgibbs/ggutils)

Install this package using `devtools`:

```{shell}
devtools::install_github("hamishgibbs/ggutils")
```

A version change from `sf 0.9.6` to `sf 1.0` introduced unexpected behaviour. Recommend using `sf 0.9.6`.

```{shell}
packageurl <- "http://cran.r-project.org/src/contrib/Archive/sf/sf_0.9-6.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

#### Data Availability

Please note that this analysis uses private line list data collected and shared under an agreement with the Ghana Health Service. The analysis also uses private mobility data collected and shared under and agreement with Vodafone Ghana. The terms of data use prohibit redistribution of both datasets. Protected data is not stored or tracked in the source repository.
