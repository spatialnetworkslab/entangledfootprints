
## Entangled footprints: Understanding urban neighbourhoods by measuring distance, diversity, and direction of flows in Singapore

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/spatialnetworkslab/entangledfootprints/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> Chen, Q., Chuang, I.-T. and Poorthuis, A. (2021) *Entangled
> footprints: Understanding urban neighbourhoods by measuring distance,
> diversity, and direction of flows in Singapore*. Computers, Computers,
> Environment and Urban Systems, 90, p. 101708.
> <doi:10.1016/j.compenvurbsys.2021.101708>

### Introduction

<div style="text-align: justify">

Traditional approaches to human mobility analysis in Geography often
rely on census or survey data that is resource-intensive to collect and
often has a limited spatio-temporal scope. The advent of new
technologies (e.g. geosocial media platforms) provides opportunities to
overcome these limitations and, if properly leveraged, can yield more
granular insights about human mobility. In this paper, we use an
anonymized Twitter dataset collected in Singapore from 2012 to 2016 to
investigate this potential to help understand the footprints of urban
neighbourhoods from both a spatial and a relational perspective..

</div>

<div style="text-align: justify">

We construct home-to-destination networks of individual users based on
their inferred home locations. In aggregated form, these networks allow
us to analyze three specific mobility indicators at the neighbourhood
level, namely the distance, diversity, and direction of urban
interactions. By mapping these three indicators of the spatial footprint
of each neighbourhood, we can capture the nuances in the position of
individual neighbourhoods within the larger urban network. An
exploratory spatial regression reveals that socio-economic
characteristics (e.g. share of rental housing) and the built environment
(i.e. land use) only partially explain these three indicators and a
residual analysis points to the need to explicitly include each
neighbourhood’s position within the transportation network in future
work.

</div>

### Content

This repository contains all the data and code needed to reproduce the
results and figures in our paper. The detailed steps can be found in:

-   [analysis/01-design-base-map.Rmd](analysis/01-design-base-map.md):
    Design base map.
-   [analysis/02-dataset-information.Rmd](analysis/02-dataset-information.md):
    Overview the basic information of the dataset used in the study.
-   [analysis/03-create-hexagonal-grids.Rmd](analysis/03-create-hexagonal-grids.md):
    Create 750m hexagonal grids for subsequent analyses.
-   [analysis/04-identify-home-locations.Rmd](analysis/04-identify-home-locations.md):
    Identify home locations of social media users.
-   [analysis/05-distinct-locals-and-visitors.Rmd](analysis/05-distinct-locals-and-visitors.md):
    Distinct locals and visitors.
-   [analysis/06-calculate-distance.Rmd](analysis/06-calculate-distance.md):
    Measure distance of flows.
-   [analysis/07-define-sectors.Rmd](analysis/07-define-sectors.md):
    Create concentric sectors.
-   [analysis/08-calculate-diversity.Rmd](analysis/08-calculate-diversity.md):
    Measure diversity of flows.
-   [analysis/09-draw-flow-maps.Rmd](analysis/09-draw-flow-maps.md):
    Measure direction of flows.
-   [analysis/10-regression-analysis.Rmd](analysis/10-regression-analysis.md):
    Conduct spatial regression analysis.

### How to download or install

You can install this compendium as an R package, entangledfootprints,
from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("spatialnetworkslab/entangledfootprints")
```

### Licenses

Text + figures and data:
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: See the [DESCRIPTION](DESCRIPTION) file
