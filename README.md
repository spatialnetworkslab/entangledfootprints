# Entangled Footprints: Understanding Urban Neighbourhoods by Measuring Variability of Distance, Diversity, and Direction of Flows in Singapore

<!-- [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh///master?urlpath=rstudio) -->

This repository contains the data and code for our paper:

> *Entangled Footprints: Understanding Urban Neighbourhoods 
> by Measuring Variability of Distance, Diversity, and Direction 
> of Flows in Singapore*. Computers, Environment and Urban Systems (CEUS)

## Introduction

<div style="text-align: justify">
  
Human mobility has been a long-standing interest in Geography and has been approached from both spatial and relational perspectives. Traditionally many such approaches use census or survey data that is resource-intensive to collect and often has a limited spatio-temporal scope. The advent of new technologies (e.g. geosocial media platforms) provides opportunities to overcome these limitations and, if properly treated, can yield more granular insights about human mobility. In this paper, we use a de-identified Location-Based Service (LBS) dataset sent in Singapore from 2012 to 2016 to investigate this potential regarding understanding the spatial footprints of urban neighbourhoods.
</div>

<div style="text-align: justify">
We construct home-to-destination networks of each user based on the inferred home locations. In aggregated form, the networks allows us to analyze three specific mobility indicators, namely the distance, diversity,  and direction of urban interactions, at the neighborhood level . By mapping these three characteristics of a spatial footprint of each neighbourhood, we can capture the nuances in the position of individual neighbourhoods within the larger urban system. Besides, we further conduct a spatial regression analysis to help illustrate the variation in distance and diversity of flows. The result reveals both socio-economic characteristics and built environment structure impact the mobility footprints, shaping different movement patterns within and across neighbourhoods.
</div>

## Contents

<div style="text-align: justify">

This repository contains all the data and code needed to reproduce the results and figures in this paper. Although we are not able to share the raw Twitter data publicly, the aggregated and de-identified Twitter dataset is found in `data/derived_data/`.

</div>

The steps needed to recreate the figures found in the paper can be found
in:

  - [analysis/001-design-base-map.Rmd](analysis/001-design-base-map.Rmd): Design base map
  - [analysis/002-dataset-information.Rmd](analysis/002-dataset-information.Rmd): Overview used de-identified LBS data
  - [analysis/003-create-hexagonal-grids.Rmd](003-create-hexagonal-grids.Rmd): Prepare hexagonal grids within Singapore boundary.
  - [analysis/004-identify-home-locations.Rmd](analysis/004-identify-home-locations.Rmd): Identify home locations for users.
  - [analysis/005-distinct-locals-and-visitors.Rmd](analysis/005-distinct-locals-and-visitors.Rmd): Distinguish users(local/visitor) fir each grid.
  - [analysis/006-calculate-distance.Rmd](analysis/006-calculate-distance.Rmd): Calculate and visualize distance of flows.
  - [analysis/007-define-sectors.Rmd](analysis/007-define-sectors.Rmd): Define circle sectors
  - [analysis/008-calculate-diversity.Rmd](analysis/008-calculate-diversity.Rmd): Calculate and visualize diversity of flows.
  - [analysis/009-draw-flow-maps.Rmd](analysis/07-draw-flow-maps.md): Calculate and visualize direction of flows.
  - [analysis/010-regression-analysis.Rmd](analysis/08-regression-analysis.md): Perform regression analysis

