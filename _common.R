# Load libraries
# library(gt)
# library(gtExtras)
library(knitr)
library(gt)
library(gtExtras)
library(patchwork)


# Set knitr print options
opts_knit$set(
  width = 85, 
  tibble.print_max = Inf
)


# Set knitr chunk options
opts_chunk$set(
  
  # General
  comment = NA,
  message = FALSE, 
  warning = FALSE,
  tidy = FALSE,
  
  # Figures
  fig.align = 'center',
  fig.height = 6,
  fig.width = 6,  
  out.width = '70%' 
)


# Set global options
options(
  htmltools.dir.version = FALSE,
  tibble.pillar.subtle = FALSE, 
  tibble.pillar.sigfig = 7, 
  tibble.pillar.min_title_chars = 10,
  scipen = 5
)