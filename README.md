# LTMR

Analysis code for 'Mourning Ritual Participation, Subjective Well-Being, and Prosocial Behavior among the Kenyan Luhya'. Data files removed for anonymity.

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```         
install.packages(
c("tidyverse",
  "brms",
  "psych",
  "patchwork",
  "GGally",
  "bayesplot",
  "WeightIt",
  "gbm",
  "marginaleffects")
)
```

Analyses from this manuscript were run in R version 4.4.1.

### Executing code

1.  Set your working directory in R to this code repository
2.  Load the `targets` package with `library(targets)`
3.  To run all analyses, run `tar_make()`
4.  To load individual targets into your environment, run `tar_load(targetName)`

To visualize how the various code and functions work together in the pipeline, run `targets::tar_visnetwork()`. Note that this pipeline will not run without the data files.
