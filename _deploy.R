# _deploy.R

library(rsconnect)
library(stringr)

support_files = c(
  'tmrtools.R',
  'data/tnc_bayarea_hh_labeled.rds'
)

app_file = '06_shiny_flex_crosstalk.Rmd'

deployApp(
  appFiles = c(support_files, app_file),
  appTitle = paste0('rted_demo_', str_replace(app_file, '[.](Rmd|R)', '')),
  forceUpdate=TRUE
)
