# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                             #
#  CREATE HTML VARIABLE TABLE                                                 #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(dplyr)
library(purrr)
library(data.table)
library(openxlsx)
library(kableExtra)

source("utils/utils.R")
source("utils/token.R") # Private PAT


# 0. Data Import --------------------------------------------------------------

# Get study shorthands (all studies in database)
studies = getIPDStudies(TOKEN)

# Extract IPD
dat.list = lapply(as.list(studies), function(x) getIPD(x, TOKEN))

# Define required variables
req.variables = c("Author", "id.study", "id.patient", "group", "sex", "ethn", 
                  "age", "dep.status.0", "dep.status.1", "dep.status.2", 
                  "dep.status.3", "dep.status.4", "employment", "unemployment",
                  "daily.structure", "fin.security", "cm.dep.0",
                  "cm.dep.1", "cm.dep.2", "cm.dep.3", "cm.dep.4", "degree",
                  "rel", "prevpsychoth", "chron.med", "depmed", "anx.status.bl",
                  "child", "histo.mdd", "time.event.weeks", "censoring", 
                  "cm.anx.1", "cm.anx.2", "cm.anx.3", "cm.anx.4")

# Combine into one dataset
dat.list %>% 
  map(~select(.,one_of(req.variables))) %>% 
  data.table::rbindlist(fill=TRUE) %>% 
  as.data.frame() -> dat.merged

# Create index of available variables
dat.list %>% 
  map(~ req.variables %in% colnames(.)) %>% 
  do.call(rbind, .) %>%
  {colnames(.) = req.variables;
   rownames(.) = map_chr(dat.list,~unique(.$id.study));.} -> var.index

# Create overview table
var.index %>% 
  kbl(table.attr = "style = \"color: white;\"") %>% 
  kable_styling(fixed_thead = TRUE, bootstrap_options = "hover") %>% 
  row_spec(0, background = "gray", color = "white") %>% 
  column_spec(1, background = "gray") %>% 
  add_footnote(paste0("<span style='color: black;'> Last updated ",
                      Sys.time(), ".</span>"), 
               notation = "symbol", escape=FALSE) -> kbl

for (i in 1:ncol(var.index)) {
  kbl = column_spec(kbl, i+1, background = 
                      spec_color(var.index[,i], begin = 0.6, end = 0.9)) 
}

save_kable(kbl, file="index.html")