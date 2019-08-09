## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(valetr)

## ----eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE------------------
cpis <- getSeriesData(series=c("V41690973","V41693242"), recent_years=21L) # get data

# load data.table
library(data.table)

# calcule the CPI %
cpis <- lapply(cpis, function(x){
  # change last col name to cpi
  names(x)[length(x)] <- "value"
  x[["value"]] <- as.numeric(x[["value"]]) # cpi val as numeric
  x[["d"]] <- format(as.Date(x[["d"]]), "%Y-%m") # format date
  data.table::setDT(x)
  # calculate % change over the past 12 mo
  x[, chng := value/data.table::shift(value, n = 12) - 1]
  # remove NAs (due to lag)
  x <- x[!is.na(chng), ]
  return(x)
})

# bind datasets
cpis <- do.call(rbind, cpis)

head(cpis)

## ----eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE------------------
# compare series
library(ggplot2)
ggplot(cpis,
       aes(y=chng, x=d, group=series_label, colour=series_label)
       ) + 
  geom_line() + 
  labs(x=NULL, 
       y = "% change over the past 12 months",
       title="Core CPI series appears less\nvolatile than Total CPI",
       caption=paste0("Valet API series: ",
                      paste0(unique(cpis[["series_name"]]),collapse =","))) + 
  scale_y_continuous(labels=function(x) paste0(round(x*100, 2), "%")) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        panel.grid = element_blank(), 
        legend.title = element_blank(),
        legend.position = c(.8,.85))

## ----eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE------------------
# capture m aggregates 
series <- regexSeriesLabel(pattern="(?i)m\\d\\+{1,} \\(gross\\)")

## ----eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE-----------------
# capture m aggregates 
series

## ----eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE------------------
fxsInfo <- getSeriesInfo(patternGroupLabel = "exchange rate",
                         ignore.case=TRUE)

## ----eval=TRUE, echo=TRUE, message=FALSE,warning=FALSE-------------------
head(fxsInfo)

