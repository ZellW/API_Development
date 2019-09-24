# LEARNING LAB 10 - PLUMBER API'S ----
# BASIC PLUMBER EXAMPLE ----

#* @apiTitle Plumber Example API

# LIBRARIES ----
library(plumber)
library(dygraphs)
library(tidyverse)


# 1.0 GET - parameterized ----

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = str_glue("The message is: {msg}"))
}

# 2.0 GET - Add Parameters as Query String ----

#* Plot a histogram
#* @png
#* @param n: Number of random normals
#* @get /plot
function(n = 100) {
    rand <- rnorm(n)
    hist(rand)
}

# 3.0 POST - Provide a and b as JSON ----

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}


# 4.0 GET - Start Year & End Year built into URL ----

#* @get /spots/<startYear>/<endYear>/graph
#* @serializer htmlwidget
function(startYear, endYear) {
    
    dygraphs::dygraph(
        datasets::sunspots, main = "Sunspots"
    ) %>%
        dyRangeSelector(dateWindow = c(paste0(startYear, "-01-01"),
                                       paste0(endYear, "-12-31")))
    
}
