## ------------------------------------------------------------------------
library(devtools)
install_github("hsavoy/anchoredDistr")
library(anchoredDistr)
data(pumping)

## ---- fig.width=7.2, fig.height=4----------------------------------------
plotMAD(pumping, "realizations")

