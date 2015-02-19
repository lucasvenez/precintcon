# precintcon

`precintcon` is an R package with functions to analyze the precipitation intensity, 
concentration, and anomaly. It supports the following indices:

* Percent of Normal (PN)
* Deciles
* Concentration Index (CI)
* Precipitation Concentration Index (PCI)
* Standardized Precipitation Index (SPI) 
* Rainfall Anomaly Index (RAI)

To install:

* the latest released version: `install.packages("precintcon")`
* the latest development version: `install_git("lucasvenez/precintcon")`

Getting start:

```R
require(precintcon)

data(daily)

ci(daily, interval = 1)

pplot.lorenz(daily, interval = 1)
```