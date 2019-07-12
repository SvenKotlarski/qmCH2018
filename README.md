# qmCH2018

## What is `qmCH2018`?

**qmCH2018** is an implementation of quantile mapping (QM) based on empirical quantiles. The package has been developed at ETH Zurich and MeteoSwiss and is used for bias-correcting and downscaling EURO-CORDEX RCM output within the frame of the [CH2018 Swiss Climate Change Scenarios](www.ch2018.ch).

****

### Installation

The recommended procedure for installing the package is using the devtools package. 

```R
devtools::install_github("SvenKotlarski/qmCH2018")
```

Check the options of the main (wrapper) function for the application of the QM: 

```R
library(qmCH2018)
?qm.doqm
```

### Reference 

Users are required to reference the following publication:

* Rajczak, J. , Kotlarski, S. , Salzmann, N. and Schär, C. (2016), Robust climate scenarios for sites with sparse observations: a two‐step bias correction approach. *Int. J. Climatol.*, 36: 1226-1243. [doi:10.1002/joc.4417](https://rmets.onlinelibrary.wiley.com/doi/10.1002/joc.4417)


### License

GPLv3 (https://www.gnu.org/licenses/gpl-3.0.de.html)


### DOI

10.5281/zenodo.3275571 




