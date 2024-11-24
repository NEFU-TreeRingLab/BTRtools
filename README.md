**Other Language Versions: [简体中文 ](README_zh_cn.md) **

# BTRtools

**This is the Beta version of `BTRtools`. If you have questions about using this R package or encounter issues during operation, please contact us.**

###### *A detailed user manual will be provided after the stable version of the R package is developed. However, it is unlikely to be available in the short term.*

`BTRtools` is a simple interactive interface on R developed based on Shiny to facilitate model parameterization.

- Note：Please install the R package `rBTR` before using it. [Link: rBTR](https://github.com/NEFU-TreeRingLab/rBTR)

- For more details, please refer to the following paper:

  > Zhao, B. *et al.* A process-based model of climate-driven xylogenesis and tree-ring formation in broad-leaved trees (BTR). *Tree Physiol.* **44**, tpae127 (2024).[DOI:[10.1093/treephys/tpae127](http://dx.doi.org/10.1093/treephys/tpae127) ]


## Installation

You can install packages by using:

```r
# Install rBTR package:
# devtools::install_github("NEFU-TreeRingLab/rBTR")

# Install BTRtools package:
# devtools::install_github("NEFU-TreeRingLab/BTRtools")
```


## How to use
#### `BTRtools` provides three functions to iteratively calibrate the parameters of the BTR model:
##### Step 1: Fit the age trend of tree growth
  ```R
  # BTRtools::trend_age() 
  ```
##### Step 2: Adjust parameters for intra-annual growth of tree cells (fiber and vessel cells)

```r
# BTRtools::cell_growth()
```

##### Step 3: Adjust parameters for inter-annual tree growth
```r
# BTRtools::calibrate_BTR()
```
###### Note: Example data required for parameter calibration is included in the R package. Before use, export the data as CSV or Excel files.
```r	
# Climate data input:
# data(Clims)
# write.csv(Clims, 'Clims.csv')
# write.xlsx(Clims, 'Clims.xlsx')

# Parameters:
# data(BPparam)
# write.csv(BPparam, 'BPparam.csv')
# write.xlsx(BPparam, 'BPparam.xlsx')

# Observed value of tree ring:
# data(BPring)
# write.csv(BPring, 'BPring.csv')
# write.xlsx(BPring, 'BPring.xlsx')

# Observed value of fiber cells:
# data(Cells)
# write.csv(Cells, 'Cells.csv')
# write.xlsx(Cells, 'Cells.xlsx')

# Observed value of vessels:
# data(Vessels)
# write.csv(Vessels, 'Vessels.csv')
# write.xlsx(Vessels, 'Vessels.xlsx')
```