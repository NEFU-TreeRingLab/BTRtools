# BTRtools

<!-- badges: start -->

<!-- badges: end -->
## We are addressing a bug that causes the program to crash and making the UI more user-friendly. The user manual will be uploaded together after the issue has been fixed.
**This is the Beta version of BTRtools**

*Detailed usage instructions may be updated on September 30th.*

BTRtools is a simple interactive interface on R developed based on Shiny to facilitate model parameterization.

note：Please install the R package "rBTR" before using it.


## Installation

You can install the development version of BTR model like so:

```r
# devtools::install_github("NEFU-TreeRingLab/rBTR")
```



### There are three functions to calibrate the parameters of BTR model.

* Funtion 1 : Fitting the Age Trend Curve for Growth

  ```R
  BTRtools::Trend_age( dt = Obs_Year  ) 
  ```

Data. Frame: **Obs_Year** is the Tree series data.

| Year | Age  | Ring width | Cell density | Max vessel lumen area | Vessel fraction |
| ---- | ---- | ---------- | ------------ | --------------------- | --------------- |
| 1960 | 1    | 3.98       | 84.96        | 1957.51               | 0.186           |
| 1961 | 2    | 3.49       | 84.96        | 1957.51               | 0.186           |
| 1962 | 3    | 2.11       | 37.09        | 1805.11               | 0.108           |

------

* Function 2 ： Calibration of Catheter and Fibroblast Growth Parameters

  ```R
  BTRtools::Sim_Cells(ObsV = ObsV,ObsF = ObsF,param = param )
  ```

  Data. Frame: **ObsV** is the Roxas output data (sheet cells) of vessels,

  Data. Frame: **ObsF** is the Roxas output data (sheet cells) of fiber cells,

  Data. Frame: **param** is the parameter list of BTR model.

  ------

* Function 3 : Calibration BTR mdoel's Parameters.

  ```R
  BTRtools::sim_btr(param = param,Tage = Tage,clims = clims,ObsF = ObsF , ObsV = ObsV )
  ```

  Data. Frame: **Tage** is the "Age Trend Curve" output by Funtion 1

  Data. Frame: **clims** is the daily climate data of sample site.

