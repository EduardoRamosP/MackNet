# MackNet
R package for running MackNet model for paid and incurred loss data. The package also includes the [NAIC Schedule P Database](https://www.casact.org/research/index.cfm?fa=loss_reserves_data) provided by CAS and some functions to obtain the triangles from the raw data. This database has been widely used for validating general insurance reserving models.

# Example of MackNet implementation
The MackNet package can be installed by cloning the repo and build the R package or installing with
``` r
require(devtools)
install_github("EduardoRamosP/MackNet")
```
MackNet library is liaded in order to obtain NAIC Schedule P Data and fit the reserving models
``` r
library(MackNet)
```

The first company of Commercial Auto is selected. Then, the paid and incurred cost triangles are obtained. As MackNet models requires the use of an exposure measure, the premiums of that company are also saved in a variable.
``` r
CompanyCode <- CA_TriangleSelectionOrderedRt[,"Company_Code"][1]               #Company code is selected
Cumulative <- CumulativeT.SchedudeP(ins.line.data(CompanyCode,comauto_pos))    #Cumulative payments triangle
Incurred <- IncurredT.SchedudeP(ins.line.data(CompanyCode,comauto_pos))        #Incurred cost triangle
Premium <- NetEarned.SchedudeP(ins.line.data(CompanyCode,comauto_pos))         #Premium vector (Premiums)
```

Once the input data is calculated, the MackNet model is fitted as follows:
``` r
MackNetPaidModel=MackNet_Paid(Cumulative, Incurred, Premium, Ensemble=20,wd=0, drop=0.05, Output="linear",
                              MinimumEpochs = 700, Epochs=1000, EarlyStoppingPatience=50, Learning=0.01)
```
The implementation carried out allows the user to modify the following variables of the MackNet model:
- Item 1
- Item 2


#Model for incurred loss data is fitted
``` r
MackNetIncModel=MackNet_Incurred(Cumulative, Incurred, Premium, Ensemble=20, wd=0, drop=0.05, Output="linear",
                                 MinimumEpochs = 700, Epochs=1000, EarlyStoppingPatience=50, Learning=0.01)
```


For more details on the **keras** R package, visit [https://keras.rstudio.com/](https://keras.rstudio.com/).
