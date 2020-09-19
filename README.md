# MackNet
R package for running MackNet model for paid and incurred loss data. The package also includes the [NAIC Schedule P Database](https://www.casact.org/research/index.cfm?fa=loss_reserves_data) provided by CAS. This database has been widely used for validating general insurance reserving models.

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
CompanyCode = CA_TriangleSelectionOrderedRt[,"Company_Code"][1]               #Company code is selected
Cumulative = CumulativeT.SchedudeP(ins.line.data(CompanyCode,comauto_pos))    #Cumulative payments triangle
Incurred = IncurredT.SchedudeP(ins.line.data(CompanyCode,comauto_pos))        #Incurred cost triangle
Premium = NetEarned.SchedudeP(ins.line.data(CompanyCode,comauto_pos))         #Premium vector (Premiums)
```
