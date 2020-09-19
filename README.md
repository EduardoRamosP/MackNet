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

Once the input data is calculated, the MackNet model for paid and incurred loss data are fitted as follows:
``` r
#Paid loss data
MackNetPaidModel <- MackNet_Paid(Cumulative, Incurred, Premium, Ensemble=20,wd=0, drop=0.05, Output="linear",
                              MinimumEpochs = 700, Epochs=1000, EarlyStoppingPatience=50, Learning=0.01)
#Incurred loss data
MackNetIncModel <- MackNet_Incurred(Cumulative, Incurred, Premium, Ensemble=20, wd=0, drop=0.05, Output="linear",
                                 MinimumEpochs = 700, Epochs=1000, EarlyStoppingPatience=50, Learning=0.01)
```
The implementation carried out allows the user to modify the all the variables of the MackNet model. The more relevant are:
- wd: Weigthed decay of ADAM optimization algorithm.
- Learning: Learning rate of the neural networks fitted.
- drop: Dropout regularization to be applied to the neural networks.
- Ensemble: Number of neural networks to be fitted.
- Epochs: Maximum number of epochs for fitting the neural networks.
- EarlyStoppingPatience: The training process stops if the error is not improved in the number of epochs defined in this variable.
- MinimumEpocs: Minimum number of epochs. The neural networks will be fit at least the number of epochs defined in this variable.
- Simulations: Number of simulations to be obtained from the MackNet model.
- Output: It defines the activation function of the output layer.

The main outputs provided by the model fitting are:
- Alpha: It provides the estimation of the alpha paramerters made by the MackNet model. The number of alpha values is the number of developments present in the triangle minus one.
- DevFactors: The development factors estimated by the model. Same number of parameters than Alpha.
- SampledReserves: Reserves sampled by the MackNet model. The number of simulations are equal to the number defined in the inputs (variable: Simulations).
- SampledUltimate: Ultimates sampled by the MackNet model.

For futher details about the **keras** R package, visit [https://keras.rstudio.com/](https://keras.rstudio.com/).
