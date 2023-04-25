\name{Trancura9414001plus}
\docType{data}
\alias{Trancura9414001plus}
\title{
Hydrometeorological time series for Trancura antes de Llafenco River Basin
}
\description{
Daily time series of precipitation, air temperature (max, min, mean), potential evapotranspiration and streamflows for the catchment draining into the 'Trancura antes de Llafenco' streamflow station (Cod.BNA: 9414001, drainage area= 1416 km2), Araucania Region, Chile (Lat:-39.3333, Lon:-71.6667), with data from 01/Jan/1979 to 31/Dec/2020 (including some gaps in streamflow data).
}
\usage{
data(Trancura9414001plus)


}

%%\details{
%%
%%}
\format{
zoo with seven columns: \cr
-) \var{Dates}: character with the date (YYYY-MM-DD) for each daily observation. \cr
-) \var{Pp_mm}: Spatially-averaged mean daily values of precipitation computed based on the CR2met dataset, [mm/day]. \cr
-) \var{Tmax_degC}: Spatially-averaged mean daily values of maximum air temperature computed based on the CR2met dataset, [degree Celsius]. \cr
-) \var{Tmin_degC}: Spatially-averaged mean daily values of minimum air temperature computed based on the CR2met dataset, [degree Celsius]. \cr
-) \var{Tmean_degC}: Spatially-averaged mean daily values of mean air temperature computed based on the CR2met dataset, [degree Celsius]. \cr
-) \var{PET_mm}: Spatially-averaged mean daily values of potential evapotranspiration (PET), computed with the Hargreaves-Samani equation based on daily maximum and minimum air temperatures obtained from the CR2met dataset, [mm/day]. \cr
-) \var{Qobs_m3s}: Daily streamflows measured at the Trancura antes de Llafenco (9414001) station. \cr
-) \var{ETobs_mm}: Daily evapotranspiration (ET) estimated at the Trancura antes de Llafenco (9414001) with the 8-day evapotranspiration product PML v2 (Zhang, 2019). The 8-day time series are disaggregated on a daily scale just dividing by 8, so, it is recommended to use this information on a weekly, monthly or annual scale. \cr
}
\source{
CR2met v2 is a gridded product of observed daily precipitation an maximum/minimim temperature, covering the period 1979-01-01 to 2020-12-31. Its developed by Boisier et al. (2018) and provided by Center for Climate and Resilience Research, Universidad de Chile, Santiago, Chile (\url{https://www.cr2.cl/datos-productos-grillados/}, last accessed [Dic 2021]). \cr
PML v2 is a gridded product of estimated 8-day evapotranspiration, covering the period 2020-03-01 to 2020-04-30. The proper use of this information as "observed values" can be a subject of discussion, however, they are included in \code{hydroMOPSO} for mere didactic purposes, hoping that future research will provide more reliable information to use as ET observations. \cr
These data are intended to be used for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

\references{
\cite{Boisier, J. P., Alvarez-Garreton, C., Cepeda, J., Osses, A., Vasquez, N., and Rondanelli, R. (2018). CR2MET: A high-resolution precipitation and temperature dataset for hydroclimatic research in Chile. EGUGA, 20, 19739. Opgehaal van https://ui.adsabs.harvard.edu/abs/2018EGUGA..2019739B/abstract}

\cite{Zhang, Y., Kong, D., Gan, R., Chiew, F. H. S., McVicar, T. R., Zhang, Q., & Yang, Y. (2019). Coupled estimation of 500 m and 8-day resolution global evapotranspiration and gross primary production in 2002-2017. Remote Sensing of Environment, 222, 165-182. doi:10.1016/J.RSE.2018.12.031}
}
\keyword{datasets}

