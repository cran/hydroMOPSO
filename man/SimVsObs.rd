\name{SimVsObs}

\alias{SimVsObs}

\title{
Comparison between observed and simulated variables
}
\description{
Simple comparison between time series of observed and simulated variables. This function is specially designed to check the correct operation of the wrapper functions prepared by the user. The premise is as follows: if the user can generate a 'nice' graph (graphically evidencing the simulated and observed values, and obtaining finite numerical values for the objectives) then they can proceed with greater confidence to the hydroMOPSO optimisation step.
}
\usage{

SimVsObs(Sim,
         Obs,
         Objs.values,
         Objs.names,
         var.names, 
         var.units, 
         legend.sim = "Simulated", 
         legend.obs = "Observed", 
         cal.period,
         warmup.period,
         full.period,
         main = "study case #1",
         analysis.period,
         digits.round = 8)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sim}{(\code{list}) \cr
List with simulations of the output variables involved in the optimisation. The list must have as many elements as variables involved, each as a zoo class
}
  \item{Obs}{(\code{list}) \cr
List with observations of the output variables involved in the optimisation.  The list must have as many elements as variables involved, each as a zoo class
}
  \item{Objs.values}{(\code{numeric}) \cr
Vector with values of the objectives considered in the optimisation
}
  \item{Objs.names}{(\code{character}) \cr
Vector with the names of the optimisation objectives
}
  \item{var.names}{(\code{character}) \cr
Vector with the names of the output variables
}
  \item{var.units}{(\code{character}) \cr
Vector with the units of measurement of the output variables
}
  \item{legend.sim}{(\code{character}) \cr
Single character with an identifiable name for the simulated values in the output graph \cr
By default \code{legend.sim = "Simulated"}
}
  \item{legend.obs}{(\code{character}) \cr
Single character with an identifiable name for the observed values in the output graph \cr
By default \code{legend.sim = "Observed"}
}
  \item{warmup.period}{(\code{Date}) \cr
Vector with the dates of the warmup period.
}
  \item{cal.period}{(\code{Date}) \cr
Vector with the dates of the calibration period.
}
  \item{full.period}{(\code{Date}) \cr
Vector with the dates of the full period (warmup + calibration and/or verification)
}
  \item{main}{(\code{character}) \cr
Title for the plot, usually an identifiable name of the case study \cr
By default \code{main = "study case #1"}
}
  \item{analysis.period}{(\code{character}) \cr
The graph to be plotted is in verification or calibration \cr
}
  \item{digits.round}{(\code{numeric}) \cr
Number of decimal places to round \code{Objs.values} \cr
By default \code{digits.round = 8}
}
}

% \details{
% }

\value{
No return value
}


%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Rodrigo Marinao Rivas \email{ra.marinao.rivas@gmail.com}, Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

% \note{
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{hydroMOPSO}
}
\examples{
\donttest{

###############################################################################################

# This example is derived from example 5 of the hydroMOPSO function

library(hydroMOPSO)
library(airGR)
library(hydroTSM)
library(hydroGOF)

# RTL basin ------------------------------------------------
basin.area <- 1415025887 # basin area in square meters

# Load time series -----------------------------------------
data(Trancura9414001plus) # Load RTL data set

# Dates ----------------------------------------------------
dates.raw <- Trancura9414001plus[,"Date"]
dates <- as.Date(dates.raw) # dates

# INPUTS time series ---------------------------------------

# Precipitation (input variable)
ts.pcp.raw <- Trancura9414001plus[,"P_mm"]
ts.pcp <- zoo(ts.pcp.raw, dates)

# Potential EvapoTranspiration (input variable)
ts.pet.raw <- Trancura9414001plus[,"PET_mm"]
ts.pet <- zoo(ts.pet.raw, dates)

# OUTPUTS time series --------------------------------------
# Observed streamflow (calibration output variable)
ts.qobs.raw <- Trancura9414001plus[,"Qobs_m3s"]
ts.qobs <- zoo(ts.qobs.raw, dates)

# Parameter ranges and noCal parameters --------------------

lower <- c("X1" = 0.01, "X2" = -100, "X3" = 0.01, "X4" = 0.5) # parameter range lower threshold
upper <- c("X1" = 1200, "X3" =  100, "X3" = 5000, "X4" = 5  ) # parameter range upper threshold

noCal.param <- (lower + upper)/2 # uncalibrated parameters

# Names and units of observed output variables -------------

char.obs.names <- "Streamflow"
char.obs.units <- "m3/s"

# Objectives names -----------------------------------------

char.objs.names <- c("KGE2012_Q", "KGEGarcia_Q")

# Calibration dates and subsetting -------------------------

WarmUpCal.dates <- dip("1979-01-01", "1979-12-31") # WarmUp for Calibration
Cal.dates <- dip("1980-01-01", "1999-12-31") # Calibration
FullCal.dates <- dip("1979-01-01", "1999-12-31") # WarmUp + Calibration

start.FullCal <- FullCal.dates[1]
end.FullCal   <- FullCal.dates[length(FullCal.dates)]

# INPUTS time series ---------------------------------------

# Precipitation (input variable)
ts.pcp.FullCal <- window(ts.pcp, start = start.FullCal, end = end.FullCal) # subsetting pcp

# Potential EvapoTranspiration (input variable)
ts.pet.FullCal <- window(ts.pet, start = start.FullCal, end = end.FullCal) # subsetting pet

# OUTPUTS time series --------------------------------------

# Observed streamflow (calibration output variable)
ts.qobs.FullCal <- window(ts.qobs, start = start.FullCal, end = end.FullCal) # subsetting qobs

list.obs.Cal <- list(Q = ts.qobs.FullCal)

# Structuring Inputs and Options of GR4J model -------------

InputsModel.Cal <- CreateInputsModel(FUN_MOD= RunModel_GR4J, 
                                     DatesR= as.POSIXlt(FullCal.dates),
                                     Precip= coredata(ts.pcp.FullCal), 
                                     PotEvap= coredata(ts.pet.FullCal))

RunOptions.Cal <- CreateRunOptions(FUN_MOD= RunModel_GR4J, InputsModel= InputsModel.Cal,
                                   IndPeriod_Run = 1:length(FullCal.dates), warnings = FALSE)

# Checking Wrapper function in calibration -----------------

noCal.results.Cal <- GR4JExampleCal(param.values = noCal.param,
                                    Obs = list.obs.Cal,
                                    Objs.names = char.objs.names,
                                    var.names = char.obs.names, 
                                    var.units = char.obs.units,
                                    warmup.period  = WarmUpCal.dates,
                                    cal.period = Cal.dates,
                                    full.period = FullCal.dates,
                                    InputsModel = InputsModel.Cal, 
                                    RunOptions = RunOptions.Cal,
                                    area = basin.area)

noCal.sim.Cal <- noCal.results.Cal[["sim"]]
noCal.objs.Cal <- noCal.results.Cal[["Objs"]]

dev.new()
SimVsObs(Sim = noCal.sim.Cal, Obs = list.obs.Cal,
         Objs.values = noCal.objs.Cal, Objs.names = char.objs.names,
         var.names = char.obs.names, var.units = char.obs.units, 
         legend.sim = "Simulated", legend.obs = "Observed", 
         warmup.period  = WarmUpCal.dates, cal.period = Cal.dates, full.period = FullCal.dates,
         main = "...just checking GR4JExampleCal function", analysis.period = "calibration",
         digits.round = 4)

}
} % end donttest
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{}