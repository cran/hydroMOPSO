\name{GR4JWrapperExamples}

\alias{GR4JExampleCal}

\alias{GR4JExampleVer}

\title{
Example wrapper functions to execute GR4J model
}

\description{

Example wrapper functions to execute the GR4J model and obtain the performance of two objective functions (KGE2012 and KGEGarcia), in a calibration (\code{GR4JExampleCal}) or a verification period (\code{GR4JExampleVer}). Keep in mind that, within \code{hydroMOPSO}, the calibration or verification wrapper functions essentially have to be \bold{prepared by the user}, with the objective functions that are convenient and the output variables that are necessary. \cr

Thus, the functions presented here are only intended to work with examples from the documentation and serve as a guide to:
1) The general scheme of the calibration/verification wrapper functions \cr
2) The assimilation of mandatory inputs \cr
3) The assimilation of mandatory outputs \cr

}
\usage{
GR4JExampleCal(param.values,
               Obs,
               Objs.names,
               var.names, 
               var.units,
               full.period,
               warmup.period,
               cal.period,
               InputsModel, 
               RunOptions,
               area)

GR4JExampleVer(param.values,
               Obs,
               Objs.names,
               var.names, 
               var.units,
               full.period,
               warmup.period,
               cal.period,
               InputsModel, 
               RunOptions,
               area)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param.values}{(\code{numeric}) \cr
Vector with parameter set of the model \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{Obs}{(\code{list}) \cr
List with time series of observations of the output variables \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{Objs.names}{(\code{character}) \cr
Vector with the names of the optimisation objectives \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{var.names}{(\code{character}) \cr
Vector with the names of the output variables \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{var.units}{(\code{character}) \cr
Vector with the units of measurement of the output variables \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{full.period}{(\code{Date}) \cr
Vector with the dates of the full period (warmup + calibration and/or verification) \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{warmup.period}{(\code{Date}) \cr
Vector with the dates of the warmup period \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{cal.period}{(\code{Date}) \cr
Vector with the dates of the calibration period \cr
\bold{This is a mandatory input for any wrapper function to work with hydroMOPSO, preserve name and class}
}
  \item{InputsModel}{(\code{list}) \cr
GR4J inputs structured with the function \code{airGR::CreateInputsModel} \cr
This input has been included only for the execution of the GR4J model in particular
}
  \item{RunOptions}{(\code{list}) \cr
GR4J run options specified with the function \code{airGR::CreateRunOptions} \cr
This input has been included only for the execution of the GR4J model in particular
}
  \item{area}{(\code{numeric}) \cr
Area of the basin (sq-m), necessary to pass the outlet streamflow to m3/s (cms) \cr
This input has been included only for the execution of the GR4J model in particular
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
(\code{list}) \cr

The returned list contains two elements

\describe{  
\item{Objs}{(\code{numeric})  \cr 
Vector with the numerical values of the objectives (GoF1 and GoF2).
}
\item{sim}{(\code{list})  \cr 
List with as many elements as time series of the output variables of the model (in this case only one output variable: streamflows).
}
}
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Rodrigo Marinao Rivas \email{ra.marinao.rivas@gmail.com}, Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

%%\note{

%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{hydroMOPSO}
}
%%\examples{
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GR4J}
\keyword{calibration}
\keyword{verification}
\keyword{hydrological model}