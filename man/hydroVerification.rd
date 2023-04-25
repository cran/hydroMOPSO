\name{hydroVerification}

\alias{hydroVerification}

\title{
Verification of a optimised model
}
\description{
It takes the optimisation results of a model and reruns the simulations in a verification period. Only applicable when the results of the previous optimisation were done with \code{fn=='hydromod' | fn=='hydromodInR'}
}
\usage{

hydroVerification(Results,
                  fn = NULL,
                  control = list(),
                  model.FUN = NULL,
                  model.FUN.args = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Results}{(\code{list}) \cr
List with \code{hydroMOPSO} optimisation results. The details of this input are explained in the value returned by \code{hydroMOPSO} function
}
  \item{fn}{(\code{function} or \code{character}) \cr
Object with the name of a valid R function to be optimised (minimised or maximised). When the goal is to optimise just simple functions (problems not associated with models with input and output data), it is possible to specify the name of any function correctly defined by the user. Special cases occur when the user is working with models, declared as internal or external functions of R. In these last cases, \code{fn='hydromod'} specifies that the optimisation is applied to a model that can be invoked from R(tipically, an executable file that must be run from the system console), but is executed entirely outside of this environment. On the other hand, \code{fn='hydromodInR'} specifies that the optimisation is applied to a model that can be executed within the R environment.\cr
In detail: \cr
-) When \code{fn!='hydromod' & fn!='hydromodInR'}, the first argument of \code{fn} has to be a vector of parameters over which optimisation is going to take place. It must return a vector with as many elements as objectives have been set in the function, and where each objective must be a scalar result. In this case, the algorithm uses the vector of values returned by \code{fn} as both model output and its corresponding set of optimised scalar results \cr
-) When \code{fn=='hydromod'} the algorithm will optimise the R-external model defined by \code{model.FUN} and \code{model.args}, which are used to extract the values simulated by the model and to compute its corresponding goodness-of-fit measures.  \cr
-) When \code{fn=='hydromodInR'} the algorithm will optimise the R model defined by \code{model.FUN} and \code{model.args}, which are used to extract the values simulated by the model and to compute its corresponding goodness-of-fit measures. \cr

When \code{fn=='hydromod' | fn=='hydromodInR'}, the function must return a list with two (2) specific elements, the first element of the list consists of the vector with as many elements as objectives have been established in the function, and where each objective must be a scalar result; the second element of the list corresponds to a matrix with the raw output data of the model that determines the scalar results of the objectives, for example time series of a hydrological model such as streamflow, evapotranspiration, soil moisture, among others. The matrix with the raw output data of the model must have as many columns as there are simulated variables being worked on in the optimisation, and this number of variables should not necessarily coincide with the number of objectives set. for example, flows could only be returned from a hydrological model to analyze three objectives. 
}
  \item{control}{(\code{list}) \cr
A list of control parameters. See \sQuote{Details}
}
  \item{model.FUN}{(\code{character}) \cr
(OPTIONAL) Used only when \code{fn=='hydromod' | fn=='hydromodInR'} \cr
A valid R function representing the model code to be optimised
}

  \item{model.FUN.args}{(\code{list}) \cr
(OPTIONAL) Used only when \code{fn=='hydromod' | fn=='hydromodInR'} \cr
A list with the arguments to be passed to \code{model.FUN}
}
}

%\details{
%
%}

\value{
(\code{list}) \cr

\describe{
\item{ParticlesFull}{(\code{data.frame}) \cr
History of positions of each Pareto Front particles in all iterations. In this \code{data.frame}, the first column indicates the simulation number \code{Sim}, in ascending order from the first simulation (first iteration, phase 1) to the last simulation (last iteration, phase 2); then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). \cr
}

\item{FilledPOF}{(\code{data.frame}) \cr
Filled Pareto front degraded in verification period. Keep in mind that strictly speaking this is not a Pareto Front since it is reached only by extending the solutions of the original front obtained by calibration to a verification period. \cr
}
\item{ParticlesFilledPOF}{(\code{data.frame}) \cr
Perticles from filled Pareto Front. In this \code{data.frame}, the first column indicates the simulation number \code{Sim}; then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). Note that in the objective columns the original calibration values have been replaced by those of the filled Pareto front degraded in verification period. \cr
}
\item{ModelOut}{(\code{list}) \cr
Time series of the model output variables in verification period, for all solutions of the filled Pareto Front obtained in calibration period. This list has as many objects as output variables, and each one corresponds to an object of class zoo with as many columns as solutions of the filled Pareto Front. \cr
}
\item{ParticleBestCS}{(\code{data.frame}) \cr
Best compromise solution, i.e., the solution with the minimum Euclidean distance from the maximum values of each objective, in calibration period. data.frame with only one row and several columns: the first column indicates the simulation number \code{Sim}; then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). Note that in the objective columns the original calibration values have been replaced by those obtained in the verification period. \cr
}
\item{ModelOutBestCS}{(\code{list}) \cr
Time series of the model output variables in verification period, just for the best compromise solution obtained in calibration period. This list has as many objects as output variables, and each one corresponds to an object of class zoo with a single time serie. \cr
}
\item{ParticleBestObjs}{(\code{list}) \cr
Solutions that minimise/maximise each of the objectives, obtained in calibration period. data.frame with only one row. In a first level, this list has as many objects as objectives involves in the optimisation, each one with a data.frame with only one row and several columns: the first column indicates the simulation number \code{Sim}; then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). Note that in the objective columns the original calibration values have been replaced by those obtained in the verification period. \cr
}
\item{ModelOutBestObjs}{(\code{list}) \cr
Time series of the model output variables in verification period, for the maximisation/minimisation of each objective in calibration. In a first level, this list has as many objects as objectives involves in the optimisation and, in a second level, each one corresponds to a list with as many objects as output variables, each one corresponding to an object of class zoo with a single time serie. \cr
}
\item{AnalysisPeriod}{(\code{character}) \cr
String indicating the analysis period, in this case \code{"verification"}. \cr
}
\item{DigitsDom}{(\code{numeric}) \cr
Number of decimal places used in dominance check. Fewer decimal places (say, 16, 8, or 4, for example) may be necessary to prevent the algorithm from resulting in solutions that are nearly the same. \cr
}
\item{ObjsNames}{(\code{data.frame}) \cr
Name of each of the objectives (\code{Obj1, Obj2, ...}). \cr
}
\item{MaxMin}{(\code{data.frame}) \cr
Specification on whether the objectives are maximised or minimised, must be in \code{c("max", "min")}. \cr
}
\item{Obs}{(\code{list}) \cr
Observed values of each of the variables involved in the optimisation, but now of the verification period. Keep in mind that the same format indicated as mandatory input data \code{Obs} within the \code{FUN} function is maintained. \cr
}
\item{Dimensions}{(\code{data.frame}) \cr
Number of objectives and number of output variables involved in the optimisation. \cr
}
\item{NamesAndUnitsVars}{(\code{data.frame}) \cr
Name and unit of measure of the output variables involved in the optimisation (\code{var1, var1_unit, var2, var2_unit, ...}). \cr
}
\item{WarmUp}{(\code{data.frame}) \cr
Time series indicating the warm-up period used in the optimisation. \cr
}
\item{DatesCal}{(\code{data.frame}) \cr
Time series indicating the calibration period used in the optimisation. \cr
}
}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Rodrigo Marinao Rivas \email{ra.marinao.rivas@gmail.com}, Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\note{
1) The intended workflow is that first you must have the results of the optimisation done with the \code{hydroMOPSO} function, which are then entered into this function (\code{hydroVerification}) \cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{hydroMOPSO}
}
%%\examples{
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multi-objective optimisation}
\keyword{verification}
\keyword{hydrological model}