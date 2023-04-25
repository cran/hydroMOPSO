\name{ReadResults}

\alias{ReadResults}

\title{
Reading the output files of a optimised model
}
\description{
Read results saved on disk from an optimization with hydroMOPSO. This feature only applies when \code{fn} is in \code{c("hydromod", "hydromodInR")}
}
\usage{
ReadResults(fn = NULL,
            control = list(), 
            model.FUN = NULL,
            model.FUN.args = list()
            )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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

\details{

The \code{control} argument is a list that can supply any of the following components:
  \describe{    
  \item{drty.in}{(\code{character}) \cr
(OPTIONAL) Used only when \code{fn='hydromod'} \cr
Name of the directory storing the input files required for PSO, i.e. \sQuote{ParamRanges.txt} and \sQuote{ParamFiles.txt}.
}
  \item{drty.out}{(\code{character}) \cr
Path to the directory storing the output files generated by hydroMOPSO.
}
  \item{digits}{(\code{numeric}) \cr
(OPTIONAL) Used only when \code{write2disk=TRUE} \cr
Number of significant digits used for writing the output files with scientific notation.
}
  \item{digits.dom}{(\code{numeric}) \cr
Number of decimal places used in dominance check. Fewer decimal places (say, 16, 8, or 4, for example) may be necessary to prevent the algorithm from resulting in solutions that are nearly the same. \cr
By default \code{digits.dom=Inf}, which basically means numbers are not rounded
}
  \item{write2disk}{(\code{logical}) \cr
Indicates if the output files will be written to the disk. \cr
By default \code{write2disk=TRUE}
}
  \item{verbose}{(\code{logical}) \cr
Indicates if progress messages are to be printed. \cr
By default \code{verbose=TRUE}
}
  \item{REPORT}{(\code{integer}) \cr
(OPTIONAL) Used only when \code{verbose=TRUE} \cr
The frequency of report messages printed to the screen. \cr
By default \code{REPORT=10}
} 
  \item{parallel}{(\code{character}) \cr
Indicates how to parallelise \sQuote{hydroMOPSO} (to be precise, only the evaluation of the objective function \code{fn} is parallelised). Valid values are: \cr
-)\kbd{none}: no parallelisation is made (this is the default value)\cr
-)\kbd{parallel}: parallel computations for network clusters or machines with multiple cores or CPUs. A \sQuote{FORK} cluster is created with the \code{\link[parallel]{makeForkCluster}} function.  When \code{fn.name='hydromod'} the evaluation of the objective function \code{fn} is done with the \code{\link[parallel]{clusterApply}} function of the \pkg{parallel} package. When \code{fn.name != 'hydromod'} the evaluation of the objective function \code{fn} is done with the \code{\link[parallel]{parRapply}} function of the \pkg{parallel} package.\cr
-)\kbd{parallelWin}: parallel computations for network clusters or machines with multiple cores or CPUs (this is the only parallel implementation that works on Windows machines). A \sQuote{PSOCK} cluster is created with the \code{\link[parallel]{makeCluster}} function. When \code{fn.name='hydromod'} the evaluation of the objective function \code{fn} is done with the \code{\link[parallel]{clusterApply}} function of the \pkg{parallel} package. When \code{fn.name != 'hydromod'} the evaluation of the objective function \code{fn} is done with the \code{\link[parallel]{parRapply}} function of the \pkg{parallel} package. 
} 
  \item{par.nnodes}{(\code{numeric}) \cr
(OPTIONAL) Used only when \code{parallel!='none'} \cr
Indicates the number of cores/CPUs to be used in the local multi-core machine, or the number of nodes to be used in the network cluster. \cr
By default \code{par.nnodes} is set to the amount of cores detected by the function \code{detectCores()} (\pkg{parallel} package)
} 
  \item{par.pkgs}{(\code{character}) \cr
(OPTIONAL) Used only when \code{parallel='parallelWin'} \cr
List of package names (as characters) that need to be loaded on each node for allowing the objective function \code{fn} to be evaluated.
} 
}
}

\value{
(\code{list}) \cr

\describe{

\item{MOPSOResults}{(\code{list}) \cr 
Particle repository history of all iterations (both phases of NMPSO), detailing:
\cr

- \emph{ParetoFront} (\code{data.frame}) \cr
History of objectives values of each Pareto Front particles in all iterations (both phases). In this \code{data.frame}, the first column indicates the iteration \code{Iter}; the second column the phase \code{Phase} (1 or 2); and the following columns are as many as objectives treated, being identified with the assigned name. \cr
- \emph{Particles_ParetoFront} (\code{data.frame}) \cr
History of positions of each Pareto Front particles in all iterations (both phases). In this \code{data.frame}, the first column indicates the iteration \code{Iter}; the second column the phase \code{Phase} (1 or 2); then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). \cr
- \emph{MaxMin} (\code{data.frame}) \cr
Specification on whether the objectives are maximised or minimised. \cr
- \emph{ObjsNames} (\code{data.frame}) \cr
Name of each of the objectives (\code{Obj1, Obj2, ...}). \cr
}

\item{hydroDetails}{(\code{list}) \cr 
(ONLY ADDED WHEN \code{fn=='hydromod' | fn=='hydromodInR'}) \cr
Details about the modeling involved in optimisation:
\cr

- \emph{Dimensions} (\code{data.frame}) \cr
Number of objectives and number of output variables involved in the optimisation. \cr
- \emph{NamesAndUnitsVars} (\code{data.frame}) \cr
Name and unit of measure of the output variables involved in the optimisation (\code{var1, var1_unit, var2, var2_unit, ...}). \cr
- \emph{Obs} (\code{list}) \cr
Observed values of each of the variables involved in the optimisation, keeping in mind that the same format indicated as mandatory input data \code{Obs} within the \code{FUN} function is maintained. \cr
- \emph{WarmUp} (\code{data.frame}) \cr
Time series indicating the warm-up period used in the optimisation. \cr
- \emph{DatesCal} (\code{data.frame}) \cr
Time series indicating the calibration period used in the optimisation. \cr
}

\item{hydroResults}{(\code{list}) \cr 
(ONLY ADDED WHEN \code{fn=='hydromod' | fn=='hydromodInR'}) \cr
Post-processed results about the modeling involved in optimisation:
\cr

- \emph{ParticlesFull} (\code{data.frame}) \cr
History of positions of each Pareto Front particles in all iterations. In this \code{data.frame}, the first column indicates the simulation number \code{Sim}, in ascending order from the first simulation (first iteration, phase 1) to the last simulation (last iteration, phase 2); then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). \cr
- \emph{FilledPOF} (\code{data.frame}) \cr
Filled Pareto front, built from evaluating the dominance of the solutions of all the iterations performed in the optimisation. To prevent the filled Pareto Front from having too many solutions, the parameters and objective values are rounded according to input \code{DigitsDom} (number of decimal places). In this \code{data.frame}, the first column indicates the simulation number \code{Sim}; then as many columns as objectives treated, being identified with the assigned name. \cr
- \emph{ParticlesFilledPOF} (\code{data.frame}) \cr
Perticles from filled Pareto Front. In this \code{data.frame}, the first column indicates the simulation number \code{Sim}; then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). \cr
- \emph{ModelOut} (\code{list}) \cr
Time series of the model output variables, for all solutions of the filled Pareto Front. This list has as many objects as output variables, and each one corresponds to an object of class zoo with as many columns as solutions of the filled Pareto Front. \cr
- \emph{ParticleBestCS} (\code{data.frame}) \cr
Best compromise solution, i.e., the solution with the minimum Euclidean distance from the maximum values of each objective. data.frame with only one row and several columns: the first column indicates the simulation number \code{Sim}; then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). \cr
- \emph{ModelOutBestCS} (\code{list}) \cr
Time series of the model output variables, just for the best compromise solution. This list has as many objects as output variables, and each one corresponds to an object of class zoo with a single time serie. \cr
- \emph{ParticleBestObjs} (\code{list}) \cr
Solutions that minimise/maximise each of the objectives. data.frame with only one row. In a first level, this list has as many objects as objectives involves in the optimisation, each one with a data.frame with only one row and several columns: the first column indicates the simulation number \code{Sim}; then as many columns as objectives treated, being identified with the assigned name; and finally, as many columns as decision variables (parameters). \cr
- \emph{ModelOutBestObjs} (\code{list}) \cr
Time series of the model output variables, for the maximisation/minimisation of each objective. In a first level, this list has as many objects as objectives involves in the optimisation and, in a second level, each one corresponds to a list with as many objects as output variables, each one corresponding to an object of class zoo with a single time serie. \cr
- \emph{AnalysisPeriod} (\code{character}) \cr
String indicating the analysis period, in this case \code{"calibration"}. \cr
- \emph{DigitsDom} (\code{numeric}) \cr
Number of decimal places used in dominance check. Fewer decimal places (say, 16, 8, or 4, for example) may be necessary to prevent the algorithm from resulting in solutions that are nearly the same. \cr
- \emph{ObjsNames} (\code{data.frame}) \cr
Name of each of the objectives (\code{Obj1, Obj2, ...}). \cr
- \emph{MaxMin} (\code{data.frame}) \cr
Specification on whether the objectives are maximised or minimised, must be in \code{c("max", "min")}. \cr
- \emph{Obs} (\code{list}) \cr
Observed values of each of the variables involved in the optimisation, keeping in mind that the same format indicated as mandatory input data \code{Obs} within the \code{FUN} function is maintained. \cr
- \emph{Dimensions} (\code{data.frame}) \cr
Number of objectives and number of output variables involved in the optimisation. \cr
- \emph{NamesAndUnitsVars} (\code{data.frame}) \cr
Name and unit of measure of the output variables involved in the optimisation (\code{var1, var1_unit, var2, var2_unit, ...}). \cr
- \emph{WarmUp} (\code{data.frame}) \cr
Time series indicating the warm-up period used in the optimisation. \cr
- \emph{DatesCal} (\code{data.frame}) \cr
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
1) The intended workflow is that first you must have the results of the optimisation done with the \code{hydroMOPSO} function, having saved the results to disk (\code{write2disk=TRUE} in \code{hydroMOPSO}) \cr
2) Based on the previous point, the user must ensure that the input arguments \code{fn}, \code{control}, \code{model.FUN} and \code{model.FUN.args} that are entered in the \code{hydroMOPSO} and \code{ReadResults} functions must be EXACTLY THE SAME \cr
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