\name{hydroMOPSO}

\alias{hydroMOPSO}

\title{
Multi-Objective Particle Swarm Optimisation algorithm (NMPSO)
}
\description{
Multi-objective Particle Swarm Optimisation algorithm (NMPSO). The default configuration of hydroMOPSO has been adapted to obtain results with the fewest number of iterations possible.  \cr \cr
\strong{Important}: In Example 5 (calibration of GR4J hydrological model), \code{maxit = 50} was set just for practical needs when testing the package. For acceptable results please change to \code{maxit = 250}. With any more robust model and with up to 12 parameters we recommend \code{maxit = 1000}.
}
\usage{
hydroMOPSO(fn='hydromod',
           lower=-Inf,
           upper=Inf,                
           control=list(),
           model.FUN=NULL,
           model.FUN.args=list(),
           obj.thr = NULL,
           ...)
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
  \item{lower}{(\code{numeric}) \cr
Lower boundary for each parameter \cr
In \kbd{hydroMOPSO} the length of \code{lower} and \code{upper} are used to defined the dimension of the solution space
}
  \item{upper}{(\code{numeric}) \cr
Upper boundary for each parameter \cr
In \kbd{hydroMOPSO} the length of \code{lower} and \code{upper} are used to defined the dimension of the solution space
}

  \item{control}{(\code{list}) \cr
A list of control parameters. See\sQuote{Details}
}
  \item{model.FUN}{(\code{character}) \cr
(OPTIONAL) Used only when \code{fn=='hydromod' | fn=='hydromodInR'} \cr
A valid R function representing the model code to be optimised
} 
  \item{model.FUN.args}{(\code{list}) \cr
(OPTIONAL) Used only when \code{fn=='hydromod' | fn=='hydromodInR'} \cr
A list with the arguments to be passed to \code{model.FUN}
}
  \item{obj.thr}{(\code{list}) \cr
(OPTIONAL) Used only when \code{fn=='hydromod' | fn=='hydromodInR'} \cr
Thresholds for each objective
} 
\item{\dots}{
further arguments to be passed to \code{fn}
}

}

\details{
By default, hydroMOPSO performs minimisation on all objectives specified in \code{fn} (\code{MinMax='min'} in \code{control} list), but this can be changed to maximisation (\code{MinMax='max'} in \code{control} list). If in \code{fn} you have to maximise some objectives and minimise others, you must make them all point to the same direction (all maximising or all minimising), which can be handled simply with a sign (See Example 2 where this type of case is presented). \cr
Although the NMPSO algorithm was formulated to deal with many objectives, the default definitions in \code{hydroMOPSO}, and therefore the applications made in research linked to this package, have been made with a focus on two and three objectives. Extending applications to optimisations with four or more objectives is possible with this package (so you are welcome to formulate such problems and solve them with \code{hydroMOPSO}!), but be very careful in analyzing your results. \cr

The \code{control} argument is a list that can supply any of the following components:
  \describe{    
  \item{drty.in}{(\code{character}) \cr
(OPTIONAL) Used only when \code{fn='hydromod'} \cr
Name of the directory storing the input files required for PSO, i.e. \sQuote{ParamRanges.txt} and \sQuote{ParamFiles.txt}.
}
  \item{drty.out}{(\code{character}) \cr
Path to the directory storing the output files generated by hydroMOPSO.
}
  \item{param.ranges}{(\code{character}) \cr
(OPTIONAL) Used only when \code{fn=='hydromod' | fn=='hydromodInR'} \cr
 Name of the file defining the minimum and maximum boundary values for each one of the parameters to be optimisated with NMPSO.
} 
  \item{digits}{(\code{numeric}) \cr
(OPTIONAL) Used only when \code{write2disk=TRUE} \cr
Number of significant digits used for writing the output files with scientific notation.
}
  \item{digits.dom}{(\code{numeric}) \cr
number of decimal places used in dominance check. Fewer decimal places (say, 16, 8, or 4, for example) may be necessary to prevent the algorithm from resulting in solutions that are nearly the same. \cr
By default \code{digits.dom=Inf}, which basically means numbers are not rounded
}
  \item{MinMax}{(\code{character}) \cr
Indicates whether a maximisation or minimisation multi-objetive problem needs to be solved. Valid values are in: \code{c('min', 'max')}. By default \code{MinMax='min'}. This \code{control} argument applies to all objective functions at the same time, so they must all go in the same direction (either all maximizing or all minimizing; keep in mind that for a particular function to go from maximizing to minimizing, or vice versa, it is only necessary add a minus sign (-)).
}
  \item{npart}{(\code{numeric}) \cr
Number of particles in the swarm. By default \code{npart=10}, inherited from R-package \code{hydroMOPSO}.
}
  \item{maxrep}{(\code{numeric}) \cr
Maximum number of particles to be stored in the updated Pareto Front in each iteration. \cr
By default \code{maxrep=100}
}
  \item{maxcross}{(\code{numeric}) \cr
Maximum number of Pareto Front particles that, for each iteration, perform the crossing and mutatiom in the application of genetic operators. \cr
By default \code{maxcross=50}
}
  \item{maxit}{(\code{numeric}) \cr
Maximum number of iterations. \cr
By default \code{maxit=1000}
}
  \item{Xini.type}{(\code{character}) \cr 
Indicates how to initialise the particles' positions in the swarm within the ranges defined by \code{lower} and \code{upper}. \cr
Valid values are: \cr
-) \kbd{Sobol}: Sobol initialisation of positions, using \code{npart} number of samples contained  in parameter space bounded by \code{lower} and \code{upper}. \bold{It requires the \pkg{randtoolbox} package}  \cr
-) \kbd{lhs}: Latin Hypercube initialisation of positions, using \code{npart} number of strata to divide each parameter range. \bold{It requires the \pkg{lhs} package}  \cr
-) \kbd{random}: random initialisation of positions within \code{lower} and \code{upper} \cr
By default \code{Xini.type='Sobol'}
}
  \item{Vini.type}{(\code{character}) \cr
Indicates how to initialise the particles' velocities in the swarm. \cr
Valid values are: \cr
-) \kbd{zero}: all the particles are initialised with zero velocity \cr
-) \kbd{random2011}: random initialisation of velocities within \code{lower-Xini} and \code{upper-Xini}, as defined in SPSO 2011 (\samp{Vini=U(lower-Xini, upper-Xini)}) (see Clerc, 2012, 2010) \cr
-) \kbd{lhs2011}: same as in \kbd{random2011}, but using a Latin Hypercube initialisation with \code{npart} number of strata instead of a random uniform distribution for each parameter. \bold{It requires the \pkg{lhs} package} \cr
-) \kbd{random2007}: random initialisation of velocities within \code{lower} and \code{upper} using the \sQuote{half-diff} method defined in SPSO 2007 (\samp{Vini=[U(lower, upper)-Xini]/2}) (see Clerc, 2012, 2010) \cr
-) \kbd{lhs2007}: same as in \kbd{random2007}, but using a Latin Hypercube initialisation with \code{npart} number of strata instead of a random uniform distribution for each parameter. \bold{It requires the \pkg{lhs} package} \cr
By default \code{Vini.type='zero'}
}

  \item{boundary.wall}{(\code{character}) \cr
Indicates the type of boundary condition to be applied during optimisation. \cr
Valid values are: \kbd{absorbing2011}, \kbd{absorbing2007}, \kbd{reflecting}, \kbd{damping}, \kbd{invisible} \cr
By default \code{boundary.wall='absorbing2011'}\cr
Experience has shown that Clerc's constriction factor and the inertia weights do not always confine the particles within the solution space. To address this problem, Robinson and Rahmat-Samii (2004) and Huang and Mohan (2005) propose different boundary conditions, namely, \kbd{reflecting}, \kbd{damping}, \kbd{absorbing} and \kbd{invisible} to define how particles are treated when reaching the boundary of the searching space (see Robinson and Rahmat-Samii (2004) and Huang and Mohan (2005) for further details).
}
  \item{cal.hv}{(\code{logical}) \cr
(OPTIONAL) \cr
Indicates whether or not the hypervolume formed between the hyperplane of the Pareto Front and a nadir point designated as \code{nadir.point} will be calculated. \cr
By default \code{cal.hv=FALSE}
}

  \item{nadir.point}{(\code{numeric}) \cr
(OPTIONAL) Only required when \code{cal.hv=TRUE} \cr
Nadir point from which the hypervolume will be calculated in each iteration step. It should correspond to a reference point considered as the worst acceptable optimal value.
}

  \item{n.samples}{(\code{integer}) \cr
(OPTIONAL) Only required when \code{cal.hv=TRUE} \cr
Number of points to estimate hypervolume, based on MonteCarlo sampling. \cr
By default \code{n.samples=10000}
}

  \item{write2disk}{(\code{logical}) \cr
Indicates if the output files will be written to the disk. \cr
By default \code{write2disk=TRUE}
}
  \item{verbose}{(\code{logical}) \cr
Indicates if progress messages are to be printed. \cr
By default \code{verbose=TRUE}
}
  \item{plot}{(\code{logical}) \cr
Indicates if a plot with the Pareto Front will be drawn after each iteration. \cr
By default \code{plot=FALSE}
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

The returned list contains elements that vary according to the input specifications
\describe{  
\item{Rep}{(\code{list})  \cr 
Particle repository for the last iteration (just second last phase of NMPSO), detailing:
\cr

- \emph{Position} (\code{matrix}) \cr
Positions of each set of Pareto Front particles until the last iteration. \cr
- \emph{Objs} (\code{matrix}) \cr
Objective values of each set of Pareto Front particles until the last iteration. \cr
- \emph{BFE} (\code{numeric}) \cr
Balanceable Fitness Estimation (BFE) of each set of Pareto Front particles until the last iteration. \cr
- \emph{Ranking.BFE} (\code{matrix}) \cr
Ranking of each set of Pareto Front particles until the last iteration, according to the BFE value. \cr
}

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

\references{

\cite{Lin, Q., Liu, S., Zhu, Q., Tang, C., Song, R., Chen, J., Coello, C. A. C., Wong, K.-C., & Zhang, J. (2018). Particle Swarm Optimization With a Balanceable Fitness Estimation for Many-Objective Optimization Problems. IEEE Transactions on Evolutionary Computation, 22(1), 32-46. doi:10.1109/TEVC.2016.2631279}

\cite{Marinao-Rivas, R., & Zambrano-Bigiarini, M. (2021). Towards best default configuration settings for NMPSO in Multiobjective Optimization. 2021 IEEE Latin American Conference on Computational Intelligence. (Accepted).}

\cite{Zambrano-Bigiarini, M.; R. Rojas (2013), A model-independent Particle Swarm Optimization software for model calibration, Environmental Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004}

\cite{Coello, C. A. C., & Lechuga, M. S. (2002). MOPSO: A proposal for multiple objective particle swarm optimization. Proceedings of the 2002 Congress on Evolutionary Computation, CEC 2002, 2, 1051-1056. doi:10.1109/CEC.2002.1004388}

\cite{Kennedy, J., & Eberhart, R. (1995). Particle swarm optimization. Proceedings of ICNN'95 - International Conference on Neural Networks, 4, 1942-1948. doi:10.1109/ICNN.1995.488968}

\cite{Deb, K. (1999). Multi-objective genetic algorithms: problem difficulties and construction of test problems. Evolutionary computation, 7, 205-230. doi:10.1162/EVCO.1999.7.3.205}

\cite{Kursawe, F. (1991). A variant of evolution strategies for vector optimization. Lecture Notes in Computer Science (including subseries Lecture Notes in Artificial Intelligence and Lecture Notes in Bioinformatics), 496 LNCS, 193-197. doi:10.1007/BFB0029752}

\cite{Deb, K., Thiele, L., Laumanns, M., & Zitzler, E. (2005). Scalable Test Problems for Evolutionary Multiobjective Optimization (bll 105-145; A. Abraham, L. Jain, & R. Goldberg, Reds). doi:10.1007/1-84628-137-7_6}

}

\author{
Rodrigo Marinao Rivas \email{ra.marinao.rivas@gmail.com}, Mauricio Zambrano-Bigiarini \email{mzb.devel@gmail.com}
}
\note{
1) For a better understanding of the application cases in which \code{fn=='hydromod' | fn=='hydromodInR'}, it is strongly recommended to review the complementary tutorials to this package. \cr
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%}
\examples{
\donttest{
###############################################################################################
# Example 1. Basic Benchmark function in minimisation
###############################################################################################

# This basic Benchmark function has two objectives (M = 2) in minimisation, its Pareto optimal
# front is discontinuous with four disconnected curves.This function works with 2 decision 
# variables (D = 2).

# Main reference for function: Deb (1999)

library(hydroMOPSO)

lower <- c(0, 0)
upper <- c(1, 1)

fnBasic <- function(param){
   
   x1 <- param[1]
   x2 <- param[2]

   obj1 <- x1
   obj2 <- (1 + 10*x2)*(1-(x1/(1+10*x2))^2 - x1/(1+10*x2)*sin(2*pi*4*x1))

   out <- list(c(obj1, obj2)) # For consistency with further examples, this must be a list
   names(out) <- "Objs"       # The name "Objs" is a mandatory requirement

   return(out)
}

set.seed(100) # Setting the seed (for reproducible results)
out <- hydroMOPSO(fn  = fnBasic,
                  lower = lower,
                  upper = upper,
                  control=list(npart = 10, maxrep = 100, maxcross = 50,
                               MinMax = "min", maxit = 50, plot = TRUE)
                  )



###############################################################################################
# Example 2. Basic Benchmark function in maximisation
###############################################################################################
#
# This example is identical to Example 1, but the functions are in maximisation
#
# IMPORTANT:
# In the literature related to multi-objective optimisation, test functions are usually 
# presented with minimisation objectives (such as the function used in Example 1). However, 
# this does not necessarily always have to be formulated that way, especially when it comes to
# real-world applications.
#
# In this second example we just want to remind you that the disjunctive between maximising or
# minimising objectives is "a matter of signs".
#
# With this in account, as explained in the documentation, for hydroMOPSO operation there is 
# one requirement which you MUST TAKE CARE OF:
#
# "The problems must be formulated in such a way that ALL objectives are either maximising or
# minimising. If your problem mixes both types of objectives, just add minus signs (-) in the
# results that require it..."
#
# Main reference for function: Kursawe (1991)

library(hydroMOPSO)

lower <- c(0, 0)
upper <- c(1, 1)


fnBasic <- function(param){
   
   x1 <- param[1]
   x2 <- param[2]

   obj1 <- -( x1 ) 
   obj2 <- -( (1 + 10*x2)*(1-(x1/(1+10*x2))^2 - x1/(1+10*x2)*sin(2*pi*4*x1)) )
   # note tha minus sign was added in obj1 and obj2

   out <- list(c(obj1, obj2)) # For consistency with further examples, this must be a list
   names(out) <- "Objs"       # The name "Objs" is a mandatory requirement

   return(out)
}

set.seed(100) # Setting the seed (for reproducible results)
out <- hydroMOPSO(fn  = fnBasic,
                  lower = lower,
                  upper = upper,
                  control=list(npart = 10, maxrep = 100, maxcross = 50,
                               MinMax = "max", maxit = 50, plot = TRUE) # note that now MinMax="max" 
                  )




###############################################################################################
# Example 3. Using 'smoof' package: Kursawe function
###############################################################################################
#
# This Benchmark function has two objectives (M = 2), its Pareto optimal front is discontinuous
# and non-convex. For this example it will be implemented with 3 decision variables (D = 3)
#
# Main reference for function: Kursawe (1991)

library(hydroMOPSO)
library(smoof)

D <- 3
lower <- rep(-5,D)
upper <- rep(5,D)

Kursawe <- smoof::makeKursaweFunction(D) # using 'smoof' package

fnKursawe <- function(param){
   
   objs <- Kursawe(x = param)
   obj1 <- objs[1]
   obj2 <- objs[2]

   out <- list(c(obj1, obj2)) # For consistency with further examples, this must be a list
   names(out) <- "Objs"       # The name "Objs" is a mandatory requirement

   return(out)
}

set.seed(100) # Setting the seed (for reproducible results)
out <- hydroMOPSO(fn  = fnKursawe,
                  lower = lower,
                  upper = upper,
                  control=list(npart = 10, maxrep = 100, maxcross = 50,
                               MinMax = "min", maxit = 50, plot = TRUE)
                  )




###############################################################################################
# Example 4. Using 'smoof' package: DTLZ2 function with three objectives
###############################################################################################
#
# In this example, this Benchmark is formulated with two objectives (M = 3) and 12 decision 
# variables (D = 12) its Pareto optimal front is concave.
#
# Main reference for function: Deb (2005)

library(hydroMOPSO)
library(smoof)

M <- 3
D <- 12
lower <- rep(0,D)
upper <- rep(1,D)

DTLZ2 <- smoof::makeDTLZ2Function(D, M) # using 'smoof' package

fnDTLZ2 <- function(param){
   
   objs <- DTLZ2(x = param)
   obj1 <- objs[1]
   obj2 <- objs[2]
   obj3 <- objs[3]

   out <- list(c(obj1, obj2, obj3)) # For consistency with further examples, this must be a list
   names(out) <- "Objs"       # The name "Objs" is a mandatory requirement

   return(out)
}

set.seed(100) # Setting the seed (for reproducible results)
out <- hydroMOPSO(fn  = fnDTLZ2,
                  lower = lower,
                  upper = upper,
                  control=list(npart = 10, maxrep = 100, maxcross = 50,
                               MinMax = "min", maxit = 50, plot = TRUE)
                  )

###############################################################################################
# Example 5. Calibration of GR4J hydrological model
###############################################################################################
#
# For this example, a "real-world" problem has been formulated: the calibration of a 
# hydrological model
#
# In detail...
# Hydrological model: GR4J (Perrin et al., 2004)
# Number of parameters: four (X1, X2, X3, X4; see Perrin et al. (2004))
# Study area: Trancura River Basin (RTL)
# Input variables: Precipitation (pcp) and Potetntial EvapoTranspiration (pet)
# Calibration output variable: Streamflow (qobs)

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

# hydroMOPSO calibration -----------------------------------

set.seed(100) # Setting the seed (for reproducible results)
Cal.results <- hydroMOPSO(fn="hydromodInR",
                          lower=lower,
                          upper=upper,
                          control=list(MinMax="max", Xini.type = "lhs", npart=10, 
                                       maxit=50,  # for better results set maxit=250 in this case
                                       maxrep = 100, maxcross = 50,
                                       maxeval = 15000, write2disk = FALSE,  REPORT=1,
                                       digits = 8, plot = TRUE, parallel = "none"),
                          model.FUN="GR4JExampleCal",
                          model.FUN.args = list(Obs=list.obs.Cal, # mandatory
                                                Objs.names = char.objs.names, # mandatory
                                                var.names = char.obs.names, # mandatory
                                                var.units = char.obs.units, # mandatory
                                                full.period = FullCal.dates, # mandatory
                                                warmup.period  = WarmUpCal.dates,
                                                cal.period = Cal.dates,
                                                # Model specific inputs
                                                InputsModel = InputsModel.Cal, # model specific
                                                RunOptions = RunOptions.Cal, # model specific
                                                area = basin.area # model specific
                                                )
                          )

}
} % end donttest
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multi-objective optimisation}
\keyword{multi-objetive calibration}
\keyword{hydrological model}