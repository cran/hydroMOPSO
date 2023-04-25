\name{hydromod}

\alias{hydromod}

\title{
Definition and execution of the model to be optimised using an executable file that runs out of R (system console or external script)
}
\description{
It runs a user-defined model to be optimised and returns the output variables of the model requested by the user according to the number of functions indicated in the list \code{out.FUNs}.
This specific function was designed to run an executable file from the system console
}
\usage{
hydromod(param.values,
         param.files="ParamFiles.txt",
         model.drty=getwd(),
         exe.fname,
         exe.args = character(),
         stdout=FALSE,
         stderr="",
         verbose= FALSE,
         out.FUNs,
         out.FUNs.args
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param.values}{(\code{numeric}) \cr
A numeric vector with the parameter set used to run the model specified in \code{exe.fname}.
}
  \item{param.files}{(\code{character}) \cr
character, file name (full path) storing location and names of the files that have to be modified for each parameter. \cr
By default \code{param.files="ParamFiles.txt"}
}
  \item{model.drty}{(\code{character}) \cr
Path to the executable file of the model specified in \code{exe.fname}. ALL the files required to run the model have to be located within this directory (input files for the model may be located in a different directory, if properly referenced).
}
  \item{exe.fname}{(\code{character}) \cr
Model command line arguments to be entered through a prompted string to execute the user-defined model.
}
  \item{exe.args}{(\code{character}) \cr
Optional arguments to be passed in the command line to the user-defined model.
}
  \item{stdout}{(\code{logical or character}) \cr
Where output to \sQuote{stdout} should be sent. Possible values are \code{FALSE} (discard output, the default), \code{""}, to the R console. See \code{\link[base]{system2}}\cr
By default \code{stdout=FALSE} and any message printed by the model code to the screen will be omitted. This setting is recommended when calibrating the model with \code{hydroMOPSO}. However, when trying to run the model code with \code{hydromod} by the first time, it is recommend to set \code{stdout=""}, in order to detect if the model was properly executed or not. \cr
}
  \item{stderr}{(\code{logical or character}) \cr
Where output to \sQuote{stderr} should be sent. Possible values are \code{FALSE} (discard output, the default), \code{""}, to the R console. See \code{\link[base]{system2}}\cr
By default \code{stderr=""} and any error message of the model code will be printed to the screen
}
  \item{verbose}{(\code{logical}) \cr
Indicate if progress messages are printed to the screen \cr
If \code{verbose=TRUE}, the following messages will appear: i) parameter values for each particle; (ii) model execution; iii) extraction of simulated values; and iv) computation of the goodness-of-fit measures
}
  \item{out.FUNs}{(\code{list}) \cr
Name of valid R functions to read the model outputs and transform them into a (zoo) object (Should generally require at least basic use of \code{\link[utils]{read.table}} or \code{\link[utils]{read.csv}}. The list must have as many elements (names) as output variables of the model to read.
}
  \item{out.FUNs.args}{(\code{list}) \cr
At a first level, each object inside this list corresponds to a set of arguments that, RESPECTIVELY, must be passed to \code{out.FUNs}. At a second level, the arguments to read each of the output variables are entered as lists (let's say sub-lists).
}
  
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A list with as many output variables (usually time series in zoo class) as functions listed in out.FUNs 
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}, Rodrigo Marinao Rivas \email{ra.marinao.rivas@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hydroMOPSO}}
}
%%\examples{
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{files}
\keyword{optimisation}% __ONLY ONE__ keyword per line
\keyword{calibration}
\keyword{hydrological model}