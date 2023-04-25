\name{hydroMOPSO-package}

\alias{hydroMOPSO-package}

\docType{package}

\title{
Multi-Objective Calibration of Hydrological Models using MOPSO
}

\description{

State-of-the-art Multi-Objective Particle Swarm Optimiser (MOPSO), based on the NMPSO algorithm developed by Lin et al. (2018), which in turn is based on the work by Coello and Lechuga (2002) and Kennedy and Eberhart (1995). To maintain diversity and accelerate convergence to the Pareto-optimal front (POF),  NMPSO combines two search mechanism (Lin et al., 2015), these being a PSO search and the application of genetic operators. Lin et al. (2015) also included a balanceable fitness estimation (BFE) procedure to rank particles in a \emph{external archive} (A), in order to provide an effective guidance to the true POF, while keeping diversity among particles. \code{hydroMOPSO} is developed with a focus on solving multi-objective problems with the least amount of function/model evaluations, taking the work done by Marinao-Rivas and Zambrano-Bigiarini (2021), who provided a default configuration of: i) the swarm size, ii) the maximum number of particles in the external archive, and iii) the maximum amount of genetic operations in the external archive.

This package and the attached tutorials have been developed with a special focus on the calibration of hydrological/environmental models and related real-world problems, but it can also be used for the calibration of any type of model that can be run from the command-line or the optimisation of certain functions to be defined by the user in the R environment.

For calibration problems, \code{hydroMOPSO} is model-independent, allowing the user to easily interface any computer simulation model with the calibration engine (NMPSO). Thus, the user only needs to indicate which model parameters will be modified and where, how and where to run the hydrological model, either in the R environment or from the system console; and finally how to read the model results. With these aspects properly structured by the user, the algorithm it will take control over the model to be calibrated until a maximum number of iterations is reached.

Adittionally, this package provides intuitive plot summaries and detailed information about optimisation performance. These features make it easier to interpret and assess the multi-objective calibration results, particularly for non-experts. The package offers a comprehensive set of tools that streamline the calibration process, from model parameter estimation to result analysis. With these features, users can quickly identify the best-performing models and gain deeper insights into the underlying processes and mechanisms of hydrological systems.

The operating mechanism of \code{hydroMOPSO} is based on the \code{hydroPSO} R package developed by Zambrano-Bigiarini and Rojas (2013), inheriting the philosophy of flexibility that allows dealing with any calibration problem with different fine-tuning options, as well as taking advantage of multicore machines or network clusters to alleviate the computational burden of complex models with \emph{long} execution time.


}
\details{
\tabular{ll}{
Package: \tab hydroMOPSO\cr
Type: \tab Package\cr
Version: \tab 0.1-3\cr
Date: \tab 2023-04-24\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
Packaged: \tab 2023-04-24; rmarinao \cr
BuiltUnder: \tab R version 4.3.0 (2023-04-21) -- "Already Tomorrow"; x86_64-pc-linux-gnu (64-bit)\cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Rodrigo Marinao-Rivas and Mauricio Zambrano-Bigiarini \cr
Maintainer: Rodrigo Marinao-Rivas <ra.marinao.rivas@gmail.com>
}
\references{

\cite{Coello, C. A. C., & Lechuga, M. S. (2002). MOPSO: A proposal for multiple objective particle swarm optimization. Proceedings of the 2002 Congress on Evolutionary Computation, CEC 2002, 2, 1051-1056. doi:10.1109/CEC.2002.1004388}

\cite{Kennedy, J., & Eberhart, R. (1995). Particle swarm optimization. Proceedings of ICNN95 - International Conference on Neural Networks, 4, 1942-1948. doi:10.1109/ICNN.1995.488968}

\cite{Lin, Q., Li, J., Du, Z., Chen, J., & Ming, Z. (2015). A novel multi-objective particle swarm optimization with multiple search strategies. European Journal of Operational Research, 247, 732-744. doi:10.1016/J.EJOR.2015.06.071}

\cite{Lin, Q., Liu, S., Zhu, Q., Tang, C., Song, R., Chen, J., ... Zhang, J. (2016). Particle Swarm Optimization With a Balanceable Fitness Estimation for Many-Objective Optimization Problems. IEEE Transactions on Evolutionary Computation, 22, 32-46. doi:10.1109/TEVC.2016.2631279}

\cite{Marinao-Rivas, R., & Zambrano-Bigiarini, M. (2021). Towards best default configuration settings for NMPSO in multi-objective optimization. 2021 IEEE Latin American Conference on Computational Intelligence, LA-CCI 2021. doi:10.1109/LA-CCI48322.2021.9769844}

\cite{Zambrano-Bigiarini, M., & Rojas, R. (2013). A model-independent Particle Swarm Optimisation software for model calibration. Environmental Modelling & Software, 43, 5-25. doi:10.1016/j.envsoft.2013.01.004}

}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%%~~ the R documentation directory ~~
\keyword{ package }
\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%\code{\link[hydroGOF:hydroGOF]{<pkg>}}
\url{https://CRAN.R-project.org/package=hydroPSO} \cr
\url{https://CRAN.R-project.org/package=hydroGOF} \cr
\url{https://CRAN.R-project.org/package=hydroTSM} \cr
}
%%\examples{
%%~~ simple examples of the most important functions ~~
%%}