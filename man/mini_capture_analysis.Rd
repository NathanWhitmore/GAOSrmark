\name{mini_capture_analysis}
\alias{mini_capture_analysis}

\title{mini_capture_analysis}

\usage{
mini_capture_analysis(path, site_species, year) 
}

\description{

An alternate version of the \code{closed_capture_analysis} function in which the heterogeneity models are removed. This model suite is based on just 2  models: \emph{M0} (detection probabilities constant), and \emph{Mt} (detection probabilities time varying). 

}

\format{

\itemize{
\item \code{path}: the internal path to the folder where the results and outputs should be deposited.
\item \code{site_species}: the site and species name corresponding to those used by the \code{sloop_robust_design} function.
\item \code{year}: the year in which the closed capture monitoring occurred (required for graphs). Note \code{year} is numeric.   
 
}
}


\details{

\strong{Note: depends upon the \code{closed_capture_encounters} function being run first.}

The function will write the following outputs to the folder:
\itemize{
\item a AICc model selection table labelled \code{"_AICctable_closed"} in .csv format.
\item a table of population estimates from the top model labelled \code{"_estimates_closed"} in .csv format.
}

In addition, the function will return:

\itemize{
\item  a complete list of RMark files to the "Results" subfolder.

\strong{Note:} These RMark outputs can only be opened within R. The files are not named by model, but the model name can be found in the output text. The new files will not overwrite old files, consequently, the "Results" folder should be cleared before an analysis is run. 
}

}

\examples{
closed_capture_encounters(file.name = "Extract_Grand.csv",
                          site = "TopMini",
                          include1s = FALSE,
                          start_date = "2016-02-10",
                          finish_date = "2016-02-16",
                          year_folder = "2015_16")

mini_capture_analysis(path = "Closed_capture/2015_16/Grand/TopMini",
                      site_species = "TopMini_grand",
                      year = 2016)
}





