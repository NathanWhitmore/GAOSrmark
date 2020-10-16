\name{sloop_robust_design}
\alias{sloop_robust_design}

\title{sloop_robust_design}

\usage{
sloop_robust_design(path, site_species, start_year)
}

\description{
Runs the Huggins variant of the robust design through program MARK using the RMark package. It tests 12 candidate models based on the code of Andy Hutcheon. This model suite is based on 4 core models: \emph{M0} (detection probabilities constant), \emph{Mt} (detection probabilities time varying),  \emph{Mh} (detection probabilities constant with a two mixture heterogeneous population), and \emph{Mht} (detection probabilities time varying with a two mixture heterogeneous population). On top of these 3 different sets of movement models are examined : (1) no movement, (2) random movement, and (3) Markovian movement (in which the the probability of the animal migrating differs according to whether it is or is not detected initially). 

The function will write the following outputs to the folder:
\itemize{
\item a graph labelled \code{"_estimates_robust"} in .png format.
\item a AICc model selection table labelled \code{"_AICctable_robust"} in .csv format.
\item a table of population estimates from the top model labelled \code{"_estimates_robust"} in .csv format.
}

In addition, the function will return:

\itemize{
\item  a complete list of RMark files to the "Results" subfolder.

\strong{Note:} These RMark outputs can only be opened within R. The files are not named by model, but the model name can be found in the output text. The new files will not overwrite old files, consequently, the "Results" folder should be cleared before an analysis is run. 
}



}


\format{

\itemize{
\item \code{path}: path within project folder to .inp and .txt files.
\item \code{site_species}: site species name to find the correct .inp and .txt files. 
\item \code{start_year}: the year in which robust design monitoring began (typically 2006). This input is required because the .txt files derived from Sloop do not state the start year (this causes issues when graphing). Note: \code{start_year} is numeric. 
}
}


\examples{
sloop_robust_design(path = "Sloop_robust_design/2012_13/Grand/Airport",
                    site_species = "Airport_grand",
                    start_year = 2006)

}





