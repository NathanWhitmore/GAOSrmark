\name{sloop_model_average}
\alias{sloop_model_average}

\title{sloop_model_average}

\usage{
sloop_model_average(start_year)
}

\description{
Generates model averaged estimates from the outputs of \code{sloop_robust_design}. 

}

\format{

\itemize{
\item \code{start_year}: the year in which robust design monitoring began (typically 2006). This input is required because the .txt files derived from Sloop does not state the start year (this causes issues when graphing). Note: \code{start_year} is numeric.   
 
}
}


\details{
The function will write the following outputs to the folder:
\itemize{
\item a graph labelled \code{"_average_robust"} in .png format.
\item a table of population estimates labelled \code{"_average_robust"} in .csv format.
}

}

\examples{
sloop_robust_design(path = "Sloop_robust_design/2012_13/Grand/Airport",
                    site_species = "Airport_grand",
                    start_year = 2006)

sloop_model_average(start_year = 2006)
}



