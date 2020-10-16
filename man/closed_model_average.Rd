\name{closed_model_average}
\alias{closed_model_average}

\title{closed_model_average}

\usage{
closed_model_average(site_species, year)
}

\description{
Generates model averaged estimates from the outputs of \code{closed_capture_analysis}. 
}

\format{



\itemize{
\item \code{site_species}: the site and species name corresponding to those used by the \code{sloop_robust_design} function.
\item \code{year}: the year in which the closed capture monitoring occurred (required for graphing). Note: \code{year} is numeric.    
 
}

}

\details{

\strong{Note: depends upon \code{closed_capture_analysis} function being run first.}

The function will write the following outputs to the folder:
\itemize{
\item closed capture population estimates labelled \code{"_averaged_closed"} in .csv format.
}
}




\examples{
closed_capture_encounters(file.name = "Extract_Grand.csv",
                          site = "Airport",
                          include1s = FALSE,
                          start_date = "2016-02-10",
                          finish_date = "2016-02-16")
                          
closed_capture_analysis(path = "Closed_capture/2015_16/Grand/Airport",
                        site_species = "Airport_grand",
                        year = 2016)                          

closed_model_average(site_species = "Airport_grand", year = 2016)
}



