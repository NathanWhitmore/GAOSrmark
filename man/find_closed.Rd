\name{find_closed}
\alias{find_closed}

\title{find_closed}

\usage{
find_closed(period,
            species,
            site,
            average = FALSE)
}

\description{
Retrieves estimate data from the specified closed capture analysis. This is a prerequisite for the \code{amalgamate} and the \code{synopsis} functions.
}

\format{

Note: these arguments are required to accurately locate the file.

\itemize{
\item \code{period}: monitoring season (e.g. "2015_16")
\item \code{species}: the species name (e.g. "Grand")
\item \code{site}: the site name (e.g. "Airport")
\item \code{average}: whether  model averaging is require (e.g. FALSE)           

}
}



\examples{
b <- find_closed(period = "2015_16",
                 species = "Grand",
                 site = "Airport",
                 average = FALSE)
}




