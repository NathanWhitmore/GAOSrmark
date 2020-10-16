\name{amalgamate}
\alias{amalgamate}
\title{amalgamate}
\usage{
my.data <- amalgamate(a, b, ...)
}
\description{
Combines objects produced by \code{find_robust} and \code{find_closed} functions into a single object.
}

\format{

\itemize{
\item \code{a, b, ...}: two or more objects to be combined.  
 
}

}
\examples{
a <- find_robust(period = "2012_13",
            species = "Grand",
            site = "Airport",
            average = FALSE)
            
b <- find_closed(period = "2015_16",
            species = "Grand",
            site = "Airport",
            average = FALSE)            

my.data <- amalgamate(a, b)

}




