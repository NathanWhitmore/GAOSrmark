\name{synopsis}
\alias{synopsis}
\title{synopsis}

\usage{
synopsis(folder_name, data, site_species)
}


\description{
Combines differing analyses into a single data set and graph.
}


\arguments{

\itemize{
\item \code{folder_name}: the folder into which the outputs of the \code{synopsis} are to be delivered.
\item \code{data}: the name of the data object produced by the \code{amalgamate} function.
\item \code{site_species}: the site and species name for naming purposes.
}


}



\details{
The function will write the following outputs to the folder:
\itemize{
\item a graph labelled \code{"_synopsis"} in .png format.
\item a table of the estimates used to produce the graph  \code{""_synopsis""} in .csv format.
}
}



\examples{
synopsis(folder_name = "2018_synopsis", 
         data = my.data,
         site_species = "Airport_grand")
