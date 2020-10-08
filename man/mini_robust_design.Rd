\name{mini_robust_design}

\title{mini_robust_design}

\usage{
mini_robust_design(path, site_species, start_year)
}


\description{
An alternate version of the \code{sloop_robust_design} function in which the heterogeneity models are removed. This function is useful at sites with  sample sizes (e.g. mini enclosures), as under such situations the \code{sloop_robust_design} function can often produce nonsensical population estimates. Consequently, this function should be always be used for the mini enclosures. It can also be used for other sites if the heterogeneity models are causing spurious estimates to be produced.


}


\format{

\itemize{
\item \code{path}: path within project folder to .inp and .txt files.
\item \code{site_species}: site species name to find the correct .inp and .txt files. 
\item \code{start_year}: the year in which robust design monitoring began (typically 2006). This input is required because the .txt files derived from Sloop do not state the start year (this causes issues when graphing).
}


}



}


\details{
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
\examples{
mini_robust_design(path = "Sloop_robust_design/2012_13/Grand/BottomMini",
                   site_species = "BottomMini_grand",
                   start_year = 2006)


}



}

