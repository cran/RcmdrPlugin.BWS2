\name{bws2Design}
\alias{bws2Design}
\alias{resetBws2Table}


\title{Design Choice Sets for BWS2}

\description{
Design choice sets for BWS2 with an orthogonal design.
}

\details{
This dialog box designs choice sets (profiles) for BWS2 with an orthogonal
design using the function \code{\link[DoE.base]{oa.design}} in
\pkg{DoE.base}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for design}: An R object name into which choice sets
generated using the dialog box are stored. The default name 
\sQuote{\code{BWS2design}} can be changed following R's rules for naming
objects. If changed, you should take note of the new object name,
which will be used later.

\item \code{Name for attributes and levels}: An object name into which
attributes and their levels entered in the 
\sQuote{\code{Attributes and their levels}} table (explained later)
are stored. The default name \sQuote{\code{BWS2attributs}} cannot be
changed (A fixed variable is colored in green).

\item \code{Save to file}: A check box for saving both the resultant
design object and the items object to a single external file in RDA
format (\code{.rda}). If necessary, check this box.

\item \code{Attributes and their levels}: A specification (table) of
attributes and their levels used to generate BWS2 choice sets.

\item \code{Attribute}: An attribute variable name following R's rules
for naming variables. Type names without skipping from the first row.

\item \code{Level 1} to \code{Level 6}: Level variable names corresponding 
to the attribute entered in the first column. These variable names must
also follow R's rules for naming variables. Type names without skipping
from the \sQuote{\code{Level 1}}.

\item \code{Randomize the order of sets}: A check box for randomizing 
the row order of resultant choice sets. A row corresponds to a set in
the resultant choice sets.

\item \code{Seed for random number generator (optional)}: An integer
value used as a seed for generating random numbers. 
If reproducibility is needed, type an arbitrary value into this box.
}

}

