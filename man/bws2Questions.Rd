\name{bws2Questions}
\alias{bws2Questions}


\title{Display BWS2 Questions}

\description{
Display a series questions for BWS2.
}

\details{
This dialog box displays a series of questions for BWS2 according to
the profiles designed in the \sQuote{\code{Design Choice Sets for BWS2}}
dialog box using the function 
\code{\link[support.BWS2]{bws2.questionnaire}} in \pkg{support.BWS2}.
For details on the results of this dialog box, 
refer to help for \code{\link[support.BWS2]{bws2.questionnaire}}. 

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Design}: An R object name containing BWS2 choice sets generated 
in the \sQuote{\code{Design Choice Sets for BWS2}} dialog box.
The default name \sQuote{\code{BWS2design}} is displayed automatically.
The correct name should be entered if the name for design (\code{BWS2design})
was changed in the dialog box for BWS2 design.

\item \code{Attributes and levels}: An R object name containing attributes
and their levels set in the previous dialog box. 
The object name \sQuote{\code{BWS2attributes}}
cannot be changed (A fixed variable is colored in green). 

\item \code{Position of attribute column}: A position where attribute
levels are shown in questions. Select one from three radio buttons
corresponding to \sQuote{\code{Left}}, \sQuote{\code{Center}}, and
\sQuote{\code{Right}}.
}

}

