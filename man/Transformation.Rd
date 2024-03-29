% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline.R
\name{Transformation}
\alias{Transformation}
\title{Transformation}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}

Container for the type of transformation used in
\code{\link[rearrr:Pipeline]{Pipeline}}.

\strong{Note}: For internal use.
}
\seealso{
Other transformation classes: 
\code{\link{FixedGroupsTransformation}},
\code{\link{GeneratedTransformation}}
}
\author{
Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
}
\concept{transformation classes}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{Name of transformation.}

\item{\code{fn}}{Transformation function.}

\item{\code{args}}{\code{list} of arguments for \code{`fn`}.}

\item{\code{group_cols}}{Names of columns to group \code{data.frame}
by before applying \code{`fn`}.

When \code{`NULL`}, the \code{data.frame} is not grouped.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Transformation-new}{\code{Transformation$new()}}
\item \href{#method-Transformation-apply}{\code{Transformation$apply()}}
\item \href{#method-Transformation-print}{\code{Transformation$print()}}
\item \href{#method-Transformation-clone}{\code{Transformation$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Transformation-new"></a>}}
\if{latex}{\out{\hypertarget{method-Transformation-new}{}}}
\subsection{Method \code{new()}}{
Initialize transformation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Transformation$new(fn, args, name = NULL, group_cols = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fn}}{Transformation function.}

\item{\code{args}}{\code{list} of arguments for \code{`fn`}.}

\item{\code{name}}{Name of transformation.}

\item{\code{group_cols}}{Names of columns to group \code{data.frame}
by before applying \code{`fn`}.

When \code{`NULL`}, the \code{data.frame} is not grouped.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Transformation-apply"></a>}}
\if{latex}{\out{\hypertarget{method-Transformation-apply}{}}}
\subsection{Method \code{apply()}}{
Apply the transformation to a \code{data.frame}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Transformation$apply(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{\code{data.frame}.

A grouped \code{data.frame} will first be ungrouped. If \code{`group_cols`} is specified,
it will then be grouped by those columns.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Transformed version of \code{`data`}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Transformation-print"></a>}}
\if{latex}{\out{\hypertarget{method-Transformation-print}{}}}
\subsection{Method \code{print()}}{
Print an overview of the transformation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Transformation$print(..., indent = 0, show_class = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{further arguments passed to or from other methods.}

\item{\code{indent}}{How many spaces to indent when printing.}

\item{\code{show_class}}{Whether to print the transformation class name.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The pipeline. To allow chaining of methods.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Transformation-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Transformation-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Transformation$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
