% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Columns.R
\name{Columns}
\alias{Columns}
\title{DataFrame Columns Class}
\description{
A class for column names of data objects of class \code{DataFrame}.
}
\examples{

## ------------------------------------------------
## Method `Columns$drop`
## ------------------------------------------------

x <- DF(data.frame(a=1:5, b=1:5))
x$columns$drop("b") #
x$columns$names

## ------------------------------------------------
## Method `Columns$reorder`
## ------------------------------------------------

x <- DF(data.frame(a=1:5, b=1:5))
x$columns$reorder(c("b", "a")) # same as x_cols$reorder("b")
x

## ------------------------------------------------
## Method `Columns$rename`
## ------------------------------------------------

x <- DF(data.frame(a=1:5, b=1:5))
x$columns$rename(c("a"="A", "b"="B"))

## ------------------------------------------------
## Method `Columns$rename_with`
## ------------------------------------------------

x <- DF(data.frame(a=1:5, b=1:5))
x$columns$rename_with(toupper)
custom_mapper = function(x) {return(paste0(x, 1))}
x$columns$rename_with(custom_mapper)
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{names}}{Vector of column names.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Columns-new}{\code{Columns$new()}}
\item \href{#method-Columns-print}{\code{Columns$print()}}
\item \href{#method-Columns-drop}{\code{Columns$drop()}}
\item \href{#method-Columns-reorder}{\code{Columns$reorder()}}
\item \href{#method-Columns-rename}{\code{Columns$rename()}}
\item \href{#method-Columns-rename_with}{\code{Columns$rename_with()}}
\item \href{#method-Columns-clone}{\code{Columns$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Columns-new"></a>}}
\if{latex}{\out{\hypertarget{method-Columns-new}{}}}
\subsection{Method \code{new()}}{
Constructor
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Columns$new(env = private)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{env}}{Private environment of the passed \code{DataFrame}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Columns-print"></a>}}
\if{latex}{\out{\hypertarget{method-Columns-print}{}}}
\subsection{Method \code{print()}}{
Print method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Columns$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Columns-drop"></a>}}
\if{latex}{\out{\hypertarget{method-Columns-drop}{}}}
\subsection{Method \code{drop()}}{
Drop columns in place
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Columns$drop(columns)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{columns}}{Character vector of the column names to remove.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- DF(data.frame(a=1:5, b=1:5))
x$columns$drop("b") #
x$columns$names
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Columns-reorder"></a>}}
\if{latex}{\out{\hypertarget{method-Columns-reorder}{}}}
\subsection{Method \code{reorder()}}{
Reorder columns in place
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Columns$reorder(order = key(private$df_env$tbl))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{order}}{Character vector of the new column name ordering.
May also be column numbers. If length(order) < length(x),
the specified columns are moved in order to the "front" of x.
By default, reorder without a specified order moves the key columns in order to the "front".}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- DF(data.frame(a=1:5, b=1:5))
x$columns$reorder(c("b", "a")) # same as x_cols$reorder("b")
x
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Columns-rename"></a>}}
\if{latex}{\out{\hypertarget{method-Columns-rename}{}}}
\subsection{Method \code{rename()}}{
Rename column names in place.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Columns$rename(mapping)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mapping}}{Named character vector. Names of the vector elements are the old names and the elements itself are the new names.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- DF(data.frame(a=1:5, b=1:5))
x$columns$rename(c("a"="A", "b"="B"))
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Columns-rename_with"></a>}}
\if{latex}{\out{\hypertarget{method-Columns-rename_with}{}}}
\subsection{Method \code{rename_with()}}{
Rename column names in place using a mapping function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Columns$rename_with(mapper)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mapper}}{Function that accepts old column names as a character vector and returns a character vector of new column names.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{x <- DF(data.frame(a=1:5, b=1:5))
x$columns$rename_with(toupper)
custom_mapper = function(x) {return(paste0(x, 1))}
x$columns$rename_with(custom_mapper)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Columns-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Columns-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Columns$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
