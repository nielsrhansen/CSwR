- [Protocols, Conventions and FAQ: CSwR](#protocols-conventions-and-faq-cswr)
  - [Protocols](#protocols)
    - [Source location and management](#source-location-and-management)
    - [Compiling the book](#compiling-the-book)
    - [The CSwR package](#the-cswr-package)
    - [Using the book](#using-the-book)
  - [Conventions](#conventions)
    - [Code](#code)
    - [Organization](#organization)
    - [Notation](#notation)
    - [Terminology](#terminology)
    - [LaTeX details](#latex-details)
  - [Book structure and code progression](#book-structure-and-code-progression)
    - [2. Smoothing](#2-smoothing)
    - [3. Scatterplot smoothing](#3-scatterplot-smoothing)
    - [4. Time series smoothing](#4-time-series-smoothing)
    - [4. Univariate random variables](#4-univariate-random-variables)
    - [5. Monte Carlo Integration](#5-monte-carlo-integration)
    - [6. Likelihood and optimization](#6-likelihood-and-optimization)
    - [8. Numerical optimization](#8-numerical-optimization)
    - [9. Expectation maximization
      algorithms](#9-expectation-maximization-algorithms)
    - [10. Stochastic optimization](#10-stochastic-optimization)
    - [11. The stochastic EM algorithm](#11-the-stochastic-em-algorithm)
    - [Appendix A. R programming](#appendix-a-r-programming)
  - [FAQ](#faq)

# Protocols, Conventions and FAQ: CSwR

The book Computational Statistics with R is written using
[bookdown](https://bookdown.org/). It is (will be) published by
Chapman & Hall/CRC Statistical Science series and is available
[online](https://cswr.nrhstat.org).

This document collects some protocols used for writing the book and developing
the code included in the book, and some conventions that the book adheres to.

## Protocols

### Source location and management

The book source resides in directory CSwR. This is the project directory of
RStudio project CSwR. The project uses the renv package for maintaining the
project library of packages used. The directory is under git version control
with a remote *public* [copy on GitHub](https://github.com/nielsrhansen/CSwR/).

Compiled HTML and pdf versions of the book are in the CSwR_html and CSwR_pdf
directories, respectively, both located in the parent directory of CSwR.

CSwR*html is under git version control with a remote _private*
[copy on GitHub](https://github.com/nielsrhansen/CSwR_html). The
[online](https://cswr.nrhstat.org) version is published from this repository via
[Netlify](https://app.netlify.com/teams/nielsrhansen).

The accompanying package CSwR resides in the subdirectory CSwR_package. It has
its own RStudio project, which can be opened to develop and build the package.

### Compiling the book

Install the following packages

```r
install.packages(
    c(
        "renv",
        "tidyverse",
        "rbookdown",
        "lintr",
        "Rcpp",
)
```

The HTML version of the book is compiled by the shell script
`build_html_book.sh`. Configuration is done by `output.yml` and by the included
`style.css` file.

To compile a pdf version you need the `build_pdf_book.sh` and various other
files that are not shared on the GitHub repo. Within RStudio, "Build All"
(SHIFT+CMD+B or click *Build All* in the Build pane) will also compile the pdf
book.

### The CSwR package

Uses roxygen2 for documentation.

### Using the book

- All packages needed are installed via installation of CSwR.
- Exercises and solutions

## Conventions

### Code

- Code follows the [Tidyverse Style Guide](https://style.tidyverse.org/)
- R functions from non-base packages are called as `package::function()`.
  Exceptions are: All ggplot2 functions. phipsi data from CSwR
- Functions are in text referred to as `function_name()`, e.g., `lm()`
- Code to be sourced for profiling is in the scripts directory
- No pseudo-code
- Dataset names are not capitalized
- Do we want to avoid using `tmp` or similar generic variable names?

### Organization

- Each chapter can be executed independently. No cross-chapter dependencies.
- The package CSwR contains data and a few selected R functions.

### Notation

- $N$ and `N` denote sample size throughout
- $n$ and `n` denote iteration index/number or other integer variables/arguments
- $i$, $j$, $k$, $l$ are data/parameter indices
- $x$ and $X$ are (univariate) data variables: Chapters 1, 2, 5, 6, 7,
- $y$ and $Y$ are (univariate) data variables, used when there is a need for
  $x$:
  - Chapters 3 (bivariate data), 4 ($x$ is time)
  - Chapters 8 and 9 (regression on $x$)
  - Chapter 10 ($x$ is unobserved, $y = M(x)$ is observed; potentially
    $x = (y, z)$)
  - Chapter
- $z$ and $Z$ are auxiliary data variables, typically latent/unobserved
- $u$ and $U$ are uniformly distributed variable
- $\ell$ denotes the log-likelihood, $-\ell$ the negative log-likelihood
- $H$ is a generic objective function to be minimized
- $P$ is a distribution, $P_N$ the empirical distribution
- Use \P, \E, \V, \cov for probability, expectation, variance and covariance.
  These are defined for pdf in latex/preamble.tex, and for HTML in
  mathjax_header.html

### Terminology and grammar

- Terminology conventions:
  - Use values/observations for individual data points (not samples)
  - A sample is a dataset
- The following terms are one word:
  - dataset
  - nonparametric
  - runtime
  - tradeoff
- The following terms are with a hyphen:
  - log-likelihood
  - mini-batch
- The following terms are in two words:
  - data point

**Johan's remark**: isn't it somewhat inconsistent to have "dataset" as one word
but "data point" as two words?

Which convention to follow on using contractions?

#### Questions

- Do we follow American English or British English?
- Oxford comma or no Oxford comma?
- En-dash (--) or em-dash (---) with no spacing for parenthetical statements?

### LaTeX details

## Book structure and code progression

The book consists of three parts on the topics: smoothing, simulation and
optimization. The introduction chapter gives an overview of the three topics.

The code topics/progression in the chapters are as outlined here:

### 2. Smoothing

- Loops and vectorization
- First benchmarks
- Runtime complexity
- A bit of linear algebra and grid-optimization

### 3. Scatterplot smoothing

- Numerical linear algebra
- While loop
- More grid optimization
- Profiling and optimizing

### 4. Time series smoothing

- Loops
- Convolutions and FFT
- Rcpp

### 4. Univariate random variables

- Low level (bit manipulation) via Rcpp and interfaces
- Function factories and while loops
- Benchmarking and tracing

### 5. Monte Carlo Integration

- Accuracy of results from randomized algorithms
- S3 objects
- Rcpp

### 6. Likelihood and optimization

### 8. Numerical optimization

- Function factories
- Convergence and stopping
- Tracing

### 9. Expectation maximization algorithms

- Numerical differentiation

### 10. Stochastic optimization

- Terminator functions
- Rcpp armadillo

### 11. The stochastic EM algorithm

### Appendix A. R programming

## FAQ

**Clear the cache**

Delete directories ...

**Publish the online book**

Commit and push all changes in CSwR_html to GitHub. Publication is then
automatic.

**Automatization of profiling**

How profiling is done in an automatized way to produce both a static png image
of profile data for direct inclusion and a dynamic html widget that can be
linked to.
