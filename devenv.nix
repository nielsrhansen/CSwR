{
  pkgs,
  ...
}:

{
  packages = [
    pkgs.git
    pkgs.bashInteractive
    pkgs.checkbashisms
    pkgs.go-task
    pkgs.air-formatter
    pkgs.panache
    pkgs.jarl
    pkgs.pandoc
    pkgs.chromium
  ];

  languages = {
    r = {
      enable = true;

      package = (
        pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            MASS
            Matrix
            MatrixModels
            Rcpp
            RcppArmadillo
            bench
            bookdown
            doParallel
            foreach
            codetools
            doParallel
            devtools
            downlit
            dqrng
            foreach
            future
            ggbeeswarm
            gridExtra
            mvtnorm
            patchwork
            renv
            htmlwidgets
            igraph
            knitr
            languageserver
            lubridate
            magick
            mgcv
            mirai
            movMF
            nlme
            numDeriv
            pagedown
            plotly
            profvis
            readr
            rmarkdown
            testthat
            tidyverse
            usethis
            xaringan
            zeallot
          ];
        }
      );
    };
  };
}
