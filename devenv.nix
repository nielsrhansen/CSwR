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

      package =
        let
          CSwR = pkgs.rPackages.buildRPackage {
            name = "CSwR";
            src = pkgs.fetchFromGitHub {
              owner = "jolars";
              repo = "CSwR-package";
              rev = "1868e5c87d3c07002e2acff0ba6ee9c3a183fd80";
              sha256 = "sha256-TYdxwuPRzq74NbLPQW9kDxSAmsGCFqXIDT/vYcmDHm4=";
            };
            propagatedBuildInputs = with pkgs.rPackages; [
              bench
              ggplot2
              rlang
            ];
          };
        in
        (pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            CSwR
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
        });
    };
  };
}
