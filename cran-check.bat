setlocal
set PATH=C:\Program Files\R\R-devel\bin\x64;%PATH%
R CMD check --as-cran ../ggpmisc_0.2.5.tar.gz
pause
