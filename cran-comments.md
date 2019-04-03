## Fixes

- Do not test examples that led to NOTE on a CRAN flavor

Flavor: r-devel-linux-x86_64-debian-gcc
Check: examples, Result: NOTE
 Examples with CPU time > 2.5 times elapsed time
               user system elapsed ratio
 ts_span      1.080  0.008   0.369 2.949
 ts_frequency 1.928  0.064   0.698 2.854

## Test environments

- macOS Mojave, 10.14.4, R version 3.5.3 (local, with --run-donttest)
- Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub)
- Fedora Linux, R-devel, clang, gfortran (rhub)
- Ubuntu Linux 16.04 LTS, R-release, GCC (rhub)
- Windows 64-bit, R version 3.5.3 (2019-03-11) (winbuilder)

## Multiple Cores

- Imports and uses package 'data.table', which uses multiple cores


Thanks for everything!
