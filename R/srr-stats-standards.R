#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
# General Dokumentation
#' @srrstats {G1.0} *Statistical Software should list at least one primary reference from published academic literature.*
#' @srrstats {G1.1} *Statistical Software should document whether the algorithm(s) it implements are:* - *The first implementation of a novel algorithm*; or - *The first implementation within **R** of an algorithm which has previously been implemented in other languages or contexts*; or - *An improvement on other implementations of similar algorithms in **R***.
#' @srrstats {G1.2} *Statistical Software should include a* Life Cycle Statement *describing current and anticipated future states of development.*
#' @srrstats {G1.3} *All statistical terminology should be clarified and unambiguously defined.*
#'
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
#'  Applied to all internal functions
#'
#' @srrstats {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstats {G2.0a} Provide explicit secondary documentation of any expectations on lengths of inputs
#' @srrstats {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
#'   Done everywhere.
#' @srrstats {G2.1a} *Provide explicit secondary documentation of expectations on data types of all vector inputs.*
#'   Done everywhere.
#' @srrstats {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' @srrstats {G2.3} *For univariate character input:*
#' @srrstats {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#' @srrstats {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#'   This is tested by 'autotest'
#'
#' @srrstats {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstats {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstats {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstats {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstats {G2.4d} *explicit conversion to factor via `as.factor()`*
#' @srrstats {G2.4e} *explicit conversion from factor via `as...()` functions*
#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstats {G2.7} *Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.*
#'   Accepts data.frame, tibble, data.table, plus domain-specific time series classes
#' @srrstats {G2.8} *Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.*
#'   This is at the core of tsbox: Everything is converted to a 'dts' object, operations are performed on theser, then they are converted back.
#' @srrstats {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*

#' @srrstats {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
#'   All functions work identically on data.frame, tibble, data.table
#' @srrstats {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*
#'   Usage of the 'units' package is tested
#' @srrstats {G2.12} *Software should ensure that `data.frame`-like tabular objects which have list columns should ensure that those columns are appropriately pre-processed either through being removed, converted to equivalent vector columns where appropriate, or some other appropriate treatment such as an informative error. This behaviour should be tested.*
#'   For now, gives an appropriate error. A planned feature will allow for nested dfs everywhere.
#' @srrstats {G2.14c} *replace missing data with appropriately imputed values*
#'   Use `ts_na_interpolation()` to replace missing by various algorithms. See
#'   ?imputeTS::na_interpolation
#' @srrstats {G2.15} *Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*
#'   Functions do not assume non-missingness, which is tested.
#' @srrstats {G2.16} *All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.*
#'   NaN, Inf, and -Inf are kept as such. Tt is tested that NaN values preserve
#'   and can be omitted an interpolated the same way as NA. Inf and -Inf will be
#'   treated as finite numbers.

#' @srrstats {TS2.0} *Time Series Software which presumes or requires regular data should only allow **explicit** missing values, and should issue appropriate diagnostic messages, potentially including errors, in response to any **implicit** missing values.*
#'   In data.frame-like structures, tsbox allows explicit and implicit NA values
#'   on purpose. ts_regular() can be used to make all NAs explicit, ts_na_omit()
#'   omits them. It is tested that implicit and explicit NAs are treated the
#'   same.

#' @srrstats {G3.0} *Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made between integers, or use appropriate tolerances for approximate equality.*
#'   Either compare integers, or use `is_near()` for numeric equality

#' @srrstats {G4.0} *Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffices are automatically generated where not provided.*
#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#'   Uses R datasets everywhere

#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#'   All errors and messages are unique and tested. No warnings are used.
#'
#' @srrstats {G5.3} *For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return objects should be explicitly tested.*
#'   They are tested.

#' @srrstats {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstats {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
#'   All conversions are tested both-ways, ensuring that transforming into
#'   another class and back results in the orignal values. For this to go wrong
#'   an error must cancel itself, which is unlikely.
#' @srrstats {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#'   A few transformation can be compared to other functions in R, such as
#'   `ts_lag()` with `stats::lag()`

# Edge conditions
#' @srrstats {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstats {G5.8a} *Zero-length data*
#' @srrstats {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstats {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*

#' @srrstats {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
#' @srrstats {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
#'   Checked by autotest
#' @srrstats {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*

#' @srrstats {TS1.0} *Time Series Software should use and rely on explicit class systems developed for representing time series data, and should not permit generic, non-time-series input*
#'   This is the core of tsbox.
#' @srrstats {TS1.1} *Time Series Software should explicitly document the types and classes of input data able to be passed to each function.*
#'   Done for all functions.
#' @srrstats {TS1.2} *Time Series Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).*
#'   Done for all functions.
#' @srrstats {TS1.3} *Time Series Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions (the [`tsbox` package](https://docs.ropensci.org/tsbox/) provides one convenient approach for this).*
#'   This is at the core of tsbox: Everything is converted to a 'dts' object, operations are performed on theser, then they are converted back.
#' @srrstats {TS1.4} *The pre-processing function described above should maintain all time- or date-based components or attributes of input data.*
#'   All ts-boxable classes as well as 'dts' objects keep this information intact.
#' @srrstats {TS1.5} *The software should ensure strict ordering of the time, frequency, or equivalent ordering index variable.*
#' @srrstats {TS1.6} *Any violations of ordering should be caught in the pre-processing stages of all functions.*
#' @srrstats {TS1.7} *Accept inputs defined via the [`units` package](https://github.com/r-quantities/units/) for attributing SI units to R vectors.*
#'   Units definied via the `units` package, stay alive as long as one works with data frame like objects (data table, tibble). ts objects currently loose the unit, as they are not supported by `units`.
#' @srrstats {TS1.8} *Where time intervals or periods may be days or months, be explicit about the system used to represent such, particularly regarding whether a calendar system is used, or whether a year is presumed to have 365 days, 365.2422 days, or some other value.*
#'   A vignette documents the time conversion and lists the applied period
#'   lenght (`vignette(package = "tsbox", "convert")`)
#' @srrstats {TS2.5} *Incorporate a system to ensure that both row and column orders follow the same ordering as the underlying time series data. This may, for example, be done by including the `index` attribute of the time series data as an attribute of the covariance matrix.*
#'   All output keeps column order. Row order is re-arranged by time if needed, in line with {TS1.5}. ts_default() reorders columns to id columns, `time` and `value`.
#' @srrstats {TS4.0} *Return values should either:*
#' @srrstats {TS4.0a} *Be in same class as input data, for example by using the [`tsbox` package](https://docs.ropensci.org/tsbox/) to re-convert from standard internal format (see 1.4, above); or*
#'   The core of this package.
#' @srrstats {TS4.1} *Any units included as attributes of input data should also be included within return values.*
#'   Units definied via the `units` package stay alive as long as one works with data frame like objects (data table, tibble). ts objects currently loose the unit, as they are not supported by `units`.
#' @srrstats {TS4.2} *The type and class of all return values should be explicitly documented.*
#'   Done throughout the package and ensured by autotest
#' @srrstats {TS4.3} *Return values should explicitly include all appropriate units and/or time scales*
#'   Units definied via the `units` package stay alive as long as one works with data frame like objects (data table, tibble). ts objects currently loose the unit, as they are not supported by `units`.
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#'
#' @srrstatsNA {G1.5} *Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications.*
#'   No such claims are made.
#' @srrstatsNA {G1.6} *Software should include code necessary to compare performance claims with alternative implementations in other R packages.*
#'   No performance comparisons are made.
#' @srrstatsNA {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#'   No comparisons to results from published paper
#' @srrstatsNA {G5.5} *Correctness tests should be run with a fixed random seed*
#'   No seed dependency of tests
#' @srrstatsNA {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#'   Parameter recovery tests do not make sense for the functions implemented in tsbox.
#' @srrstatsNA {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstatsNA {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G4.10-4.12, below).*
#'   See {G5.6}


#' @srrstatsNA {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#'   No algorithm performance tests are performed.

#' @srrstatsNA {G5.11} *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#'   No extended tests require large data sets
#' @srrstatsNA {G5.11a} *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#'   No downloads of additional data
#' @srrstatsNA {G5.12} *Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
#'   No conditions apply.
#'


#' @srrstatsNA {TS2.2} *Consider stationarity of all relevant moments - typically first (mean) and second (variance) order, or otherwise document why such consideration may be restricted to lower orders only.*
#'   Validity of tsbox transformations generally does not depend on stationarity, so no such checks are performed.
#' @srrstatsNA {TS2.3} *Explicitly document all assumptions and/or requirements of stationarity*
#'   Validity of tsbox transformations generally does not depend on stationarity, so no such checks are performed.
#' @srrstatsNA {TS2.4} *Implement appropriate checks for all relevant forms of stationarity, and either:*
#'   Validity of tsbox transformations generally does not depend on stationarity, so no such checks are performed.
#' @srrstatsNA {TS2.4a} *issue diagnostic messages or warnings; or*
#'   Validity of tsbox transformations generally does not depend on stationarity, so no such checks are performed.
#' @srrstatsNA {TS2.4b} *enable or advise on appropriate transformations to ensure stationarity.*
#' @srrstatsNA {TS4.0b} *Be in a unique, preferably class-defined, format.*
#'   Alternative {TS4.0a} is used.

#' @srrstatsNA {G2.5} *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.*
#'   No inputs are expected to be of `factor` type

#' @srrstatsNA {G3.1} *Statistical software which relies on covariance calculations should enable users to choose between different algorithms for calculating covariances, and should not rely solely on covariances from the `stats::cov` function.*
#'   No covariance calculations are performed.
#' @srrstatsNA {G3.1a} *The ability to use arbitrarily specified covariance methods should be documented (typically in examples or vignettes).*
#'   No covariance calculations are performed.
#' @srrstatsNA {G5.10} *Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS=1` environment variable.*
#'   No extended tests are used. >600 tests run in 10s in parallel, wich is fine.
#' @srrstatsNA {TS2.6} *Where applicable, covariance matrices should also include specification of appropriate units.*

#' @srrstatsNA {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#'   No specific test data set is used.

#' @srrstatsNA {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*
#'   Missing values are allowed and are processed in all ts-boxable classes. In
#'   data.frame-like classes, they are used to ensure regularity (which can be
#'   enforced by `ts_regular`). In data.frame-like classes, they can be omitted,
#'   usually withouth a loss of information (`ts_na_omit()`). In regular time
#'   series classes, such as `ts`, NAs will be kept.
#' @srrstatsNA {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#'   see explanation on {G2.13}
#' @srrstatsNA {G2.14a} *error on missing data*
#'   see explanation on {G2.13}
#' @srrstatsNA {G2.14b} *ignore missing data with default warnings or messages issued*
#'   see explanation on {G2.13}


#' @srrstatsNA {TS2.1} *Where possible, all functions should provide options for users to specify how to handle missing data, with options minimally including:*
#'   see explanation on {G2.13}
#' @srrstatsNA {TS2.1a} *error on missing data; or.
#'   see explanation on {G2.13}
#' @srrstatsNA {TS2.1b} *warn or ignore missing data, and proceed to analyse irregular data, ensuring that results from function calls with regular yet missing data return identical values to submitting equivalent irregular data with no missing values; or*
#'   see explanation on {G2.13}
#' @srrstatsNA {TS2.1c} *replace missing data with appropriately imputed values.*


#' @srrstatsNA {TS3.0} *Provide tests to demonstrate at least one case in which errors widen appropriately with forecast horizon.*
#'   tsbox contains `ts_forecast()` mainly to showcase how function from other
#'   packages can be wrapped as a ts-boxable function. `ts_forecast()` is meant
#'   to give a quick forecast of time series, without going into the details.
#'   Adding foreacast errors would make it less useful for this purpose.
#' @srrstatsNA {TS3.1} *If possible, provide at least one test which violates TS3.0*
#' @srrstatsNA {TS3.2} *Document the general drivers of forecast errors or horizons, as demonstrated via the particular cases of TS3.0 and TS3.1*
#' @srrstatsNA {TS3.3} *Either:*
#' @srrstatsNA {TS3.3a} *Document, preferable via an example, how to trim forecast values based on a specified error margin or equivalent; or*
#' @srrstatsNA {TS3.3b} *Provide an explicit mechanism to trim forecast values to a specified error margin, either via an explicit post-processing function, or via an input parameter to a primary analytic function.*
#' @srrstatsNA {TS4.4} *Document the effect of any such transformations on forecast data, including potential effects on both first- and second-order estimates.*
#' @srrstatsNA {TS4.5} *In decreasing order of preference, either:*
#' @srrstatsNA {TS4.5a} *Provide explicit routines or options to back-transform data commensurate with original, non-stationary input data*
#' @srrstatsNA {TS4.5b} *Demonstrate how data may be back-transformed to a form commensurate with original, non-stationary input data.*
#' @srrstatsNA {TS4.5c} *Document associated limitations on forecast values*
#' @srrstatsNA {TS4.6} *Time Series Software which implements or otherwise enables forecasting should return either:*
#' @srrstatsNA {TS4.6a} *A distribution object, for example via one of the many packages described in the CRAN Task View on [Probability Distributions](https://cran.r-project.org/web/views/Distributions.html) (or the new [`distributional` package](https://pkg.mitchelloharawild.com/distributional/) as used in the [`fable` package](https://fable.tidyverts.org) for time-series forecasting).*
#' @srrstatsNA {TS4.6b} *For each variable to be forecast, predicted values equivalent to first- and second-order moments (for example, mean and standard error values).*
#' @srrstatsNA {TS4.6c} *Some more general indication of error associated with forecast estimates.*
#' @srrstatsNA {TS4.7} *Ensure that forecast (modelled) values are clearly distinguished from observed (model or input) values, either (in this case in no order of preference) by*
#' @srrstatsNA {TS4.7a} *Returning forecast values alone*
#' @srrstatsNA {TS4.7b} *Returning distinct list items for model and forecast values*
#' @srrstatsNA {TS4.7c} *Combining model and forecast values into a single return object with an appropriate additional column clearly distinguishing the two kinds of data.*
#'   See {TS3.0}

#' @srrstatsNA {TS5.1} *When representing results in temporal domain(s), ensure that one axis is clearly labelled "time" (or equivalent), with continuous units.*
#'   This could be done, but it seems an unnessecary waste of scree space. Many
#'   newspaper outlets, such as Economist or Financial Times, don't do it.
#'   I think this standard should be weakened.
#' @srrstatsNA {TS5.3} *Ensure that units of the time, frequency, or index variable are printed by default on the axis.*
#'   `ts_plot()` is meant to provide a quick glimpse at any time series. In
#'   principle, support for the `units` package is possible, but there are
#'   considerable challenges to deal with different units among series, so I
#'   would rather not doing it at the moment.
#' @srrstatsNA {TS5.4} *For frequency visualization, abscissa spanning $[-\pi, \pi]$ should be avoided in favour of positive units of $[0, 2\pi]$ or $[0, 0.5]$, in all cases with appropriate additional explanation of units.*
#'   No frequency visualization.
#' @srrstatsNA {TS5.5} *Provide options to determine whether plots of data with missing values should generate continuous or broken lines.*
#' @srrstatsNA {TS5.6} *By default indicate distributional limits of forecast on plot*
#'   No particular forecast plot.
#' @srrstatsNA {TS5.7} *By default include model (input) values in plot, as well as forecast (output) values*
#'   No particular forecast plot.
#' @srrstatsNA {TS5.8} *By default provide clear visual distinction between model (input) values and forecast (output) values.*
#'   No particular forecast plot.


#' @noRd
NULL
