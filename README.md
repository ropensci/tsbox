Time Series Toolbox
===================

Very alpha!

```
devtools::install_github("christophsax/tsbox")

```


Examples:

```

library(tsbox)
library(data.table)

# Convert everything to everything

x.ts <- tsbind(mdeaths, fdeaths) 
x.xts <- as_xts(x.ts)
x.df <- as_df(x.ts)
x.dt <- as_dt(x.ts)



# Use same methods for ts, xts, data.frame or data.table

tsscale(x.ts)
tsscale(x.xts)
tsscale(x.df)
tsscale(x.dt)

tstrend(x.ts)
tstrend(x.xts)
tstrend(x.df)
tstrend(x.dt)

tspc(x.ts)
tspc(x.xts)
tspc(x.df)
tspc(x.dt)

tspcy(x.ts)
tspcy(x.xts)
tspcy(x.df)
tspcy(x.dt)

