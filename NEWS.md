# tsbox 0.0.3 (2018-06-18)

## Features

- Support for tibbletime time series (#90)

## Bug fixes

- Automatically detect numbers or text from 1600 to 2200 as time column (#92)
- Fix to correctly ensure uniqueness of non unique id combinations (#93)
- Fix to correctly parse POSIXct columns for monthly data in different time 
  zones. Remove two way testing for daily series with POSIXct columns. This 
  should fix mac binary build on CRAN (#97)

# tsbox 0.0.2 (2018-05-12)

## Features

- initial version


