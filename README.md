# BankscopeR
Load Bankscope Data to R

# Install

To install BankscopeR use ``devtools::install_github("DavZim/BankscopeR")`.

A minimum working example is to load multiple Excel-exports using the `load_folder`-function.

```
dt <- load_folder("../data/bankscope_export")

summary(dt)
```

To save and compress the data use the `save_data`-function (it allows for `zip`-files, `RDS`, and `csv`, where the first option gives the best results)
```
save_data(dt, file = "mydata.zip")
```

To load the data again use the `load_data`-function

```
dt2 <- load_data("mydata.zip")
```
