library(BankscopeR)


save_data(dt, file = "myexport.zip", verbose = T) # roughly 19.4 MB
save_data(dt, file = "myexport.RDS", verbose = T) # roughly 19.6 MB
# save_data(dt, file = "myexport.csv", verbose = T) # roughly 1.1GB

dt_new <- load_data("myexport.zip", verbose = T)
all.equal(dt, dt_new) # basically identical
dt_new_rds <- load_data("myexport.RDS", verbose = T)
all.equal(dt, dt_new_rds)

