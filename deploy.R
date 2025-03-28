# Authenticate
rsconnect::setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
               token = Sys.getenv("TOKEN"),
               secret = Sys.getenv("SECRET"))
# Deploy
rsconnect::deployApp(appFiles = c("app.R", "data_final.csv", "js4checkbox.js", "www/UL_logo-RGB_barv.png"),
                    forceUpdate = T)
