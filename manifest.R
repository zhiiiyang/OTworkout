create_manifest(path = "../OTworkout/", 
                name = "Workout tracker", 
                shortName = "Zhi's 30-day Workout Challenge", 
                lang = "en-US", 
                startUrl = "https://zhiyang.shinyapps.io/otworkout/", 
                display = "standalone",
                icon  = data.frame(src = c("icons/icon-72x72.png",
                                           "icons/icon-96x96.png",
                                           "icons/icon-128x128.png",
                                           "icons/icon-144x144.png",
                                           "icons/icon-152x152.png",
                                           "icons/icon-192x192.png",
                                           "icons/icon-384x384.png",
                                           "icons/icon-512x512.png"
                                           ),
                                   sizes = c("72x72",
                                             "96x96",
                                             "128x128",
                                             "144x144",
                                             "152x152",
                                             "192x192",
                                             "384x384",
                                             "512x512"),
                                   type = rep("image/png", 8)))


#https://app-manifest.firebaseapp.com/