# ip: 13.92.29.109
# user: vmuser
# pwd: 2Proteccionanimal!

# sudo apt install mongodb-clients
# sudo mkdir /data/db
# sudo apt install libssl-dev
# sudo apt install libsasl2-dev
# sudo apt install mongodb-server-core
# sudo chown -R vmuser /data/db

# db.createUser({
#   user: "user",
#   pwd: "root",
#   roles: [
#     {role: "root", db: "admin"}
#   ]
# })


# cd /etc/ssl/
# sudo openssl req -newkey rsa:2048 -new -x509 -days 365 -nodes -out mongodb-cert.crt -keyout mongodb-cert.key
# sudp cat mongodb-cert.key mongodb-cert.crt > /home/vmuser/mongodb.pem
# sudo cp /home/vmuser/mongodb.pem /etc/ssl/mongodb.pem
# 
# sudo mongod --sslMode requireSSL --sslPEMKeyFile /etc/ssl/mongodb.pem
# 
# m <- mongo(url = "mongodb://localhost/?ssl=true", options = ssl_options(weak_cert_validation = T))
# testdb <- mongo(
#   collection = "test",
#   db = "test",
#   url = "mongodb://localhost",
#   verbose = FALSE,
#   options = ssl_options()
# )
# mongoUrl <- "mongodb://vmusername:a_password@localhost:27017/admin" #<-admin here is the mongodb database that stores the authentication info
# mongoUrl <- "mongodb://vmuser:2Proteccionanimal!@localhost:27017/vmuser" #<-admin here is the mongodb database that stores the authentication info
# 
# BD: gedidb
# user: gedidb
# pwd: gedidb123
# mongoSpe <<- mongo(db = "records", collection = "species", url = "mongodb://192.168.11.81:27017", verbose = FALSE)

#https://stackoverflow.com/questions/38050869/authentication-error-with-mongolite
#mongoUrl <- "mongodb://vmuser:root@localhost:27017/admin" #<-admin here is the mongodb database that stores the authentication info

library(mongolite)
mongoUrl <- "mongodb://gedidb:gedidb123@localhost:27017/gedidb" #<-admin here is the mongodb database that stores the authentication info
gedidb <- mongo(db = "gedidb", collection = "gedicol", mongoUrl)
gedidb$count()
gedidb$drop()

rdatPath <- 'X:/GEDI/v2';
rdatPath <- '/home/vmuser/gedidb/' #

s <- 0
(rdat <- list.files(pattern = '^G0.+.RData', path = rdatPath, full.names = TRUE))
for(i in (rdat)){ #}; i = rdat[12]
  csv <- gsub('.RData', '.csv', i)
  if (!file.exists(csv)){
    print(i)
    t0 <- Sys.time()
    sss <- load(i)
    print(i)
    print(nrow(csv.i))
    csv.i <- subset(csv.i, year == 2019)
    print(paste0('filtered 2019:', nrow(csv.i)))
    s <- s + nrow(csv.i)
    print(s)
    if (TRUE){
      gedidb$insert(csv.i)
      print(t0 - Sys.time())
      # testdb$insert(csv.i)
      write.csv(data.frame(id = i, n = nrow(csv.i), stringsAsFactors = FALSE), file = csv)
      print(gedidb$count())
      if (FALSE) { file.remove(i) }
    }
  }
}
#8m recors ~ 7GB


### Only used -natural pts
# rdatPath <- 'C:/GoogleDrive/IG/server_IG/gedivis/rdat_2b/'; outPath <- 'X:/GEDI/v2'
# rdatPath <- '/home/vmuser/gedivis/rdat_2b/'; outPath <- '/home/vmuser/gedidb/'
# 
# (rdat <- list.files(pattern = 'mat_.+.RData', path = rdatPath, full.names = TRUE))
# s <- 0
# for(i in (rdat)){ #}
#   csv <- paste0(outPath, '/', basename(gsub('.RData', '.csv', i)))
#   if (!file.exists(csv)){
#     print(i)
#     t0 <- Sys.time()
#     load(i)
#     print(i)
#     print(nrow(mat))
#     s <- s + nrow(mat)
#     print(s)
#     if (TRUE){
#       gedidb$insert(mat)
#       print(t0 - Sys.time())
#       # testdb$insert(csv.i)
#       write.csv(data.frame(id = i, n = nrow(mat), stringsAsFactors = FALSE), file = csv)
#       print(gedidb$count())
#       #if (FALSE) { file.remove(i) }
#     }
#   }
# }
