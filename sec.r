
# r --arch i386 --help
# r -f sec.r

library("RJSONIO")

# two JSON objects
# con <- file("sec.write.out3.txt", "rt")

con <- file("sec.write.out.txt", "rt")

myRobject <- fromJSON(content=con)
print(myRobject)

close(con) # always after any 'fullread'

# gather properties

totalRobjectsCount <- length(as.list(myRobject))

print(paste("totalRobjectsCount : ",totalRobjectsCount,sep='') )

if ( totalRobjectsCount > 0) {
    totalaRobjectAttributeCount <- length(as.list(myRobject[[1]]))
    print(paste("totalaRobjectAttributeCount : ",totalaRobjectAttributeCount,sep='') )
}

# control ( iterate though the list )

i <- 0
while (i < totalRobjectsCount ) {
    j <- 0
    while (j < totalaRobjectAttributeCount ) {
        print(as.list(myRobject[[i + 1]])[[j + 1]])
        j <- j + 1
    }
    i <- i + 1
}

# [1] "MSFT"
# [1] "31-03-2013"
# [1] 20489
# [1] 6055
# [1] 0.72
# [1] 8364
# [1] 0.23
# [1] 9666


# LATER: put put back into a better R object e.g. dataframe or matrix





