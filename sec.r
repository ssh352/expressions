
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

# i <- 0
# while () {
    # print()
    # i <- i + 1
# }

# later put put back into a better R object e.g. dataframe or matrix




