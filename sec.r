
# r --arch i386 --help
# r -f sec.r

library("RJSONIO")

con <- file("sec.write.out.txt", "rt")

myRobject <- fromJSON(content=con)
print(myRobject)

close(con) # always after any 'fullread'






