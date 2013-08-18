
# r --arch i386 --help
# r -f sec.r

library("RJSONIO")

con <- file("sec.write.out.txt", "rt")
print(isValidJSON(con))
close(con) # always after any 'fullread'



