

############### BEGIN PRODUCTION #######################

# keep stays persistent between bootups

# http://dev.mysql.com/doc/refman/5.6/en/analyze-table.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-restrictions.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-parameters.html#sysvar_innodb_stats_persistent
# DEFAULT IS 'ON' ... NOTHING TO DO

# automatically figure out WHEN to 'update stats'

# http://dev.mysql.com/doc/refman/5.6/en/analyze-table.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-restrictions.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-parameters.html#sysvar_innodb_stats_auto_recalc
# I WILL DO THIS MANUALLY
innodb_stats_auto_recalc=OFF

# instead of 'sample statistics' do 'compute statistics' ( all pages )

# http://dev.mysql.com/doc/refman/5.6/en/analyze-table.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-restrictions.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-parameters.html#sysvar_innodb_stats_persistent_sample_pages
innodb_stats_persistent_sample_pages=4294967295

# instead of 'sample statistics' do 'compute statistics' ( all pages )

# http://dev.mysql.com/doc/refman/5.6/en/analyze-table.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-restrictions.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-parameters.html#sysvar_innodb_stats_transient_sample_pages
innodb_stats_transient_sample_pages=4294967295 

# http://dev.mysql.com/doc/refman/5.6/en/analyze-table.html
# http://dev.mysql.com/doc/refman/5.6/en/innodb-restrictions.html
# http://dev.mysql.com/doc/refman/5.6/en/server-system-variables.html#sysvar_max_seeks_for_key
max_seeks_for_key=1


############### END PRODUCTION #########################


###################### BEGIN GENERATOR ############################

# OF firmshistory_thismonth_partition
#   PARTITIONED by ThisMonth 
# VERIFY THAT 'I DO NOT GET'
#   Impossible WHERE noticed after reading const tables

mysqllines <- ""
for ( i in 1990:2013 ) {
  for ( j in 1:12 ) {
  yyyy_mm <- paste0(i,"/", if ( j < 10 ) { paste0(0,j) } else { j })
  
  mysqllines <- paste0(mysqllines,"

EXPLAIN EXTENDED
SELECT fhm.ThisMonth, fhm.EXCHANGE_TICKER, fhm.total_net_income
  FROM firmshistory_thismonth_partition fhm
    WHERE 
          fhm.ThisMonth IN ('",yyyy_mm,"')\\G
SHOW WARNINGS\\G
")

  # break;

  }
  
# break;
  
}

# print it
writeLines(mysqllines)

# easier to see
fileConn <- file("mysqllines.out.txt")
writeLines(mysqllines,fileConn)
close(fileConn)

# run a script
# F:\Documents and Settings\Administrator>
# copy mysqllines.out.txt "F:\Program Files\MySQL\MySQL Server 5.6\bin"
# mysql> source mysqllines.out.txt

# IN F:\Program Files\MySQL\MySQL Server 5.6\bin
# windows> mysql ... -e "source mysqllines.out.txt" --verbose  > mysqllines.out.txt.out.txt

# IN F:\Program Files\MySQL\MySQL Server 5.6\bin
#  1. open notepad and 
#  2. Find for "Impossible" of "Impossible WHERE noticed after reading const tables"
#  NOT FOUND "Impossible"

###################### END GENERATOR ############################



