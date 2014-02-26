
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


################### BEGIN PRODUCTION ################

# sometimes in 50% of the cases ( rownombres partitions ) e.g. "total net income" 
# the SQL optimizer in?correctly deterimines  
#  "Extra: Impossible WHERE noticed after reading const tables"
# MySQL parameters of 'updating statistics' would not work
# OPTIMIZE PARTITION would not work
# FORCE INDEX would not work
# an index on rownombries.rownombres would not work
# FIXED by HACK 
#   "fhr.rownombres LIKE 'total net income%' AND  LENGTH(fhr.rownombres) = 16"
# BUT I WOULD NEED 'more programing to split my IN clause into parts.'
# JUST CUMBERSOME SQL code to hack around that bug

# SUMMARY: MYSQL treats THE PARTITION column AS THE TABLE separator
# OFTEN ( but not always ) INTERNAL BUG: Impossible WHERE noticed after reading const tables
# THAT IS WRONG
# BAD THING ( NOT ALWAYS RELIABLE (50%) ) TO join_on ( perhaps BAD THING TO index_on ) 
# Better to create an Non-Partitioned Column to JOIN ON

# Add THAT non-partitioned column

ALTER TABLE advfn.firmshistory_partition_rownombres
 ADD rownombresNonPart VARCHAR(64) AFTER rownombres;
-- started 10:27:53
-- Query OK, 0 rows affected (8 min 55.54 sec)
 
# Fill that non-partitioned column with data 
 
UPDATE advfn.firmshistory_partition_rownombres
 SET rownombresNonPart = rownombres;
 
-- started 10:58:45
-- Query OK, 1254522 rows affected (11 min 7.56 sec)

 # sanity check
 
SELECT COUNT(*) 
  FROM firmshistory_partition_rownombres fhr
    WHERE fhr.rownombresNonPart IS NULL;
-- zero rows

# Create a useful index

ALTER TABLE firmshistory_partition_rownombres
  ADD INDEX fh_partition_rownombres_rownombresNonPart_exchange_ticker_idx(rownombresNonPart,EXCHANGE_TICKER);
-- Query OK, 0 rows affected (3 min 35.45 sec)
  
# since I am MANUAL, perhaps? I should 'COMPUTE STATISTICS' so this index can be found
# anything wrong?
  
ALTER TABLE firmshistory_partition_rownombres CHECK PARTITION ALL;
-- started 12:14:20
-- 1 row in set (2 min 1.12 sec)
  
# since 
# innodb_stats_auto_recalc=OFF
# innodb_stats_persistent_sample_pages=4294967295
# innodb_stats_transient_sample_pages=4294967295 
# I will MANUALLY 'COMPUTE' ( not SAMPLE ) STATISTCS

# actually 'analyze and rebuild'
ALTER TABLE firmshistory_partition_rownombres OPTIMIZE PARTITION ALL;
-- started 12:24:30 ( this is going to take a while )
-- 2 rows in set (10 min 18.14 sec)
  
#################### END PRODUCTION ############


