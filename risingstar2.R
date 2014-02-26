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


############# BEGIN EXECUTION #########

# TO FIX THE miss by the UPDATE total_net_income AND total_revenue

# HACKFIX: UPDATE stmt; replaced fhr.rownombres by fhr.rownombresNonPart
# See above: 'added column' and 'added multi-column index'

# put into firmshistory_thismonth_partition
#   the rownombres attributes: total_net_income AND total_revenue
#   found in firmshistory_partition_rownombres

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`annonproc`;

DELIMITER $$
CREATE PROCEDURE advfn.annonproc()
  MODIFIES SQL DATA
BEGIN
  DECLARE l_last_row_fetched         INT;
  DECLARE c_l_partition_name         VARCHAR(64);
  DECLARE c_l_partition_description  LONGTEXT;
  DECLARE d_l_partition_name         VARCHAR(64);
  DECLARE d_l_partition_description  LONGTEXT;

  # I just want to UPDATE those attribute COLUMNS that the FIRST round of UPDATES missed
  
  DECLARE c_cursor cursor FOR
    SELECT p.PARTITION_NAME, REPLACE(PARTITION_DESCRIPTION,"´","'") PARTITION_DESCRIPTION 
      FROM INFORMATION_SCHEMA.PARTITIONS p  
        WHERE
          p.table_schema = 'advfn' AND 
          p.table_name = 'firmshistory_partition_rownombres' AND
          p.partition_name IN (
        --   'quarter_end_date'
        --  ,'total_common_shares_out'
             'total_net_income'
        --  ,'total_equity'
            ,'total_revenue'
        --  ,'earnings_period_indicator'
        --  ,'quarterly_indicator'
        --  ,'number_of_months_last_report_period'
            ) ORDER BY p.partition_name;
            -- debugging: 
            -- 'total_common_shares_out'
            -- ,'total_net_income'
            -- ,'total_equity'
  
  
  DECLARE d_cursor cursor FOR
    SELECT p.PARTITION_NAME, REPLACE(PARTITION_DESCRIPTION,"´","'") PARTITION_DESCRIPTION
      FROM INFORMATION_SCHEMA.PARTITIONS p  
        WHERE
          p.table_schema = 'advfn' AND 
          p.table_name = 'firmshistory_thismonth_partition' AND
          p.partition_name LIKE 'X%'
    ORDER BY p.PARTITION_NAME DESC;
      -- debugging: p.partition_name IN ('X1990_01','X2013_12','X2006_05')
  
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET l_last_row_fetched=1;
  SET l_last_row_fetched=0;
  
  OPEN c_cursor;
  
    c_cursor_loop:LOOP
      FETCH c_cursor INTO c_l_partition_name, c_l_partition_description;
    
      IF l_last_row_fetched=1 THEN
        LEAVE c_cursor_loop;
      END IF;

      OPEN d_cursor;
        d_cursor_loop:LOOP
          FETCH d_cursor INTO d_l_partition_name, d_l_partition_description;
          
          IF l_last_row_fetched=1 THEN
            LEAVE d_cursor_loop;
          END IF;
          
          -- WORK      
          SET @l_source = CONCAT(
         
            "UPDATE firmshistory_thismonth_partition fhm 
              JOIN firmshistory_partition_rownombres fhr ON 
                fhr.rownombresNonPart IN (",c_l_partition_description,") AND 
                fhm.ThisMonth IN (",d_l_partition_description,") AND 
                fhr.EXCHANGE_TICKER = fhm.EXCHANGE_TICKER 
                  SET fhm.",c_l_partition_name," = fhr.",d_l_partition_name," "
            );

          
          SELECT @l_source;
          
          PREPARE stmt1 FROM @l_source;
          EXECUTE stmt1;
          DEALLOCATE PREPARE stmt1;
          
        END LOOP d_cursor_loop;
      CLOSE d_cursor;  
      SET l_last_row_fetched=0;
      
    END LOOP c_cursor_loop;

  CLOSE c_cursor;
  SET l_last_row_fetched=0;
  
END$$

DELIMITER ;
CALL annonproc();

DELIMITER ;
DROP PROCEDURE IF EXISTS advfn.`annonproc`;


############# END EXECUTION #########
## HACK FIX
## started 3:47:45 p.m.
## about 2-3 seconds per UPDATE STATEMENT
## 10 minutes for total_net_income
##
## 1 row in set (16 min 1.53 sec) ( TOTAL OF BOTH )
## (EARLIER) NON-HACK FIX 6 attributes in 11 minutes ( 5x FASTER )


