


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
