

-- TEMPORARY.sql

-- EXPERIMENT
set effective_cache_size to '6144MB';  -- os + 'shared_buffers'

set search_path to fe_data_store;
-- set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
-- maximum amount of memory to be used by maintenance operations, such as VACUUM, CREATE INDEX, and ALTER TABLE ADD FOREIGN KEY
-- https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server
set maintenance_work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

-- by design can not find plr
-- select r_version();

select current_database();

SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();

CREATE TABLE sipro_data_store.si_retdate
(
  dateindex integer,
  dateindexf12meom integer,
  dateindexf12mlwd integer,
  dateindexf09meom integer,
  dateindexf09mlwd integer,
  dateindexf06meom integer,
  dateindexf06mlwd integer,
  dateindexf03meom integer,
  dateindexf03mlwd integer
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sipro_data_store.si_retdate
  OWNER TO postgres;


sipro_data_store.si_finecon


  price_m001 text,
  price_m002 text,
  price_m003 text,
  price_m004 text,
  price_m005 text,
  price_m006 text,
  price_m007 text,
  price_m008 text,
  price_m009 text,
  price_m010 text,
  price_m011 text,
  price_m012 text,
  price_m013 text,
  pricedm001 date,
  pricedm002 date,
  pricedm003 date,
  pricedm004 date,
  pricedm005 date,
  pricedm006 date,
  pricedm007 date,
  pricedm008 date,
  pricedm009 date,
  pricedm010 date,
  pricedm011 date,
  pricedm012 date,
  pricedm013 date,


select * from sipro_stage.si_psdd_14911 limit 100;


m01_prchg_01m
approximate price change
d30_aprchg_30d


> shell("rstudio", wait = FALSE)
> zoo::as.Date(14911)
[1] "2010-10-29"

> as.integer(zoo::as.Date("2011-01-31"))
[1] 15005


select pricedm001 - pricedm002 from sipro_stage.si_psdd_14911 limit 100;

select pricedm001::int from sipro_stage.si_psdd_14911 limit 100;

--  retdate_dateindex    integer
--  retdate_dateindexeom integer
--  now_dateindex        integer
--  now_dateindexeom     integer

select 
    dateindex 
  , pricedm002 abspricedm
  , price_m001, pricedm001
  , price_m002, pricedm002
  , pricedm001 - pricedm002 m001_daychg_m002
  , price_m001::numeric(15,2)  - price_m002::numeric(15,2) m001_prchg_m002
  , (price_m001::numeric(15,2) - price_m002::numeric(15,2)) / nullif(abs(price_m002::numeric(15,2)),0) * 30.0 / nullif(pricedm001 - pricedm002,0) * 100.0 d30_aprchg_30d
from sipro_data_store.si_finecon limit 100;


select 
    (extract('epoch' from pricedm004) /(3600*24))::int absnow_dateindex
  , (date_trunc('month',pricedm004) + interval '1 month' - interval '1 day')::date pricedm004eom
  , (date_trunc('month',pricedm001) + interval '1 month' - interval '1 day')::date pricedm001eom
  --, case when pricedm004 > 0 then 99 else - 1 end  in_progress
  , pricedm004 abspricedm
  , price_m001, pricedm001
  , price_m004, pricedm004
  , pricedm001 - pricedm004 m001_daychg_m004
  , price_m001::numeric(15,2)  - price_m004::numeric(15,2) m001_prchg_m004
  , (price_m001::numeric(15,2) - price_m004::numeric(15,2)) / nullif(abs(price_m004::numeric(15,2)),0) * 90.0 / nullif(pricedm001 - pricedm004,0) * 100.0 d90_aprchg_90d
from sipro_data_store.si_finecon limit 100;


select 
    (extract('epoch' from pricedm004) /(3600*24))::int absnow_dateindex
  , (date_trunc('month',pricedm004) + interval '1 month' - interval '1 day')::date pricedm004eom
  , (date_trunc('month',pricedm001) + interval '1 month' - interval '1 day')::date pricedm001eom
  , pricedm004 abspricedm
  , price_m001, pricedm001
  , price_m004, pricedm004
  , pricedm001 - pricedm004 m001_daychg_m004
  , price_m001::numeric(15,2)  - price_m004::numeric(15,2) m001_prchg_m004
  , (price_m001::numeric(15,2) - price_m004::numeric(15,2)) / nullif(abs(price_m004::numeric(15,2)),0) * 90.0 / nullif(pricedm001 - pricedm004,0) * 100.0 d90_aprchg_90d
from sipro_data_store.si_finecon limit 100;

create index        si_finecon_dateindex_idx on sipro_data_store.si_finecon(dateindex);        -- drop index si_finecon_dateindex_idx
-- create index si_retdate_dateindexf03meom_idx on sipro_data_store.si_retdate(dateindexf03meom); -- drop index si_retdate_dateindexf03meom_idx

-- cheaper? ( S & P 500 )? -- yahoo end of oct 2010 to now histories
-- do 'variance and risk analysis there'

to_timestamp(dateindex*3600*24)::date

explain
select dateindex, (extract('epoch' from date_trunc('month', to_timestamp(dateindex*3600*24)::date) + interval '1 month' - interval '1 day')  /(3600*24))::int   dateindexeom
from (
select distinct dateindex from sipro_data_store.si_finecon 
) sq order by 1;

select 
    (extract('epoch' from pricedm004) /(3600*24))::int pricedm004
  , rd.dateindexf03meom
  , (extract('epoch' from pricedm001) /(3600*24))::int pricedm001
  , fc.dateindex
from  sipro_data_store.si_finecon fc, sipro_data_store.si_retdate rd
where  

drop   function dateindexeom(int[]);
create function dateindexeom(int[]) returns setof int as 'select s.i from unnest($1) s(i)' language sql strict immutable;
select dateindexeom(array_agg(dateindex)) from sipro_data_store.si_finecon; -- 628,000 rows 33 seconds ( array_agg ... unnest ... setof int)

drop   function dateindexeom(int);

create function dateindexeom(int) returns setof int as 'select s.i from (select $1 i) s' language sql strict immutable;
select dateindexeom(dateindex) from sipro_data_store.si_finecon; -- 628,000 rows 53 seconds ... from () ... setof int


drop   function dateindexeom(int[]);
create function dateindexeom(int[]) returns setof int as 'select (extract(''epoch'' from date_trunc(''month'', to_timestamp(s.i*3600*24)::date) + interval ''1 month'' - interval ''1 day'') /(3600*24))::int from unnest($1) s(i)' language sql strict immutable; 
select dateindex, dateindexeom(array_agg(dateindex)) from sipro_data_store.si_finecon group by dateindex; -- 54 seconds



ERROR:  column "si_finecon.dateindex" must appear in the GROUP BY clause or be used in an aggregate function
LINE 1: select dateindex, dateindexeom(array_agg(dateindex)) from si...
               ^
********** Error **********

ERROR: column "si_finecon.dateindex" must appear in the GROUP BY clause or be used in an aggregate function
SQL state: 42803
Character: 8

BUT ALSO SEE
Postgres error [column must appear in the GROUP BY clause or be used in an aggregate function] when sub query is used
http://dba.stackexchange.com/questions/88988/postgres-error-column-must-appear-in-the-group-by-clause-or-be-used-in-an-aggre

-- I DO NOT LIKE THIS FORM
select sq.* from ( select dateindex, dateindexeom(array_agg(dateindex)) from sipro_data_store.si_finecon group by dateindex ) sq;


select count(*) from sipro_data_store.si_finecon;


        (extract('epoch' from pricedm001) /(3600*24))::int <= fc.dateindex 
      and
        (extract('epoch' from pricedm001) /(3600*24))::int + 10 > fc.dateindex 
      and
        (extract('epoch' from pricedm004) /(3600*24))::int <= rd.dateindexf03meom
      and
        (extract('epoch' from pricedm004) /(3600*24))::int  + 10 > rd.dateindexf03meom

     (extract('epoch' from date_trunc('month', to_timestamp(fc.dateindex*3600*24)::date) + interval '1 month' - interval '1 day')  /(3600*24))::int = rd.dateindexf03meom
     and
     rd.dateindexf03meom = 15005 -- "2011-01-31"

select 
    company_id_unq
    ticker_unq
  , (extract('epoch' from pricedm001) /(3600*24))::int pricedm001
  , fc.dateindex
  , (extract('epoch' from pricedm004) /(3600*24))::int pricedm004
  , rd.dateindexf03meom
from  sipro_data_store.si_finecon fc, sipro_data_store.si_retdate rd
where  
  fc.dateindex = rd.dateindex  -- eliminate some

  --same
  --fc.dateindexeom = (extract('epoch' from date_trunc('month', to_timestamp(rd.dateindex*3600*24)::date) + interval '1 month' - interval '1 day')  /(3600*24))::int

select 
    company_id_unq
  , ticker_unq
  , (extract('epoch' from pricedm001) /(3600*24))::int pricedm001
  , fc.dateindex
  , (extract('epoch' from pricedm004) /(3600*24))::int pricedm004
  , rd.dateindexf03meom
from  sipro_data_store.si_finecon fc full outer join sipro_data_store.si_retdate rd
on
  fc.dateindexeom = rd.dateindexf03meom 
  
  -- MAYBE? NEED TO APPEND PAST (p) VALUES TO si_retdate
  -- MAYBE? NEED TO 

-- FUTURE VALUES
-- CREATE rd.dateindexp03meom [] PAST VALUES
-- --- APPEND INTERMEDIATE VALUES 01,02,04,05 to si_retdate ( PROB GOOD IDEA )
-- LATERAL ( COME BACK )  -- FUTURE LOOK (VERIFY THAT 3 MONTHS AHEAD fc.pricedm001 IS VERY CLOSE(5 days) TO 'THREE MONTHS AHEAD fc.dateindexeom )
                          -- FUTURE LOOK (VERIFY THAT 3 MONTHS AHEAD fc.pricedm003 IS VERY CLOSE(5 days) TO 'THREE MONTHS AHEAD rd.dateindexp03meom p-PAST )
                          -- GRAB FUTURE VALUE fc.pricedm001
                          -- FUTURE: fc.pricedm001 - fc.pricedm003
-- COMEBACK  

-- RAW

-- already there: sipro_data_store.si_finecon fc

  price_m001,
  price_m002,
  price_m003,
  price_m004,
  price_m005,
  price_m006,
  price_m007,
  price_m008,
  price_m009,
  price_m010,
  price_m011,
  price_m012,
  price_m013,

-- percent changes (02-03 .... 12-12)

-- sd03 ( 02 03 04 )
-- sd06 ( .  .  .  05 06 07 )

select stddev_pop(unnest(array[1.0,2.0,3,0]))
-- ERROR:  set-valued function called in context that cannot accept a set-- THIS ONE WORKS

select stddev_pop(s.i) from unnest(array[1.0,2.0,3,0]) s(i);
-- WORKS

select price_m001::numeric(15,2), array[price_m001::numeric(15,2)] from sipro_data_store.si_finecon;
-- OK

select price_m001::numeric(15,2), array[price_m001::numeric(15,2), price_m002::numeric(15,2)] from sipro_data_store.si_finecon;
-- OK

select price_m001::numeric(15,2), stddev_pop(unnest(array[price_m001::numeric(15,2), price_m002::numeric(15,2)])) from sipro_data_store.si_finecon group by price_m001;
-- ERROR:  column "si_finecon.price_m001" must appear in the GROUP BY clause or be used in an aggregate function
-- ERROR: set-valued function called in context that cannot accept a set




select stddev_pop(s.i) from unnest(array[1.0,2.0,3,0]) s(i), sipro_data_store.si_finecon;
-- NO

create temp table stddev_pops asselect -- ETC LEFT OFF

select 
    (price_m001::numeric(15,2) - price_m002::numeric(15,2)) / nullif(abs(price_m002::numeric(15,2)),0) * 100 m001m002prchg
  , (price_m002::numeric(15,2) - price_m003::numeric(15,2)) / nullif(abs(price_m003::numeric(15,2)),0) * 100 m002m003prchg
  , (price_m003::numeric(15,2) - price_m004::numeric(15,2)) / nullif(abs(price_m004::numeric(15,2)),0) * 100 m003m004prchg
from sipro_data_store.si_finecon
-- WORKS

create temp table arr_m001m004 as 
select company_id_unq, unnest(array[ (price_m001::numeric(15,2) - price_m002::numeric(15,2)) / nullif(abs(price_m002::numeric(15,2)),0) * 100  
       , (price_m002::numeric(15,2) - price_m003::numeric(15,2)) / nullif(abs(price_m003::numeric(15,2)),0) * 100  
       , (price_m003::numeric(15,2) - price_m004::numeric(15,2)) / nullif(abs(price_m004::numeric(15,2)),0) * 100 ]) arr_m001m004
from sipro_data_store.si_finecon;
- WORKS

select  company_id_unq, stddev_pop(arr_m001m004) from  arr_m001m004 sq group by company_id_unq;



--where fc.dateindex = rd.dateindexf03meom and rd.dateindexf03meom = 15005 and fc.dateindex = 15005 -- "2011-01-31"


drop function sipro_data_store.recreate_si_retdate();


create or replace function recreate_si_retdate() returns boolean as 
$body$
  res <- try(pg.spi.exec("
    select 't';
  "))
  if(class(res) =="try-error") pg.throwerror (res)
  return(res)
$body$ 
language plr;

select recreate_si_retdate();



paste0(
  paste0(
    paste0("dateindex", c(rep("p",length(months_range)),rep("f",length(months_range)))),
    stringr::str_pad(c(rev(months_range),months_range),2,"left","0")
  ),
  rep(c("meom","mlwd"),2*length(months_range))
)


paste0(
  paste0(
    paste0("dateindex", unlist(lapply(c(rev(months_range),months_range), function(x) { c(x,x) }))),
    stringr::str_pad(c(rev(months_range),months_range),2,"left","0")
  ),
  rep(c("p","f"),2*length(months_range))
)


c(rep("p",2*length(months_range)),rep("f",2*length(months_range)))



paste0("dateindex",
  paste0( 
    paste0(c(rep("p",2*length(months_range)),rep("f",2*length(months_range))), unlist(lapply(c(rev(months_range),months_range), function(x) { c(x,x) }))),
    rep(c("eom","lwd"),2*length(months_range))
  )
) -> column_names


stringr::str_pad(x,2,"left","0")


# THIS ONE


months_range <- 1:36

paste0("dateindex",
  paste0( 
    paste0(c(rep("p",2*length(months_range)),rep("f",2*length(months_range))), unlist(lapply(c(rev(months_range),months_range), function(x) { c(stringr::str_pad(x,2,"left","0"),stringr::str_pad(x,2,"left","0")) }))),
    rep(c("eom","lwd"),2*length(months_range))
  )
) -> column_names



create or replace function


library(RPostgreSQL)


drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv,user="postgres",password="postgres",dbname="finance_econ")

res <- dbGetQuery(con,"

  set search_path to sipro_data_store,sipro_stage;
  set time zone 'utc';
  set work_mem to '1200MB';
  set constraint_exclusion = on;

") 

try(dbColumnInfo(res), silent = TRUE)

require(magrittr)

res <- dbGetQuery(con,"drop table if exists sipro_data_store.si_retdate2;")

writeLines(
  paste0(
    "create table( dateindex integer, \n",
    paste(column_names, " integer", collapse = ", \n" )," );"
  )
)

dbGetQuery(con,

  paste0(
    "create table  sipro_data_store.si_retdate2( dateindex integer, \n",
    paste(column_names, " integer", collapse = ", \n" )," );"
  )

) -> res





create or replace function recreate_si_retdate() returns bool as boolean
$body$
  res <- try(pg.spi.exec(arg1))
  if(class(res) =="try-error") pg.throwerror (res)
$body$ 
language plr;

drop function sipro_data_store.recreate_si_retdate();


create or replace function recreate_si_retdate() returns boolean as 
$body$
  res <- try(pg.spi.exec("
    select 't';
  "))
  if(class(res) =="try-error") pg.throwerror (res)
  return(res)
$body$ 
language plr;

select recreate_si_retdate();

drop function sipro_data_store.recreate_si_retdate(int);






create or replace function recreate_si_retdate(months_limit int) returns boolean as 
$body$

  require(stringr)

  1:months_limit -> both_months_range 

  both_months_range ->   future_months_range
                  length(future_months_range) -> len_direction_months_range

  c(rev(future_months_range),future_months_range) -> past_and_future_months_range
    length(past_and_future_months_range)      -> len_past_and_future_months_range

  rep(c('p','f'), each = len_past_and_future_months_range) %>%
    # TWO_m per month
    str_c(., rep(past_and_future_months_range, each= 2) %>% str_pad(.,2,'left','0') ) %>%
      # suffix 'eom','lwd'
      str_c( .,rep(c('eom','lwd'), each = len_past_and_future_months_range)) %>%
        # prefix 'dateindex'
        str_c('dateindex', .) -> column_names

  str_c('create table si_retdate2( dateindex integer, \n', paste(column_names, ' integer', collapse = ', \n' ),' );') -> sql

  res <- try(pg.spi.exec(sql), silent = FALSE)
  if(class(res) =='try-error') { on.exit(FALSE); pg.throwerror(res) }
  return(TRUE)
  
$body$ 
language plr;



select recreate_si_retdate(36);

----------------------------------
----------------------------------


BELOW ( but WITH fixes )

HINT:  In a moment you should be able to reconnect to the database and repeat your command.
WARNING:  terminating connection because of crash of another server process
DETAIL:  The postmaster has commanded this server process to roll back the current transaction and exit, because another server process exited abnormally and possibly corrupted shared memory.
HINT:  In a moment you should be able to reconnect to the database and repeat your command.
FATAL:  the database system is in recovery mode
FATAL:  the database system is in recovery mode
LOG:  all server processes terminated; reinitializing
LOG:  database system was interrupted; last known up at 2016-12-03 20:46:33 CST
FATAL:  the database system is in recovery mode
FATAL:  the database system is in recovery mode


drop function sipro_data_store.recreate_si_retdate(int);

create or replace function recreate_si_retdate(months_limit int) returns boolean as 
$body$

  # rm(list=ls())

  # 36 -> months_limit

  require(magrittr)
  require(stringr)

  1:months_limit -> both_months_range 

  both_months_range ->   future_months_range
                  length(future_months_range) -> len_direction_months_range

  c(rev(future_months_range),future_months_range) -> past_and_future_months_range
    length(past_and_future_months_range)      -> len_past_and_future_months_range

  rep(c('p','f'), each = len_past_and_future_months_range) %>%
    # TWO_m per month
    str_c(., rep(past_and_future_months_range, each= 2) %>% str_pad(.,2,'left','0') ) %>%
      # suffix 'eom','lwd'
      str_c( .,rep(c('eom','lwd'), each = len_past_and_future_months_range)) %>%
        # prefix 'dateindex'
        str_c('dateindex', .) -> column_names

  str_c('create table si_retdate2( dateindex integer, \n', paste(column_names, ' integer', collapse = ', \n' ),' );') -> sql

  pg.thrownotice(sql)

  res <- try(pg.spi.exec(sql), silent = FALSE)
  if(class(res) =='try-error') { on.exit(FALSE); pg.throwerror('ERROR') }
  return(TRUE)
  
$body$ 
language plr;


select recreate_si_retdate(36);





********** Error **********


Connection reset.

CONTINUE TOMORROW

-- create table xyz(x int);

-- , user="postgres", password = "postgres",dbname="finance_econ", host="localhost", port="5432"








-- as t(name text, sclass text, type text, len int, precision int, nullok boolean);





# currenly not used
  browser()

  res <- try(pg.spi.exec(sql), silent = FALSE) # pg.spi.exec internal error will cause an automatic disconnect
  if(class(res) =='try-error') { on.exit(FALSE); pg.thrownotice('ERROR') } # do not use: pg.throwerror?
  return(TRUE)



--KEEP(KEPT) (FOUND IN NEW DATABASE UPON TRANSFER) [ ]
create or replace function sipro_data_store.dateindexeom(integer[])
  returns setof integer as
$body$
  select (extract('epoch' from date_trunc('month', to_timestamp(s.i*3600*24)::date) + interval '1 month' - interval '1 day') /(3600*24))::int from unnest($1) s(i)
$body$ 
language sql immutable strict

-- I DO NOT LIKE THIS FORM
select sq.* from ( select dateindex, dateindexeom(array_agg(dateindex)) from sipro_data_store.si_finecon group by dateindex ) sq;
--WORKS



-- KEEP[ ]
create or replace function recreate_si_retdate(months_limit int) returns boolean as 
$body$


  require(magrittr)
  require(stringr)

  1:months_limit -> both_months_range 

  both_months_range ->   future_months_range
                  length(future_months_range) -> len_direction_months_range

  c(rev(future_months_range),future_months_range) -> past_and_future_months_range
    length(past_and_future_months_range)      -> len_past_and_future_months_range

  rep(c('p','f'), each = len_past_and_future_months_range) %>%
    # TWO_m per month
    str_c(., rep(past_and_future_months_range, each= 2) %>% str_pad(.,2,'left','0') ) %>%
      # suffix 'eom','lwd'
      str_c( .,rep(c('lwd','eom'), times = len_past_and_future_months_range)) %>%
        # prefix 'dateindex'
        str_c('dateindex', .) -> column_names

  pg.spi.exec('drop table if exists sipro_data_store.si_retdate2;')

  str_c('create table sipro_data_store.si_retdate2( \ndateindex integer, \ndateindexlwd integer, \ndateindexeom integer, \n', paste(column_names, ' integer', collapse = ', \n' ),'\n);') -> sql

  pg.thrownotice(sql)

  res <- pg.spi.exec(sql)

  disc <- pg.spi.exec('alter table sipro_data_store.si_retdate2 add constraint si_retdate2_dateindex_pk primary key (dateindex);')

  return(TRUE)
  
$body$ 
language plr;


select recreate_si_retdate(38); 


> months_limit
[1] 38

> len_direction_months_range
[1] 38

> past_and_future_months_range
 [1] 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14
[26] 13 12 11 10  9  8  7  6  5  4  3  2  1  1  2  3  4  5  6  7  8  9 10 11 12
[51] 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
[76] 38

> len_past_and_future_months_range
[1] 76

> column_names
  [1] "dateindexp38lwd" "dateindexp38eom" "dateindexp37lwd" "dateindexp37eom"
  [5] "dateindexp36lwd" "dateindexp36eom" "dateindexp35lwd" "dateindexp35eom"
  [9] "dateindexp34lwd" "dateindexp34eom" "dateindexp33lwd" "dateindexp33eom"
 [13] "dateindexp32lwd" "dateindexp32eom" "dateindexp31lwd" "dateindexp31eom"
 [17] "dateindexp30lwd" "dateindexp30eom" "dateindexp29lwd" "dateindexp29eom"
 [21] "dateindexp28lwd" "dateindexp28eom" "dateindexp27lwd" "dateindexp27eom"
 [25] "dateindexp26lwd" "dateindexp26eom" "dateindexp25lwd" "dateindexp25eom"
 [29] "dateindexp24lwd" "dateindexp24eom" "dateindexp23lwd" "dateindexp23eom"
 [33] "dateindexp22lwd" "dateindexp22eom" "dateindexp21lwd" "dateindexp21eom"
 [37] "dateindexp20lwd" "dateindexp20eom" "dateindexp19lwd" "dateindexp19eom"
 [41] "dateindexp18lwd" "dateindexp18eom" "dateindexp17lwd" "dateindexp17eom"
 [45] "dateindexp16lwd" "dateindexp16eom" "dateindexp15lwd" "dateindexp15eom"
 [49] "dateindexp14lwd" "dateindexp14eom" "dateindexp13lwd" "dateindexp13eom"
 [53] "dateindexp12lwd" "dateindexp12eom" "dateindexp11lwd" "dateindexp11eom"
 [57] "dateindexp10lwd" "dateindexp10eom" "dateindexp09lwd" "dateindexp09eom"
 [61] "dateindexp08lwd" "dateindexp08eom" "dateindexp07lwd" "dateindexp07eom"
 [65] "dateindexp06lwd" "dateindexp06eom" "dateindexp05lwd" "dateindexp05eom"
 [69] "dateindexp04lwd" "dateindexp04eom" "dateindexp03lwd" "dateindexp03eom"
 [73] "dateindexp02lwd" "dateindexp02eom" "dateindexp01lwd" "dateindexp01eom"
 [77] "dateindexf01lwd" "dateindexf01eom" "dateindexf02lwd" "dateindexf02eom"
 [81] "dateindexf03lwd" "dateindexf03eom" "dateindexf04lwd" "dateindexf04eom"
 [85] "dateindexf05lwd" "dateindexf05eom" "dateindexf06lwd" "dateindexf06eom"
 [89] "dateindexf07lwd" "dateindexf07eom" "dateindexf08lwd" "dateindexf08eom"
 [93] "dateindexf09lwd" "dateindexf09eom" "dateindexf10lwd" "dateindexf10eom"
 [97] "dateindexf11lwd" "dateindexf11eom" "dateindexf12lwd" "dateindexf12eom"
[101] "dateindexf13lwd" "dateindexf13eom" "dateindexf14lwd" "dateindexf14eom"
[105] "dateindexf15lwd" "dateindexf15eom" "dateindexf16lwd" "dateindexf16eom"
[109] "dateindexf17lwd" "dateindexf17eom" "dateindexf18lwd" "dateindexf18eom"
[113] "dateindexf19lwd" "dateindexf19eom" "dateindexf20lwd" "dateindexf20eom"
[117] "dateindexf21lwd" "dateindexf21eom" "dateindexf22lwd" "dateindexf22eom"
[121] "dateindexf23lwd" "dateindexf23eom" "dateindexf24lwd" "dateindexf24eom"
[125] "dateindexf25lwd" "dateindexf25eom" "dateindexf26lwd" "dateindexf26eom"
[129] "dateindexf27lwd" "dateindexf27eom" "dateindexf28lwd" "dateindexf28eom"
[133] "dateindexf29lwd" "dateindexf29eom" "dateindexf30lwd" "dateindexf30eom"
[137] "dateindexf31lwd" "dateindexf31eom" "dateindexf32lwd" "dateindexf32eom"
[141] "dateindexf33lwd" "dateindexf33eom" "dateindexf34lwd" "dateindexf34eom"
[145] "dateindexf35lwd" "dateindexf35eom" "dateindexf36lwd" "dateindexf36eom"
[149] "dateindexf37lwd" "dateindexf37eom" "dateindexf38lwd" "dateindexf38eom"

 str_c('create table sipro_data_store.si_retdate2( \ndateindex integer, \ndateindexlwd integer, \ndateindexeom integer, \n', paste(column_names, ' integer', collapse = ', \n' ),'\n);') -> sql

create table sipro_data_store.si_retdate2(
dateindex integer,
dateindexlwd integer,
dateindexeom integer,
dateindexp38lwd  integer,
dateindexp38eom  integer,
dateindexp37lwd  integer,
dateindexp37eom  integer,
...
dateindexp01lwd  integer,
dateindexp01eom  integer,
dateindexf01lwd  integer,
dateindexf01eom  integer,
...
dateindexf38lwd  integer,
dateindexf38eom  integer
);

-- alter table sipro_data_store.si_retdate2 add constraint si_retdate2_dateindex_pk primary key (dateindex);

-- WORKS





DROP TABLE IF EXISTS abalone;
CREATE TABLE abalone (sex text, length float8, diameter float8, height float8, whole_weight float8, shucked_weight float8, viscera_weight float8, shell_weight float8, rings float8);
COPY abalone FROM 'W:/R-Portable.3.2.2/App/R-Portable/bin/x64/RDebug/Home/abalone.data' WITH CSV;


select count(*) from abalone;


DROP TABLE IF EXISTS abalone_array;
CREATE TABLE abalone_array AS SELECT 
sex::text
, array_agg(shucked_weight::float8) as s_weight
, array_agg(rings::float8) as rings
, array_agg(diameter::float8) as diameter 
FROM abalone 
GROUP BY sex;



DROP TYPE IF EXISTS lm_abalone_type CASCADE;
CREATE TYPE lm_abalone_type AS (
Variable text, Coef_Est float, Std_Error float, T_Stat float, P_Value float);




CREATE OR REPLACE FUNCTION lm_abalone_plr(s_weight float8[], rings float8[], diameter float8[]) 
RETURNS SETOF lm_abalone_type AS 
$$ 
    m1<- lm(s_weight~rings+diameter)
    m1_s<- summary(m1)$coef
    temp_m1<- data.frame(rownames(m1_s), m1_s)
    return(temp_m1)
$$ 
LANGUAGE 'plr';




require(magrittr)
require(lubridate)

zoo::as.Date("2012-02-29") -> today 

# > zoo::as.Date(as.integer(zoo::as.Date("2012-02-29")))
# [1] "2012-02-29"

months_length

Hmisc::trunc.POSIXt(zoo::as.Date("2012-02-29"), units='months') %>%
  zoo::as.Date(seq(., by = "month", length.out = 36) %m+% months(1) %m+% days(-1)) %>%
   c(.)[-1]

Hmisc::ceil.POSIXt(zoo::as.Date("2012-02-29"), units='months') -> mm


zoo::as.Date(seq(mm, by = "month", length.out = 36) %m+% months(1) %m+% days(-1))

# next month
zoo::as.Date("2012-02-29") %m+% months(1) %>%
  # 1st day of next month
  Hmisc::trunc.POSIXt(., units='months')  %>%
    # 36 or so months
    seq(., by = "month", length.out = 36) %m+% 
      # last day of month
      months(1) %m+% days(-1) %>% 
        zoo::as.Date(.) %>% as.integer(.)
## WORKS


unlist(
zoo::rollapply( zoo::as.Date(c("2012-03-31", "2012-04-30", "2012-05-31"))
  ,width = 1
  , FUN = function(x) { list(x,x) } )
)

# now lwd eom dates





zoo::as.Date("2012-02-28") -> now_date

# this month
now_date %m+% months(1) %>%
  # 1st day of next month # last day of this month
  Hmisc::trunc.POSIXt(., units='months') %m+% days(-1) %>% 
    zoo::as.Date(.)  %>%
      # add in lwd ( Sat or Sun falls back to Fri)
      lapply(.,function(x) { (x - match(weekdays(x), c("Saturday","Sunday"), nomatch = 0)) %>% 
                              # lwd, eom
                              c(.,x)
                           } ) %>% 
        # flattened (Date class is stripped)
        unlist(.) %>% zoo::as.Date(.) -> now_lwd_eom_dates


# past lwd eom dates

# previous 36th month back
now_date %m+% months(-36) %>%
  # 1st day of previous 36th month
  Hmisc::trunc.POSIXt(., units='months')  %>%
    # 36 or so months
    seq(., by = "month", length.out = 36) %m+% 
      # last day of month
      months(1) %m+% days(-1) %>% 
        zoo::as.Date(.) %>% 
          # add in lwd ( Sat or Sun falls back to Fri)
          lapply(.,function(x) { (x - match(weekdays(x), c("Saturday","Sunday"), nomatch = 0)) %>% 
                                 # lwd, eom
                                 c(.,x)
                               } ) %>% 
            # flattened (Date class is stripped)
            unlist(.) %>% zoo::as.Date(.) -> past_lwd_eom_dates

# future lwd eom dates

# next month
now_date %m+% months(1) %>%
  # 1st day of next month
  Hmisc::trunc.POSIXt(., units='months')  %>%
    # 36 or so months
    seq(., by = "month", length.out = 36) %m+% 
      # last day of month
      months(1) %m+% days(-1) %>% 
        zoo::as.Date(.) %>% 
          # add in lwd ( Sat or Sun falls back to Fri)
          lapply(.,function(x) { (x - match(weekdays(x), c("Saturday","Sunday"), nomatch = 0)) %>% 
                                 # lwd, eom
                                 c(.,x)
                               } ) %>% 
            # flattened (Date class is stripped)
            unlist(.) %>% zoo::as.Date(.) -> future_lwd_eom_dates

c(now_date,now_lwd_eom_dates,past_lwd_eom_dates,future_lwd_eom_dates) %>% as.integer(.)

--

> weekdays(zoo::as.Date("2012-02-28"))
[1] "Tuesday"
> as.integer(zoo::as.Date("2012-02-28"))
[1] 15398
> as.integer(zoo::as.Date("2012-02-29"))
[1] 15399
> weekdays(zoo::as.Date("2012-02-29"))
[1] "Wednesday"


drop function test(int);

create or replace function test(now_date int) returns integer as 
$body$
  zoo::as.Date(now_date) -> now_date -- PostgreSQL converts back to integer
  return(now_date)
$body$ 
language plr;

select test(16000);




-- KEEP[ ]


drop function fill_si_retdate(int, int);

create or replace function fill_si_retdate(now_date int, months_limit int) returns integer as 
$body$

  ## must be because zoo::as.date(NULL) -> ERROR
  ## dateindex is not null
  ## 
  ## LEFT up to user responsibility not to enter NULL now_date
  ##
  ## > zoo::as.Date(NULL)
  ## Error in as.Date.default(NULL) :
  ## do not know how to convert 'NULL' to class "Date"
  ##
  ## ERROR:  R interpreter expression evaluation error
  ## DETAIL:  Error in as.Date.default(now_date) : 
  ## do not know how to convert 'now_date' to class "Date"

  #correct for TZ 
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ='UTC')
  }

  require(magrittr)
  #
  # uses zoo # zoo::as.Date
  #
  require(lubridate)
  #
  # uses Hmisc # Hmisc::Hmisc::trunc.POSIXt
  #
  require(stringr)
  #

  zoo::as.Date(now_date) -> now_date

  # this month
  now_date %m+% months(1) %>%
    # 1st day of next month # last day of this month
    Hmisc::trunc.POSIXt(., units='months') %m+% days(-1) %>% 
      zoo::as.Date(.)  %>%
        # add in lwd ( Sat or Sun falls back to Fri)
        lapply(.,function(x) { (x - match(weekdays(x), c('Saturday','Sunday'), nomatch = 0)) %>% 
                                # lwd, eom
                                c(.,x)
                             } ) %>% 
          # flattened (Date class is stripped)
          unlist(.) %>% zoo::as.Date(.) -> now_lwd_eom_dates

  # past lwd eom dates

  # previous Nth(months_limit) month back
  now_date %m+% months(-months_limit) %>%
    # 1st day of previous Nth month
    Hmisc::trunc.POSIXt(., units='months')  %>%
      # N(months_limit) or so months
      seq(., by = 'month', length.out = months_limit) %m+% 
        # last day of month
        months(1) %m+% days(-1) %>% 
          zoo::as.Date(.) %>% 
            # add in lwd ( Sat or Sun falls back to Fri)
            lapply(.,function(x) { (x - match(weekdays(x), c('Saturday','Sunday'), nomatch = 0)) %>% 
                                   # lwd, eom
                                   c(.,x)
                                 } ) %>% 
              # flattened (Date class is stripped)
              unlist(.) %>% zoo::as.Date(.) -> past_lwd_eom_dates

  # future lwd eom dates

  # next month
  now_date %m+% months(1) %>%
    # 1st day of next month
    Hmisc::trunc.POSIXt(., units='months') %>%
      # N(months_limit) or so months
      seq(., by = 'month', length.out = months_limit) %m+% 
        # last day of month
        months(1) %m+% days(-1) %>% 
          zoo::as.Date(.) %>% 
            # add in lwd ( Sat or Sun falls back to Fri)
            lapply(.,function(x) { (x - match(weekdays(x), c('Saturday','Sunday'), nomatch = 0)) %>% 
                                   # lwd, eom
                                   c(.,x)
                                 } ) %>% 
              # flattened (Date class is stripped)
              unlist(.) %>% zoo::as.Date(.) -> future_lwd_eom_dates


  c(now_date,now_lwd_eom_dates,past_lwd_eom_dates,future_lwd_eom_dates) %>% as.integer(.) -> all_dates

  str_c('insert into sipro_data_store.si_retdate2 values (', str_c(all_dates, collapse = ', '),');') -> sql

  pg.thrownotice(sql)

  res <- pg.spi.exec(sql)

  res2 <- pg.spi.exec('reindex table sipro_data_store.si_retdate2;')

  Sys.setenv(TZ=oldtz)

  return(res)
  
$body$ 
language plr;

-- must be because zoo::as.date(NULL) -> ERROR
-- dateindex is not null
--
-- > zoo::as.Date(NULL)
-- Error in as.Date.default(NULL) :
-- do not know how to convert 'NULL' to class "Date"
--
-- ERROR:  R interpreter expression evaluation error
-- DETAIL:  Error in as.Date.default(now_date) : 
-- do not know how to convert 'now_date' to class "Date"
--
-- truncate table sipro_data_store.si_retdate2;
--

-- -- WORKS?! ( BUT TOO SLOW ) -- SLOW VERSION
--    select fill_si_retdate(sq.dateindex,38) from (select distinct dateindex from sipro_data_store.si_finecon where dateindex is not null order by dateindex) sq;

--
-- NOTE: select distinct dateindex can be very time consuming ( may need an 'outrigger' table )
-- ...

-- WORKS -- FAST VERSION 
-- BEGIN
drop table if exists sipro_data_store.dateindexes;

create table dateindexes as
select distinct dateindex from sipro_data_store.si_finecon where dateindex is not null order by dateindex;

alter table sipro_data_store.dateindexes
  add constraint dateindexes_dateindex_pk primary key (dateindex);

select * from dateindexes;

-- truncate table sipro_data_store.si_retdate2;
-- RUN
select fill_si_retdate(dateindex,38) from dateindexes;
-- VERIFY RUN WAS SUCCESSFUL
select * from sipro_data_store.si_retdate2;


> now_date
[1] "2011-06-30"

> now_lwd_eom_dates
[1] "2011-06-30" "2011-06-30"

> months_limit <- 38

>  past_lwd_eom_dates
 [1] "2008-04-30" "2008-04-30" "2008-05-30" "2008-05-31" "2008-06-30"
 [6] "2008-06-30" "2008-07-31" "2008-07-31" "2008-08-29" "2008-08-31"
[11] "2008-09-30" "2008-09-30" "2008-10-31" "2008-10-31" "2008-11-28"
[16] "2008-11-30" "2008-12-31" "2008-12-31" "2009-01-30" "2009-01-31"
[21] "2009-02-27" "2009-02-28" "2009-03-31" "2009-03-31" "2009-04-30"
[26] "2009-04-30" "2009-05-29" "2009-05-31" "2009-06-30" "2009-06-30"
[31] "2009-07-31" "2009-07-31" "2009-08-31" "2009-08-31" "2009-09-30"
[36] "2009-09-30" "2009-10-30" "2009-10-31" "2009-11-30" "2009-11-30"
[41] "2009-12-31" "2009-12-31" "2010-01-29" "2010-01-31" "2010-02-26"
[46] "2010-02-28" "2010-03-31" "2010-03-31" "2010-04-30" "2010-04-30"
[51] "2010-05-31" "2010-05-31" "2010-06-30" "2010-06-30" "2010-07-30"
[56] "2010-07-31" "2010-08-31" "2010-08-31" "2010-09-30" "2010-09-30"
[61] "2010-10-29" "2010-10-31" "2010-11-30" "2010-11-30" "2010-12-31"
[66] "2010-12-31" "2011-01-31" "2011-01-31" "2011-02-28" "2011-02-28"
[71] "2011-03-31" "2011-03-31" "2011-04-29" "2011-04-30" "2011-05-31"
[76] "2011-05-31"

> future_lwd_eom_dates
 [1] "2011-07-29" "2011-07-31" "2011-08-31" "2011-08-31" "2011-09-30"
 [6] "2011-09-30" "2011-10-31" "2011-10-31" "2011-11-30" "2011-11-30"
[11] "2011-12-30" "2011-12-31" "2012-01-31" "2012-01-31" "2012-02-29"
[16] "2012-02-29" "2012-03-30" "2012-03-31" "2012-04-30" "2012-04-30"
[21] "2012-05-31" "2012-05-31" "2012-06-29" "2012-06-30" "2012-07-31"
[26] "2012-07-31" "2012-08-31" "2012-08-31" "2012-09-28" "2012-09-30"
[31] "2012-10-31" "2012-10-31" "2012-11-30" "2012-11-30" "2012-12-31"
[36] "2012-12-31" "2013-01-31" "2013-01-31" "2013-02-28" "2013-02-28"
[41] "2013-03-29" "2013-03-31" "2013-04-30" "2013-04-30" "2013-05-31"
[46] "2013-05-31" "2013-06-28" "2013-06-30" "2013-07-31" "2013-07-31"
[51] "2013-08-30" "2013-08-31" "2013-09-30" "2013-09-30" "2013-10-31"
[56] "2013-10-31" "2013-11-29" "2013-11-30" "2013-12-31" "2013-12-31"
[61] "2014-01-31" "2014-01-31" "2014-02-28" "2014-02-28" "2014-03-31"
[66] "2014-03-31" "2014-04-30" "2014-04-30" "2014-05-30" "2014-05-31"
[71] "2014-06-30" "2014-06-30" "2014-07-31" "2014-07-31" "2014-08-29"
[76] "2014-08-31"

c(now_date,now_lwd_eom_dates,past_lwd_eom_dates,future_lwd_eom_dates) %>% as.integer(.) -> all_dates

> all_dates
  [1] 15155 15155 15155 13999 13999 14029 14030 14060 14060 14091 14091 14120
 [13] 14122 14152 14152 14183 14183 14211 14213 14244 14244 14274 14275 14302
 [25] 14303 14334 14334 14364 14364 14393 14395 14425 14425 14456 14456 14487
 [37] 14487 14517 14517 14547 14548 14578 14578 14609 14609 14638 14640 14666
 [49] 14668 14699 14699 14729 14729 14760 14760 14790 14790 14820 14821 14852
 [61] 14852 14882 14882 14911 14913 14943 14943 14974 14974 15005 15005 15033
 [73] 15033 15064 15064 15093 15094 15125 15125 15184 15186 15217 15217 15247
 [85] 15247 15278 15278 15308 15308 15338 15339 15370 15370 15399 15399 15429
 [97] 15430 15460 15460 15491 15491 15520 15521 15552 15552 15583 15583 15611
[109] 15613 15644 15644 15674 15674 15705 15705 15736 15736 15764 15764 15793
[121] 15795 15825 15825 15856 15856 15884 15886 15917 15917 15947 15948 15978
[133] 15978 16009 16009 16038 16039 16070 16070 16101 16101 16129 16129 16160
[145] 16160 16190 16190 16220 16221 16251 16251 16282 16282 16311 16313

 str_c('insert into sipro_data_store.si_retdate2 values (', str_c(all_dates, collapse = ', '),');') -> sql

>  str_c('insert into sipro_data_store.si_retdate2 values (', str_c(all_dates,$
> sql
[1] "insert into sipro_data_store.si_retdate2 values (15155, 15155, 15155, 13999, 13999, 14029, 14030, 14060, 14060, 14091, 14091, 14120, 14122, 14152, 14152, 14183, 14183, 14211, 14213, 14244, 14244, 14274, 14275, 14302, 14303, 14334, 14334, 14364, 14364, 14393, 14395, 14425, 14425, 14456, 14456, 14487, 14487, 14517, 14517, 14547, 14548, 14578, 14578, 14609, 14609, 14638, 14640, 14666, 14668, 14699, 14699, 14729, 14729, 14760, 14760, 14790, 14790, 14820, 14821, 14852, 14852, 14882, 14882, 14911, 14913, 14943, 14943, 14974, 14974, 15005, 15005, 15033, 15033, 15064, 15064, 15093, 15094, 15125, 15125, 15184, 15186, 15217, 15217, 15247, 15247, 15278, 15278, 15308, 15308, 15338, 15339, 15370, 15370, 15399, 15399, 15429, 15430, 15460, 15460, 15491, 15491, 15520, 15521, 15552, 15552, 15583, 15583, 15611, 15613, 15644, 15644, 15674, 15674, 15705, 15705, 15736, 15736, 15764, 15764, 15793, 15795, 15825, 15825, 15856, 15856, 15884, 15886, 15917, 15917, 15947, 15948, 15978, 15978, 16009, 16009, 16038, 16039, 16070, 16070, 16101, 16101, 16129, 16129, 16160, 16160, 16190, 16190, 16220, 16221, 16251, 16251, 16282, 16282, 16311, 16313);"


-- END OF KEEP



select fill_si_retdate(sq.my_dates,36) from (select 15399 my_dates union select 14500) sq;

(select distinct dateindex from sipro_data_store.si_finecon order by dateindex) sq;

select fill_si_retdate(sq.dateindex,36) from (select distinct dateindex from sipro_data_store.si_finecon order by dateindex) sq;

select company_id, lastmod, updated from sipro_stage.si_isq_17074 order by lastmod, updated;
--1900-01-01 't'

select company_id, lastmod, updated from sipro_stage.si_isq_17074 order by lastmod, updated;

select company_id from sipro_stage.si_isq_17074 where company_id not in 
( select company_id from sipro_stage.si_isq_17074 group by company_id having count(company_id) = 1 );  

-- conclusion: lastmod, updated HAVE NO VALUE


--- THURSDAY-SUNDAY ( RE-BEGIN SQL WORK ) ---

set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 2; -- not 'written in docs'

select count(1) from sipro_data_store.si_finecon fc;
-- 638,840

select count(1) from sipro_data_store.si_finecon fc where fc.company_id_unq is null;
-- 91,321 ...  etc ...

select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company from sipro_data_store.si_finecon fc;

select rd.dateindex, rd.dateindexlwd, rd.dateindexeom, 
       rd.dateindexf01lwd, dateindexf01eom
from sipro_data_store.si_retdate2 rd;


select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, f01lwd.dateindex dateindexf01lwd
             from sipro_data_store.si_finecon  fc 
     join lateral ( select fc.dateindex from sipro_data_store.si_retdate2 rd where fc.dateindex = rd.dateindexf01lwd ) f01lwd on true;
-- 581,725
-- 1 min and 28 seconds ( NO EMPTY records )

--explain
select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, f01lwd.dateindex dateindexf01lwd
             from sipro_data_store.si_finecon  fc 
left join lateral ( select fc.dateindex from sipro_data_store.si_retdate2 rd where fc.dateindex = rd.dateindexf01lwd ) f01lwd on true;
-- 3 min & 34 seconds
-- 628,840 records -- note: lateral prevents a 'cross join'
-- MANY 'pure empty records' ( PROB fc.company_id_unq is null above                      

-- (extract('epoch' from DATE)  /(3600*24))::int
      
-- price_m001::numeric(7,1), (extract('epoch' from pricedm001)  /(3600*24))::int
    
--select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, fc.price_m001::numeric(7,1),
--       f01lwd.dateindex dateindexf01lwd, f01lwd.price_m001::numeric(7,1) price_m001f01lwd
--             from sipro_data_store.si_finecon  fc 
--left join lateral ( select fc.dateindex from sipro_data_store.si_retdate2 rd where fc.dateindex = rd.dateindexf01lwd ) f01lwd on true;

select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, 
                     f01lwd.rdf01lwddateindex, f01lwd.fcf01lwddateindex 
             from sipro_data_store.si_finecon  fc 
left join lateral ( select rd.dateindex rdf01lwddateindex, fcf01lwd.dateindex fcf01lwddateindex from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fcf01lwd 
                      where fc.dateindex       = rd.dateindexf01lwd 
                      and   rd.dateindexf01lwd = fcf01lwd.dateindex 
                      and   fc.company_id_unq  = fcf01lwd.company_id_unq ) f01lwd on true;

explain -- 337 thousand
select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, 
                     f01lwd.rdf01lwddateindex, f01lwd.fcf01lwddateindex 
             from sipro_data_store.si_finecon  fc 
left join lateral ( select rd.dateindex rdf01lwddateindex, fcf01lwd.dateindex fcf01lwddateindex from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fcf01lwd 
                      where fc.dateindex       = rd.dateindexf01lwd 
                      and   rd.dateindexf01lwd = fcf01lwd.dateindex 
                      and   fc.company_id_unq  = fcf01lwd.company_id_unq ) f01lwd on true;
-- GOOD -- update_process_title = off ( BETTER )
-- 2:34 -- 1:02 0:53 0:53
        -- 1024 ... > ... set work_mem to '2047MB' ( IT did not matter Windows postgre.exe process only reached 127M )

-- drop index si_finecon_dateindex_company_id_unq_idx; -- BEFORE/AFTER index SAME_SPEED

CREATE INDEX si_finecon_dateindex_company_id_unq_idx
  ON sipro_data_store.si_finecon
  USING btree
  (dateindex,company_id_unq); -- explain goes down from 500,000 to 300,000 ... Index Only Scan
-- memory goes down to 6M .. speen 38 SECONDS ( but may have been doing that before? )

-- WRONG BUSINESS?, WRONG DATA ANSWER
explain -- 337 thousand
select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, 
                     fu.rd_dateindex, fu.fc_dateindex fu_fc_dateindex, fu.f_fc_dateindex fu_f_fc_dateindex 
             from sipro_data_store.si_finecon  fc 
left join lateral ( select rd.dateindex rd_dateindex, f_fc.dateindex, fc.dateindex fc_dateindex, f_fc.dateindex  f_fc_dateindex  from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon f_fc
                      where fc.dateindex       = rd.dateindexf01lwd 
                      and   rd.dateindexf01lwd = f_fc.dateindex 
                      and   fc.company_id_unq  = f_fc.company_id_unq ) fu on true; -- PERFORMANCE TRICK

-- fc.price_m001::numeric(15,2)




select fc.dateindex fc_dateindex, fc.company_id_unq fc_company_id_unq, fc.ticker_unq fc_ticker_unq, fc.company fc_company, 
                     f01lwd.rdf01lwddateindex f01lwd_rdf01lwddateindex, f01lwd.fcf01lwddateindex f01lwd_fcf01lwddateindex
             from sipro_data_store.si_finecon  fc 
left join lateral ( select rd.dateindex rdf01lwddateindex, fcf01lwd.dateindex fcf01lwddateindex from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fcf01lwd 
                      where fc.dateindex       = rd.dateindexf01lwd 
                      and   rd.dateindexf01lwd = fcf01lwd.dateindex 
                      and   fc.company_id_unq  = fcf01lwd.company_id_unq ) f01lwd on true;



explain -- 89 billion
select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, 
                     f01lwd.rdf01lwddateindex, f01lwd.fcf01lwddateindex 
             from sipro_data_store.si_finecon  fc 
left join lateral ( select rd.dateindex rdf01lwddateindex, fcf01lwd.dateindex fcf01lwddateindex from sipro_data_store.si_retdate2 rd full outer join sipro_data_store.si_finecon fcf01lwd 
                      on    fc.dateindex       = rd.dateindexf01lwd 
                      and   rd.dateindexf01lwd = fcf01lwd.dateindex 
                      and   fc.company_id_unq  = fcf01lwd.company_id_unq ) f01lwd on true;
-- SEEMS TO WORK ( BUT TOO MANY USELESS RECORDS? )
-- OVER 5 minutes ( SO TOO MANY USELESS RECORDS - THEN KILL )

-- BETTER( IN PROGRESS )
explain -- 83 billion
select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, 
                     f01lwd.rdf01lwddateindex, f01lwd.fcf01lwddateindex 
             from sipro_data_store.si_finecon  fc 
left join lateral ( select rd.dateindex rdf01lwddateindex, fcf01lwd.dateindex fcf01lwddateindex from  sipro_data_store.si_finecon fcf01lwd  left outer join sipro_data_store.si_retdate2 rd
                      on    fc.dateindex       = rd.dateindexf01lwd 
                      and   rd.dateindexf01lwd = fcf01lwd.dateindex 
                      and   fc.company_id_unq  = fcf01lwd.company_id_unq ) f01lwd on true;
-- IN PROGRESS ( HOW MUCH TIME?: STILL OVER 5 MINUTES ... ____  KILL OVER 9 MINUTES)



-- -- BAD -- BAD -- BAD

-- explain              -- [ ] BAD NOT RIGHT: f01lwd.price_m001 == fc.price_m001::numeric(15,2)
select fc.dateindex, fc.company_id_unq, fc.ticker_unq, fc.company, fc.price_m001::numeric(15,2),
                     f01lwd.rdf01lwddateindex, f01lwd.fcf01lwddateindex, f01lwd.price_m001 f01lwdprice_m001, 
                     ( f01lwd.price_m001 - fc.price_m001::numeric(15,2) ) / nullif(abs(fc.price_m001::numeric(15,2)),0) * 100.0 pchchg_f01lwd
             from sipro_data_store.si_finecon  fc 
left join lateral ( select       rd.dateindex rdf01lwddateindex, 
                           fcf01lwd.dateindex fcf01lwddateindex,
                           fcf01lwd.price_m001::numeric(15,2)
                      from  sipro_data_store.si_finecon fcf01lwd  join sipro_data_store.si_retdate2 rd
                        on    fc.dateindex       = rd.dateindexf01lwd 
                        and   rd.dateindexf01lwd = fcf01lwd.dateindex 
                        and   fc.company_id_unq  = fcf01lwd.company_id_unq ) f01lwd on true;

-- SELECT BACK TO USER [ ] eom

-- BEGIN WRONG LATERAL:  FIXING attempts

select fc.dateindex,  fc.ticker_unq, fc.price_m001::numeric(15,2) 
  from sipro_data_store.si_finecon  fc 
    where fc.ticker_unq = 'AAPL' order by dateindex;

select rd.dateindex, rd.dateindexp01lwd,  rd.dateindexf01lwd
  from sipro_data_store.si_retdate2 rd;

--

select rd.dateindexp01lwd rd_dateindexp01lwd, rd.dateindex rd_dateindex, rd.dateindexf01lwd rd_dateindexf01lwd , fu.ticker_unq fu_ticker_unq, fu.dateindex fu_dateindex, fu.price_m001 fu_price_m001
    from sipro_data_store.si_retdate2 rd
      join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2)
    from sipro_data_store.si_finecon fc  where fc.ticker_unq = 'AAPL' and fc.dateindex =  rd.dateindexf01lwd ) fu on true order by rd.dateindex;

-- do not need a lateral to do this ( but the lateral is more elegant )
--


-- FOREVER rd.*     9.2 seconds   rd.dateindex rd_dateindex
-- explain select rd.dateindex rd_dateindex from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fc  where  fc.dateindex = rd.dateindex;
--   "  ->  Index Only Scan using si_finecon_dateindex_company_id_unq_idx on si_finecon fc  (cost=0.42..188.22 rows=8220 width=4)"


--                 14.5 seconds rd.dateindex rd_dateindex, rd.dateindexf01lwd rd_dateindexf01lwd ( extra columns AND not in the index )
-- explain select rd.dateindex rd_dateindex, rd.dateindexf01lwd rd_dateindexf01lwd from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fc  where  fc.dateindex = rd.dateindex;
--   "  ->  Index Only Scan using si_finecon_dateindex_company_id_unq_idx on si_finecon fc  (cost=0.42..188.22 rows=8220 width=4)"



explain
select cu.ticker_unq cu_ticker_unq, cu.company cu_company, cu.dateindex cu_dateindex, cu.dateindexf01lwd cu_dateindexf01lwd , cu.price_m001 cu_price_m001, fu.ticker_unq fu_ticker_unq, fu.dateindex fu_dateindex, fu.price_m001 fu_price_m001
    from ( select rd.dateindex, rd.dateindexf01lwd, fc.ticker_unq, fc.company, fc.price_m001::numeric(15,2) from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fc  where fc.ticker_unq = 'AAPL' and fc.dateindex = rd.dateindex ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2)
    from sipro_data_store.si_finecon fc  where fc.ticker_unq = 'AAPL' and fc.dateindex =  cu.dateindexf01lwd ) fu on true order by cu.dateindex;
-- WORKS LOGICALLY CORRECTLY 1.2 SECONDS -- 430,000 1.2 SECONDS

explain
select cu.ticker_unq cu_ticker_unq, cu.company cu_company, cu.dateindex cu_dateindex, cu.dateindexf01lwd cu_dateindexf01lwd , cu.price_m001 cu_price_m001, fu.ticker_unq fu_ticker_unq, fu.dateindex fu_dateindex, fu.price_m001 fu_price_m001
    from ( select rd.dateindex, rd.dateindexf01lwd, fc.ticker_unq, fc.company, fc.price_m001::numeric(15,2) from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fc  where fc.ticker_unq in ('HP','AAPL','MSFT') and fc.dateindex = rd.dateindex ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2)
    from sipro_data_store.si_finecon fc  where fc.ticker_unq in ('HP','AAPL','MSFT') and fc.dateindex =  cu.dateindexf01lwd ) fu on true order by cu.ticker_unq, cu.dateindex;
-- WORKS LOGICALLY CORRECTLY  1.0 SECONDS 423,000
-- WRONG ANSWER - CARTSIAN PRODUCT

-- NO AAPL
explain -- TOP HALF - 40 SEONDS
select cu.ticker_unq cu_ticker_unq, cu.company cu_company, cu.dateindex cu_dateindex, cu.dateindexf01lwd cu_dateindexf01lwd , cu.price_m001 cu_price_m001 -- , fu.ticker_unq fu_ticker_unq, fu.dateindex fu_dateindex, fu.price_m001 fu_price_m001
    from ( select rd.dateindex, rd.dateindexf01lwd, fc.ticker_unq, fc.company, fc.price_m001::numeric(15,2) from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fc  where fc.dateindex = rd.dateindex ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2)
    from sipro_data_store.si_finecon fc  where fc.dateindex =  cu.dateindexf01lwd ) fu on true order by cu.dateindex;
-- 143 MILLION - TAKE FOREVER

create index si_retdate2_dateindex on si_retdate2(dateindex);
create index si_retdate2_dateindexf01lwd on si_retdate2(dateindexf01lwd);
create index si_retdate2_dateindexf01lwd_dateindex on si_retdate2(dateindexf01lwd,dateindex);
-- AFTER INDEXES; 100 MILLION


explain
select cu.ticker_unq cu_ticker_unq, cu.company cu_company, cu.dateindex cu_dateindex, cu.dateindexf01lwd cu_dateindexf01lwd , cu.price_m001 cu_price_m001, fu.ticker_unq fu_ticker_unq, fu.dateindex fu_dateindex, fu.price_m001 fu_price_m001
    from ( select rd.dateindex, rd.dateindexf01lwd, fc.ticker_unq, fc.company, fc.price_m001::numeric(15,2) from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fc  where fc.ticker_unq in ('HP','AAPL','MSFT') and fc.dateindex = rd.dateindex ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2)
    from sipro_data_store.si_finecon fc  where fc.ticker_unq in ('HP','AAPL','MSFT') and fc.dateindex =  cu.dateindexf01lwd ) fu on true order by cu.ticker_unq, cu.dateindex;
-- WRONG ANSWERS AND CARTESIAN PRODUCT


explain
select cu.ticker_unq cu_ticker_unq, cu.company cu_company, cu.dateindex cu_dateindex, cu.dateindexf01lwd cu_dateindexf01lwd , cu.price_m001 cu_price_m001, fu.ticker_unq fu_ticker_unq, fu.dateindex fu_dateindex, fu.price_m001 fu_price_m001
    from ( select rd.dateindex, rd.dateindexf01lwd, fc.ticker_unq, fc.company, fc.price_m001::numeric(15,2) from sipro_data_store.si_retdate2 rd, sipro_data_store.si_finecon fc  where fc.ticker_unq in ('HP','AAPL','MSFT') and fc.dateindex = rd.dateindex ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2)
    from sipro_data_store.si_finecon fc  where fc.ticker_unq in ('HP','AAPL','MSFT')  ) fu on fu.dateindex =  cu.dateindexf01lwd order by cu.ticker_unq, cu.dateindex;

-- TOP PART 20 SECONDS 227,000
explain
select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex


select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex and fc.ticker_unq in ('HP','AAPL','MSFT') 


[ ] SAVED
easier 'on true' and some lateral deeper looking
------------------------------------------------

-- 16GB RAM -- 140MB OF OS RAM USED

-- WORKS CORRCTLY  
explain
select cu.dateindex cu_dateindex, cu.ticker_unq cu_ticker_unq, fu.dateindex fu_dateindex, fu.ticker_unq fu_ticker_unq from
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex and fc.ticker_unq in ('HP','AAPL','MSFT')  ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex and fc.ticker_unq in ('HP','AAPL','MSFT')  )  fu on cu.dateindexf01lwd = fu.dateindex and cu.ticker_unq   =  fu.ticker_unq order by cu.ticker_unq, cu.dateindex;
-- SECONDS 1.0 SEOCOND  RECORDS 394,000

-- SOMETIMES EASIER TO REPEAT THE SAME SQL TWICE

explain
select cu.dateindex cu_dateindex, cu.ticker_unq cu_ticker_unq, fu.dateindex fu_dateindex, fu.ticker_unq fu_ticker_unq from
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex   ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex  )  fu on cu.dateindexf01lwd = fu.dateindex and cu.ticker_unq   =  fu.ticker_unq order by cu.ticker_unq, cu.dateindex;
-- 40 SECONDS + 40 SECONDS + JOIN SECONDS:  31 SEOCONDS 629,000 RECORDS 
"        ->  Sort  (cost=280021.68..281501.22 rows=591817 width=8)"
"        ->  Sort  (cost=280021.68..281501.22 rows=591817 width=8)" -- NOT SMART ENOUGH TO REALIZE, THAT THE QUERY #2 IS EXALCLY THE SAME AS QUERY #1


-- NOTE: lateral is not required
explain
select cu.dateindex cu_dateindex, cu.ticker_unq cu_ticker_unq, fu.dateindex fu_dateindex, fu.ticker_unq fu_ticker_unq from
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex   ) cu
      left join lateral
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex  )  fu on cu.dateindexf01lwd = fu.dateindex and cu.ticker_unq   =  fu.ticker_unq order by cu.ticker_unq, cu.dateindex;
-- 629,000 RECORDS: 31 SECONDS

explain
with si_fincon_retdate as
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex  )
select cu.dateindex cu_dateindex, cu.ticker_unq cu_ticker_unq, fu.dateindex fu_dateindex, fu.ticker_unq fu_ticker_unq 
  from                               si_fincon_retdate   cu
  left join lateral  ( select * from si_fincon_retdate ) fu -- LATERAL requires a subquery
on 
  cu.dateindexf01lwd = fu.dateindex and cu.ticker_unq   =  fu.ticker_unq order by cu.ticker_unq, cu.dateindex;
-- 1,554,105 RECORDS ( SRANGELY ): 33 SECONDS -- 'EXPLAIN' IS fooled'

-- lateral is not required ( SO DO NOT USE IT ) ( IN THIS CASE )
explain
with si_fincon_retdate as
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex  )
select cu.dateindex cu_dateindex, cu.ticker_unq cu_ticker_unq, fu.dateindex fu_dateindex, fu.ticker_unq fu_ticker_unq 
  from                               si_fincon_retdate   cu
  left join                          si_fincon_retdate   fu
on 
  cu.dateindexf01lwd = fu.dateindex and cu.ticker_unq   =  fu.ticker_unq order by cu.ticker_unq, cu.dateindex;
-- 1,554,105 RECORDS 33 SECONDS

-- ADVANTAGE ( NON-LATERAL - FULL OUTER JOIN )

explain
with si_fincon_retdate as
  ( select fc.dateindex, fc.ticker_unq, fc.price_m001::numeric(15,2), rd.dateindexf01lwd
    from sipro_data_store.si_finecon fc, sipro_data_store.si_retdate2 rd where  fc.dateindex = rd.dateindex  )
select cu.dateindex cu_dateindex, cu.ticker_unq cu_ticker_unq, fu.dateindex fu_dateindex, fu.ticker_unq fu_ticker_unq 
  from                               si_fincon_retdate   cu
  full outer join                    si_fincon_retdate   fu
on 
  cu.dateindexf01lwd = fu.dateindex and cu.ticker_unq   =  fu.ticker_unq order by cu.ticker_unq, cu.dateindex;
-- 1,554,105 RECORDS 33 SECONDS
-- 591,000 - 613,00 EMPTY - 14,000 future DATE/TICKERS without a PAST ( mostly the FIRST index 14911 )

----------------
----------------

Andre notices
-------------

WITHs fool EXPLAIN to think DOING more work THAN actually are doing 






set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 2; -- not 'written in docs'



drop index if exists ticker_unq_AAPL_temp;
create index ticker_unq_AAPL_temp on sipro_data_store.si_psd(ticker_unq) where ticker_unq = 'AAPL';

-- select ( extract( 'epoch' from ( date '2001-10-05' ) ) / ( 3600 * 24 ) )::int;
-- 11600

select splits_plus.* from (
  select splits.*,
    lead( splits.split_dateindexeomlag != splits.split_dateindexeom ) over (order by dateindexeom) before_split,
          splits.split_dateindexeomlag != splits.split_dateindexeom                                 after_split
  from (
  select
      company_id_unq ,
      ticker_unq,
      mktcap::numeric(15,2) * 1000000 mktcap,
      mktcap::numeric(15,2) * 1000000 / nullif(price::numeric(15,2),0) shr_p,
      shsperadr::numeric(15,2) * 1000000 shsperadr,
      shr_aq1::numeric(15,2) * 1000000 shr_aq1,
      dateindexeom,
      company_id_unq_orig,
      dateindex,
      company_id,
            (extract('epoch' from date_trunc('month',split_date)::date                                        ) /(3600*24))::int split_dateindexbom,
            (extract('epoch' from split_date ) /(3600*24))::int split_dateindex,
      lag ( (extract('epoch' from date_trunc('month',split_date)::date + interval '1 month' - interval '1 day') /(3600*24))::int ) over (order by dateindexeom) split_dateindexeomlag,
            (extract('epoch' from date_trunc('month',split_date)::date + interval '1 month' - interval '1 day') /(3600*24))::int                                split_dateindexeom,
      lag(split_fact::numeric(15,2)) over (order by dateindexeom) split_factlag,
          split_fact::numeric(15,2),
      (extract('epoch' from price_date ) /(3600*24))::int price_dateindex,
      price::numeric(15,2)
    from sipro_data_store.si_psd where ticker_unq = 'HRL' 
  order by dateindexeom
  ) splits order by splits.dateindexeom
) splits_plus;

-- RAI -- 3 classic 2 for 1 splits ( one is at the early edge of my date )
-- CPL -- 3 very blank near the beginning
-- HRL -- 3 THIS ONE


select ticker_unq, count(distinct split_date) split_date_count 
  from sipro_data_store.si_psd 
    -- where ticker_unq = 'AAPL' 
  group by ticker_unq 
order by split_date_count desc; -- NEW COMPUTER -- 3 SECONDS -- NO INDEX
-- 191 with '3 or greater'  -- 25 EEQ -- keeps close to 25 or 30

select distinct (extract('epoch' from split_date ) /(3600*24))::int split_dateindex from sipro_data_store.si_psd where ticker_unq = 'HRL';

11003
15020
16841 -- 7 days earlier
16848


select splits_plus.*,
  -- running count -- zone codes
  count(case when after_split = 'f' then null else after_split end) over (order by splits_plus.dateindexeom)
from (
  select splits.*,
    lead( splits.split_dateindexeomlag != splits.split_dateindexeom ) over (order by splits.dateindexeom) before_split,
          splits.split_dateindexeomlag != splits.split_dateindexeom                                 after_split
  from (
  select
      company_id_unq ,
      ticker_unq,
      mktcap::numeric(15,2) * 1000000 mktcap,
      mktcap::numeric(15,2) * 1000000 / nullif(price::numeric(15,2),0) shr_p,
      shsperadr::numeric(15,2) * 1000000 shsperadr,
      shr_aq1::numeric(15,2) * 1000000 shr_aq1,
      dateindexeom,
      company_id_unq_orig,
      dateindex,
      company_id,
            (extract('epoch' from date_trunc('month',coalesce(split_date,date('1800-01-01')))::date                                        ) /(3600*24))::int split_dateindexbom,
            (extract('epoch' from coalesce(split_date,date('1800-01-01'))) /(3600*24))::int split_dateindex,
      lag ( (extract('epoch' from date_trunc('month',coalesce(split_date,date('1800-01-01')))::date + interval '1 month' - interval '1 day') /(3600*24))::int ) over (order by dateindexeom) split_dateindexeomlag,
            (extract('epoch' from date_trunc('month',coalesce(split_date,date('1800-01-01')))::date + interval '1 month' - interval '1 day') /(3600*24))::int                                split_dateindexeom,
      lag(split_fact::numeric(15,2)) over (order by dateindexeom) split_factlag,
          split_fact::numeric(15,2),
      (extract('epoch' from price_date ) /(3600*24))::int price_dateindex,
      price::numeric(15,2)
    from sipro_data_store.si_psd where ticker_unq = 'RAI' -- CPL -- AAPL -- RAI
  order by dateindexeom
  ) splits order by splits.dateindexeom
) splits_plus order by splits_plus.dateindexeom;
-- LOTS OF EARLY NULLS REPLACED BY EARLY NEGATIVES


-- before get TOO DEEP check out sipro_data_store.si_psdc

-- where are the splits?
select * from sipro_data_store.si_psdc where ticker_unq = 'AAPL' order by dateindexeom;
-- JUST EASIER LIKE THIS OR FROM si_fincon

select * from sipro_data_store.si_psdd where ticker_unq = 'AAPL' order by dateindexeom;



library(stringr)
library(R.rsp)
n = (38-1) # 
writeLines(str_trim(str_c(rstring('
select dateindex, company_id_unq, dateindexeom, ticker_unq,
  <% for (i in 1:n) { -%>
      <% res <- sprintf("(price_m%1$s::numeric(15,2) - price_m%2$s::numeric(15,2)) / nullif(abs(price_m%2$s::numeric(15,2)),0) * 100 * 12 m%1$s_m%2$s_prch_ann", 
          str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0"))
      -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
  <% } %>
  from sipro_data_store.si_psdc where ticker_unq = \'AAPL\' order by dateindexeom;
'))))

-- HOW MUCH TIME TO DO THE ENTIRE UNIVERSE AND HISTORY?

select dateindex, company_id_unq, dateindexeom, ticker_unq,
    (price_m001::numeric(15,2) - price_m002::numeric(15,2)) / nullif(abs(price_m002::numeric(15,2)),0) * 100 * 12 m001_m002_prch_ann,
    (price_m002::numeric(15,2) - price_m003::numeric(15,2)) / nullif(abs(price_m003::numeric(15,2)),0) * 100 * 12 m002_m003_prch_ann,
    (price_m003::numeric(15,2) - price_m004::numeric(15,2)) / nullif(abs(price_m004::numeric(15,2)),0) * 100 * 12 m003_m004_prch_ann,
    (price_m004::numeric(15,2) - price_m005::numeric(15,2)) / nullif(abs(price_m005::numeric(15,2)),0) * 100 * 12 m004_m005_prch_ann,
    (price_m005::numeric(15,2) - price_m006::numeric(15,2)) / nullif(abs(price_m006::numeric(15,2)),0) * 100 * 12 m005_m006_prch_ann,
    (price_m006::numeric(15,2) - price_m007::numeric(15,2)) / nullif(abs(price_m007::numeric(15,2)),0) * 100 * 12 m006_m007_prch_ann,
    (price_m007::numeric(15,2) - price_m008::numeric(15,2)) / nullif(abs(price_m008::numeric(15,2)),0) * 100 * 12 m007_m008_prch_ann,
    (price_m008::numeric(15,2) - price_m009::numeric(15,2)) / nullif(abs(price_m009::numeric(15,2)),0) * 100 * 12 m008_m009_prch_ann,
    (price_m009::numeric(15,2) - price_m010::numeric(15,2)) / nullif(abs(price_m010::numeric(15,2)),0) * 100 * 12 m009_m010_prch_ann,
    (price_m010::numeric(15,2) - price_m011::numeric(15,2)) / nullif(abs(price_m011::numeric(15,2)),0) * 100 * 12 m010_m011_prch_ann,
    (price_m011::numeric(15,2) - price_m012::numeric(15,2)) / nullif(abs(price_m012::numeric(15,2)),0) * 100 * 12 m011_m012_prch_ann,
    (price_m012::numeric(15,2) - price_m013::numeric(15,2)) / nullif(abs(price_m013::numeric(15,2)),0) * 100 * 12 m012_m013_prch_ann,
    (price_m013::numeric(15,2) - price_m014::numeric(15,2)) / nullif(abs(price_m014::numeric(15,2)),0) * 100 * 12 m013_m014_prch_ann,
    (price_m014::numeric(15,2) - price_m015::numeric(15,2)) / nullif(abs(price_m015::numeric(15,2)),0) * 100 * 12 m014_m015_prch_ann,
    (price_m015::numeric(15,2) - price_m016::numeric(15,2)) / nullif(abs(price_m016::numeric(15,2)),0) * 100 * 12 m015_m016_prch_ann,
    (price_m016::numeric(15,2) - price_m017::numeric(15,2)) / nullif(abs(price_m017::numeric(15,2)),0) * 100 * 12 m016_m017_prch_ann,
    (price_m017::numeric(15,2) - price_m018::numeric(15,2)) / nullif(abs(price_m018::numeric(15,2)),0) * 100 * 12 m017_m018_prch_ann,
    (price_m018::numeric(15,2) - price_m019::numeric(15,2)) / nullif(abs(price_m019::numeric(15,2)),0) * 100 * 12 m018_m019_prch_ann,
    (price_m019::numeric(15,2) - price_m020::numeric(15,2)) / nullif(abs(price_m020::numeric(15,2)),0) * 100 * 12 m019_m020_prch_ann,
    (price_m020::numeric(15,2) - price_m021::numeric(15,2)) / nullif(abs(price_m021::numeric(15,2)),0) * 100 * 12 m020_m021_prch_ann,
    (price_m021::numeric(15,2) - price_m022::numeric(15,2)) / nullif(abs(price_m022::numeric(15,2)),0) * 100 * 12 m021_m022_prch_ann,
    (price_m022::numeric(15,2) - price_m023::numeric(15,2)) / nullif(abs(price_m023::numeric(15,2)),0) * 100 * 12 m022_m023_prch_ann,
    (price_m023::numeric(15,2) - price_m024::numeric(15,2)) / nullif(abs(price_m024::numeric(15,2)),0) * 100 * 12 m023_m024_prch_ann,
    (price_m024::numeric(15,2) - price_m025::numeric(15,2)) / nullif(abs(price_m025::numeric(15,2)),0) * 100 * 12 m024_m025_prch_ann,
    (price_m025::numeric(15,2) - price_m026::numeric(15,2)) / nullif(abs(price_m026::numeric(15,2)),0) * 100 * 12 m025_m026_prch_ann,
    (price_m026::numeric(15,2) - price_m027::numeric(15,2)) / nullif(abs(price_m027::numeric(15,2)),0) * 100 * 12 m026_m027_prch_ann,
    (price_m027::numeric(15,2) - price_m028::numeric(15,2)) / nullif(abs(price_m028::numeric(15,2)),0) * 100 * 12 m027_m028_prch_ann,
    (price_m028::numeric(15,2) - price_m029::numeric(15,2)) / nullif(abs(price_m029::numeric(15,2)),0) * 100 * 12 m028_m029_prch_ann,
    (price_m029::numeric(15,2) - price_m030::numeric(15,2)) / nullif(abs(price_m030::numeric(15,2)),0) * 100 * 12 m029_m030_prch_ann,
    (price_m030::numeric(15,2) - price_m031::numeric(15,2)) / nullif(abs(price_m031::numeric(15,2)),0) * 100 * 12 m030_m031_prch_ann,
    (price_m031::numeric(15,2) - price_m032::numeric(15,2)) / nullif(abs(price_m032::numeric(15,2)),0) * 100 * 12 m031_m032_prch_ann,
    (price_m032::numeric(15,2) - price_m033::numeric(15,2)) / nullif(abs(price_m033::numeric(15,2)),0) * 100 * 12 m032_m033_prch_ann,
    (price_m033::numeric(15,2) - price_m034::numeric(15,2)) / nullif(abs(price_m034::numeric(15,2)),0) * 100 * 12 m033_m034_prch_ann,
    (price_m034::numeric(15,2) - price_m035::numeric(15,2)) / nullif(abs(price_m035::numeric(15,2)),0) * 100 * 12 m034_m035_prch_ann,
    (price_m035::numeric(15,2) - price_m036::numeric(15,2)) / nullif(abs(price_m036::numeric(15,2)),0) * 100 * 12 m035_m036_prch_ann,
    (price_m036::numeric(15,2) - price_m037::numeric(15,2)) / nullif(abs(price_m037::numeric(15,2)),0) * 100 * 12 m036_m037_prch_ann,
    (price_m037::numeric(15,2) - price_m038::numeric(15,2)) / nullif(abs(price_m038::numeric(15,2)),0) * 100 * 12 m037_m038_prch_ann
  from sipro_data_store.si_psdc  order by dateindexeom; -- where ticker_unq = 'AAPL'

-- 19.6 SECONDS ( NEW COMPUTER )
ERROR:  numeric field overflow
DETAIL:  A field with precision 15, scale 2 must round to an absolute value less than 10^13.
********** Error **********

ERROR: numeric field overflow
SQL state: 22003
Detail: A field with precision 15, scale 2 must round to an absolute value less than 10^13.


-- KEEP-ISH [ ]
library(stringr)
library(R.rsp)
n = (38-1) # 
writeLines(str_trim(str_c(rstring('
select dateindex, company_id_unq, dateindexeom, ticker_unq,
  <% for (i in 1:n) { -%>
      <% sprintf("(price_m%1$s::float - price_m%2$s::float) / nullif(abs(price_m%2$s::float),0) * 100 * 12 m%1$s_m%2$s_prch_ann", 
          str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
      -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
  <% } %>
  from sipro_data_store.si_psdc where ticker_unq = \'AAPL\' order by dateindexeom;
'))))

--explain 
"Sort  (cost=741969.77..743446.07 rows=590521 width=314)"
"  Sort Key: dateindexeom"
"  ->  Seq Scan on si_psdc  (cost=0.00..685363.52 rows=590521 width=314)"


-- HOW MUCH TIME TO DO THE ENTIRE UNIVERSE AND HISTORY?
-- 7 MINUTES EVEN

select dateindex, company_id_unq, dateindexeom, ticker_unq,
    (price_m001::float - price_m002::float) / nullif(abs(price_m002::float),0) * 100 * 12 m001_m002_prch_ann,
    (price_m002::float - price_m003::float) / nullif(abs(price_m003::float),0) * 100 * 12 m002_m003_prch_ann,
    (price_m003::float - price_m004::float) / nullif(abs(price_m004::float),0) * 100 * 12 m003_m004_prch_ann,
    (price_m004::float - price_m005::float) / nullif(abs(price_m005::float),0) * 100 * 12 m004_m005_prch_ann,
    (price_m005::float - price_m006::float) / nullif(abs(price_m006::float),0) * 100 * 12 m005_m006_prch_ann,
    (price_m006::float - price_m007::float) / nullif(abs(price_m007::float),0) * 100 * 12 m006_m007_prch_ann,
    (price_m007::float - price_m008::float) / nullif(abs(price_m008::float),0) * 100 * 12 m007_m008_prch_ann,
    (price_m008::float - price_m009::float) / nullif(abs(price_m009::float),0) * 100 * 12 m008_m009_prch_ann,
    (price_m009::float - price_m010::float) / nullif(abs(price_m010::float),0) * 100 * 12 m009_m010_prch_ann,
    (price_m010::float - price_m011::float) / nullif(abs(price_m011::float),0) * 100 * 12 m010_m011_prch_ann,
    (price_m011::float - price_m012::float) / nullif(abs(price_m012::float),0) * 100 * 12 m011_m012_prch_ann,
    (price_m012::float - price_m013::float) / nullif(abs(price_m013::float),0) * 100 * 12 m012_m013_prch_ann,
    (price_m013::float - price_m014::float) / nullif(abs(price_m014::float),0) * 100 * 12 m013_m014_prch_ann,
    (price_m014::float - price_m015::float) / nullif(abs(price_m015::float),0) * 100 * 12 m014_m015_prch_ann,
    (price_m015::float - price_m016::float) / nullif(abs(price_m016::float),0) * 100 * 12 m015_m016_prch_ann,
    (price_m016::float - price_m017::float) / nullif(abs(price_m017::float),0) * 100 * 12 m016_m017_prch_ann,
    (price_m017::float - price_m018::float) / nullif(abs(price_m018::float),0) * 100 * 12 m017_m018_prch_ann,
    (price_m018::float - price_m019::float) / nullif(abs(price_m019::float),0) * 100 * 12 m018_m019_prch_ann,
    (price_m019::float - price_m020::float) / nullif(abs(price_m020::float),0) * 100 * 12 m019_m020_prch_ann,
    (price_m020::float - price_m021::float) / nullif(abs(price_m021::float),0) * 100 * 12 m020_m021_prch_ann,
    (price_m021::float - price_m022::float) / nullif(abs(price_m022::float),0) * 100 * 12 m021_m022_prch_ann,
    (price_m022::float - price_m023::float) / nullif(abs(price_m023::float),0) * 100 * 12 m022_m023_prch_ann,
    (price_m023::float - price_m024::float) / nullif(abs(price_m024::float),0) * 100 * 12 m023_m024_prch_ann,
    (price_m024::float - price_m025::float) / nullif(abs(price_m025::float),0) * 100 * 12 m024_m025_prch_ann,
    (price_m025::float - price_m026::float) / nullif(abs(price_m026::float),0) * 100 * 12 m025_m026_prch_ann,
    (price_m026::float - price_m027::float) / nullif(abs(price_m027::float),0) * 100 * 12 m026_m027_prch_ann,
    (price_m027::float - price_m028::float) / nullif(abs(price_m028::float),0) * 100 * 12 m027_m028_prch_ann,
    (price_m028::float - price_m029::float) / nullif(abs(price_m029::float),0) * 100 * 12 m028_m029_prch_ann,
    (price_m029::float - price_m030::float) / nullif(abs(price_m030::float),0) * 100 * 12 m029_m030_prch_ann,
    (price_m030::float - price_m031::float) / nullif(abs(price_m031::float),0) * 100 * 12 m030_m031_prch_ann,
    (price_m031::float - price_m032::float) / nullif(abs(price_m032::float),0) * 100 * 12 m031_m032_prch_ann,
    (price_m032::float - price_m033::float) / nullif(abs(price_m033::float),0) * 100 * 12 m032_m033_prch_ann,
    (price_m033::float - price_m034::float) / nullif(abs(price_m034::float),0) * 100 * 12 m033_m034_prch_ann,
    (price_m034::float - price_m035::float) / nullif(abs(price_m035::float),0) * 100 * 12 m034_m035_prch_ann,
    (price_m035::float - price_m036::float) / nullif(abs(price_m036::float),0) * 100 * 12 m035_m036_prch_ann,
    (price_m036::float - price_m037::float) / nullif(abs(price_m037::float),0) * 100 * 12 m036_m037_prch_ann,
    (price_m037::float - price_m038::float) / nullif(abs(price_m038::float),0) * 100 * 12 m037_m038_prch_ann
  from sipro_data_store.si_psdc  order by dateindexeom; -- where ticker_unq = 'AAPL'


set force_parallel_mode = on; 
-- how much time ?

--explain
"Gather  (cost=742969.77..803498.17 rows=590521 width=314)"
"  Workers Planned: 1"
"  Single Copy: true"
"  ->  Sort  (cost=741969.77..743446.07 rows=590521 width=314)"
"        Sort Key: dateindexeom"
"        ->  Seq Scan on si_psdc  (cost=0.00..685363.52 rows=590521 width=314)"

-- 7 MINUTES

set force_parallel_mode = off; 

-- BELOW: SHOULD HAVE MADE THE DATATYPES numeric FRIENDLIER WITH FUNCTIONS --

-- [ ] KEEP
library(stringr)
library(R.rsp)
n = (38-1) # 
writeLines({str_trim(str_c(rstring('
alter table if exists sipro_data_store.si_finecon
  <% for (i in 1:n) { -%>
      <% sprintf("add if not exists m%1$s_m%2$s_prch_ann float", 
           str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
      -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
  <% } %>
; 
'))) -> out})
--writeLines(out)

writeLines({str_trim(str_c(rstring('
'))) -> out})
--writeLines(out)

alter table if exists sipro_data_store.si_finecon
    add if not exists m001_m002_prch_ann float,
    add if not exists m002_m003_prch_ann float,
    add if not exists m003_m004_prch_ann float,
    add if not exists m004_m005_prch_ann float,
    add if not exists m005_m006_prch_ann float,
    add if not exists m006_m007_prch_ann float,
    add if not exists m007_m008_prch_ann float,
    add if not exists m008_m009_prch_ann float,
    add if not exists m009_m010_prch_ann float,
    add if not exists m010_m011_prch_ann float,
    add if not exists m011_m012_prch_ann float,
    add if not exists m012_m013_prch_ann float,
    add if not exists m013_m014_prch_ann float,
    add if not exists m014_m015_prch_ann float,
    add if not exists m015_m016_prch_ann float,
    add if not exists m016_m017_prch_ann float,
    add if not exists m017_m018_prch_ann float,
    add if not exists m018_m019_prch_ann float,
    add if not exists m019_m020_prch_ann float,
    add if not exists m020_m021_prch_ann float,
    add if not exists m021_m022_prch_ann float,
    add if not exists m022_m023_prch_ann float,
    add if not exists m023_m024_prch_ann float,
    add if not exists m024_m025_prch_ann float,
    add if not exists m025_m026_prch_ann float,
    add if not exists m026_m027_prch_ann float,
    add if not exists m027_m028_prch_ann float,
    add if not exists m028_m029_prch_ann float,
    add if not exists m029_m030_prch_ann float,
    add if not exists m030_m031_prch_ann float,
    add if not exists m031_m032_prch_ann float,
    add if not exists m032_m033_prch_ann float,
    add if not exists m033_m034_prch_ann float,
    add if not exists m034_m035_prch_ann float,
    add if not exists m035_m036_prch_ann float,
    add if not exists m036_m037_prch_ann float,
    add if not exists m037_m038_prch_ann float
;
-- [x] DONE ( INSTANTANEOUSLY )



--update sipro_data_store.atable at set
--   bcolumn =  5.00,
--   ccolumn = 10.00
--where acolumn = 12;

-- [ ] KEEP
library(stringr)
library(R.rsp)
n = (38-1) # 
writeLines({str_trim(str_c(rstring('
update sipro_data_store.si_finecon fc set
  <% for (i in 1:n) { -%>
      <% sprintf("m%1$s_m%2$s_prch_ann = sq.m%1$s_m%2$s_prch_ann", 
           str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
      -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
  <% } %>
from (
select dateindex, company_id_unq, dateindexeom, ticker_unq,
  <% for (i in 1:n) { -%>
      <% sprintf("(price_m%1$s::float - price_m%2$s::float) / nullif(abs(price_m%2$s::float),0) * 100 * 12 m%1$s_m%2$s_prch_ann", 
          str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
      -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
  <% } %>
  from sipro_data_store.si_psdc order by dateindexeom
) sq
where fc.dateindex = sq.dateindex and fc.company_id_unq = sq.company_id_unq;
'))) -> out})
--writeLines(out)

-- explain
"Update on si_finecon fc  (cost=758209.09..1838339.69 rows=752545 width=3347)"
"  ->  Hash Join  (cost=758209.09..1838339.69 rows=752545 width=3347)"
"        Hash Cond: ((fc.dateindex = sq.dateindex) AND (fc.company_id_unq = sq.company_id_unq))"
"        ->  Seq Scan on si_finecon fc  (cost=0.00..222261.49 rows=1429149 width=2713)"
"        ->  Hash  (cost=749351.28..749351.28 rows=590521 width=644)"
"              ->  Subquery Scan on sq  (cost=741969.77..749351.28 rows=590521 width=644)"
"                    ->  Sort  (cost=741969.77..743446.07 rows=590521 width=314)"
"                          Sort Key: si_psdc.dateindexeom"
"                          ->  Seq Scan on si_psdc  (cost=0.00..685363.52 rows=590521 width=314)"

update sipro_data_store.si_finecon fc set
    m001_m002_prch_ann = sq.m001_m002_prch_ann,
    m002_m003_prch_ann = sq.m002_m003_prch_ann,
    m003_m004_prch_ann = sq.m003_m004_prch_ann,
    m004_m005_prch_ann = sq.m004_m005_prch_ann,
    m005_m006_prch_ann = sq.m005_m006_prch_ann,
    m006_m007_prch_ann = sq.m006_m007_prch_ann,
    m007_m008_prch_ann = sq.m007_m008_prch_ann,
    m008_m009_prch_ann = sq.m008_m009_prch_ann,
    m009_m010_prch_ann = sq.m009_m010_prch_ann,
    m010_m011_prch_ann = sq.m010_m011_prch_ann,
    m011_m012_prch_ann = sq.m011_m012_prch_ann,
    m012_m013_prch_ann = sq.m012_m013_prch_ann,
    m013_m014_prch_ann = sq.m013_m014_prch_ann,
    m014_m015_prch_ann = sq.m014_m015_prch_ann,
    m015_m016_prch_ann = sq.m015_m016_prch_ann,
    m016_m017_prch_ann = sq.m016_m017_prch_ann,
    m017_m018_prch_ann = sq.m017_m018_prch_ann,
    m018_m019_prch_ann = sq.m018_m019_prch_ann,
    m019_m020_prch_ann = sq.m019_m020_prch_ann,
    m020_m021_prch_ann = sq.m020_m021_prch_ann,
    m021_m022_prch_ann = sq.m021_m022_prch_ann,
    m022_m023_prch_ann = sq.m022_m023_prch_ann,
    m023_m024_prch_ann = sq.m023_m024_prch_ann,
    m024_m025_prch_ann = sq.m024_m025_prch_ann,
    m025_m026_prch_ann = sq.m025_m026_prch_ann,
    m026_m027_prch_ann = sq.m026_m027_prch_ann,
    m027_m028_prch_ann = sq.m027_m028_prch_ann,
    m028_m029_prch_ann = sq.m028_m029_prch_ann,
    m029_m030_prch_ann = sq.m029_m030_prch_ann,
    m030_m031_prch_ann = sq.m030_m031_prch_ann,
    m031_m032_prch_ann = sq.m031_m032_prch_ann,
    m032_m033_prch_ann = sq.m032_m033_prch_ann,
    m033_m034_prch_ann = sq.m033_m034_prch_ann,
    m034_m035_prch_ann = sq.m034_m035_prch_ann,
    m035_m036_prch_ann = sq.m035_m036_prch_ann,
    m036_m037_prch_ann = sq.m036_m037_prch_ann,
    m037_m038_prch_ann = sq.m037_m038_prch_ann
from (
select dateindex, company_id_unq, dateindexeom, ticker_unq,
    (price_m001::float - price_m002::float) / nullif(abs(price_m002::float),0) * 100 * 12 m001_m002_prch_ann,
    (price_m002::float - price_m003::float) / nullif(abs(price_m003::float),0) * 100 * 12 m002_m003_prch_ann,
    (price_m003::float - price_m004::float) / nullif(abs(price_m004::float),0) * 100 * 12 m003_m004_prch_ann,
    (price_m004::float - price_m005::float) / nullif(abs(price_m005::float),0) * 100 * 12 m004_m005_prch_ann,
    (price_m005::float - price_m006::float) / nullif(abs(price_m006::float),0) * 100 * 12 m005_m006_prch_ann,
    (price_m006::float - price_m007::float) / nullif(abs(price_m007::float),0) * 100 * 12 m006_m007_prch_ann,
    (price_m007::float - price_m008::float) / nullif(abs(price_m008::float),0) * 100 * 12 m007_m008_prch_ann,
    (price_m008::float - price_m009::float) / nullif(abs(price_m009::float),0) * 100 * 12 m008_m009_prch_ann,
    (price_m009::float - price_m010::float) / nullif(abs(price_m010::float),0) * 100 * 12 m009_m010_prch_ann,
    (price_m010::float - price_m011::float) / nullif(abs(price_m011::float),0) * 100 * 12 m010_m011_prch_ann,
    (price_m011::float - price_m012::float) / nullif(abs(price_m012::float),0) * 100 * 12 m011_m012_prch_ann,
    (price_m012::float - price_m013::float) / nullif(abs(price_m013::float),0) * 100 * 12 m012_m013_prch_ann,
    (price_m013::float - price_m014::float) / nullif(abs(price_m014::float),0) * 100 * 12 m013_m014_prch_ann,
    (price_m014::float - price_m015::float) / nullif(abs(price_m015::float),0) * 100 * 12 m014_m015_prch_ann,
    (price_m015::float - price_m016::float) / nullif(abs(price_m016::float),0) * 100 * 12 m015_m016_prch_ann,
    (price_m016::float - price_m017::float) / nullif(abs(price_m017::float),0) * 100 * 12 m016_m017_prch_ann,
    (price_m017::float - price_m018::float) / nullif(abs(price_m018::float),0) * 100 * 12 m017_m018_prch_ann,
    (price_m018::float - price_m019::float) / nullif(abs(price_m019::float),0) * 100 * 12 m018_m019_prch_ann,
    (price_m019::float - price_m020::float) / nullif(abs(price_m020::float),0) * 100 * 12 m019_m020_prch_ann,
    (price_m020::float - price_m021::float) / nullif(abs(price_m021::float),0) * 100 * 12 m020_m021_prch_ann,
    (price_m021::float - price_m022::float) / nullif(abs(price_m022::float),0) * 100 * 12 m021_m022_prch_ann,
    (price_m022::float - price_m023::float) / nullif(abs(price_m023::float),0) * 100 * 12 m022_m023_prch_ann,
    (price_m023::float - price_m024::float) / nullif(abs(price_m024::float),0) * 100 * 12 m023_m024_prch_ann,
    (price_m024::float - price_m025::float) / nullif(abs(price_m025::float),0) * 100 * 12 m024_m025_prch_ann,
    (price_m025::float - price_m026::float) / nullif(abs(price_m026::float),0) * 100 * 12 m025_m026_prch_ann,
    (price_m026::float - price_m027::float) / nullif(abs(price_m027::float),0) * 100 * 12 m026_m027_prch_ann,
    (price_m027::float - price_m028::float) / nullif(abs(price_m028::float),0) * 100 * 12 m027_m028_prch_ann,
    (price_m028::float - price_m029::float) / nullif(abs(price_m029::float),0) * 100 * 12 m028_m029_prch_ann,
    (price_m029::float - price_m030::float) / nullif(abs(price_m030::float),0) * 100 * 12 m029_m030_prch_ann,
    (price_m030::float - price_m031::float) / nullif(abs(price_m031::float),0) * 100 * 12 m030_m031_prch_ann,
    (price_m031::float - price_m032::float) / nullif(abs(price_m032::float),0) * 100 * 12 m031_m032_prch_ann,
    (price_m032::float - price_m033::float) / nullif(abs(price_m033::float),0) * 100 * 12 m032_m033_prch_ann,
    (price_m033::float - price_m034::float) / nullif(abs(price_m034::float),0) * 100 * 12 m033_m034_prch_ann,
    (price_m034::float - price_m035::float) / nullif(abs(price_m035::float),0) * 100 * 12 m034_m035_prch_ann,
    (price_m035::float - price_m036::float) / nullif(abs(price_m036::float),0) * 100 * 12 m035_m036_prch_ann,
    (price_m036::float - price_m037::float) / nullif(abs(price_m037::float),0) * 100 * 12 m036_m037_prch_ann,
    (price_m037::float - price_m038::float) / nullif(abs(price_m038::float),0) * 100 * 12 m037_m038_prch_ann
  from sipro_data_store.si_psdc order by dateindexeom
) sq
where fc.dateindex = sq.dateindex and fc.company_id_unq = sq.company_id_unq;
-- ONLY 4 minutes
-- [X] DONE


-- [ ] KEEP
library(stringr)
library(R.rsp)
n = (38-1) # 
writeLines({str_trim(str_c(rstring('
alter table if exists sipro_data_store.si_finecon
  <% for (i in 1:n) { -%>
      <% sprintf("drop if exists m%1$s_m%2$s_prch_ann", 
           str_pad(i,3,"left","0"), str_pad((i+1),3,"left","0")) -> res
      -%><%= str_c("    ",res) %><%=if(i  < n) ", \n" -%><%=if(i == n) "  \n" -%>
  <% } %>
; 
'))) -> out})
--writeLines(out)

-- NOT EXECUTING
alter table if exists sipro_data_store.si_finecon
    drop if exists m001_m002_prch_ann,
    drop if exists m002_m003_prch_ann,
    drop if exists m003_m004_prch_ann,
    drop if exists m004_m005_prch_ann,
    drop if exists m005_m006_prch_ann,
    drop if exists m006_m007_prch_ann,
    drop if exists m007_m008_prch_ann,
    drop if exists m008_m009_prch_ann,
    drop if exists m009_m010_prch_ann,
    drop if exists m010_m011_prch_ann,
    drop if exists m011_m012_prch_ann,
    drop if exists m012_m013_prch_ann,
    drop if exists m013_m014_prch_ann,
    drop if exists m014_m015_prch_ann,
    drop if exists m015_m016_prch_ann,
    drop if exists m016_m017_prch_ann,
    drop if exists m017_m018_prch_ann,
    drop if exists m018_m019_prch_ann,
    drop if exists m019_m020_prch_ann,
    drop if exists m020_m021_prch_ann,
    drop if exists m021_m022_prch_ann,
    drop if exists m022_m023_prch_ann,
    drop if exists m023_m024_prch_ann,
    drop if exists m024_m025_prch_ann,
    drop if exists m025_m026_prch_ann,
    drop if exists m026_m027_prch_ann,
    drop if exists m027_m028_prch_ann,
    drop if exists m028_m029_prch_ann,
    drop if exists m029_m030_prch_ann,
    drop if exists m030_m031_prch_ann,
    drop if exists m031_m032_prch_ann,
    drop if exists m032_m033_prch_ann,
    drop if exists m033_m034_prch_ann,
    drop if exists m034_m035_prch_ann,
    drop if exists m035_m036_prch_ann,
    drop if exists m036_m037_prch_ann,
    drop if exists m037_m038_prch_ann
;
--writeLines(out)


show all;

-- ACTUAL RUN --
-- ALREADY DONE



-- true sortino ratio
-- calculates across the column values

drop function tsortino(risk_free_float numeric, actual_columns numeric[]);

create or replace function tsortino(numeric, numeric[]) returns numeric as $$
  -- if an empty colun value exists, then I do not want it to calculate a resulting value
  select case when array_length(array_agg(v),1) > count(v) then null else 
    (avg(v) - $1) / nullif(stddev_pop(case when v > 0 then 0 else v end ),0)
  end from unnest($2) g(v)
$$ language sql;

select t.a,t.b,t.c from ( select 1.0 a, 2.0 b, null c union select -8.0, -16.0, -32.0 ) t;

  a   |   b   |   c
------+-------+-------
 -8.0 | -16.0 | -32.0
  1.0 |   2.0 |
(2 rows)

select tsortino(0, array[t.a,t.b,t.c]) from ( select 1.0 a, 2.0 b, null c union select -8.0, -16.0, -32.0 ) t;

      tsortino
---------------------
 -1.8708286933869707
                     -- 2nd ROW is null becuase ( if an empty colun value exists, then I do not want it to calculate a resulting value )
(2 rows)

explain
select tsortino(0, array[m001_m002_prch_ann::numeric, m002_m003_prch_ann::numeric, m003_m004_prch_ann::numeric]) prch_ann_p3m_tsortinoo 
from sipro_data_store.si_finecon where dateindex = 17044;
-- instant response -- index scan

-- ALL many HUNDREDS OF THOUSANDS ( left off )
-- 38 SECONDS -- ANONYMOUST
select tsortino(0, array[m001_m002_prch_ann::numeric, m002_m003_prch_ann::numeric, m003_m004_prch_ann::numeric]) prch_ann_p3m_tsortinoo 
from sipro_data_store.si_finecon;

-- LEFT_OFF ... VACATION 



------------------------------------------------------------------
------------------------------------------------------------------
------------- begin figure out updates ----------------------------

set search_path to fe_data_store,public;

set time zone 'utc';
--set work_mem to  '1200MB';
   set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'


# verify_company_basics(dateindex = c(15155)) -> si_all_g_df
# rm(list=setdiff(ls(all.names=TRUE),c("si_all_g_df","con","cid")))
# update_from_future_new_company_ids(si_all_g_df,15155) -> OUTPUT


select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg 
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company      = src.company
order by 3
-- BUT THIS WORKS


select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg 
      where trg.company_id != src.company_id and
            trg.ticker      = src.ticker
order by 3
-- 9608


select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st
        from trg 
where company like  '%United Bancorp, Inc.%';

  trg_ci  | trg_tk |        trg_c         |         trg_st
----------+--------+----------------------+-------------------------
 90944L10 | UBMI   | United Bancorp, Inc. | 2723 South State Street
 90991110 | UBCP   | United Bancorp, Inc. | 201 South 4th Street
(2 rows)

               update trg
                 set company_id =    src.company_id
               from src
                 where trg.company_id != src.company_id and
                       trg.ticker      = src.ticker;
-- 9608

select count(1)
               from src, trg
                 where trg.company_id  = src.company_id and
                       trg.ticker      = src.ticker;
--9608

select count(1)
               from trg;
--9847

select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st
        from trg 
where company like  '%United Bancorp, Inc.%';

 trg_ci | trg_tk |        trg_c         |         trg_st
--------+--------+----------------------+-------------------------
 A0DD1  | UBCP   | United Bancorp, Inc. | 201 South 4th Street
 AA911  | UBMI   | United Bancorp, Inc. | 2723 South State Street
(2 rows)


-- VERY GOOD
select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st
from trg
where trg.company_id
not in ( 
  -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select src.company_id
     from trg, src
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker -- 9608
) 
order by 3;
-- 239


select count(1) 
        from src, trg
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company     = src.company  -- 117
and
trg.company_id
not in ( 
  -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select src1.company_id
     from trg trg1, src src1
      where trg1.company_id = src1.company_id and
            trg1.ticker     = src1.ticker -- 9608
) 
-- 117

select trg.trg_ci, trg. trg_tk, trg.trg_c, trg.trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
from src left join lateral (
  -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st
     from trg, src
      where trg.company_id  != src.company_id and
            trg.ticker      != src.ticker and
            trg.company_id  = src.company_id
) trg on (true)
order by 3;
--10188 ( I can see the entir src side )

update trg
set   company_id = src.company_id
from src left join lateral (
  -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st
     from trg
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker 
) trg on (true) where trg


select src.company_id trg_ci, src.ticker trg_tk, src.company trg_c, src.street trg_st
from src
where src.company_id
not in ( 
  -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select trg.company_id
     from trg, src
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker -- 9608
) 
order by 3;
-- 580


select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
from trg, src
where 
trg.company_id
not in ( 
  -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select src.company_id
     from trg, src
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker -- 9608
) 
and
src.company_id
not in ( 
  -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select trg.company_id
     from trg, src
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker -- 9608
) 
and 
trg.company_id = src.company_id and
trg.ticker     = src.ticker;
-- zero
> 9847 - 239
[1] 9608

select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company     = src.company
order by 3;
- NOT RIGHT SOME DUPS
-- 117 NOT RIGHT SOME DUPS



select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg
      where ( trg.company_id != src.company_id and trg.company  = src.company ) and
            ( trg.ticker     != src.ticker     and  trg.company  = src.company )
order by 3;
-- 117 NOT RIGHT SOME DUPS

select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg
      where trg.company_id != src.company_id and
            trg.company     = src.company
order by 3;
--117 rows


select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company     = src.company  -- 117
and trg.company in -- DUPLICATES COPANY NAMES
(select trg.company 
from trg
where trg.company
in ( -- eliminates duplicate company names
     -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select src.company
     from trg, src
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker 
)
)
order by 3

explain
select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company     = src.company  -- 117
and trg.company not in -- IN -> NOT_IN -> ELMINTATES DUPLICATED COMPANY NAMES
(select trg.company 
from trg
where trg.company
in ( 
     -- un-accounted-for targets  _UNMATCHED_ _OR_ _NEW_COMPANIES_
  select src.company
     from trg, src
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker 
  )
)
order by 3
--99 rows ( 5 second query )

-- KEEP ( horribly inefficient but gets the job done )
explain
update trg
set company_id = src.company_id
        from src
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company     = src.company  -- 117
and trg.company not in -- IN -> NOT_IN -> ELMINTATES DUPLICATED COMPANY NAMES
(select trg.company 
from trg
where trg.company
in ( 
  select src.company
     from trg, src
      where trg.company_id = src.company_id and
            trg.ticker     = src.ticker 
  )
) -- 99 rows ( 5 second update )



select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg
      where -- trg.company_id != src.company_id and
            trg.ticker      = src.ticker and -- 9608
            trg.company     = src.company
order by 3;
--9577

select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg
      where trg.company_id  = src.company_id and
            trg.ticker     != src.ticker and
            trg.company    != src.company
order by 3;
-- zero ... OTHER zero



update trg
  set company_id =    src.company_id
        from src
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company     = src.company;
-- 116 affected

select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st
        from trg 
where company like  '%United Bancorp, Inc.%';

 trg_ci | trg_tk |        trg_c         |         trg_st
--------+--------+----------------------+-------------------------
 A0DD1  | UBMI   | United Bancorp, Inc. | 2723 South State Street
 AA911  | UBCP   | United Bancorp, Inc. | 201 South 4th Street
(2 rows)
-- GOOD ( NO CHANGE )


select trg.company_id trg_ci, trg.ticker trg_tk, trg.company trg_c, trg.street trg_st, src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src, trg 
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company      = src.company
order by 3;

 trg_ci | trg_tk |             trg_c              |      trg_st      |           src_st            | src_tk |             src_c              | src_ci
--------+--------+--------------------------------+------------------+-----------------------------+--------+--------------------------------+--------
 AA29B  | EDD    | Morgan Stanley Emerging Market | 522 Fifth Avenue | 1221 Avenue of The Americas | MSF    | Morgan Stanley Emerging Market | ACE79
(1 row)


update trg
  set company_id =    src.company_id
        from src
      where trg.company_id != src.company_id and
            trg.ticker     != src.ticker and
            trg.company     = src.company;
-- 1 row affected

select trg.street src_st, trg.ticker src_tk, trg.company src_c, trg.company_id src_ci
        from trg
where company like  '%Morgan Stanley Emerging Market%';


---

select src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src
where company like  '%United Bancorp, Inc.%';

         src_st          | src_tk |        src_c         | src_ci
-------------------------+--------+----------------------+--------
 201 South 4th Street    | UBCP   | United Bancorp, Inc. | A0DD1
 2723 South State Street | UBMI   | United Bancorp, Inc. | AA911
(2 rows)

select src.street src_st, src.ticker src_tk, src.company src_c, src.company_id src_ci
        from src
where company like  '%Morgan Stanley Emerging Market%';

           src_st            | src_tk |             src_c              | src_ci
-----------------------------+--------+--------------------------------+--------
 522 FIFTH AVENUE            | MSD    | Morgan Stanley Emerging Market | AA29B
 1221 Avenue of The Americas | MSF    | Morgan Stanley Emerging Market | ACE79
 522 Fifth Avenue            | EDD    | Morgan Stanley Emerging Market | AE694
(3 rows)

------------- end figure out updates ----------------------------
------------------------------------------------------------------
------------------------------------------------------------------



-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

DROP INDEX IF EXISTS fe_data_store.si_finecon2_dateindex_dateindexp01lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2_dateindex_dateindexp01lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindex, dateindexp01lwd, company_id COLLATE pg_catalog."default");
--60 seconds

DROP INDEX IF EXISTS fe_data_store.si_finecon2_dateindex_dateindexp02lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2_dateindex_dateindexp02lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindex, dateindexp02lwd, company_id COLLATE pg_catalog."default");
--after the first, 6 seconds

DROP INDEX IF EXISTS fe_data_store.si_finecon2p01lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2p01lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindexp01lwd, company_id COLLATE pg_catalog."default");
--8 seconds

DROP INDEX IF EXISTS fe_data_store.si_finecon2p02lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2p02lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindexp02lwd, company_id COLLATE pg_catalog."default");
--after the first, 6 seconds


explain
select sq.*     -- Index Only Scan
  from ( select distinct dateindex from si_finecon2 ) curr
join lateral  -- for each dateindex,  load in 'current record + previous record', process, then loop to the next dateindex
(
  with clump as (
    -- pairs(sets) for performance reasons
    select dateindex, company_id, ticker, company, street, dateindexp01lwd from si_finecon2 now where now.dateindex = curr.dateindex  -- even a WITH statement CAN reference the OUTSIDE of LATERAL
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp01lwd) dateindexp01lwd from si_finecon2 now where now.dateindex = curr.dateindex )
  )
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  from
  clump now left outer join clump p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
) sq on (true) -- order by dateindex, now_company;
-- 3 minutes ( 1,293,947 ) rows - PER ITERATION, LEFT OUTER JOIN IS PRODUCING AND EMPTY RECORD RVALUE PER NON-EMPTY RECORD 


explain
select sq.*     -- Index Only Scan
  from ( select distinct dateindex from si_finecon2 ) curr
join lateral  -- for each dateindex,  load in 'current record + previous record', process, then loop to the next dateindex
(
  with clump as (
    -- pairs(sets) for performance reasons
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd from si_finecon2 now where now.dateindex = curr.dateindex  -- even a WITH statement CAN reference the OUTSIDE of LATERAL
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp01lwd) dateindexp01lwd from si_finecon2 now where now.dateindex = curr.dateindex )
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp02lwd) dateindexp02lwd from si_finecon2 now where now.dateindex = curr.dateindex )
  )
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  , p02lwd.dateindex           p02lwd_dateindex
  , p02lwd.company_id          p02lwd_company_id
  , p02lwd.ticker              p02lwd_ticker
  , p02lwd.company             p02lwd_company
  , p02lwd.street              p02lwd_street
  from
  clump now left outer join clump p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
            left outer join clump p02lwd on now.dateindexp02lwd  = p02lwd.dateindex and now.company_id = p02lwd.company_id
) sq on (true) -- order by dateindex, now_company;


-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

    explain
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = 17135  -- even a WITH statement CAN reference the OUTSIDE of LATERAL
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp01lwd) dateindexp01lwd from si_finecon2 now where now.dateindex = 17135 )
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp02lwd) dateindexp02lwd from si_finecon2 now where now.dateindex = 17135 )
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp03lwd) dateindexp03lwd from si_finecon2 now where now.dateindex = 17135 )
-- 3 seconds to go get the data
-- ALT
-- Index Only Scan using si_finecon2_dateindex_company_id_orig_key ( only used dateindex )
-- Index      Scan using si_finecon2_dateindex_company_id_orig_key ( only used dateindex )


------------------
------------------



-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

DROP INDEX IF EXISTS fe_data_store.si_finecon2_dateindex_dateindexp01lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2_dateindex_dateindexp01lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindex, dateindexp01lwd, company_id COLLATE pg_catalog."default");
--60 seconds

DROP INDEX IF EXISTS fe_data_store.si_finecon2_dateindex_dateindexp02lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2_dateindex_dateindexp02lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindex, dateindexp02lwd, company_id COLLATE pg_catalog."default");
--after the first, 6 seconds

DROP INDEX IF EXISTS fe_data_store.si_finecon2p01lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2p01lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindexp01lwd, company_id COLLATE pg_catalog."default");
--8 seconds

DROP INDEX IF EXISTS fe_data_store.si_finecon2p02lwd_company_id_idx;
CREATE UNIQUE INDEX si_finecon2p02lwd_company_id_idx
  ON fe_data_store.si_finecon2
  USING btree
  (dateindexp02lwd, company_id COLLATE pg_catalog."default");
--after the first, 6 seconds


-- begin falure: DUPS

-- DROP INDEX IF EXISTS fe_data_store.si_finecon2_dateindex_idx;
-- CREATE UNIQUE INDEX si_finecon2_dateindex_idx
--   ON fe_data_store.si_finecon2
--   USING btree
--   (dateindex);
--   
-- DROP INDEX IF EXISTS fe_data_store.si_finecon2_dateindexp01lwd_idx;
-- CREATE UNIQUE INDEX si_finecon2_dateindexp01lwd_idx
--   ON fe_data_store.si_finecon2
--   USING btree
--   (dateindexp01lwd);
  
-- DROP INDEX IF EXISTS fe_data_store.si_finecon2_dateindexp02lwd_idx;
-- CREATE UNIQUE INDEX si_finecon2_dateindexp02lwd_idx
--   ON fe_data_store.si_finecon2
--   USING btree
--   (dateindexp02lwd);
  
-- DROP INDEX IF EXISTS fe_data_store.si_finecon2_company_id_idx;
-- CREATE UNIQUE INDEX si_finecon2_company_id_idx
--   ON fe_data_store.si_finecon2
--   USING btree
--   (company_id);

--  end falure: DUPS


explain
select sq.*     -- Index Only Scan
  from ( select distinct dateindex from si_finecon2 ) curr
join lateral  -- for each dateindex,  load in 'current record + previous record', process, then loop to the next dateindex
(
  with clump as (
    -- pairs(sets) for performance reasons
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd from si_finecon2 now where now.dateindex = curr.dateindex  -- even a WITH statement CAN reference the OUTSIDE of LATERAL
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp01lwd) dateindexp01lwd from si_finecon2 now where now.dateindex = curr.dateindex )
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp02lwd) dateindexp02lwd from si_finecon2 now where now.dateindex = curr.dateindex )
  )
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  , p02lwd.dateindex           p02lwd_dateindex
  , p02lwd.company_id          p02lwd_company_id
  , p02lwd.ticker              p02lwd_ticker
  , p02lwd.company             p02lwd_company
  , p02lwd.street              p02lwd_street
  from
  clump now left outer join clump p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
            left outer join clump p02lwd on now.dateindexp02lwd  = p02lwd.dateindex and now.company_id = p02lwd.company_id
) sq on (true) -- order by dateindex, now_company;
-- Index Only Scan using finecon2_dateindex_dateindexp02lwd_company_id_idx
-- Index      Scan using finecon2_dateindex_dateindexp02lwd_company_id_idx
-- ALT
-- Index Only Scan using si_finecon2_dateindex_company_id_orig_key ( only used dateindex )
-- Index      Scan using si_finecon2_dateindex_company_id_orig_key ( only used dateindex )
-- 6 minutes

explain
select sq.*     -- Index Only Scan
  from ( select distinct dateindex from si_finecon2 ) curr
join lateral  -- for each dateindex,  load in 'current record + previous record', process, then loop to the next dateindex
(
  with clump as (
    -- pairs(sets) for performance reasons
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = curr.dateindex  -- even a WITH statement CAN reference the OUTSIDE of LATERAL
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp01lwd) dateindexp01lwd from si_finecon2 now where now.dateindex = curr.dateindex )
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp02lwd) dateindexp02lwd from si_finecon2 now where now.dateindex = curr.dateindex )
    union all
    select dateindex, company_id, ticker, company, street, dateindexp01lwd, dateindexp02lwd, dateindexp03lwd from si_finecon2 now where now.dateindex = ( select distinct on (now.dateindexp03lwd) dateindexp03lwd from si_finecon2 now where now.dateindex = curr.dateindex )
  )
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  , p02lwd.dateindex           p02lwd_dateindex
  , p02lwd.company_id          p02lwd_company_id
  , p02lwd.ticker              p02lwd_ticker
  , p02lwd.company             p02lwd_company
  , p02lwd.street              p02lwd_street
  , p03lwd.dateindex           p03lwd_dateindex
  , p03lwd.company_id          p03lwd_company_id
  , p03lwd.ticker              p03lwd_ticker
  , p03lwd.company             p03lwd_company
  , p03lwd.street              p03lwd_street
  from
  clump now left outer join clump p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
            left outer join clump p02lwd on now.dateindexp02lwd  = p02lwd.dateindex and now.company_id = p02lwd.company_id
            left outer join clump p03lwd on now.dateindexp03lwd  = p03lwd.dateindex and now.company_id = p03lwd.company_id 
) sq on (true) 
-- ALT
-- Index Only Scan using si_finecon2_dateindex_company_id_orig_key ( only used dateindex )
-- Index      Scan using si_finecon2_dateindex_company_id_orig_key ( only used dateindex )
-- 10:20 ( + 4 minutes per past data )

explain
select 1 
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id


explain
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
-- Index Scan
-- 2 minutes ( 4 workers ) 

explain
select 1 
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
                    left outer join si_finecon2 p02lwd on now.dateindexp02lwd  = p02lwd.dateindex and now.company_id = p02lwd.company_id

explain
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  , p02lwd.dateindex           p02lwd_dateindex
  , p02lwd.company_id          p02lwd_company_id
  , p02lwd.ticker              p02lwd_ticker
  , p02lwd.company             p02lwd_company
  , p02lwd.street              p02lwd_street
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
                    left outer join si_finecon2 p02lwd on now.dateindexp02lwd  = p02lwd.dateindex and now.company_id = p02lwd.company_id                    
-- Sequential Scan
-- No indexes used
-- 3 minutes ( 650,000 rows )

explain
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  , p02lwd.dateindex           p02lwd_dateindex
  , p02lwd.company_id          p02lwd_company_id
  , p02lwd.ticker              p02lwd_ticker
  , p02lwd.company             p02lwd_company
  , p02lwd.street              p02lwd_street
  , p03lwd.dateindex           p03lwd_dateindex
  , p03lwd.company_id          p03lwd_company_id
  , p03lwd.ticker              p03lwd_ticker
  , p03lwd.company             p03lwd_company
  , p03lwd.street              p03lwd_street
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
                    left outer join si_finecon2 p02lwd on now.dateindexp02lwd  = p02lwd.dateindex and now.company_id = p02lwd.company_id     
                    left outer join si_finecon2 p03lwd on now.dateindexp03lwd  = p03lwd.dateindex and now.company_id = p03lwd.company_id    
-- Sequential Scan        
-- No indexes used
-- 3:45 ( + 41 seconds per every level added ) ( 650,000 rows )


explain
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
-- 2 minutes ( 650,000 rows )
-- Index si_finecon2_dateindex_company_id_orig_key 

---------------------
---------------------

-- DROP INDEX IF EXISTS fe_data_store.si_finecon2_company_id_idx;
-- CREATE INDEX si_finecon2_company_id_idx
--   ON fe_data_store.si_finecon2
--   USING btree
--   (company_id);


explain
  select
    now.dateindex
  , now.dateindex              now_dateindex
  , now.company_id             now_company_id
  , now.ticker                 now_ticker
  , now.company                now_company
  , now.street                 now_street
  , p01lwd.dateindex           p01lwd_dateindex
  , p01lwd.company_id          p01lwd_company_id
  , p01lwd.ticker              p01lwd_ticker
  , p01lwd.company             p01lwd_company
  , p01lwd.street              p01lwd_street
  , p02lwd.dateindex           p02lwd_dateindex
  , p02lwd.company_id          p02lwd_company_id
  , p02lwd.ticker              p02lwd_ticker
  , p02lwd.company             p02lwd_company
  , p02lwd.street              p02lwd_street
  , p03lwd.dateindex           p03lwd_dateindex
  , p03lwd.company_id          p03lwd_company_id
  , p03lwd.ticker              p03lwd_ticker
  , p03lwd.company             p03lwd_company
  , p03lwd.street              p03lwd_street
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
                    left outer join si_finecon2 p02lwd on now.dateindexp02lwd  = p02lwd.dateindex and now.company_id = p02lwd.company_id     
                    left outer join si_finecon2 p03lwd on now.dateindexp03lwd  = p03lwd.dateindex and now.company_id = p03lwd.company_id    
-- Sequential Scan        
-- No indexes used
-- 3:45 ( + 41 seconds per every level added ) ( 650,000 rows )


-- inbounds, that have 'just' reported and have not reported in the 'previous month'



explain
  select
  now.dateindex
, now.company_id
, p01lwd.dateindex     p01lwd_dateindex
, p01lwd.company_id    p01lwd_company_id
, p01lwd.sales_q1      p01lwd_sales_q1
, p01lwd.netinc_q1     p01lwd_netinc_q1
, p01lwd.mktcap        p01lwd_mktcap
, p01lwd.price         p01lwd_price
, p01lwd.date_eq0      p01lwd_date_eq0
, p01lwd.perend_q1     p01lwd_perend_q1
, p01lwd.pertyp_q1     p01lwd_pertyp_q1
, p01lwd.perlen_q1     p01lwd_perlen_q1
  from
    si_finecon2 now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id
-- 1:38 seconds ( 650, 000 records )

-- IF I loop IN r as part of the monthly load ( GOOD IDEA )

-- watch out for blanks by left join

-- LEFT_OFF

-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

-- now.dateindex = 17135 and now.sp = '500' and

explain
select
    now.dateindex
  , now.company_id
--   , now.dateindex        now_dateindex
--   , now.company_id       now_company_id
--   , now.sales_q1         now_sales_q1
--   , now.netinc_q1        now_netinc_q1
--   , now.mktcap           now_mktcap
--   , now.price            now_price
  , now.netinc_q1/nullif(now.mktcap,0) now_netinc_q1_o_mktcap
  , now.date_eq0         now_date_eq0
  , now.perend_q1        now_perend_q1
  , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
         when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
         else                                now.perend_q1                           -- ... otherwise everything is null so just null
    end now_eff_date_eq0
--   , now.pertyp_q1        now_pertyp_q1
--   , now.perlen_q1        now_perlen_q1
--   , p01lwd.dateindex     p01lwd_dateindex
--   , p01lwd.company_id    p01lwd_company_id
--   , p01lwd.sales_q1      p01lwd_sales_q1
--   , p01lwd.netinc_q1     p01lwd_netinc_q1
--   , p01lwd.mktcap        p01lwd_mktcap
--   , p01lwd.price         p01lwd_price
  , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap
  , p01lwd.date_eq0      p01lwd_date_eq0
  , p01lwd.perend_q1     p01lwd_perend_q1
  , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
         when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
         else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
    end p01lwd_eff_date_eq0
--   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
--   , p01lwd.perlen_q1     p01lwd_perlen_q1
  from
    ( select * from si_finecon2 now  where  now.ticker = 'MSFT') now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    order by dateindex
-- Index Scan using si_finecon2_dateindex_company_id_key
-- 6608 rows
-- 0.7 seconds



-- requires mktcap, netinc_q1, sales_q1,  perend_q1, date_eq0
explain
select 
    sq1.dateindex
  , sq1.company_id
  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap 
  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
from (
  select
      now.dateindex
    , now.company_id
    , now.dateindex           now_dateindex
  --   , now.company_id       now_company_id
  --   , now.sales_q1         now_sales_q1
  --   , now.netinc_q1        now_netinc_q1
       , now.mktcap           now_mktcap
  --   , now.price            now_price
    , now.netinc_q1 /nullif(now.mktcap,0)    now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
    , now.sales_q1  /nullif(now.mktcap,0)    now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
    , now.netinc_q1 /nullif(now.sales_q1,0)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
  --  , now.date_eq0         now_date_eq0
  --  , now.perend_q1        now_perend_q1
    , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
           when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
           else                                now.perend_q1                           -- ... otherwise everything is null so just null
        end now_eff_date_eq0
  --   , now.pertyp_q1        now_pertyp_q1
  --   , now.perlen_q1        now_perlen_q1
  --   , p01lwd.dateindex     p01lwd_dateindex
  --   , p01lwd.company_id    p01lwd_company_id
  --   , p01lwd.sales_q1      p01lwd_sales_q1
  --   , p01lwd.netinc_q1     p01lwd_netinc_q1
  --   , p01lwd.mktcap        p01lwd_mktcap
  --   , p01lwd.price         p01lwd_price
  --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
  --   , p01lwd.date_eq0      p01lwd_date_eq0
  --   , p01lwd.perend_q1     p01lwd_perend_q1
    , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
           when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
           else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
        end p01lwd_eff_date_eq0
  --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
  --   , p01lwd.perlen_q1     p01lwd_perlen_q1
    from
      ( select * from si_finecon2 now  where now.ticker = 'MSFT') now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
) sq1
order by 1


-- requires mktcap, netinc_q1, sales_q1, perend_q1, date_eq0
explain
select 
    sq2.dateindex
  , sq2.company_id  
  , sq2.now_inbnd_stmtid_dateindex
  , sq2.now_inbnd_stmtstat_price
  , sq2.now_inbnd_stmtstat_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , max(sq2.now_inbnd_stmtid_dateindex) over(rows between 1 preceding and current row) 
from (
  select 
      sq1.dateindex
    , sq1.company_id
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price     -- math convenience
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap    -- math convenience
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
  from (
    select
        now.dateindex
      , now.company_id
      , now.dateindex           now_dateindex
    --   , now.company_id       now_company_id
    --   , now.sales_q1         now_sales_q1
    --   , now.netinc_q1        now_netinc_q1
         , now.mktcap           now_mktcap
         , now.price            now_price
      , now.netinc_q1 /nullif(now.mktcap,0)    now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
      , now.sales_q1  /nullif(now.mktcap,0)    now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
      , now.netinc_q1 /nullif(now.sales_q1,0)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
    --  , now.date_eq0         now_date_eq0
    --  , now.perend_q1        now_perend_q1
      , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
             when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
             else                                now.perend_q1                           -- ... otherwise everything is null so just null
          end now_eff_date_eq0
    --   , now.pertyp_q1        now_pertyp_q1
    --   , now.perlen_q1        now_perlen_q1
    --   , p01lwd.dateindex     p01lwd_dateindex
    --   , p01lwd.company_id    p01lwd_company_id
    --   , p01lwd.sales_q1      p01lwd_sales_q1
    --   , p01lwd.netinc_q1     p01lwd_netinc_q1
    --   , p01lwd.mktcap        p01lwd_mktcap
    --   , p01lwd.price         p01lwd_price
    --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
    --   , p01lwd.date_eq0      p01lwd_date_eq0
    --   , p01lwd.perend_q1     p01lwd_perend_q1
      , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
             when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
             else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
          end p01lwd_eff_date_eq0
    --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
    --   , p01lwd.perlen_q1     p01lwd_perlen_q1
      from
        ( select * from si_finecon2 now  where now.ticker = 'MSFT') now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
  ) sq1
) sq2
order by 1,2



-- requires mktcap, netinc_q1, sales_q1, perend_q1, date_eq0
explain
select 
    sq2.dateindex
  , sq2.company_id  
  , sq2.now_inbnd_stmtid_dateindex
  , sq2.now_inbnd_stmtstat_price
  , sq2.now_inbnd_stmtstat_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , max(sq2.now_inbnd_stmtid_dateindex) over(partition by company_id rows between 1 preceding and current row) last_inbnd_stmtid_dateindex
from (
  select 
      sq1.dateindex
    , sq1.company_id
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price     -- math convenience
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap    -- math convenience
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
  from (
    select
        now.dateindex
      , now.company_id
      , now.dateindex           now_dateindex
    --   , now.company_id       now_company_id
    --   , now.sales_q1         now_sales_q1
    --   , now.netinc_q1        now_netinc_q1
         , now.mktcap           now_mktcap
         , now.price            now_price
      , now.netinc_q1 /nullif(now.mktcap,0)    now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
      , now.sales_q1  /nullif(now.mktcap,0)    now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
      , now.netinc_q1 /nullif(now.sales_q1,0)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
    --  , now.date_eq0         now_date_eq0
    --  , now.perend_q1        now_perend_q1
      , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
             when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
             else                                now.perend_q1                           -- ... otherwise everything is null so just null
          end now_eff_date_eq0
    --   , now.pertyp_q1        now_pertyp_q1
    --   , now.perlen_q1        now_perlen_q1
    --   , p01lwd.dateindex     p01lwd_dateindex
    --   , p01lwd.company_id    p01lwd_company_id
    --   , p01lwd.sales_q1      p01lwd_sales_q1
    --   , p01lwd.netinc_q1     p01lwd_netinc_q1
    --   , p01lwd.mktcap        p01lwd_mktcap
    --   , p01lwd.price         p01lwd_price
    --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
    --   , p01lwd.date_eq0      p01lwd_date_eq0
    --   , p01lwd.perend_q1     p01lwd_perend_q1
      , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
             when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
             else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
          end p01lwd_eff_date_eq0
    --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
    --   , p01lwd.perlen_q1     p01lwd_perlen_q1
      from
        ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
  ) sq1
) sq2
order by 2,1



  --, coalesce(sq2.now_inbnd_stmtid_dateindex) over(partition by company_id rows between 1 preceding and current row) last_inbnd_stmtid_dateindex

-- requires mktcap, netinc_q1, sales_q1, perend_q1, date_eq0
explain
select 
    sq2.dateindex
  , sq2.company_id  
  , sq2.now_inbnd_stmtid_dateindex
  ----, sq2.now_inbnd_stmtstat_price
  ----, sq2.now_inbnd_stmtstat_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , max(sq2.now_inbnd_stmtid_dateindex) over (partition by company_id rows between 1 preceding and current row) last_inbnd_stmtid_dateindex
from (
  select 
      sq1.dateindex
    , sq1.company_id
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
    ----, case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price     -- math convenience
    ----, case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap    -- math convenience
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
  from (
    select
        now.dateindex
      , now.company_id
      , now.dateindex           now_dateindex
    --   , now.company_id       now_company_id
    --   , now.sales_q1         now_sales_q1
    --   , now.netinc_q1        now_netinc_q1
    ----  , now.mktcap           now_mktcap
    ----  , now.price            now_price
      , now.netinc_q1 /nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else 365 / 12 * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
      , now.sales_q1  /nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else 365 / 12 * now.perlen_q1 end / (365 / 4)  now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
      , now.netinc_q1 /nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else 365 / 12 * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
    --  , now.date_eq0         now_date_eq0
    --  , now.perend_q1        now_perend_q1
      , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
             when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
             else                                now.perend_q1                           -- ... otherwise everything is null so just null
          end now_eff_date_eq0
    --   , now.pertyp_q1        now_pertyp_q1
    --   , now.perlen_q1        now_perlen_q1
    --   , p01lwd.dateindex     p01lwd_dateindex
    --   , p01lwd.company_id    p01lwd_company_id
    --   , p01lwd.sales_q1      p01lwd_sales_q1
    --   , p01lwd.netinc_q1     p01lwd_netinc_q1
    --   , p01lwd.mktcap        p01lwd_mktcap
    --   , p01lwd.price         p01lwd_price
    --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
    --   , p01lwd.date_eq0      p01lwd_date_eq0
    --   , p01lwd.perend_q1     p01lwd_perend_q1
      , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
             when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
             else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
          end p01lwd_eff_date_eq0
    --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
    --   , p01lwd.perlen_q1     p01lwd_perlen_q1
      from
        ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
  ) sq1
) sq2
order by 2,1




--, coalesce(sq2.now_inbnd_stmtid_dateindex) over(partition by company_id rows between 1 preceding and current row) last_inbnd_stmtid_dateindex

-- requires mktcap, netinc_q1, sales_q1, perend_q1, date_eq0
explain
select 
    sq2.dateindex
  , sq2.company_id  
  , sq2.now_inbnd_stmtid_dateindex
  , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , max(sq2.now_inbnd_stmtid_dateindex) over (partition by company_id rows between 2 preceding and current row) last_inbnd_stmtid_dateindex
  , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
from (
  select 
      sq1.dateindex
    , sq1.company_id
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
  from (
    select
        now.dateindex
      , now.company_id
      , now.dateindex           now_dateindex
    --   , now.company_id       now_company_id
    --   , now.sales_q1         now_sales_q1
    --   , now.netinc_q1        now_netinc_q1  -- per 3 months
      , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
      , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
      , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
    --  , now.date_eq0         now_date_eq0
    --  , now.perend_q1        now_perend_q1
      , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
             when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
             else                                now.perend_q1                           -- ... otherwise everything is null so just null
          end now_eff_date_eq0
    --   , now.pertyp_q1        now_pertyp_q1
    --   , now.perlen_q1        now_perlen_q1
    --   , p01lwd.dateindex     p01lwd_dateindex
    --   , p01lwd.company_id    p01lwd_company_id
    --   , p01lwd.sales_q1      p01lwd_sales_q1
    --   , p01lwd.netinc_q1     p01lwd_netinc_q1
    --   , p01lwd.mktcap        p01lwd_mktcap
    --   , p01lwd.price         p01lwd_price
    --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
    --   , p01lwd.date_eq0      p01lwd_date_eq0
    --   , p01lwd.perend_q1     p01lwd_perend_q1
      , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
             when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
             else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
          end p01lwd_eff_date_eq0
    --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
    --   , p01lwd.perlen_q1     p01lwd_perlen_q1
      from
        ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
  ) sq1
) sq2
order by 2,1



--, coalesce(sq2.now_inbnd_stmtid_dateindex) over(partition by company_id rows between 1 preceding and current row) last_inbnd_stmtid_dateindex

-- requires mktcap, netinc_q1, sales_q1, perend_q1, date_eq0
explain

select 
    sq2.dateindex
  , sq2.company_id  
  , sq2.now_inbnd_stmtid_dateindex
  , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , max(sq2.now_inbnd_stmtid_dateindex) over (partition by sq2.company_id rows between 2 preceding and current row) last_inbnd_stmtid_dateindex
  , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
from (
  select 
      sq1.dateindex
    , sq1.company_id
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
    , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
  from (
    select
        now.dateindex
      , now.company_id
      , now.dateindex           now_dateindex
    --   , now.company_id       now_company_id
    --   , now.sales_q1         now_sales_q1
    --   , now.netinc_q1        now_netinc_q1  -- per 3 months
      , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
      , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
      , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
    --  , now.date_eq0         now_date_eq0
    --  , now.perend_q1        now_perend_q1
      , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
             when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
             else                                now.perend_q1                           -- ... otherwise everything is null so just null
          end now_eff_date_eq0
    --   , now.pertyp_q1        now_pertyp_q1
    --   , now.perlen_q1        now_perlen_q1
    --   , p01lwd.dateindex     p01lwd_dateindex
    --   , p01lwd.company_id    p01lwd_company_id
    --   , p01lwd.sales_q1      p01lwd_sales_q1
    --   , p01lwd.netinc_q1     p01lwd_netinc_q1
    --   , p01lwd.mktcap        p01lwd_mktcap
    --   , p01lwd.price         p01lwd_price
    --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
    --   , p01lwd.date_eq0      p01lwd_date_eq0
    --   , p01lwd.perend_q1     p01lwd_perend_q1
      , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
             when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
             else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
          end p01lwd_eff_date_eq0
    --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
    --   , p01lwd.perlen_q1     p01lwd_perlen_q1
      from
        ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
  ) sq1
) sq2
order by 2,1


-- VERY VERY GOOD
explain
select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , sq3.now_inbnd_stmtid_dateindex_partition
  , first_value(sq3.now_inbnd_stmtid_dateindex) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
        sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
          now.dateindex
        , now.company_id
        , now.dateindex           now_dateindex
      --   , now.company_id       now_company_id
      --   , now.sales_q1         now_sales_q1
      --   , now.netinc_q1        now_netinc_q1  -- per 3 months
        , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
        , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
        , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
        , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
               when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
               else                                now.perend_q1                           -- ... otherwise everything is null so just null
            end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
        , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
               when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
               else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
            end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
        from
          ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1
  ) sq2
) sq3
order by 2,1


-- VERY VERY GOOD
explain
select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , sq3.now_inbnd_stmtid_dateindex_partition
  , first_value(sq3.now_inbnd_stmtid_dateindex) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
        sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
          now.dateindex
        , now.company_id
        , now.dateindex           now_dateindex
      --   , now.company_id       now_company_id
      --   , now.sales_q1         now_sales_q1
      --   , now.netinc_q1        now_netinc_q1  -- per 3 months
        , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
        , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
        , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
        , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
               when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
               else                                now.perend_q1                           -- ... otherwise everything is null so just null
            end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
        , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
               when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
               else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
            end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
        from
          ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                              --  where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2
) sq3
order by 2,1



-- VERY VERY GOOD
explain
select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  , sq3.now_inbnd_stmtid_dateindex_partition
  , first_value(sq3.now_inbnd_stmtid_dateindex) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
        sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
          now.dateindex
        , now.company_id
        , now.dateindex           now_dateindex
      --   , now.company_id       now_company_id
      --   , now.sales_q1         now_sales_q1
      --   , now.netinc_q1        now_netinc_q1  -- per 3 months
        , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
        , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
        , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4)  now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
        , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
               when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
               else                                now.perend_q1                           -- ... otherwise everything is null so just null
            end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
        , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
               when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
               else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
            end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
        from
          ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                              --  where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2
) sq3
order by 2,1


-- LEFT_OFF datatype


select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  -- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
  , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
        sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
          now.dateindex
        , now.company_id
        , now.dateindex           now_dateindex
      --   , now.company_id       now_company_id
      --   , now.sales_q1         now_sales_q1
      --   , now.netinc_q1        now_netinc_q1  -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
        , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
        , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
        , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
        , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
               when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
               else                                now.perend_q1                           -- ... otherwise everything is null so just null
            end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
        , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
               when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
               else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
            end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
        from
          ( select * from si_finecon2 now  where now.ticker in ('AAPL','MSFT')) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                              --  where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2
) sq3
order by 2,1



-- query on the last 9 months
-- only upsert the last 2 months

-- where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347)

explain

select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  -- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
  , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
        sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
          now.dateindex
        , now.company_id
        , now.dateindex           now_dateindex
      --   , now.company_id       now_company_id
      --   , now.sales_q1         now_sales_q1
      --   , now.netinc_q1        now_netinc_q1  -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
        , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
        , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
        , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
        , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
               when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
               else                                now.perend_q1                           -- ... otherwise everything is null so just null
            end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
        , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
               when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
               else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
            end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
        from
          ( select * from si_finecon2 now  where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                              -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2                                -- where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347) -- first ONE minute AFTER 13 seconds WITH SORT
) sq3
order by 2,1
-- 80 months ( 7 minutes and 42 seconds )

-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'



select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  -- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
  , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
        sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
          now.dateindex
        , now.company_id
        , now.dateindex           now_dateindex
      --   , now.company_id       now_company_id
      --   , now.sales_q1         now_sales_q1
      --   , now.netinc_q1        now_netinc_q1  -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
        , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
        , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
        , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
        , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
               when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
               else                                now.perend_q1                           -- ... otherwise everything is null so just null
            end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
        , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
               when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
               else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
            end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
        from
          ( select * from si_finecon2 now  where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                              -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2                                -- where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347) -- first ONE minute AFTER 13 seconds WITH SORT
) sq3
order by 2,1




select * 
from ( -- sq4
  select 
      sq3.dateindex
    , sq3.company_id  
    , sq3.now_inbnd_stmtid_dateindex
    , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    -- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
    , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
    , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
    , first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
    , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
  from ( -- sq3
    select 
        sq2.dateindex
      , sq2.company_id  
      , sq2.now_inbnd_stmtid_dateindex
      , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
      , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
      , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
      , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
    from ( -- sq2
      select 
          sq1.dateindex
        , sq1.company_id
        , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
        , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
        , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
        , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
      from ( -- sq1
        select
            now.dateindex
          , now.company_id
          , now.dateindex           now_dateindex
        --   , now.company_id       now_company_id
        --   , now.sales_q1         now_sales_q1
        --   , now.netinc_q1        now_netinc_q1  -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
          , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
          , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
          , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
        --  , now.date_eq0         now_date_eq0
        --  , now.perend_q1        now_perend_q1
          , case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
                 when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
                 else                                now.perend_q1                           -- ... otherwise everything is null so just null
              end now_eff_date_eq0
        --   , now.pertyp_q1        now_pertyp_q1
        --   , now.perlen_q1        now_perlen_q1
        --   , p01lwd.dateindex     p01lwd_dateindex
        --   , p01lwd.company_id    p01lwd_company_id
        --   , p01lwd.sales_q1      p01lwd_sales_q1
        --   , p01lwd.netinc_q1     p01lwd_netinc_q1
        --   , p01lwd.mktcap        p01lwd_mktcap
        --   , p01lwd.price         p01lwd_price
        --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
        --   , p01lwd.date_eq0      p01lwd_date_eq0
        --   , p01lwd.perend_q1     p01lwd_perend_q1
          , case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
                 when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
                 else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
              end p01lwd_eff_date_eq0
        --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
        --   , p01lwd.perlen_q1     p01lwd_perlen_q1
          from
            ( select * from si_finecon2 now  where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
      ) sq1                              -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
    ) sq2                                -- where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347) -- first ONE minute AFTER 13 seconds WITH SORT
  ) sq3
  order by 2,1
) sq4 where sq4.dateindex = 17347



select now_inbnd_stmtid_dateindex from si_finecon2;

select dateindex, company_id, bby_1t from si_finecon2 order by dateindex, company_id;

select dateindex, company_id, bby_1t from si_finecon2 order by company_id, dateindex;

select dateindex, company_id, bby_1t from si_finecon2 order by bby_1t, dateindex;

select count(1) from  si_finecon2;

select distinct dateindex from si_finecon2;

select count(1) from si_finecon2 where dateindex = 17347;


select max(now_inbnd_stmtstat_netinc_q1_o_mktcap) from upsert_temp; --7
select max(now_inbnd_stmtstat_sales_q1_o_mktcap) from upsert_temp;  --7
select max(now_inbnd_stmtstat_netinc_q1_o_sales_q1) from upsert_temp; -7

select max(last_inbnd_stmtstat_netinc_q1_o_mktcap) from upsert_temp; -7
select max(last_inbnd_stmtstat_sales_q1_o_mktcap) from upsert_temp; -8
select max(last_inbnd_stmtstat_netinc_q1_o_sales_q1) from upsert_temp; -8

select 
  dateindex,
  company_id,
  ticker,
  company,
  now_inbnd_stmtid_dateindex,
  now_inbnd_stmtstat_netinc_q1_o_mktcap,
  now_inbnd_stmtstat_sales_q1_o_mktcap,
  now_inbnd_stmtstat_netinc_q1_o_sales_q1,
  last_inbnd_stmtid_dateindex,
  last_inbnd_stmtstat_netinc_q1_o_mktcap,
  last_inbnd_stmtstat_sales_q1_o_mktcap,
  last_inbnd_stmtstat_netinc_q1_o_sales_q1
from  si_finecon2 order by dateindex, company_id;



select sq4.* 
from ( -- sq4
select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  -- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
  , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
	sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
	  now.dateindex
	, now.company_id
	, now.dateindex           now_dateindex
      --   , now.company_id       now_company_id
      --   , now.sales_q1         now_sales_q1
      --   , now.netinc_q1        now_netinc_q1  -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
	, now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
	, now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
	, now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
	, case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
	       when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
	       else                                now.perend_q1                           -- ... otherwise everything is null so just null
	    end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
	, case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
	       when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
	       else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
	    end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
	from
	  ( select * from si_finecon2 now  where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                              -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2                                -- where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347) -- first ONE minute AFTER 13 seconds WITH SORT
) sq3
order by 2,1
) sq4 where sq4.dateindex = 17347


----------------------------------------------------------------------
----------------------- START SUN JUL 23 2017 ------------------------


-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

-- EXPERIMENT
 --MEMORY caching data
-- requires PostgreSQL restart
-- ONLY in postgresql.conf
-- postgres=# show shared_buffers;
--shared_buffers
------------------
---  8GB

-- EXPERIMENT
-- The setting can be changed within individual sessions, but only before the first use of temporary tables within the session; 
-- subsequent attempts to change the value will have no effect on that session.
-- ANY number I want ( no error )
-- EXPERIMENT 'sorting and temp tables
set temp_buffers to '14GB';


explain
select sq4.* 
from ( -- sq4
  select 
    sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  , sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  -- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
  , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
from ( -- sq3
  select 
      sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
    , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select 
	sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
	  now.dateindex
	, now.company_id
	, now.dateindex         now_dateindex
        , now.company_id       now_company_id
        , now.sales_q1         now_sales_q1
        , now.netinc_q1        now_netinc_q1  
        , now.mktcap           now_mktcap     
        , case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end now_perlen_days_q1
                                                  -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
	, now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
	, now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
	, now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
	, case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
	       when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
	       else                                now.perend_q1                           -- ... otherwise everything is null so just null
	    end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
	, case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
	       when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
	       else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
	    end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
	from
	  ( select * from si_finecon2 now  where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                              -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2                                -- where now.dateindex in (16982, 17011, 17044, 17074, 17105, 17135, 17165, 17197, 17225, 17256, 17284, 17317, 17347) -- first ONE minute AFTER 13 seconds WITH SORT
) sq3
order by 2,1
) sq4 where sq4.dateindex = 17347



------

explain
-- ratios not usefull 'right now' because of explosion
-- interesting compare (current) mktcap vs last_inbnd_stmtstat_mktcap
select sq4.* 
from ( -- sq4
  select 
    sq3.dateindex_company_id
  , sq3.dateindex
  , sq3.company_id  
  , sq3.now_inbnd_stmtid_dateindex
  , sq3.now_inbnd_stmtstat_sales_q1
  , sq3.now_inbnd_stmtstat_netinc_q1
  , sq3.now_inbnd_stmtstat_mktcap
  , sq3.now_inbnd_stmtstat_price
  --, sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
  --, sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
  --, sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
  ---- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
  , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
  , first_value(sq3.now_inbnd_stmtstat_sales_q1)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1
  , first_value(sq3.now_inbnd_stmtstat_netinc_q1)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1
  , first_value(sq3.now_inbnd_stmtstat_mktcap)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_mktcap
  , first_value(sq3.now_inbnd_stmtstat_mktcap)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_price
  --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
  --, first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
  --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
from ( -- sq3
  select 
      sq2.dateindex_company_id
    , sq2.dateindex
    , sq2.company_id  
    , sq2.now_inbnd_stmtid_dateindex
    , sq2.now_inbnd_stmtstat_sales_q1
    , sq2.now_inbnd_stmtstat_netinc_q1
    , sq2.now_inbnd_stmtstat_mktcap
    , sq2.now_inbnd_stmtstat_price
 -- , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
 -- , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
 -- , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
    , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
  from ( -- sq2
    select
        sq1.dateindex_company_id
      , sq1.dateindex
      , sq1.company_id
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1             else null end now_inbnd_stmtstat_sales_q1
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1            else null end now_inbnd_stmtstat_netinc_q1
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap 
      , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price 
  --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
  --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
  --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
    from ( -- sq1
      select
          now.dateindex_company_id
	, now.dateindex
	, now.company_id
	, now.dateindex        now_dateindex
        , now.company_id       now_company_id
        , now.sales_q1         now_sales_q1
        , now.netinc_q1        now_netinc_q1  
        , now.mktcap           now_mktcap     
        , now.price            now_price
        , case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end now_perlen_days_q1
                                                  -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
    --  , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
    --  , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
    --  , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
      --  , now.date_eq0         now_date_eq0
      --  , now.perend_q1        now_perend_q1
	, case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
	       when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
	       else                                now.perend_q1                           -- ... otherwise everything is null so just null
	    end now_eff_date_eq0
      --   , now.pertyp_q1        now_pertyp_q1
      --   , now.perlen_q1        now_perlen_q1
      --   , p01lwd.dateindex     p01lwd_dateindex
      --   , p01lwd.company_id    p01lwd_company_id
      --   , p01lwd.sales_q1      p01lwd_sales_q1
      --   , p01lwd.netinc_q1     p01lwd_netinc_q1
      --   , p01lwd.mktcap        p01lwd_mktcap
      --   , p01lwd.price         p01lwd_price
      --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
      --   , p01lwd.date_eq0      p01lwd_date_eq0
      --   , p01lwd.perend_q1     p01lwd_perend_q1
	, case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
	       when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
               else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
	    end p01lwd_eff_date_eq0
      --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
      --   , p01lwd.perlen_q1     p01lwd_perlen_q1
	from
	  ( select   date_eq0, perend_q1, perlen_q1, pertyp_q1, dateindex_company_id, dateindex, dateindexp01lwd, company_id, sales_q1, netinc_q1, mktcap, price
	             from si_finecon2 now  where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
    ) sq1                               -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
  ) sq2                                 -- where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982) -- first ONE minute AFTER 13 seconds WITH SORT
) sq3
order by 2,1
) sq4 where sq4.dateindex = 17347



-- TEST ALETERNATE ENDINGS

-- -- current

-- 	from
-- 	  ( select * from si_finecon2 now  where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
--     ) sq1                               -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
--   ) sq2                                 -- where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982) -- first ONE minute AFTER 13 seconds WITH SORT
-- ) sq3
-- order by 2,1
-- ) sq4 where sq4.dateindex = 17347

-- (pertyp_q1 = 'W' and now.perlen_q1 > 13) or (pertyp_q1 = 'M' and now.perlen_q1 > 3)

-- -- other possible

-- 	from
-- 	  ( select * from si_finecon2 now  where                  (pertyp_q1 = 'W' and now.perlen_q1 > 13) or (pertyp_q1 = 'M' and now.perlen_q1 > 3) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
--     ) sq1                               -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
--   ) sq2                                 -- where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982)) -- first ONE minute AFTER 13 seconds WITH SORT
-- ) sq3
-- order by 2,1
-- ) sq4 where sq4.dateindex = 17347
-- PER 17347 ( 198 records of 6500 firms ) -- 2:25 seconds WITH heavy DISK IO


ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN now_inbnd_stmtid_dateindex;

ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN now_inbnd_stmtstat_sales_q1;
ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN now_inbnd_stmtstat_netinc_q1;
ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN now_inbnd_stmtstat_mktcap;
ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN now_inbnd_stmtstat_price;

ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN last_inbnd_stmtid_dateindex;

ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN last_inbnd_stmtstat_sales_q1;
ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN last_inbnd_stmtstat_netinc_q1;
ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN last_inbnd_stmtstat_mktcap;
ALTER TABLE fe_data_store.si_finecon2 DROP COLUMN last_inbnd_stmtstat_price;

--------------
--------------

-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

---------------









select mktcap, sp, adr, industry_desc, sector_desc 
fe_data_store.si_finecon2

select distinct sp from fe_data_store.si_finecon2

select company, mktcap from fe_data_store.si_finecon2 where sp in ('500','400','600') and dateindex = 17347

company, mktcap 

dateindex in (17347, 17317, 17284, 17256)

-- information_schema

-- FORWARD: return per dollar is  if no split within last 30 days, then (mktcap/price) / lag(mktcap/price) -> bonus
--                                if no split since now (clump)    then (mktcap/price) / (last_inbnd_stmtstat_mktcap/last_inbnd_stmtstat_price) else 1 end * 100 -> current_from_last_bb_ratio
--                                case when split_date < last_inbnd_stmtid_dateindex  then (mktcap/price) / (last_inbnd_stmtstat_mktcap/last_inbnd_stmtstat_price) else 1 end * ( (dateindex - last_inbnd_stmtid_dateindex ) / 365.0) *100 -> current_from_last_bby_ratio
--                                case when split_date < last_inbnd_stmtid_dateindex  then (mktcap/nullif(price,0)) / (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) * ((dateindex - last_inbnd_stmtid_dateindex) / 365.0 ) else 1 end  * 100 -> pct_free_price_current_from_last_bb



-- (mktcap/nullif(price,0)) / (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) * ((dateindex - last_inbnd_stmtid_dateindex) / 365.0 )

-- update fe_data_store.si_finecon2 fe
-- set last_inbnd_stmtstat_price = ??.price

select fe.dateindex, fe.company_id,  fe.ticker, fe.last_inbnd_stmtid_dateindex, fe.now_inbnd_stmtid_dateindex, xx.price xx_price
from fe_data_store.si_finecon2 fe, fe_data_store.si_finecon2 xx where
fe.company_id                  = xx.company_id and 
fe.last_inbnd_stmtid_dateindex = xx.now_inbnd_stmtid_dateindex  and  xx.now_inbnd_stmtid_dateindex is not null
and fe.ticker in ('AAPL','MSFT')
order by fe.company_id, fe.dateindex; 
-- WORKS

-- THE FIXER
-- update fe_data_store.si_finecon2 fe
-- set last_inbnd_stmtstat_price =  xx.price
-- from                                fe_data_store.si_finecon2 xx where
-- fe.company_id                  = xx.company_id and 
-- fe.last_inbnd_stmtid_dateindex = xx.now_inbnd_stmtid_dateindex  and  xx.now_inbnd_stmtid_dateindex is not null
-- INPROGRESS -- [x] DONE?
-- 9:37:25
-- ran 3 minutes and 45 seconds


 

select dateindex, company_id
  , ticker, company, street
  , sp
  , sector_desc
  , industry_desc
  , dateindex
  , last_inbnd_stmtid_dateindex
  , now_inbnd_stmtid_dateindex
  , price
  , last_inbnd_stmtstat_price 
  , mktcap
  , split_date
  , dateindexp01lwd
  , split_fact
  -- trying
  , case when split_date > dateindexp01lwd then 1 else 0 end split_within_01m_from_01m  -- would need the month back history of splits ( ... geometric multiplication )
  , lag(split_date) over (partition by company_id order by dateindex ) lag_split_date_01m

  , last_inbnd_stmtstat_mktcap
  , last_inbnd_stmtstat_price
  , case when split_date < last_inbnd_stmtid_dateindex then 1 else 1 end  * 100.0  pct_free_price_current_from_last_bb
  -- POTENTIAL BUT I HAVE TO RELOAD -- LEFT_OFF : TEST AFTER re_load
  , mktcap
  , price
  , last_inbnd_stmtstat_mktcap
  , last_inbnd_stmtstat_price
  , mktcap / nullif(price,0) shares  -- inteesting one
  , last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0) last_inbnd_stmtstat_shares
  -- * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )
  , case when (split_date < last_inbnd_stmtid_dateindex) and ( dateindex != last_inbnd_stmtid_dateindex )
      then (( (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) /(mktcap/nullif(price,0)) - 1 ) * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )) + 1
      else 1 end  * 100.0  pct_free_price_current_cummul_from_last_bb  -- WORK-ISH - some sort of RISK FREE RATE benefit of HOLDING stock
                                                                       -- HARD to FIGURE out A useful FORM ( BUT REALLY need MO 2 MO - not cummulative )
  , mktcap/nullif(price,0) shares                                                                     
  ,   lag(mktcap/nullif(price,0)) over (partition by company_id order by dateindex) shares_m01                                                         
  ,   lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) / (mktcap/nullif(price,0))  rat_free_prc_01m                                                     
  , now_inbnd_stmtid_dateindex                                         
  , now_inbnd_stmtstat_sales_q1
  , now_inbnd_stmtstat_netinc_q1
  , now_inbnd_stmtstat_mktcap
  , now_inbnd_stmtstat_price
  from fe_data_store.si_finecon2 
    where 
          -- sp in ('500','400','600') and 
          -- dateindex in (17347, 17317, 17284, 17256) 
          ticker = 'AAPL' -- company_id = '05680'
          -- ticker in ('AAPL','MSFT')
  order by company_id, dateindex;

-- good

select
    dateindex 
  , sector_desc
  , count(now_inbnd_stmtstat_mktcap) count_now_inbnd_stmtstat_mktcap
  , sum(now_inbnd_stmtstat_mktcap)   sum_now_inbnd_stmtstat_mktcap
  , sum(now_inbnd_stmtstat_mktcap) / sum(last_inbnd_stmtstat_mktcap)  * 100 pct_sum_now_o_last_inbnd_stmtstat_mktcap
  
  from fe_data_store.si_finecon2 
    where sp in ('500','400','600') and 
          dateindex in (17347, 17317, 17284, 17256) and
          sector_desc = 'Energy'
group by dateindex, sector_desc
order by dateindex, sector_desc;
-- good



-- LEFT_OFF -put on the outside avg

select
    dateindex 
  , sector_desc
  , count(now_inbnd_stmtid_dateindex) count_now_inbnd_stmtstat_dateindex                                             -- weighted cnt by stmtid reported this month
  , sum(now_inbnd_stmtstat_mktcap)   sum_now_inbnd_stmtstat_mktcap                                                   
  , sum(now_inbnd_stmtstat_mktcap) / sum(last_inbnd_stmtstat_mktcap)  * 100 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- weighted pct by mktcap reported this month
  , sum(now_inbnd_stmtstat_netinc_q1 * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) / 
      sum(case when now_inbnd_stmtstat_mktcap is null then 0.0 else mktcap end) * 100 pct_sum_now_inbnd_stmtstat_netinc_q1_o_sum_mktcap -- now netinc_q1 over (if now) current mktcap ( investor return per dollar )
  , sum(now_inbnd_stmtstat_sales_q1  * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) / 
      sum(case when now_inbnd_stmtstat_mktcap is null then 0.0 else mktcap end) * 100 pct_sum_now_inbnd_stmtstat_sales_q1_o_sum_mktcap  -- now sales_q1  over (if now) current mktcap ( customer satisfaction )
  , sum(now_inbnd_stmtstat_netinc_q1 * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) /
    sum(now_inbnd_stmtstat_sales_q1  * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) * 100 pct_sum_now_inbnd_stmtstat_netinc_q1_o_sum_sales_q1  -- now netinc_q1 over now sales_q1 ( company (internal) efficiency ) 
  from fe_data_store.si_finecon2 
    where sp in (  '500'
                 , '400','600'
                ) 
                and 
          dateindex in (17347, 17317, 17284, 17256) and
          sector_desc = 'Energy'
group by dateindex, sector_desc
order by dateindex, sector_desc;
-- good

--       ABOVE ( LATER )
--       [ ] ( FIGURE OUT HOW TO ACCUMULATE: prchg_free_01m ( like netinc ) # WINDOWS FUNCTION LIKE?

--       ABOVE
--       [x] replace    case when pertyp_q1 = 'W' then 7.0 * perlen_q1  BY  perlen_q1 - perlen_q2
-- 90 day rate adjuster
-- * case when now.pertyp_q1 = 'W' then 7.0 * now.perlen_q1 else (365.0 / 12) * now.perlen_q1 end / (365.0 / 4)
-- could have (still) used perend_q2 ... perend_q1 - perendq2 = NUMBER of days



-- EXPERIMENT
set effective_cache_size to '14GB';

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'



          select sq4.* 
          from ( -- sq4
            select 
              sq3.dateindex_company_id
            , sq3.dateindex
            , sq3.company_id  
            , sq3.now_inbnd_stmtid_dateindex
            , sq3.now_inbnd_stmtstat_sales_q1
            , sq3.now_inbnd_stmtstat_netinc_q1
            , sq3.now_inbnd_stmtstat_mktcap
            , sq3.now_inbnd_stmtstat_price
            --, sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
            --, sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
            --, sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
            ---- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
            , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
            , first_value(sq3.now_inbnd_stmtstat_sales_q1)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1
            , first_value(sq3.now_inbnd_stmtstat_netinc_q1)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1
            , first_value(sq3.now_inbnd_stmtstat_mktcap)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_mktcap
            , first_value(sq3.now_inbnd_stmtstat_price)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_price
            --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
            --, first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
            --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
          from ( -- sq3
            select 
                sq2.dateindex_company_id
              , sq2.dateindex
              , sq2.company_id  
              , sq2.now_inbnd_stmtid_dateindex
              , sq2.now_inbnd_stmtstat_sales_q1
              , sq2.now_inbnd_stmtstat_netinc_q1
              , sq2.now_inbnd_stmtstat_mktcap
              , sq2.now_inbnd_stmtstat_price
           -- , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
           -- , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
           -- , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
              , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
            from ( -- sq2
              select
                  sq1.dateindex_company_id
                , sq1.dateindex
                , sq1.company_id
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1             else null end now_inbnd_stmtstat_sales_q1
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1            else null end now_inbnd_stmtstat_netinc_q1
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap 
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price 
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
              from ( -- sq1
                select
                    now.dateindex_company_id
          	, now.dateindex
          	, now.company_id
          	, now.dateindex        now_dateindex
                  , now.company_id       now_company_id
                  , now.sales_q1         now_sales_q1
                  , now.netinc_q1        now_netinc_q1  
                  , now.mktcap           now_mktcap     
                  , now.price            now_price
                  , case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end now_perlen_days_q1
              --  , case when perend_q1 - perend_q2 > 0 then perend_q1 - perend_q2 else (365 / 12)       end now_perlen_days_q1
                                                            -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
              --  , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
              --  , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
              --  , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
                --  , now.date_eq0         now_date_eq0
                --  , now.perend_q1        now_perend_q1
          	, case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
          	       when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
          	       else                                now.perend_q1                           -- ... otherwise everything is null so just null
          	    end now_eff_date_eq0
                --   , now.pertyp_q1        now_pertyp_q1
                --   , now.perlen_q1        now_perlen_q1
                --   , p01lwd.dateindex     p01lwd_dateindex
                --   , p01lwd.company_id    p01lwd_company_id
                --   , p01lwd.sales_q1      p01lwd_sales_q1
                --   , p01lwd.netinc_q1     p01lwd_netinc_q1
                --   , p01lwd.mktcap        p01lwd_mktcap
                --   , p01lwd.price         p01lwd_price
                --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
                --   , p01lwd.date_eq0      p01lwd_date_eq0
                --   , p01lwd.perend_q1     p01lwd_perend_q1
          	, case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
          	       when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
                         else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
          	    end p01lwd_eff_date_eq0
                --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
                --   , p01lwd.perlen_q1     p01lwd_perlen_q1
                	from
                    ( select   date_eq0, perend_q1, perlen_q1, pertyp_q1, dateindex_company_id, dateindex, dateindexp01lwd, company_id, sales_q1, netinc_q1, mktcap, price
                               from si_finecon2 now  where now.dateindex  in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
              ) sq1                               -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
            ) sq2                                 -- where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982) -- first ONE minute AFTER 13 seconds WITH SORT
          ) sq3
          order by 2,1
        ) sq4 where sq4.dateindex  = 17347


select src.street src_street, trg.street trg_street
from src, trg 
where
src.company_id != trg.company_id and
src.ticker      = trg.ticker  and sif_agrep(src.street, trg.street) = 't' limit 100




dbGetQuery(con,'
create or replace function sif_agrep(pattern text, x text)
returns boolean as $$
  ret <- agrepl(pattern = pattern, x = x, ignore.case = TRUE, fixed = FALSE) 
  return(ret)
$$ language plr;
;')

tryCatch({  }, error = function(e) { print(e); print(pattern); print(x); stop() })


dbGetQuery(con,'
create or replace function sif_agrep(pattern text, x text)
returns boolean as $$
  ret <- tryCatch({ agrepl(pattern = pattern, x = x, ignore.case = TRUE, fixed = FALSE) }, error = function(e) { print(e); print(pattern); print(x); stop() })
  return(ret)
$$ language plr;
;')


Error in agrepl(pattern = pattern, x = x, ignore.case = TRUE, fixed = FALSE) :
  invalid 'pattern' argument
<simpleError in agrepl(pattern = pattern, x = x, ignore.case = TRUE, fixed = FALSE): invalid 'pattern' argument>
NULL
[1] "45 Fremont Street"
Error in value[[3L]](cond) :


select src.company, src.street src_street, trg.street trg_street, trg.company
from src, trg 
where
src.company_id != trg.company_id and
src.ticker      = trg.ticker and trg.street = '45 Fremont Street' order by 1



--------------

-- EXPERIMENT
set effective_cache_size to '6144MB';  -- os + 'shared_buffers'

set search_path to fe_data_store;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

---------------



-- LEFT_OFF -put on the outside avg

select
    dateindex 
  , sector_desc
  , count(now_inbnd_stmtid_dateindex) count_now_inbnd_stmtstat_dateindex                                             -- weighted cnt by stmtid reported this month
  , sum(now_inbnd_stmtstat_mktcap)   sum_now_inbnd_stmtstat_mktcap                                                   
  , sum(now_inbnd_stmtstat_mktcap) / sum(last_inbnd_stmtstat_mktcap)  * 100 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- weighted pct by mktcap reported this month
  , sum(now_inbnd_stmtstat_netinc_q1 * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) / 
      sum(case when now_inbnd_stmtstat_mktcap is null then 0.0 else mktcap end) * 100 pct_sum_now_inbnd_stmtstat_netinc_q1_o_sum_mktcap -- now netinc_q1 over (if now) current mktcap ( investor return per dollar )
  , sum(now_inbnd_stmtstat_sales_q1  * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) / 
      sum(case when now_inbnd_stmtstat_mktcap is null then 0.0 else mktcap end) * 100 pct_sum_now_inbnd_stmtstat_sales_q1_o_sum_mktcap  -- now sales_q1  over (if now) current mktcap ( customer satisfaction )
  , sum(now_inbnd_stmtstat_netinc_q1 * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) /
    sum(now_inbnd_stmtstat_sales_q1  * case when pertyp_q1 = 'W' then 7.0 * perlen_q1 else (365.0 / 12) * perlen_q1 end / (365.0 / 4)) * 100 pct_sum_now_inbnd_stmtstat_netinc_q1_o_sum_sales_q1  -- now netinc_q1 over now sales_q1 ( company (internal) efficiency ) 
  from fe_data_store.si_finecon2 
    where sp in (  '500'
                 , '400','600'
                ) 
                and 
          dateindex in (17347, 17317, 17284, 17256) and
          sector_desc = 'Energy'
group by dateindex, sector_desc
order by dateindex, sector_desc;
-- good ( MOSTLY THIS ONE )



--       ABOVE ( LATER )
--       [ ] ( FIGURE OUT HOW TO ACCUMULATE: prchg_free_01m ( like netinc ) # WINDOWS FUNCTION LIKE?

--       ABOVE
--       [x] replace    case when pertyp_q1 = 'W' then 7.0 * perlen_q1  BY  perlen_q1 - perlen_q2
-- 90 day rate adjuster
-- * case when now.pertyp_q1 = 'W' then 7.0 * now.perlen_q1 else (365.0 / 12) * now.perlen_q1 end / (365.0 / 4)
-- could have (still) used perend_q2 ... perend_q1 - perendq2 = NUMBER of days




select dateindex, company_id, ticker
 , mktcap/nullif(price,0) shares
 , case when (split_date < last_inbnd_stmtid_dateindex) and ( dateindex != last_inbnd_stmtid_dateindex )
      then (( (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) /(mktcap/nullif(price,0)) - 1 ) * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )) + 1
      else 1 end  * 100.0  pct_free_price_current_cummul_from_last_bb
from fe_data_store.si_finecon2 
where ticker in ('AAPL')
order by company_id, dateindex;



-- bb? -- from last buy back

-- COPY FROM ABOVE

select dateindex, company_id
  , ticker, company, street
  , sp
  , sector_desc
  , industry_code
  , industry_desc
  , dateindex
  , last_inbnd_stmtid_dateindex
  , now_inbnd_stmtid_dateindex
  , price
  , last_inbnd_stmtstat_price 
  , mktcap
  , split_date
  , dateindexp01lwd
  , split_fact
  -- trying
  , case when split_date > dateindexp01lwd then 1 else 0 end split_within_01m_from_01m  -- would need the month back history of splits ( ... geometric multiplication )
  , lag(split_date) over (partition by company_id order by dateindex ) lag_split_date_01m

  , last_inbnd_stmtstat_mktcap
  , last_inbnd_stmtstat_price
  , case when split_date < last_inbnd_stmtid_dateindex then 1 else 1 end  * 100.0  pct_free_price_current_from_last_bb
  -- POTENTIAL BUT I HAVE TO RELOAD -- LEFT_OFF : TEST AFTER re_load
  , mktcap
  , price
  , last_inbnd_stmtstat_mktcap
  , last_inbnd_stmtstat_price
  , mktcap / nullif(price,0) shares  -- inteesting one
  , last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0) last_inbnd_stmtstat_shares
  -- * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )
  , case when (split_date < last_inbnd_stmtid_dateindex) and ( dateindex != last_inbnd_stmtid_dateindex )
      then (( (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) /(mktcap/nullif(price,0)) - 1 ) * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )) + 1
      else 1 end  * 100.0  pct_free_price_current_cummul_from_last_bb  -- WORK-ISH - some sort of RISK FREE RATE benefit of HOLDING stock
                                                                       -- HARD to FIGURE out A useful FORM ( BUT REALLY need MO 2 MO - not cummulative )
  , mktcap/nullif(price,0) shares                                                                     
  ,   lag(mktcap/nullif(price,0)) over (partition by company_id order by dateindex) shares_m01                                                         
  ,   lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) / (mktcap/nullif(price,0))  rat_free_prc_01m                                                     
  , now_inbnd_stmtid_dateindex                                         
  , now_inbnd_stmtstat_sales_q1
  , now_inbnd_stmtstat_netinc_q1
  , now_inbnd_stmtstat_mktcap
  , now_inbnd_stmtstat_price
  from fe_data_store.si_finecon2 
    where 
          -- sp in ('500','400','600') and 
          -- dateindex in (17347, 17317, 17284, 17256) 
          ticker = 'AAPL' -- company_id = '05680'
          -- ticker in ('AAPL','MSFT')
  order by company_id, dateindex;

-- good

-- ALREADY IN THE DATABASE

  bby_1t numeric(8,2),
  now_inbnd_stmtid_dateindex integer,
  now_inbnd_stmtstat_sales_q1 numeric(8,2),
  now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  now_inbnd_stmtstat_mktcap numeric(8,2),
  now_inbnd_stmtstat_price numeric(8,2),
  last_inbnd_stmtid_dateindex integer,
  last_inbnd_stmtstat_sales_q1 numeric(8,2),
  last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  last_inbnd_stmtstat_mktcap numeric(8,2),
  last_inbnd_stmtstat_price numeric(8,2),





-- bb? -- from last buy back

-- COPY FROM ABOVE

select dateindex, company_id, lag(price) over (partition by company_id order by dateindex) / lag(mktcap) over (partition by company_id order by dateindex)
                , lag((price/mktcap)) over (partition by company_id order by dateindex)
  from fe_data_store.si_finecon2 
    where 
          -- sp in ('500','400','600') and 
          -- dateindex in (17347, 17317, 17284, 17256) 
          ticker = 'AAPL' -- company_id = '05680'
          -- ticker in ('AAPL','MSFT')
  order by company_id, dateindex;


select dateindex, company_id
  , ticker, company, street
  , sp
  , sector_desc
  , industry_code
  , industry_desc
  , dateindex
  , last_inbnd_stmtid_dateindex
  , now_inbnd_stmtid_dateindex
  , price
  , last_inbnd_stmtstat_price 
  , mktcap
  , split_date
  , dateindexp01lwd
  , split_fact
  -- trying
  , case when split_date > dateindexp01lwd then 1 else 0 end split_within_01m_from_01m  -- would need the month back history of splits ( ... geometric multiplication )
  , lag(split_date) over (partition by company_id order by dateindex ) lag_split_date_01m

  , last_inbnd_stmtstat_mktcap
  , last_inbnd_stmtstat_price
  , case when split_date < last_inbnd_stmtid_dateindex then 1 else 1 end  * 100.0  pct_free_price_current_from_last_bb
  -- POTENTIAL BUT I HAVE TO RELOAD -- LEFT_OFF : TEST AFTER re_load
  , mktcap
  , price
  , last_inbnd_stmtstat_mktcap
  , last_inbnd_stmtstat_price
  , mktcap / nullif(price,0) shares  -- inteesting one
  , last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0) last_inbnd_stmtstat_shares
  -- * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )
  , case when (split_date < last_inbnd_stmtid_dateindex) then 1 else 0 end split_lt_last_inbnd_stmtid_dateindex
  -- NOT RIGHT
  -- , case when dateindex != last_inbnd_stmtid_dateindex   then 1 else 0 end di_neq_last_inbnd_stmtid_dateindex
  -- NOT RIGHT
  --, case when (split_date < last_inbnd_stmtid_dateindex) and ( dateindex != last_inbnd_stmtid_dateindex ) then 1 else 0 end free_rate_elegible
  -- NOT RIGHT
--   , case when (split_date < last_inbnd_stmtid_dateindex) and ( dateindex != last_inbnd_stmtid_dateindex )
--       then (( (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) /(mktcap/nullif(price,0)) - 1 ) * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )) + 1
--       else 1 end  * 100.0  pctchg_f_free_price_current_cummul_from_last_bb  -- WORK-ISH - some sort of RISK FREE RATE benefit of HOLDING stock
                                                                            -- HARD to FIGURE out A useful FORM ( BUT REALLY need MO 2 MO - not cummulative )
  -- NO ( DO NOT NEED TO MIX balance sheets and 'free money from stock buy backs )
--   , case when (split_date < last_inbnd_stmtid_dateindex) and ( split_date > dateindexp01lwd )
--       then (( (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) /(mktcap/nullif(price,0)) - 1 ) * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )) + 1
--       else 1 end  * 100.0  pctchg_f_free_price_current_cummul_from_last_bb  

    , dateindex
    , now_inbnd_stmtid_dateindex
    , last_inbnd_stmtid_dateindex
    , dateindex - last_inbnd_stmtid_dateindex id_less_lis_di
--   -- NO BECAUSE:  dateindex = now_inbnd_stmtid_dateindex = last_inbnd_stmtid_dateindex
--     , case when not (split_date > dateindexp01lwd)
--         then (( (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) /(mktcap/nullif(price,0)) - 1 ) * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )) + 1
--         else 1 end  * 100.0  pctchg_f_free_price_current_cummul_from_last_bb  



--   -- FIX LATER ( CORRECT % PART )
--   , (case when (split_date < last_inbnd_stmtid_dateindex) and ( dateindex != last_inbnd_stmtid_dateindex )
--       then (( (last_inbnd_stmtstat_mktcap/nullif(last_inbnd_stmtstat_price,0)) /(mktcap/nullif(price,0)) - 1 ) * ( 365.0 / (dateindex - last_inbnd_stmtid_dateindex)  )) + 1
--       else 1 end  * 100.0 - 100 ) * 12  pctchg_f_free_price_current_cummul_from_last_bb_ann  -- WORK-ISH - some sort of RISK FREE RATE benefit of HOLDING stock
                                                                                             -- HARD to FIGURE out A useful FORM ( BUT REALLY need MO 2 MO - not cummulative )
  , mktcap/nullif(price,0) shares                                                                     
  ,   lag(mktcap/nullif(price,0)) over (partition by company_id order by dateindex) shares_m01                                                         
  ,   lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) / (mktcap/nullif(price,0))  rat_free_prc_01m                                                     
  , now_inbnd_stmtid_dateindex                                         
  , now_inbnd_stmtstat_sales_q1
  , now_inbnd_stmtstat_netinc_q1
  , now_inbnd_stmtstat_mktcap
  , now_inbnd_stmtstat_price
  from fe_data_store.si_finecon2 
    where 
          -- sp in ('500','400','600') and 
          -- dateindex in (17347, 17317, 17284, 17256) 
          ticker = 'AAPL' -- company_id = '05680'
          -- ticker in ('AAPL','MSFT')
  order by company_id, dateindex;


-- WORKS ( KEEP ) -- BUT I DO NOT 
select dateindex, company_id, ticker, split_fact, dateindex, split_date, dateindexp01lwd
     , mktcap/nullif(price,0) shares
     , lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) lag_01m
     , case when not ( split_date > dateindexp01lwd )
         then  (  lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) - (mktcap/nullif(price,0)) )  / (mktcap/nullif(price,0))
         else 0.0 end * 100.0 * 12  pctchg_freeprice_01m_ann 
  from fe_data_store.si_finecon2 
    where 
          -- sp in ('500','400','600') and 
          -- dateindex in (17347, 17317, 17284, 17256) 
          ticker in ('MSFT','AAPL') -- company_id = '05680'
          -- ticker in ('AAPL','MSFT')
  order by company_id, dateindex;

     , case when not ( split_date > dateindexp01lwd )
         then  ( lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) - (mktcap/nullif(price,0)) ) / (mktcap/nullif(price,0))
         else 0.0 end * 100.0 * 12  pctchg_f_freeprice_01m_ann 


            , case when not ( split_date > dateindexp01lwd )
                then  ( lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) - (mktcap/nullif(price,0)) ) / (mktcap/nullif(price,0))
                else 0.0 end * 100.0 * 12  pctchg_f_freeprice_01m_ann 



          select sq4.* 
          from ( -- sq4
            select 
              sq3.dateindex_company_id
            , sq3.dateindex
            , sq3.company_id  
            , sq3.now_inbnd_stmtid_dateindex
            , sq3.now_inbnd_stmtstat_sales_q1
            , sq3.now_inbnd_stmtstat_netinc_q1
            , sq3.now_inbnd_stmtstat_mktcap
            , sq3.now_inbnd_stmtstat_price
            -- SMALL RATIO EXPLOSIONS MAKE THESE USELESS
            --, sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap
            --, sq3.now_inbnd_stmtstat_sales_q1_o_mktcap
            --, sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1
            ---- , sq3.now_inbnd_stmtid_dateindex_partition -- sql debugging utility
            , first_value(sq3.now_inbnd_stmtid_dateindex)              over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtid_dateindex
            , first_value(sq3.now_inbnd_stmtstat_sales_q1)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1
            , first_value(sq3.now_inbnd_stmtstat_netinc_q1)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1
            , first_value(sq3.now_inbnd_stmtstat_mktcap)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_mktcap
            , first_value(sq3.now_inbnd_stmtstat_price)      over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_price
            --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_mktcap)   over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_mktcap
            --, first_value(sq3.now_inbnd_stmtstat_sales_q1_o_mktcap)    over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_sales_q1_o_mktcap
            --, first_value(sq3.now_inbnd_stmtstat_netinc_q1_o_sales_q1) over (partition by sq3.company_id, sq3.now_inbnd_stmtid_dateindex_partition order by sq3.dateindex) last_inbnd_stmtstat_netinc_q1_o_sales_q1
            , sq3.pct_freeprice_ret_01m_ann
          from ( -- sq3
            select 
                sq2.dateindex_company_id
              , sq2.dateindex
              , sq2.company_id  
              , sq2.now_inbnd_stmtid_dateindex
              , sq2.now_inbnd_stmtstat_sales_q1
              , sq2.now_inbnd_stmtstat_netinc_q1
              , sq2.now_inbnd_stmtstat_mktcap
              , sq2.now_inbnd_stmtstat_price
           -- , sq2.now_inbnd_stmtstat_netinc_q1_o_mktcap
           -- , sq2.now_inbnd_stmtstat_sales_q1_o_mktcap
           -- , sq2.now_inbnd_stmtstat_netinc_q1_o_sales_q1
              , sum(case when sq2.now_inbnd_stmtid_dateindex is null then 0 else 1 end) over (partition by sq2.company_id order by sq2.dateindex) as now_inbnd_stmtid_dateindex_partition
              , sq2.pct_freeprice_ret_01m_ann
            from ( -- sq2
              select
                  sq1.dateindex_company_id
                , sq1.dateindex
                , sq1.company_id
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_dateindex            else null end now_inbnd_stmtid_dateindex 
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1             else null end now_inbnd_stmtstat_sales_q1
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1            else null end now_inbnd_stmtstat_netinc_q1
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_mktcap               else null end now_inbnd_stmtstat_mktcap 
                , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_price                else null end now_inbnd_stmtstat_price 
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_mktcap   else null end now_inbnd_stmtstat_netinc_q1_o_mktcap  
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_sales_q1_o_mktcap    else null end now_inbnd_stmtstat_sales_q1_o_mktcap  
            --  , case when sq1.now_eff_date_eq0 != sq1.p01lwd_eff_date_eq0 then sq1.now_netinc_q1_o_sales_q1 else null end now_inbnd_stmtstat_netinc_q1_o_sales_q1  
                , sq1.pct_freeprice_ret_01m_ann
              from ( -- sq1
                select
                    now.dateindex_company_id
          	, now.dateindex
          	, now.company_id
          	, now.dateindex        now_dateindex
                  , now.company_id       now_company_id
                  , now.sales_q1         now_sales_q1
                  , now.netinc_q1        now_netinc_q1  
                  , now.mktcap           now_mktcap     
                  , now.price            now_price
                  , case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end now_perlen_days_q1
              --  , case when perend_q1 - perend_q2 > 0 then perend_q1 - perend_q2 else (365 / 12)       end now_perlen_days_q1
                                                            -- per 3 months -- netinc/mktcap is '1% per quarter' -- UNITS of 100,000 ( one hundred thousand  )  -- typically  $1000/100_thousoand (per quarter)
              --  , now.netinc_q1 / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_mktcap    -- I care about the current mktcap ( investor return per dollar )
              --  , now.sales_q1  / nullif(now.mktcap,0)   * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_sales_q1_o_mktcap     -- I care about the current mktcap ( customer satisfaction )
              --  , now.netinc_q1 / nullif(now.sales_q1,0) * case when now.pertyp_q1 = 'W' then 7 * now.perlen_q1 else (365 / 12) * now.perlen_q1 end / (365 / 4) * 100000 now_netinc_q1_o_sales_q1  -- I care about the current        ( company (internal) efficiency )
                --  , now.date_eq0         now_date_eq0
                --  , now.perend_q1        now_perend_q1
          	, case when   now.date_eq0             >   now.perend_q1         then now.date_eq0 -- greater than and neither is null
          	       when   now.date_eq0 is not null and now.perend_q1 is null then now.date_eq0
          	       else                                now.perend_q1                           -- ... otherwise everything is null so just null
          	    end now_eff_date_eq0
                --   , now.pertyp_q1        now_pertyp_q1
                --   , now.perlen_q1        now_perlen_q1
                --   , p01lwd.dateindex     p01lwd_dateindex
                --   , p01lwd.company_id    p01lwd_company_id
                --   , p01lwd.sales_q1      p01lwd_sales_q1
                --   , p01lwd.netinc_q1     p01lwd_netinc_q1
                --   , p01lwd.mktcap        p01lwd_mktcap
                --   , p01lwd.price         p01lwd_price
                --   , p01lwd.netinc_q1/nullif(p01lwd.mktcap,0) p01lwd_netinc_q1_o_mktcap  
                --   , p01lwd.date_eq0      p01lwd_date_eq0
                --   , p01lwd.perend_q1     p01lwd_perend_q1
          	, case when   p01lwd.date_eq0             >   p01lwd.perend_q1         then p01lwd.date_eq0 -- greater than and neither is null
          	       when   p01lwd.date_eq0 is not null and p01lwd.perend_q1 is null then p01lwd.date_eq0
                         else                                   p01lwd.perend_q1                              -- ... otherwise everything is null so just null
          	    end p01lwd_eff_date_eq0
                --   , p01lwd.pertyp_q1     p01lwd_pertyp_q1
                --   , p01lwd.perlen_q1     p01lwd_perlen_q1
            , case when not ( now.split_date > now.dateindexp01lwd )
                  then  ( lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindex) - (now.mktcap/nullif(now.price,0)) ) / (now.mktcap/nullif(now.price,0)) --HERE(NEW)
                  else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return                                                                                                          --HERE(NEW)
                	from
                    ( select   date_eq0, perend_q1, perlen_q1, pertyp_q1, dateindex_company_id, dateindex, dateindexp01lwd, company_id, sales_q1, netinc_q1, mktcap, price, split_date  -- HERE(END)
                               from si_finecon2 now  where now.ticker in ('AAPL') ) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
              ) sq1                               -- where now.ticker in ('AAPL','MSFT') -- VERY easy to test
            ) sq2                                 -- where now.dateindex in (17347, 17317, 17284, 17256, 17225, 17197, 17165, 17135, 17105, 17074, 17044, 17011, 16982) -- first ONE minute AFTER 13 seconds WITH SORT
          ) sq3
          order by 2,1
        ) sq4 -- where sq4.dateindex ", display_where_condition, "



            select * from (
              select
              case when not ( now.split_date > now.dateindexp01lwd )
                   then  ( lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindex) - (now.mktcap/nullif(now.price,0)) ) / (now.mktcap/nullif(now.price,0)) 
                   else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return  
                	 from
                    ( select   date_eq0, perend_q1, perlen_q1, pertyp_q1, dateindex_company_id, dateindex, dateindexp01lwd, company_id, sales_q1, netinc_q1, mktcap, price, split_date
                               from si_finecon2 now  where now.dateindex  in (17378, 17347)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
              ) sq1

            select * from (
              select
                now.mktcap
              , now.price
              , (now.mktcap/nullif(now.price,0)) mkt_o_price
              , lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindex) old_mkt_o_price
              , ( lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindex) - (now.mktcap/nullif(now.price,0)) ) mkt_o_price_less_old_mkt_o_price
              , ( lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindex) - (now.mktcap/nullif(now.price,0)) ) / nullif((now.mktcap/nullif(now.price,0)),0) mkt_o_price_less_old_mkt_o_price_div_mkt_o_price
                	 from
                    ( select   date_eq0, perend_q1, perlen_q1, pertyp_q1, dateindex_company_id, dateindex, dateindexp01lwd, company_id, sales_q1, netinc_q1, mktcap, price, split_date
                               from si_finecon2 now  where now.dateindex  in (17378, 17347)) now left outer join si_finecon2 p01lwd on now.dateindexp01lwd  = p01lwd.dateindex and now.company_id = p01lwd.company_id 
                               where   (now.mktcap/nullif(now.price,0))  is null
              ) sq1


select mktcap/nullif(price,0) now
     , lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) lag_01m
     , case when not ( split_date > dateindexp01lwd )
         then  ( lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) - (mktcap/nullif(price,0)) ) / nullif((mktcap/nullif(price,0)),0)
         else 0.0 end * 100.0 * 12 pctchg_f_free_price_within_dateindex_from_01m_ann  
  from fe_data_store.si_finecon2 
    where 
          -- sp in ('500','400','600') and 
          -- dateindex in (17347, 17317, 17284, 17256) 
          ticker = 'AAPL' -- company_id = '05680'
          -- ticker in ('AAPL','MSFT')
  order by company_id, dateindex;


update fe_data_store.
set 


select dateindex
     , company_id
     , case when not ( split_date > dateindexp01lwd )
         then  ( lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) - (mktcap/nullif(price,0)) ) / nullif((mktcap/nullif(price,0)),0)
         else 0.0 end * 100.0 * 12 pctchg_f_free_price_within_dateindex_from_01m_ann  
  from fe_data_store.si_finecon2 
    where 
          -- sp in ('500','400','600') and 
          -- dateindex in (17347, 17317, 17284, 17256) 
          ticker = 'AAPL' -- company_id = '05680'
          -- ticker in ('AAPL','MSFT')
  order by company_id, dateindex;
  

update fe_data_store.si_finecon2 fe
  set pct_freeprice_ret_01m_ann =
  case when not ( split_date > dateindexp01lwd )
         then  ( lag((mktcap/nullif(price,0))) over (partition by company_id order by dateindex) - (mktcap/nullif(price,0)) ) / nullif((mktcap/nullif(price,0)),0)
         else 0.0 end * 100.0 * 12 pctchg_f_free_price_within_dateindex_from_01m_ann  
  from fe_data_store.si_finecon2 
    where  ticker = 'AAPL' -- company_id = '05680'

select 
  now.dateindex
, now.company_id
, now.pct_freeprice_ret_01m_ann pct_freeprice_ret_01m_ann_old
, case when not ( now.split_date > now.dateindexp01lwd )
       then  ( lag((now.mktcap/nullif(now.price,0))) over (partition by now.company_id order by now.dateindex) - (now.mktcap/nullif(now.price,0)) ) /  nullif((now.mktcap/nullif(now.price,0)),0)
       else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return
from  fe_data_store.si_finecon2 now where ticker = 'AAPL';


select 
  fe2.dateindex
, fe2.company_id
, fe2.pct_freeprice_ret_01m_ann pct_freeprice_ret_01m_ann_old
, case when not ( fe2.split_date > fe2.dateindexp01lwd )
       then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
       else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return
from  fe_data_store.si_finecon2 fe2 where fe2.ticker = 'AAPL';


update fe_data_store.si_finecon2 fe
set pct_freeprice_ret_01m_ann  =
case when not ( fe2.split_date > fe2.dateindexp01lwd )
       then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
       else 0.0 end * 100.0 * 12    
from  fe_data_store.si_finecon2 fe2 where 
      fe2.dateindex  = fe.dateindex and
      fe2.company_id = fe.company_id
  and fe2.ticker = 'AAPL';
  
-- ERROR: window functions are not allowed in UPDATE

select * from (
  select 
    fe2.dateindex
  , fe2.company_id
  , fe2.ticker
  , case when not ( fe2.split_date > fe2.dateindexp01lwd )
         then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
         else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return
  from  fe_data_store.si_finecon2 fe2 -- where fe2.ticker = 'AAPL'
) f2;


update fe_data_store.si_finecon2 fe
set pct_freeprice_ret_01m_ann  = fe3.pct_freeprice_ret_01m_ann
from (
  select 
    fe2.dateindex
  , fe2.company_id
  , fe2.ticker
  , case when not ( fe2.split_date > fe2.dateindexp01lwd )
         then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
         else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return
  from  fe_data_store.si_finecon2 fe2 
) fe3 where 
      fe3.dateindex  = fe.dateindex and
      fe3.company_id = fe.company_id
  and fe3.ticker = 'AAPL';


select * from (
  select 
    fe2.dateindex
  , fe2.company_id
  , fe2.ticker
  , pct_freeprice_ret_01m_ann
  , case when not ( fe2.split_date > fe2.dateindexp01lwd )
         then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
         else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann_query  -- a PAST return
  from  fe_data_store.si_finecon2 fe2 where fe2.ticker = 'AAPL'
) f2;
--GOOD

-- ALL
update fe_data_store.si_finecon2 fe
set pct_freeprice_ret_01m_ann  = fe3.pct_freeprice_ret_01m_ann
from (
  select 
    fe2.dateindex
  , fe2.company_id
  , fe2.ticker
  , case when not ( fe2.split_date > fe2.dateindexp01lwd )
         then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
         else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return
  from  fe_data_store.si_finecon2 fe2 
) fe3 where 
      fe3.dateindex  = fe.dateindex and
      fe3.company_id = fe.company_id
  -- and fe3.ticker = 'AAPL';

ERROR:  numeric field overflow
DETAIL:  A field with precision 9, scale 2 must round to an absolute value less than 10^7.
********** Error **********
ERROR: numeric field overflow
SQL state: 22003
Detail: A field with precision 9, scale 2 must round to an absolute value less than 10^7.



select * from (
  select max(
  case when not ( fe2.split_date > fe2.dateindexp01lwd )
         then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
         else 0.0 end * 100.0 * 12  
  ) pct_freeprice_ret_01m_ann_query_max  
  from  fe_data_store.si_finecon2 fe2 
) f2;
-- ERROR: aggregate function calls cannot contain window function calls

select max(fe3.pct_freeprice_ret_01m_ann_query) from (
  select
  case when not ( fe2.split_date > fe2.dateindexp01lwd )
         then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
         else 0.0 end * 100.0 * 12  
  pct_freeprice_ret_01m_ann_query 
  from  fe_data_store.si_finecon2 fe2 
) fe3;
--23380467.692307692307390060000
--10,2


-- ALL
update fe_data_store.si_finecon2 fe
set pct_freeprice_ret_01m_ann  = fe3.pct_freeprice_ret_01m_ann
from (
  select 
    fe2.dateindex
  , fe2.company_id
  , fe2.ticker
  , case when not ( fe2.split_date > fe2.dateindexp01lwd )
         then  ( lag((fe2.mktcap/nullif(fe2.price,0))) over (partition by fe2.company_id order by fe2.dateindex) - (fe2.mktcap/nullif(fe2.price,0)) ) /  nullif((fe2.mktcap/nullif(fe2.price,0)),0)
         else 0.0 end * 100.0 * 12  pct_freeprice_ret_01m_ann  -- a PAST return
  from  fe_data_store.si_finecon2 fe2 
) fe3 where 
      fe3.dateindex  = fe.dateindex and
      fe3.company_id = fe.company_id
  -- and fe3.ticker = 'AAPL';
-- 4 MINUTES FLAT
-- DONE


-- FIXED

create or replace function percentile_rank(integer[], numeric[], integer)
  returns setof integer as $$                          
  select
    trunc(least(case when 
                                                 query.idx is not null and         query.measure is not null then
    percent_rank() over (partition by query.idx, query.idx is not null,            query.measure is not null order by (
      query.measure      
    ) desc nulls last )  
    else null end,0.99)::numeric * $3::numeric,0)::int + 1 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;

-- TRY AGAIN

create or replace function percentile_rank(integer[], numeric[], integer)
  returns setof integer as $$                          
  select
    floor(least(case when 
                                                 query.idx is not null and         query.measure is not null then
    percent_rank() over (partition by query.idx, query.idx is not null,            query.measure is not null order by (
      query.measure      
    ) desc nulls last )  
    else null end,0.99)::numeric * $3::numeric)::int + 1 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;


-----------------

-- FIXED FOR GOOD

--           drop function percentile_rank(integer[], numeric[], integer);

create or replace function percentile_rank(integer[], numeric[], integer)
  returns setof integer as $$                          
  select
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;


-- drop function percentile_rank(anyarray, anyarray, int);

-- NO
create or replace function percentile_rank(anyarray, anyarray, int)
  returns setof anyelement as $$                          
  select
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;
--  No function matches the given name and argument types. You might need to add explicit type casts.


-- drop function percentile_rank(anyarray, numeric[], int)

create or replace function percentile_rank(anyarray, numeric[], int)
  returns setof anyelement as $$                          
  select
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;
-- THIS WORKS KEPT


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;

select sq.* from (
select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query ) sq where percentile_rank > 100
;

--------------
--------------

-- LONG LIVE
             drop function percentile_rank(anyarray, numeric[], int);
create or replace function percentile_rank(anyarray, numeric[], int)
  returns setof anyelement as $$                          
  select
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1
      from sipro_data_store.si_finecon ) query;

select dateindex, percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100)
  from ( select dateindex, 
    netinc_q1
      from sipro_data_store.si_finecon ) query;

select sq.* from (
select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query ) sq where percentile_rank > 100
      

-- drop function test_pctrnk()
create  function test_pctrnk()
returns setof record as $$
select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query
$$ language sql;

select t.percentile_rank from test_pctrnk() t(percentile_rank int);
-- WORKS

select t.s from test_pctrnk() t(s int);
-- NOT WORK


-- drop function test_pctrnk2()
create  function test_pctrnk2(dateindex int, )
returns setof record as $$
select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query
$$ language sql;



-- drop function percentile_rank(anyarray, numeric[], int)
create or replace function percentile_rank(anyarray, numeric[], int)
  returns table(idx int, measure numeric, percentile_rank_score int) as $$                          
  select
    idx, measure,
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100)
  from ( select dateindex, 
    netinc_q1
      from sipro_data_store.si_finecon ) query;

-- drop function percentile_rank(anyarray, numeric[], int)
create or replace function percentile_rank(anyarray, numeric[], int)
  returns setof record as $$                          
  select
    idx, measure,
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;



percentile_rank_type

create or replace function percentile_rank(anyarray, numeric[], int)
  returns setof record as $$                          
  select
    idx, measure,
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;







select (sq.*).* from (
select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100)
  from ( select dateindex, 
    netinc_q1
      from sipro_data_store.si_finecon ) query



select (sq.*).* from (
select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100)
  from ( select dateindex, 
    netinc_q1
      from sipro_data_store.si_finecon ) query
) sq;
-- NO



select (percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100)).percentile_rank_score
  from ( select dateindex, 
    netinc_q1
      from sipro_data_store.si_finecon ) 



select qu2.dateindex, qu2.company_id, qu2.rank from ( 
  select (percentile_rank2(array_agg(qu.dateindex), array_agg(qu.company_id), array_agg(qu.statistic), 2)).* rank from bar qu) qu2(dateindex, company_id,




drop type if exists percentile_rank_type;
create type percentile_rank_type as (idx integer, rnk integer, cmp_id text);

  

drop function if exists percentile_rank(integer[], numeric[], integer, text[]);

             drop function percentile_rank(integer[], numeric[], integer, text[], out idx integer, out rnk integer, out cmp_id text)
create or replace function percentile_rank(integer[], numeric[], integer, text[], out idx integer, out rnk integer, out cmp_id text)
  returns setof record as $$                          
  select
    query.idx,
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score,
    query.cmp_id
  from unnest($1,$2,$4) query(idx,measure,cmp_id) 
$$ language sql strict immutable;



select row(dateindex, 
    netinc_q1::numeric(15,2), company_id_unq)
      from sipro_data_store.si_finecon ;


select (percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100, array_agg(company_id_unq))).*
  from ( select dateindex, 
    netinc_q1::numeric(15,2), company_id_unq
      from sipro_data_store.si_finecon ) query;






            drop  function less_simple_percentile_rank(numeric[], integer, text[]);
create or replace function less_simple_percentile_rank(numeric[], integer, text[])
  returns table(measure numeric, percentile_rank_score int, id text) as $$                  
  select 
    query.measure,
    case when 
      query.measure
    is not null then
    trunc(least(percent_rank() over (order by (
      query.measure     
    ) desc ), 0.9999)::numeric * $2::numeric,0)::int + 1
    else null end 
    percentile_rank_score,
    id
  from unnest($1,$3) query(measure,id)
$$ language sql strict immutable;

select (less_simple_percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100, array_agg(company_id_unq))).*
  from ( select dateindex, 
    netinc_q1::numeric(15,2), company_id_unq
      from sipro_data_store.si_finecon ) query;



select sp.dateindex, (sq.spr).* from ( select distinct fc.dateindex from sipro_data_store.si_finecon fc where fc.ticker_unq in ('HP','MSFT','AAPL') order by fc.dateindex ) sp
join lateral (
	select less_simple_percentile_rank(array_agg(query.netinc_q1::numeric(15,2)), 100, array_agg(ticker_unq)) spr
	  from ( select fcl.dateindex,  fcl.netinc_q1::numeric(15,2), fcl.company_id_unq, fcl.ticker_unq
	            from sipro_data_store.si_finecon fcl where fcl.ticker_unq in ('HP','MSFT','AAPL') and fcl.dateindex = sp.dateindex
	order by dateindex) query 
) sq on true;



select sp.dateindex, (sq.spr).* from ( select distinct fc.dateindex from sipro_data_store.si_finecon fc where fc.ticker_unq in ('HP','MSFT','AAPL') order by fc.dateindex ) sp
join lateral (
	select less_simple_percentile_rank(array_agg(query.netinc_q1::numeric(15,2)), 100, array_agg(ticker_unq)) spr
	  from ( select fcl.dateindex,  fcl.netinc_q1::numeric(15,2), fcl.company_id_unq, fcl.ticker_unq
	            from sipro_data_store.si_finecon fcl where  fcl.dateindex = sp.dateindex
	order by dateindex) query 
) sq on true;


----------
----------

             drop function percentile_rank(anyarray, numeric[], int);
create or replace function percentile_rank(anyarray, numeric[], int)
  returns setof anyelement as $$                          
  select
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;


----------
----------

             drop function percentile_rank(numeric[], numeric[], int, out idx int, out measure int, out percentile_rank_score int);
create or replace function percentile_rank(numeric[], numeric[], int, out idx int, out measure int, out percentile_rank_score int)
  returns setof record as $$                          
  select query.idx, query.measure, 
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;


select percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100) 
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;

----------
----------

drop function percentile_rank(anyarray, numeric[], int)

             drop function percentile_rank(anyarray, numeric[], int, out idx int, out measure numeric, out percentile_rank_score int);
create or replace function percentile_rank(anyarray, numeric[], int, out idx int, out measure numeric, out percentile_rank_score int)
  returns setof record as $$                          
  select query.idx, query.measure, 
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;

select (percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100)).*
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;

-- WORKS: KEEP

--------------
--------------

-- THESE WORK

-- KEEP
-- variant
--              drop function percentile_rank(anyarray, numeric[], int, out idx int, out measure numeric, out percentile_rank_score int);
-- create or replace function percentile_rank(anyarray, numeric[], int, out idx int, out measure numeric, out percentile_rank_score int)
--   returns setof record as $$  

             drop function percentile_rank(anyarray, numeric[], int);
create or replace function percentile_rank(anyarray, numeric[], int)
  returns table(idx int, measure numeric, percentile_rank_score int) as $$                          
  select query.idx, query.measure, 
    case when 
                                                  query.idx is not null and          query.measure is not null then
    floor(least(percent_rank() over (partition by query.idx, query.idx is not null,  query.measure is not null order by (
      query.measure      
    ) desc), 0.9999)::numeric * $3::numeric)::int + 1
    else null end 
    percentile_rank_score      
  from unnest($1,$2) query(idx,measure) 
$$ language sql strict immutable;

select (percentile_rank(array_agg(query.dateindex), array_agg(query.netinc_q1::numeric(15,2)), 100)).*
  from ( select dateindex, 
    netinc_q1::numeric(15,2)
      from sipro_data_store.si_finecon ) query;


----------------
----------------

-- GET INTO AN XTS OBJECT --

-- , company_id, ticker, company, sp, industry_desc 

select dateindex, dateindexeom, company_id, ticker, mktcap, sp, sector_desc, industry_desc,
  now_inbnd_stmtid_dateindex,
  now_inbnd_stmtstat_sales_q1,
  now_inbnd_stmtstat_netinc_q1,
  now_inbnd_stmtstat_mktcap,
  now_inbnd_stmtstat_price,
  pct_freeprice_ret_01m_ann 
from fe_data_store.si_finecon2
  where ticker = 'AAPL';


from fe_data_store.si_finecon2 limit 1;

---------------------


-- COPY FROM ABOVE "
select
    dateindex 
  , sector_desc
  , count(now_inbnd_stmtid_dateindex) count_now_inbnd_stmtstat_dateindex                                             -- weighted cnt by stmtid reported this month
  , sum(now_inbnd_stmtstat_mktcap)   sum_now_inbnd_stmtstat_mktcap                                                   
  , sum(now_inbnd_stmtstat_mktcap) / sum(last_inbnd_stmtstat_mktcap)  * 100 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- weighted pct by mktcap reported this month
  from fe_data_store.si_finecon2 
    where sp in (  '500'
                 , '400','600'
                ) 
                and 
          dateindex in (17347, 17317, 17284, 17256) and
          sector_desc = 'Energy'
group by dateindex, sector_desc
order by dateindex, sector_desc;



  select distinct(industry_desc) from fe_data_store.si_finecon2 where industry_desc = 'Gold & Silver' order by 1;

  select distinct(sector_desc) from fe_data_store.si_finecon2 where sector_desc = 'Basic Materials';

 sum(price) / nullif(sum(mktcap))  approx_price_o_mktcap_x100





-- BASED ON "-- good ( MOSTLY THIS ONE )"


-- will not use an index
-- INDEX si_finecon2_finecon_jamesos_partial_ignore_mktcap_idx;
--
--
--
-- all  82 - "GroupAggregate  (cost=207750.95..1362174.34 rows=3980930 width=520)" -- 72 seconds ( with gld 144 seconds )  -- 270 seconds ( also add __materials_ )
-- no 'where clause'
-- 
-- just  2 - "Sort  (cost=258234.06..261118.31 rows=1153700 width=520)" -- 1.6 seconds
-- where dateindex in (17347, 17317)
--
-- probably a waste of time(since subset of data that I already have: sector, industry) doing: gld  basicmat 
--
explain

-- create table aggregates as
select 
    case when sq2.dateindex_fct not in ('empty','all') then                                                                 sq2.dateindex_fct::int                                                                                       else null end dateindex
  , case when sq2.dateindex_fct not in ('empty','all') then (extract( 'epoch' from ( select date_trunc('month', to_timestamp(sq2.dateindex_fct::int * 3600 *24 )::date) +  interval '1 month' - interval '1 day' )) / ( 3600* 24 ))::int else null end dateindexeom
  , sq2.dateindex_fct
  , sq2.is_sp_fct
  , sq2.is_sp500_fct
  , sq2.sector_desc_fct
  , sq2.is_materials_fct
  , sq2.industry_desc_fct
  , sq2.is_gld_fct
  , sq2.approx_price_o_mktcap_x100
  , sq2.count_now_inbnd_stmtstat_dateindex
  , sq2.pct_sum_now_o_last_inbnd_stmtstat_mktcap
  , sq2.rat_now_netinc_q1_o_mktcap_x_100
  , sq2.rat_now_sales_q1_o_mktcap_x_100
  , sq2.rat_now_netinc_q1_o_sales_x_100
  , sq2.unweighted_pct_freeprice_ret_01m_ann
  , sq2.sum_mktcap
from ( -- sq2
  select 
      coalesce(sq1.dateindex_fct,        'all') dateindex_fct
    , coalesce(sq1.is_sp_fct,            'all') is_sp_fct
    , coalesce(sq1.is_sp500_fct,         'all') is_sp500_fct
    , coalesce(sq1.sector_desc_fct,      'all') sector_desc_fct
    , coalesce(sq1.is_materials_fct,     'all') is_materials_fct
    , coalesce(sq1.industry_desc_fct,    'all') industry_desc_fct
    , coalesce(sq1.is_gld_fct,           'all') is_gld_fct
    , sum(sq1.price)                        / nullif(sum(sq1.mktcap), 0)                    * 100.00 approx_price_o_mktcap_x100  -- also EXTERIOR DATA: e.g. just the  S&P value would have been just fine
    , count(sq1.now_inbnd_stmtid_dateindex)::numeric   count_now_inbnd_stmtstat_dateindex                                                         -- cnt           reported this month
    , sum(sq1.now_inbnd_stmtstat_mktcap)    /   nullif(sum(sq1.last_inbnd_stmtstat_mktcap), 0)  * 100.00 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- pct by mktcap reported this month
    , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00 rat_now_netinc_q1_o_mktcap_x_100
    , sum(sq1.now_inbnd_stmtstat_sales_q1)  /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_sales_q1_o_mktcap_x_100
    , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_sales_q1), 0) * 100.00 rat_now_netinc_q1_o_sales_x_100
    , avg(sq1.pct_freeprice_ret_01m_ann)                                                                 unweighted_pct_freeprice_ret_01m_ann
    , sum(sq1.mktcap)                                                                                    sum_mktcap
  from ( -- sq1
    select
        coalesce(dateindex::text, 'empty')  dateindex_fct
      , case when sp in ('500', '400','600') then 'issp'    else 'isnotsp'    end is_sp_fct
      , case when sp   = '500'               then 'issp500' else 'isnotsp500' end is_sp500_fct
      , coalesce(sector_desc,   'empty')      sector_desc_fct
      , case when sector_desc   = 'Basic Materials'               then 'isbasicmtrls' else 'notisbasicmtrls' end is_materials_fct
      , coalesce(industry_desc, 'empty')      industry_desc_fct
      , case when industry_desc   = 'Gold & Silver'               then 'isgld'      else 'isnotgld'        end is_gld_fct
      , mktcap
      , case when price < 0.10 or price > 1000.00 then null else price end price
      , now_inbnd_stmtid_dateindex
      , last_inbnd_stmtid_dateindex
      , now_inbnd_stmtstat_mktcap
      , last_inbnd_stmtstat_mktcap
      , now_inbnd_stmtstat_netinc_q1
      , last_inbnd_stmtstat_netinc_q1
      , now_inbnd_stmtstat_sales_q1
      , last_inbnd_stmtstat_sales_q1
      , case when pct_freeprice_ret_01m_ann > 100.00 or pct_freeprice_ret_01m_ann < -100.00 then null else pct_freeprice_ret_01m_ann end pct_freeprice_ret_01m_ann
    from fe_data_store.si_finecon2 where adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
    -- AND mktcap > 200.0
    -- and dateindex in (17347, 17317) 
    and dateindex = 17347  -- EVERYTHING HERE 2.7 SECONDS
    ) sq1
  group by cube(dateindex_fct, is_sp_fct, is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct)
) sq2 where sq2.dateindex_fct not in ('empty','all')  -- (NO COST DIFFERENCE): SPEED INCREASE by LESS DATA MANIP/RETURNED
order by dateindex_fct, is_sp_fct , is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct;



-- REMEMBER -- [ ] run recession_sight
--          -- [X] turn off postgre-sql statement logging ( high disk io )

select distinct dateindex from fe_data_store.inbnd_stmtstats_aggregates; 

NULL
17347
17317

select * from fe_data_store.inbnd_stmtstats_aggregates where dateindex is null;

select distinct dateindex from fe_data_store.inbnd_stmtstats_aggregates; 

select count(1) from fe_data_store.inbnd_stmtstats_aggregates;

39648



      select 
          case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then                                                                  sq2.dateindex_fct::int                                                                                      else null end dateindex
        , case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then (extract( 'epoch' from ( select date_trunc('month', to_timestamp(sq2.dateindex_fct::int * 3600 *24 )::date) +  interval '1 month' - interval '1 day' )) / ( 3600* 24 ))::int else null end dateindexeom
        , sq2.dateindex_fct
        , sq2.is_sp_fct
        , sq2.is_sp500_fct
        , sq2.sector_desc_fct
        , sq2.is_materials_fct
        , sq2.industry_desc_fct
        , sq2.is_gld_fct
        , sq2.approx_price_o_mktcap_x100
        , sq2.count_now_inbnd_stmtstat_dateindex
        , sq2.pct_sum_now_o_last_inbnd_stmtstat_mktcap
        , sq2.rat_now_netinc_q1_o_mktcap_x_100
        , sq2.rat_now_sales_q1_o_mktcap_x_100
        , sq2.rat_now_netinc_q1_o_sales_x_100
        , sq2.unweighted_pct_freeprice_ret_01m_ann
        , sq2.sum_mktcap
      from ( -- sq2
        select 
            coalesce(sq1.dateindex_fct,        'alldateindex') dateindex_fct
          , coalesce(sq1.is_sp_fct,            'allsp') is_sp_fct
          , coalesce(sq1.is_sp500_fct,         'allsp500') is_sp500_fct
          , coalesce(sq1.sector_desc_fct,      'allsector_desc') sector_desc_fct
          , coalesce(sq1.is_materials_fct,     'allmaterials') is_materials_fct
          , coalesce(sq1.industry_desc_fct,    'allindustry') industry_desc_fct
          , coalesce(sq1.is_gld_fct,           'allgld') is_gld_fct
          , sum(sq1.price)                        / nullif(sum(sq1.mktcap), 0)                    * 100.00 approx_price_o_mktcap_x100  -- also EXTERIOR DATA: e.g. just the  S&P value would have been just fine
          , count(sq1.now_inbnd_stmtid_dateindex)::numeric   count_now_inbnd_stmtstat_dateindex                                                         -- cnt           reported this month
          , sum(sq1.now_inbnd_stmtstat_mktcap)    /   nullif(sum(sq1.last_inbnd_stmtstat_mktcap), 0)  * 100.00 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- pct by mktcap reported this month
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00 rat_now_netinc_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_sales_q1)  /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_sales_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_sales_q1), 0) * 100.00 rat_now_netinc_q1_o_sales_x_100
          , avg(sq1.pct_freeprice_ret_01m_ann)                                                                 unweighted_pct_freeprice_ret_01m_ann
          , sum(sq1.mktcap)                                                                                    sum_mktcap
        from ( -- sq1
          select
              coalesce(dateindex::text, 'emptydateindex')  dateindex_fct
            , case when sp in ('500', '400','600') then 'issp'    else 'notissp'    end is_sp_fct
            , case when sp   = '500'               then 'issp500' else 'notissp500' end is_sp500_fct
            , coalesce(sector_desc,   'emptysector')     sector_desc_fct
            , case when sector_desc   = 'Basic Materials'               then 'isbasicmat' else 'notisbasicmat' end is_materials_fct
            , coalesce(industry_desc, 'emptyindustry')      industry_desc_fct
            , case when industry_desc   = 'Gold & Silver'               then 'isgld'      else 'notisgld'     end is_gld_fct
            , mktcap
            , case when price < 0.10 or price > 1000.00 then null else price end price
            , now_inbnd_stmtid_dateindex
            , last_inbnd_stmtid_dateindex
            , now_inbnd_stmtstat_mktcap
            , last_inbnd_stmtstat_mktcap
            , now_inbnd_stmtstat_netinc_q1
            , last_inbnd_stmtstat_netinc_q1
            , now_inbnd_stmtstat_sales_q1
            , last_inbnd_stmtstat_sales_q1
            , case when pct_freeprice_ret_01m_ann > 100.00 or pct_freeprice_ret_01m_ann < -100.00 then null else pct_freeprice_ret_01m_ann end pct_freeprice_ret_01m_ann
          from fe_data_store.si_finecon2 where adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
          -- AND mktcap > 200.0
          -- and dateindex in (17347, 17317) 
          and dateindex = 17347  -- EVERYTHING HERE 2.7 SECONDS
          ) sq1
        group by cube(dateindex_fct, is_sp_fct, is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct)
      ) sq2 where sq2.dateindex_fct not in ('emptydateindex','alldateindex')  -- (NO COST DIFFERENCE): SPEED INCREASE by LESS DATA MANIP/RETURNED
      order by dateindex_fct, is_sp_fct , is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct;


create table groupingsets (alpha text, beta text, gamma real);

insert into groupingsets(alpha, beta,gamma) values(NULL, 'B1',    1.0);
insert into groupingsets(alpha, beta,gamma) values('A1', 'B1',    2.0);
insert into groupingsets(alpha, beta,gamma) values('A1', 'B2',    4.0);
insert into groupingsets(alpha, beta,gamma) values('A1', NULL,    8.0);
insert into groupingsets(alpha, beta,gamma) values(NULL, NULL,   16.0);
insert into groupingsets(alpha, beta,gamma) values(NULL, 'B2',   32.0);
insert into groupingsets(alpha, beta,gamma) values('A2', 'B1',   64.0);
insert into groupingsets(alpha, beta,gamma) values('A2', 'B2',  128.0);
insert into groupingsets(alpha, beta,gamma) values('A2', NULL,  256.0);



  select coalesce(sq.alpha,'ALL') alpha, coalesce(sq.beta,'ALL') beta, sum(sq.gamma) gamma
  from (
    select case when gr.alpha is null then 'EMPTY' else gr.alpha end, 
         case when gr.beta  is null then 'EMPTY' else gr.beta  end,
         gamma
    from groupingsets gr
  ) sq
  group by cube(sq.alpha,sq.beta);



      select 
          case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then                                                                  sq2.dateindex_fct::int                                                                                      else null end dateindex
        , case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then (extract( 'epoch' from ( select date_trunc('month', to_timestamp(sq2.dateindex_fct::int * 3600 *24 )::date) +  interval '1 month' - interval '1 day' )) / ( 3600* 24 ))::int else null end dateindexeom
        , sq2.dateindex_fct
        , sq2.is_sp_fct
        , sq2.is_sp500_fct
        , sq2.sector_desc_fct
        , sq2.is_materials_fct
        , sq2.industry_desc_fct
        , sq2.is_gld_fct
        , sq2.approx_price_o_mktcap_x100
        , sq2.count_now_inbnd_stmtstat_dateindex
        , sq2.pct_sum_now_o_last_inbnd_stmtstat_mktcap
        , sq2.rat_now_netinc_q1_o_mktcap_x_100
        , sq2.rat_now_sales_q1_o_mktcap_x_100
        , sq2.rat_now_netinc_q1_o_sales_x_100
        , sq2.unweighted_pct_freeprice_ret_01m_ann
        , sq2.sum_mktcap
      from ( -- sq2
        select 
            coalesce(sq1.dateindex_fct,        'alldateindex') dateindex_fct
          , coalesce(sq1.is_sp_fct,            'allsp') is_sp_fct
          , coalesce(sq1.is_sp500_fct,         'allsp500') is_sp500_fct
          , coalesce(sq1.sector_desc_fct,      'allsector_desc') sector_desc_fct
          , coalesce(sq1.is_materials_fct,     'allmaterials') is_materials_fct
          , coalesce(sq1.industry_desc_fct,    'allindustry') industry_desc_fct
          , coalesce(sq1.is_gld_fct,           'allgld') is_gld_fct
          , sum(sq1.price)                        / nullif(sum(sq1.mktcap), 0)                    * 100.00 approx_price_o_mktcap_x100  -- also EXTERIOR DATA: e.g. just the  S&P value would have been just fine
          , count(sq1.now_inbnd_stmtid_dateindex)::numeric   count_now_inbnd_stmtstat_dateindex                                                         -- cnt           reported this month
          , sum(sq1.now_inbnd_stmtstat_mktcap)    /   nullif(sum(sq1.last_inbnd_stmtstat_mktcap), 0)  * 100.00 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- pct by mktcap reported this month
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00 rat_now_netinc_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_sales_q1)  /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_sales_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_sales_q1), 0) * 100.00 rat_now_netinc_q1_o_sales_x_100
          , avg(sq1.pct_freeprice_ret_01m_ann)                                                                 unweighted_pct_freeprice_ret_01m_ann
          , sum(sq1.mktcap)                                                                                    sum_mktcap
        from ( -- sq1
          select
              coalesce(dateindex::text, 'emptydateindex')  dateindex_fct
            , case when sp in ('500', '400','600') then 'issp'    else 'notissp'    end is_sp_fct
            , case when sp   = '500'               then 'issp500' else 'notissp500' end is_sp500_fct
            , coalesce(sector_desc,   'emptysector')     sector_desc_fct
            , case when sector_desc   = 'Basic Materials'               then 'isbasicmat' else 'notisbasicmat' end is_materials_fct
            , coalesce(industry_desc, 'emptyindustry')      industry_desc_fct
            , case when industry_desc   = 'Gold & Silver'               then 'isgld'      else 'notisgld'     end is_gld_fct
            , mktcap
            , case when price < 0.10 or price > 1000.00 then null else price end price
            , now_inbnd_stmtid_dateindex
            , last_inbnd_stmtid_dateindex
            , now_inbnd_stmtstat_mktcap
            , last_inbnd_stmtstat_mktcap
            , now_inbnd_stmtstat_netinc_q1
            , last_inbnd_stmtstat_netinc_q1
            , now_inbnd_stmtstat_sales_q1
            , last_inbnd_stmtstat_sales_q1
            , case when pct_freeprice_ret_01m_ann > 100.00 or pct_freeprice_ret_01m_ann < -100.00 then null else pct_freeprice_ret_01m_ann end pct_freeprice_ret_01m_ann
          from fe_data_store.si_finecon2 where adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
          -- AND mktcap > 200.0
          -- and dateindex in (17347, 17317) 
          and dateindex = 17347  -- EVERYTHING HERE 2.7 SECONDS
          ) sq1
        group by cube(dateindex_fct, is_sp_fct, is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct)
      ) sq2 where sq2.dateindex_fct not in ('emptydateindex','alldateindex')  -- (NO COST DIFFERENCE): SPEED INCREASE by LESS DATA MANIP/RETURNED
      order by dateindex_fct, is_sp_fct , is_sp500_fct, sector_desc_fct, is_materials_fct, industry_desc_fct, is_gld_fct;



      select 
          case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then                                                                  sq2.dateindex_fct::int                                                                                      else null end dateindex
        , case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then (extract( 'epoch' from ( select date_trunc('month', to_timestamp(sq2.dateindex_fct::int * 3600 *24 )::date) +  interval '1 month' - interval '1 day' )) / ( 3600* 24 ))::int else null end dateindexeom
        , sq2.dateindex_fct
        , sq2.is_sp_fct
        , sq2.approx_price_o_mktcap_x100
        , sq2.count_now_inbnd_stmtstat_dateindex
        , sq2.pct_sum_now_o_last_inbnd_stmtstat_mktcap
        , sq2.rat_now_netinc_q1_o_mktcap_x_100
        , sq2.rat_now_sales_q1_o_mktcap_x_100
        , sq2.rat_now_netinc_q1_o_sales_x_100
        , sq2.unweighted_pct_freeprice_ret_01m_ann
        , sq2.sum_mktcap
      from ( -- sq2
        select 
            coalesce(sq1.dateindex_fct,        'alldateindex') dateindex_fct
          , coalesce(sq1.is_sp_fct,            'allsp') is_sp_fct
          , sum(sq1.price)                        / nullif(sum(sq1.mktcap), 0)                    * 100.00 approx_price_o_mktcap_x100  -- also EXTERIOR DATA: e.g. just the  S&P value would have been just fine
          , count(sq1.now_inbnd_stmtid_dateindex)::numeric   count_now_inbnd_stmtstat_dateindex                                                         -- cnt           reported this month
          , sum(sq1.now_inbnd_stmtstat_mktcap)    /   nullif(sum(sq1.last_inbnd_stmtstat_mktcap), 0)  * 100.00 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- pct by mktcap reported this month
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00 rat_now_netinc_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_sales_q1)  /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_sales_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_sales_q1), 0) * 100.00 rat_now_netinc_q1_o_sales_x_100
          , avg(sq1.pct_freeprice_ret_01m_ann)                                                                 unweighted_pct_freeprice_ret_01m_ann
          , sum(sq1.mktcap)                                                                                    sum_mktcap
        from ( -- sq1
          select
              coalesce(dateindex::text, 'emptydateindex')  dateindex_fct
            , case when sp in ('500', '400','600') then 'issp'    else 'notissp'    end is_sp_fct
            , mktcap
            , case when price < 0.10 or price > 1000.00 then null else price end price
            , now_inbnd_stmtid_dateindex
            , last_inbnd_stmtid_dateindex
            , now_inbnd_stmtstat_mktcap
            , last_inbnd_stmtstat_mktcap
            , now_inbnd_stmtstat_netinc_q1
            , last_inbnd_stmtstat_netinc_q1
            , now_inbnd_stmtstat_sales_q1
            , last_inbnd_stmtstat_sales_q1
            , case when pct_freeprice_ret_01m_ann > 100.00 or pct_freeprice_ret_01m_ann < -100.00 then null else pct_freeprice_ret_01m_ann end pct_freeprice_ret_01m_ann
          from fe_data_store.si_finecon2 where adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
          -- AND mktcap > 200.0
          -- and dateindex in (17347, 17317) 
          and dateindex = 17347  -- EVERYTHING HERE 2.7 SECONDS
          ) sq1
        group by cube(dateindex_fct, is_sp_fct)
      ) sq2 where sq2.dateindex_fct not in ('emptydateindex','alldateindex')  -- (NO COST DIFFERENCE): SPEED INCREASE by LESS DATA MANIP/RETURNED
      order by dateindex_fct, is_sp_fct;



 dateindex | dateindexeom | dateindex_fct | is_sp_fct | approx_price_o_mktcap_x100 | count_now_inbnd_stmtstat_dateindex | pct_sum_now_o_last_inbnd_stmtstat_mktcap | rat_now_netinc_q1_o_mktcap_x_100 | rat_now_sales_q1_o_mktcap_x_100 | rat_now_netinc_q1_o_sales_x_100 | unweighted_pct_freeprice_ret_01m_ann | sum_mktcap
-----------+--------------+---------------+-----------+----------------------------+------------------------------------+------------------------------------------+----------------------------------+---------------------------------+---------------------------------+--------------------------------------+-------------
     17347 |        17347 | 17347         | allsp     |   0.5175184990386289970000 |                                254 |                 5.1559631335096084330000 |         0.9613114328189371820000 |       18.1823833047186013600000 |        5.2870485497325448940000 |               0.59277805611222444890 | 28113704.20
     17347 |        17347 | 17347         | issp      |   0.3813841044745876780000 |                                 89 |                 5.3032500109766440120000 |         1.1842060443399664620000 |       17.2896553871473166920000 |        6.8492171638092604970000 |                   1.1041786743515850 | 23323811.60
     17347 |        17347 | 17347         | notissp   |   1.1804087214815630730000 |                                165 |                 4.4190135357192741760000 |        -0.3771013331081302070000 |       23.5429365869327187760000 |       -1.6017599661608768310000 |               0.32018817204301075269 |  4789892.60
(3 rows)


finance_econ=#


dbGetQuery(con, "

      select 
          case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then                                                                  sq2.dateindex_fct::int                                                                                      else null end dateindex
        , case when sq2.dateindex_fct not in ('emptydateindex','alldateindex') then (extract( 'epoch' from ( select date_trunc('month', to_timestamp(sq2.dateindex_fct::int * 3600 *24 )::date) +  interval '1 month' - interval '1 day' )) / ( 3600* 24 ))::int else null end dateindexeom
        , sq2.dateindex_fct
        , sq2.is_sp_fct
        , sq2.is_sp500_fct
        , sq2.approx_price_o_mktcap_x100
        , sq2.count_now_inbnd_stmtstat_dateindex
        , sq2.pct_sum_now_o_last_inbnd_stmtstat_mktcap
        , sq2.rat_now_netinc_q1_o_mktcap_x_100
        , sq2.rat_now_sales_q1_o_mktcap_x_100
        , sq2.rat_now_netinc_q1_o_sales_x_100
        , sq2.unweighted_pct_freeprice_ret_01m_ann
        , sq2.sum_mktcap
      from ( -- sq2
        select 
            coalesce(sq1.dateindex_fct,        'alldateindex') dateindex_fct
          , coalesce(sq1.is_sp_fct,            'allsp') is_sp_fct
          , coalesce(sq1.is_sp500_fct,         'allsp500') is_sp500_fct
          , sum(sq1.price)                        / nullif(sum(sq1.mktcap), 0)                    * 100.00 approx_price_o_mktcap_x100  -- also EXTERIOR DATA: e.g. just the  S&P value would have been just fine
          , count(sq1.now_inbnd_stmtid_dateindex)::numeric   count_now_inbnd_stmtstat_dateindex                                                         -- cnt           reported this month
          , sum(sq1.now_inbnd_stmtstat_mktcap)    /   nullif(sum(sq1.last_inbnd_stmtstat_mktcap), 0)  * 100.00 pct_sum_now_o_last_inbnd_stmtstat_mktcap -- pct by mktcap reported this month
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00 rat_now_netinc_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_sales_q1)  /   nullif(sum(sq1.now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_sales_q1_o_mktcap_x_100
          , sum(sq1.now_inbnd_stmtstat_netinc_q1) /   nullif(sum(sq1.now_inbnd_stmtstat_sales_q1), 0) * 100.00 rat_now_netinc_q1_o_sales_x_100
          , avg(sq1.pct_freeprice_ret_01m_ann)                                                                 unweighted_pct_freeprice_ret_01m_ann
          , sum(sq1.mktcap)                                                                                    sum_mktcap
        from ( -- sq1
          select
              coalesce(dateindex::text, 'emptydateindex')  dateindex_fct
            , case when sp in ('500', '400','600') then 'issp'    else 'notissp'    end is_sp_fct
            , case when sp   = '500'               then 'issp500' else 'notissp500' end is_sp500_fct
            , mktcap
            , case when price < 0.10 or price > 1000.00 then null else price end price
            , now_inbnd_stmtid_dateindex
            , last_inbnd_stmtid_dateindex
            , now_inbnd_stmtstat_mktcap
            , last_inbnd_stmtstat_mktcap
            , now_inbnd_stmtstat_netinc_q1
            , last_inbnd_stmtstat_netinc_q1
            , now_inbnd_stmtstat_sales_q1
            , last_inbnd_stmtstat_sales_q1
            , case when pct_freeprice_ret_01m_ann > 100.00 or pct_freeprice_ret_01m_ann < -100.00 then null else pct_freeprice_ret_01m_ann end pct_freeprice_ret_01m_ann
          from fe_data_store.si_finecon2 where adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
          -- AND mktcap > 200.0
          -- and dateindex in (17347, 17317) 
          and dateindex = 17347  -- EVERYTHING HERE 2.7 SECONDS
          ) sq1
        group by cube(dateindex_fct, is_sp_fct, is_sp500_fct)
      ) sq2 where sq2.dateindex_fct not in ('emptydateindex','alldateindex')  -- (NO COST DIFFERENCE): SPEED INCREASE by LESS DATA MANIP/RETURNED
      order by dateindex_fct, is_sp_fct , is_sp500_fct


") -> GT


 dateindex | dateindexeom | dateindex_fct | is_sp_fct | is_sp500_fct | approx_price_o_mktcap_x100 | count_now_inbnd_stmtstat_dateindex | pct_sum_now_o_last_inbnd_stmtstat_mktcap | rat_now_netinc_q1_o_mktcap_x_100 | rat_now_sales_q1_o_mktcap_x_100 | rat_now_netinc_q1_o_sales_x_100 | unweighted_pct_freeprice_ret_01m_ann | sum_mktcap
-----------+--------------+---------------+-----------+--------------+----------------------------+------------------------------------+------------------------------------------+----------------------------------+---------------------------------+---------------------------------+--------------------------------------+-------------
     17347 |        17347 | 17347         | allsp     | allsp500     |   0.5175184990386289970000 |                                254 |                 5.1559631335096084330000 |         0.9613114328189371820000 |       18.1823833047186013600000 |        5.2870485497325448940000 |               0.59277805611222444890 | 28113704.20
     17347 |        17347 | 17347         | allsp     | issp500      |   0.2136159816202809800000 |                                 28 |                 5.2907328952632605760000 |         1.3054309464344947210000 |       15.0395397952483461170000 |        8.6799926341292565890000 |                   1.9113617021276596 | 20998101.20
     17347 |        17347 | 17347         | allsp     | notissp500   |   1.4143329806342484260000 |                                226 |                 4.7508099140242344410000 |        -0.1907730216161953450000 |       28.7043705990169856470000 |       -0.6646131499665371080000 |               0.41681714934696195344 |  7115603.00
     17347 |        17347 | 17347         | issp      | allsp500     |   0.3813841044745876780000 |                                 89 |                 5.3032500109766440120000 |         1.1842060443399664620000 |       17.2896553871473166920000 |        6.8492171638092604970000 |                   1.1041786743515850 | 23323811.60
     17347 |        17347 | 17347         | issp      | issp500      |   0.2136159816202809800000 |                                 28 |                 5.2907328952632605760000 |         1.3054309464344947210000 |       15.0395397952483461170000 |        8.6799926341292565890000 |                   1.9113617021276596 | 20998101.20
     17347 |        17347 | 17347         | issp      | notissp500   |   1.8961092490277379330000 |                                 61 |                 5.4163613371108738430000 |         0.1141614525746643060000 |       37.1512842963692024520000 |        0.3072880379153441880000 |               0.69091503267973856209 |  2325710.40
     17347 |        17347 | 17347         | notissp   | allsp500     |   1.1804087214815630730000 |                                165 |                 4.4190135357192741760000 |        -0.3771013331081302070000 |       23.5429365869327187760000 |       -1.6017599661608768310000 |               0.32018817204301075269 |  4789892.60
     17347 |        17347 | 17347         | notissp   | notissp500   |   1.1804087214815630730000 |                                165 |                 4.4190135357192741760000 |        -0.3771013331081302070000 |       23.5429365869327187760000 |       -1.6017599661608768310000 |               0.32018817204301075269 |  4789892.60
(8 rows)
-- SELECT is__ THEN all* ACRODSS THE REST OF THE FACTORS




select attrelid::regclass, attname, attnum, format_type(atttypid, atttypmod) as type
from   pg_attribute
where  attrelid = ('fe_data_store' || '.' || 'si_finecon2')::regclass
and    attnum > 0
and    not attisdropped
order  by attnum;

select pg_attribute.attrelid::regclass, pg_attribute.attname, pg_attribute.attnum, format_type(pg_attribute.atttypid, pg_attribute.atttypmod) as type
from   pg_attribute
where  pg_attribute.attrelid = ('fe_data_store' || '.' || 'si_finecon2')::regclass
and    pg_attribute.attnum > 0
and    not pg_attribute.attisdropped
order  by pg_attribute.attnum;

pg_attribute.


select table_schema, ordinal_position, table_name, column_name,
case 
    when domain_name is not null then domain_name
    when data_type='character varying' THEN 'varchar('||character_maximum_length||')'
    when data_type='numeric' THEN 'numeric('||numeric_precision||','||numeric_scale||')'
    else data_type
end as myType
from information_schema.columns
where table_schema = 'fe_data_store' and table_name = 'si_finecon2'






select * from information_schema.columns 
where information_schema.columns.table_schema = 'fe_data_store' and information_schema.columns.table_name = 'si_finecon2'






select information_schema.columns.table_schema, information_schema.columns.ordinal_position, information_schema.columns.table_name, information_schema.columns.column_name
  , information_schema.columns.data_type
  , information_schema.columns.character_maximum_length
  , information_schema.columns.numeric_precision
  , information_schema.columns.numeric_scale
  , case 
    when information_schema.columns.domain_name is not null then information_schema.columns.domain_name
    when information_schema.columns.data_type='character varying' then 'varchar('||information_schema.columns.character_maximum_length||')'
    when information_schema.columns.data_type='numeric' then 'numeric('||information_schema.columns.numeric_precision||','||information_schema.columns.numeric_scale||')'
    else information_schema.columns.data_type
end as myType
from information_schema.columns, pg_attribute
where pg_attribute.attrelid::regclass =
  and 
  andinformation_schema.columns.table_schema = 'fe_data_store' and information_schema.columns.table_name = 'si_finecon2'


-- needs schema
select pg_catalog.pg_attribute.attrelid,
       pg_catalog.pg_attribute.attrelid::regclass, pg_catalog.pg_attribute.attname, pg_catalog.pg_attribute.attnum, format_type(pg_catalog.pg_attribute.atttypid, pg_catalog.pg_attribute.atttypmod) as type
from   pg_catalog.pg_attribute
where  pg_catalog.pg_attribute.attrelid = ('fe_data_store' || '.' || 'si_finecon2')::regclass
and    pg_catalog.pg_attribute.attnum > 0
and    not pg_catalog.pg_attribute.attisdropped
order  by pg_catalog.pg_attribute.attnum;

--also help from 
--Query the schema details of a table in PostgreSQL?
--https://stackoverflow.com/questions/4336259/query-the-schema-details-of-a-table-in-postgresql

SELECT  
  a.attname as Column,
  pg_catalog.format_type(a.atttypid, a.atttypmod) as Datatype
  FROM
  pg_catalog.pg_attribute a
  WHERE
    a.attnum > 0
  AND NOT a.attisdropped
  AND a.attrelid = (
    SELECT c.oid
    FROM pg_catalog.pg_class c
    LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relname = 'si_finecon2'
   AND pg_catalog.pg_table_is_visible(c.oid)
  )


----

-- KEEP
select 
       pg_catalog.pg_namespace.nspname,
       pg_catalog.pg_attribute.attrelid::regclass, pg_catalog.pg_attribute.attname, pg_catalog.pg_attribute.attnum, format_type(pg_catalog.pg_attribute.atttypid, pg_catalog.pg_attribute.atttypmod) as type
from   pg_catalog.pg_attribute, pg_catalog.pg_class, pg_catalog.pg_namespace
where  pg_catalog.pg_attribute.attrelid = pg_catalog.pg_class.oid and pg_catalog.pg_class.relnamespace = pg_catalog.pg_namespace.oid
and    pg_catalog.pg_attribute.attrelid = ('fe_data_store' || '.' || 'si_finecon2')::regclass
and    pg_catalog.pg_attribute.attnum > 0
and    not pg_catalog.pg_attribute.attisdropped
order  by pg_catalog.pg_attribute.attnum;


# Also note that the cast to regclass resolves the table name somewhat 
# intelligently according to the current search_path. 
# It also raises an exception if the name is not valid. Details:

select information_schema.columns.table_schema, information_schema.columns.table_name, information_schema.columns.column_name, information_schema.columns.ordinal_position
  , information_schema.columns.data_type
  , information_schema.columns.character_maximum_length
  , information_schema.columns.numeric_precision
  , information_schema.columns.numeric_scale
  , case 
    when information_schema.columns.domain_name is not null then information_schema.columns.domain_name
    when information_schema.columns.data_type='character varying' then 'varchar('||information_schema.columns.character_maximum_length||')'
    when information_schema.columns.data_type='numeric' then 'numeric('||information_schema.columns.numeric_precision||','||information_schema.columns.numeric_scale||')'
    else information_schema.columns.data_type
end as myType
from information_schema.columns
where information_schema.columns.table_schema = 'fe_data_store' and information_schema.columns.table_name = 'si_finecon2';

----


select 
       pg_catalog.pg_namespace.nspname,
       pg_catalog.pg_attribute.attrelid::regclass, pg_catalog.pg_attribute.attname, pg_catalog.pg_attribute.attnum, format_type(pg_catalog.pg_attribute.atttypid, pg_catalog.pg_attribute.atttypmod) as type
from   pg_catalog.pg_attribute, pg_catalog.pg_class, pg_catalog.pg_namespace
where  pg_catalog.pg_attribute.attrelid = pg_catalog.pg_class.oid and pg_catalog.pg_class.relnamespace = pg_catalog.pg_namespace.oid
and    pg_catalog.pg_attribute.attrelid = ('fe_data_store' || '.' || 'si_finecon2')::regclass
and    pg_catalog.pg_attribute.attnum > 0
and    not pg_catalog.pg_attribute.attisdropped
order  by pg_catalog.pg_attribute.attnum;

---
---- SUPER KEEP

select 
    information_schema.columns.udt_catalog
  , information_schema.columns.udt_schema
  , information_schema.columns.table_schema
  , information_schema.columns.table_name
  , information_schema.columns.column_name
  , information_schema.columns.ordinal_position
  , information_schema.columns.udt_name
  , information_schema.columns.data_type
  , information_schema.columns.character_maximum_length
  , information_schema.columns.numeric_precision
  , information_schema.columns.numeric_scale
  , information_schema.columns.datetime_precision
  , format_type(pg_catalog.pg_attribute.atttypid, pg_catalog.pg_attribute.atttypmod) as format_type_type
  , (case 
      when information_schema.columns.domain_name is not null then information_schema.columns.domain_name
      when information_schema.columns.data_type='character varying' then 'varchar('||information_schema.columns.character_maximum_length||')'
      when information_schema.columns.data_type='numeric' then 'numeric('||information_schema.columns.numeric_precision||','||information_schema.columns.numeric_scale||')'
    else 
      information_schema.columns.data_type
    end)::text as info_schema_type
  , information_schema.columns.is_nullable
  , information_schema.columns.column_default
  , information_schema.columns.is_identity
from information_schema.columns, pg_catalog.pg_attribute, pg_catalog.pg_class, pg_catalog.pg_namespace
where information_schema.columns.table_schema = 'fe_data_store' and information_schema.columns.table_name = 'si_finecon2'
and    pg_catalog.pg_attribute.attrelid = pg_catalog.pg_class.oid and pg_catalog.pg_class.relnamespace = pg_catalog.pg_namespace.oid
and    pg_catalog.pg_attribute.attrelid = ('fe_data_store' || '.' || 'si_finecon2')::regclass
and    pg_catalog.pg_attribute.attname = 'mktcap'
and    pg_catalog.pg_attribute.attnum > 0
and    not pg_catalog.pg_attribute.attisdropped
and    information_schema.columns.table_schema     = pg_catalog.pg_namespace.nspname and 
       information_schema.columns.table_name       = pg_catalog.pg_attribute.attrelid::regclass::text and
       information_schema.columns.column_name      = pg_catalog.pg_attribute.attname and
       information_schema.columns.ordinal_position = pg_catalog.pg_attribute.attnum
order  by pg_catalog.pg_attribute.attnum;

---- 

    select 
        information_schema.columns.udt_catalog
      , information_schema.columns.udt_schema
      , information_schema.columns.table_schema
      , information_schema.columns.table_name
      , information_schema.columns.column_name
      , information_schema.columns.ordinal_position
      , information_schema.columns.udt_name
      , information_schema.columns.data_type
      , information_schema.columns.character_maximum_length
      , information_schema.columns.numeric_precision
      , information_schema.columns.numeric_scale
      , information_schema.columns.datetime_precision
      , format_type(pg_catalog.pg_attribute.atttypid, pg_catalog.pg_attribute.atttypmod) as format_type_type
      , (case 
          when information_schema.columns.domain_name is not null then information_schema.columns.domain_name
          when information_schema.columns.data_type='character varying' then 'varchar('||information_schema.columns.character_maximum_length||')'
          when information_schema.columns.data_type='numeric' then 'numeric('||information_schema.columns.numeric_precision||','||information_schema.columns.numeric_scale||')'
        else 
          information_schema.columns.data_type
        end)::text as info_schema_type
      , information_schema.columns.is_nullable
      , information_schema.columns.column_default
      , information_schema.columns.is_identity
    from information_schema.columns, pg_catalog.pg_attribute, pg_catalog.pg_class, pg_catalog.pg_namespace
    where information_schema.columns.table_schema = '", schema_name, "' and information_schema.columns.table_name = '", table_name,"'
    and    pg_catalog.pg_attribute.attrelid = pg_catalog.pg_class.oid and pg_catalog.pg_class.relnamespace = pg_catalog.pg_namespace.oid
    and    pg_catalog.pg_attribute.attrelid = ('", schema_name,"' || '.' || '", table_name,"')::regclass
    and    pg_catalog.pg_attribute.attname = '", column_name, "'
    and    pg_catalog.pg_attribute.attnum > 0
    and    not pg_catalog.pg_attribute.attisdropped
    and    information_schema.columns.table_schema     = pg_catalog.pg_namespace.nspname and 
           information_schema.columns.table_name       = pg_catalog.pg_attribute.attrelid::regclass::text and
           information_schema.columns.column_name      = pg_catalog.pg_attribute.attname and
           information_schema.columns.ordinal_position = pg_catalog.pg_attribute.attnum
    order  by pg_catalog.pg_attribute.attnum;



select distinct sector_desc from fe_data_store.si_finecon2;

GOOD FOR PIVOTAL_R

need mktcap per XXX items in main table 

industry_mktcap
sector_mktcap

select sum(now_inbnd_stmtstat_netinc_q1),   from
group by dateindexeom, sector_desc
order by dateindexeom, sector_desc

select distinct sector_desc from fe_data_store.si_finecon2;
"Energy"
"Basic Materials"



select
    dateindex 
  , dateindexeom
  , dateindexeom::text dateindexeom_fct
  , 'sector_desc'::text collection_name_fct
  , sector_desc sector_desc_fct
  , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1 
  , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
from si_finecon2 where sector_desc in ('Energy','Basic Materials') and dateindexeom = 17378
group by dateindex, dateindexeom, sector_desc
order by dateindex, dateindexeom, sector_desc
;

select count(1) from information_schema.columns where table_schema = 'pg_temp_8' and table_name = 'temp_xxx' and column_name = 'Species';

select ('public' || '.' || 'Iris' )::regclass;


dbGetQuery(con,"
EXPLAIN
select
    dateindex 
  , dateindexeom
  , dateindexeom::text dateindexeom_fct
  , 'sector_desc'::text collection_name_fct
  , sector_desc sector_desc_fct
  , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1 
  , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
from si_finecon2 where sector_desc in ('Energy','Basic Materials') and dateindexeom = 17378
group by dateindex, dateindexeom, sector_desc
order by dateindex, dateindexeom, sector_desc
;") -> SFS


"GroupAggregate  (cost=135376.48..135408.94 rows=925 width=147)"


dbGetQuery(con,"
--EXPLAIN
select
    dateindex 
  , dateindexlwd
  , dateindexeom
  , dateindexeom::text dateindexeom_fct
  , 'sector_desc'::text collection_name_fct
  , sector_desc sector_desc_fct
  , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1 
  , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
from si_finecon2 where sector_desc in ('Energy','Basic Materials') and dateindexeom = 17378
group by dateindex, dateindexlwd, dateindexeom, sector_desc
order by dateindex, dateindexlwd, dateindexeom, sector_desc
;") -> SFS

"GroupAggregate  (cost=135376.48..135411.27 rows=925 width=151)"
-- just 3 pts


dbGetQuery(con,"
select
    dateindex 
  , dateindexlwd
  , dateindexeom
  , dateindexeom::text dateindexeom_fct
  , 'sector_desc'::text collection_name_fct
  , sector_desc sector_desc_fct
  , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1 
  , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
from si_finecon2 where sector_desc in ('Energy','Basic Materials') and dateindexeom = 17378
group by dateindex, dateindexlwd, dateindexeom, sector_desc
order by dateindex, dateindexlwd, dateindexeom, sector_desc
;") -> SFS

select distinct dateindex from 'si_finecon2_aggregates'; 
select distinct dateindex from "si_finecon2_aggregates"; 



insert into "si_finecon2_aggregates"(dateindex, dateindexlwd, dateindexeom, dateindexeom_fct, sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1, sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1, sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap, sector_desc__energy____sum_now_inbnd_stmtstat_mktcap) 
  select dateindex, dateindexlwd, dateindexeom, dateindexeom_fct, sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1, sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1, sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap, sector_desc__energy____sum_now_inbnd_stmtstat_mktcap 
    from upsert_temp 
      on conflict (dateindex) 
        do update set (dateindex, dateindexlwd, dateindexeom, dateindexeom_fct, sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1, sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1, sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap, sector_desc__energy____sum_now_inbnd_stmtstat_mktcap) = (excluded.dateindex, excluded.dateindexlwd, excluded.dateindexeom, excluded.dateindexeom_fct, excluded.sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1, excluded.sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1, excluded.sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap, excluded.sector_desc__energy____sum_now_inbnd_stmtstat_mktcap);



update "si_finecon2_aggregates" s  set 
  dateindexlwd = t.dateindexlwd, 
  dateindexeom = t.dateindexeom, 
  dateindexeom_fct = t.dateindexeom_fct, 
  sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1 = t.sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1, 
  sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1 = t.sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1, 
  sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap = t.sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap, 
  sector_desc__energy____sum_now_inbnd_stmtstat_mktcap = t.sector_desc__energy____sum_now_inbnd_stmtstat_mktcap 
    from upsert_temp t  
      where s.dateindex = t.dateindex;

-- trying to store the mkt aggregates in a table
-- TWO SQL
select dateindex, dateindexlwd, dateindexeom, company_id 
from si_finecon2 where dateindex = 17378
( -- sq1
  select -- sq
      dateindex 
    , dateindexlwd
    , dateindexeom
    , sector_desc sector_desc_fct
    , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
    , sum(last_inbnd_stmtstat_mktcap)   sum_last_now_inbnd_stmtstat_mktcap
    , sum(mktcap)                       sum_mktcap
  from si_finecon2 sq where sq.sector_desc = sector_desc and dateindex = 17378
  group by dateindex, dateindexlwd, dateindexeom, sector_desc
  order by dateindex, dateindexlwd, dateindexeom, sector_desc ) sq1


-- NOT AN AGGREGATE
--
-- of the SP per sector the mktcap per record
select 
    sr.dateindex, sr.company_id
  , sq1.sector_desc
  , sq1.sum_sp_now_inbnd_stmtstat_mktcap        sector_desc_sp_sum_now_inbnd_stmtstat_mktcap
  , sq1.sum_sp_last_now_inbnd_stmtstat_mktcap   sector_desc_sp_sum_last_now_inbnd_stmtstat_mktcap
  , sq1.sum_sp_mktcap                           sector_desc_sp_sum_mktcap
from si_finecon2 sr inner join
( -- sq1
  select -- sq
      dateindex 
    , dateindexlwd
    , dateindexeom
    , sector_desc
    , sum(now_inbnd_stmtstat_mktcap)    sum_sp_now_inbnd_stmtstat_mktcap
    , sum(last_inbnd_stmtstat_mktcap)   sum_sp_last_now_inbnd_stmtstat_mktcap
    , sum(mktcap)                       sum_sp_mktcap
  from si_finecon2 sq where sq.sector_desc = sector_desc and dateindex = 17378
  group by dateindex, dateindexlwd, dateindexeom, sector_desc
  order by dateindex, dateindexlwd, dateindexeom, sector_desc ) sq1
on sr.dateindex   = sq1.dateindex and 
   sr.sector_desc = sq1.sector_desc and
   sr.sp in ('500','400','600') and
   sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
   sr.dateindex = 17378
-- WORKS
   
-- NOT AN AGGREGATE
--
-- of the SP per sector the mktcap per record
select 
    -- sr.dateindex, 
    sr.company_id
--   , sq1.sum_sp_sector_desc_now_inbnd_stmtstat_mktcap       
--   , sq1.sum_sp_sector_desc_last_now_inbnd_stmtstat_mktcap   
--   , sq1.sum_sp_sector_desc_mktcap      
  , sq1.*                     
from si_finecon2 sr inner join
( -- sq1
  select -- sq
      dateindex 
    , sector_desc  -- DOES NOT HUR TO RE-UPDATE
    , sum(now_inbnd_stmtstat_mktcap)    sum_sp_sector_desc_now_inbnd_stmtstat_mktcap
    , sum(last_inbnd_stmtstat_mktcap)   sum_sp_sector_desc_last_now_inbnd_stmtstat_mktcap
    , sum(mktcap)                       sum_sp_sector_desc_mktcap
  from si_finecon2 sq where sq.sector_desc = sector_desc and dateindex = 17378
  group by dateindex, sector_desc
  order by dateindex, sector_desc ) sq1
on sr.dateindex   = sq1.dateindex and 
   sr.sector_desc = sq1.sector_desc and
   sr.sp in ('500','400','600') and
   sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
   sr.dateindex = 17378
-- WORKS

-- NOT AN AGGREGATE
--
-- of the SP per sector the mktcap per record
select 
    sr.company_id
  , sq1.*                     
from si_finecon2 sr inner join
( -- sq1
  select 
      dateindex 
    , sector_desc  
    , count(1)                          count_sp_sector_desc
    , sum(now_inbnd_stmtstat_mktcap)    sum_sp_sector_desc_now_inbnd_stmtstat_mktcap
    , sum(last_inbnd_stmtstat_mktcap)   sum_sp_sector_desc_last_now_inbnd_stmtstat_mktcap
    , sum(mktcap)                       sum_sp_sector_desc_mktcap
  from si_finecon2 where dateindex = 17378
  group by dateindex, sector_desc) sq1
on sr.dateindex   = sq1.dateindex and 
   sr.sp in ('500','400','600') and
   sr.sector_desc = sq1.sector_desc and
   sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
   sr.dateindex = 17378
-- KEEP (PRISTINE)


-- TEMPLATE
-- NOT AN AGGREGATE
--
-- of the SP per sector the mktcap per record
select 
    sr.company_id
  , sq1.*                     
from si_finecon2 sr inner join
( -- sq1
  select 
      dateindex 
    , DIVISION  
    , count(1)                          count_SP_OPS_SHORT_DIVISION
    , sum(now_inbnd_stmtstat_mktcap)    sum_SP_OPS_SHORT_DIVISION_now_inbnd_stmtstat_mktcap
    , sum(last_inbnd_stmtstat_mktcap)   sum_SP_OPS_SHORT_DIVISION_last_now_inbnd_stmtstat_mktcap
    , sum(mktcap)                       sum_SP_OPS_SHORT_DIVISION_mktcap
  from si_finecon2 where dateindex = 17378
  group by dateindex, DIVISION) sq1
on sr.dateindex   = sq1.dateindex and 
   sr.sp in SP_OPS_WHAT and
   sr.DIVISION = sq1.DIVISION and
   sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
   sr.dateindex = 17378
-- KEEP (PRISTINE)


      select
          sr.company_id
        , sq1.*
      from si_finecon2 sr inner join
      ( -- sq1
        select
            dateindex
          , sector_desc
          , count(1)                         count_sp500_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex = 17378
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex = 17378



      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sp500_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex = 17347
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex = 17347
-- WRONG
--COUNT NUMERS ARE HUGHE

      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sp500_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex = 17347 and sp in ('500') -- FIXED
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex = 17347




update si_finecon2 s  set 
  sector_desc = t.sector_desc, 
  count_sp500_sector_desc = t.count_sp500_sector_desc, 
  --sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap = t.sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap, 
  sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap = t.sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap, 
  sum_sp500_sector_desc_mktcap = t.sum_sp500_sector_desc_mktcap 
    from upsert_temp t  
      where s.dateindex_company_id = t.dateindex_company_id;

NULL
 Show Traceback
 Rerun with Debug
 Error in postgresqlExecStatement(conn, statement, ...) : 
  RS-DBI driver: (could not Retrieve the result : ERROR:  numeric field overflow
DETAIL:  A field with precision 8, scale 2 must round to an absolute value less than 10^6.
) 


select numeric_precision from information_schema.columns where table_schema = 'fe_data_store' and table_name = 'si_finecon2' and column_name = 'sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap'; 
8


select current_schema(); 

Executing in database connection 1:

select numeric_precision from information_schema.columns where table_schema = 'fe_data_store' and table_name = 'si_finecon2' and column_name = 'sum_sp500_sector_desc_mktcap'; 

Executing in database connection 1:

drop table if exists upsert_temp 


select max(sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap) from upsert_temp where dateindex = 17378
3070826.6
NEED NUMBERIC(9,2) to fit

drop table if exists upsert_temp;


STATEMENT:  alter table if exists upsert_temp add primary key(dateindex_company_id);
ERROR:  multiple primary keys for table "upsert_temp" are not allowed
STATEMENT:  alter table if exists upsert_temp add primary key(dateindex_company_id);
LOG:  could not send data to client: An existing connection was forcibly closed by the remote host.

STATEMENT:  alter table if exists upsert_temp add primary key(dateindex_company_id);
FATAL:  connection to client lost
LOG:  could not send data to client: An existing connection was forcibly closed by the remote host.

FATAL:  connection to client lost
ERROR:  relation "fe_data_store.upsert_temp" does not exist at character 35
STATEMENT:  SELECT count(*) AS rows FROM ONLY fe_data_store.upsert_temp
LOG:  could not send data to client: An existing connection was forcibly closed by the remote host.

FATAL:  connection to client lost
ERROR:  multiple primary keys for table "upsert_temp" are not allowed
STATEMENT:  alter table if exists upsert_temp add primary key(dateindex_company_id);
ERROR:  multiple primary keys for table "upsert_temp" are not allowed
STATEMENT:  alter table if exists upsert_temp add primary key(dateindex_company_id);
ERROR:  multiple primary keys for table "upsert_temp" are not allowed
STATEMENT:  alter table if exists upsert_temp add primary key(dateindex_company_id);
ERROR:  multiple primary keys for table "upsert_temp" are not allowed
STATEMENT:  alter table if exists upsert_temp add primary key(dateindex_company_id);






select max(dateindex) from fe_data_store.si_finecon2;
select count(*) from fe_data_store.si_finecon2 where dateindex = 17409;
--6440
select * from fe_data_store.si_finecon2 where dateindex = 17409;
select * from fe_data_store.si_finecon2 where dateindex = 17378;

select distinct industry_desc from fe_data_store.si_finecon2 where dateindex = 17378 order by 1;
"Gold & Silver"
"Furniture & Fixtures"
"Oil & Gas Operations"



  DATEINDEX         <- dateindex

  DIVISION          <- c("sector_desc", "industry_desc")
  
  SP_OPS_WHAT       <- c("('500')","('500','400','600')")
  SP_OPS_WHAT_SHORT <- c("sp500"  ,"sp"                 )

  combo_grid   <- expand.grid(DIVISION=DIVISION, SP_OPS_WHAT=SP_OPS_WHAT)
  combo_grid_f <- seq_along(row.names(combo_grid))


      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sp500_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex = 17409 and sp in ('500')
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex = 17409
      WHERE company_id = 'A07F9';



      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sp500_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex = 17409 and sp in ('500')
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex = 17409
      WHERE company_id = 'A07F9';

"A07F9";17409;"Services";104;1591100.80;4402963.70;4381432.60

      -- + 17378

      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sp500_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex in (17409,17378) and sp in ('500')
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex in (17409,17378)
      WHERE company_id = 'A07F9';

"A07F9";17409;"Services";104;1591100.80;4402963.70;4381432.60
"A07F9";17378;"Services";106;2532962.70;4485051.40;4480013.50
-- GOOD

-- SHOULD HAVE BEEN

      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sp500_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex in (17409,17378) and sp in ('500')
                           and adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex in (17409,17378)
      WHERE company_id = 'A07F9';

-- different AND CORRECT
"A07F9";17378;"Services";103;2461479.70;4413568.40;4408530.50
"A07F9";17409;"Services";101;1591100.80;4331480.70;4311175.30

-- without DIVISION


      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          -- , sector_desc  
          , count(1)                         count_sp500_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_desc_mktcap
        from si_finecon2 where dateindex in (17409,17378) and sp in ('500')
                           and adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
        group by dateindex) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         -- sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex in (17409,17378)
      WHERE company_id = 'A07F9';

"A07F9";17378;473;13608665.90;21332450.80;21446471.10
"A07F9";17409;472;7049849.30;21354050.30;21265889.40


-- WITOUT OPS ( SILL HAS DIVISION )

      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sector_desc
          , sum(now_inbnd_stmtstat_mktcap)     sum_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sector_desc_mktcap
        from si_finecon2 where dateindex in (17409,17378) -- and sp in ('500')
                           and adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         -- sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex in (17409,17378)
      WHERE company_id = 'A07F9';

"A07F9";17409;"Services";772;2614088.60;5942609.50;5923678.10
"A07F9";17378;"Services";770;2958160.80;5956897.40;6028068.70


-- WITHOUT DIVISION and WITHOUT OPS

      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          --, sector_desc  
          , count(1)                         count
          , sum(now_inbnd_stmtstat_mktcap)     sum_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_mktcap
        from si_finecon2 where dateindex in (17409,17378) -- and sp in ('500')
                           and adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
        group by dateindex) sq1
      on sr.dateindex = sq1.dateindex and 
         -- sr.sp in ('500') and
         -- sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex in (17409,17378)
      WHERE company_id = 'A07F9';

"A07F9";17378;4107;16111023.00;28292083.60;28684556.90
"A07F9";17409;4105;11378642.00;28500417.60;28398076.00

-- MORE RETURN DATA ( SHOLD HAVE BEEN )

      select 
          sr.company_id
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
          , sector_desc  
          , count(1)                         count_sp500_sector_desc
          , count(now_inbnd_stmtstat_mktcap) count_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(now_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , count(last_inbnd_stmtstat_mktcap) count_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)    sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                        sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex in (17409,17378) and sp in ('500')
                           and adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
        group by dateindex, sector_desc) sq1
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500') and
         sr.sector_desc = sq1.sector_desc and
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and
         sr.dateindex in (17409,17378)
      WHERE company_id = 'A07F9';
      
"A07F9";17378;"Services";103;45;2461479.70;103;4413568.40;4408530.50
"A07F9";17409;"Services";101;49;1591100.80;101;4331480.70;4311175.30

-- CAN I REPLACE THE ABOVE GROUP BY WITH 'PARTITION BY' (SKIP TOO LONG)
-- GOOD: WHAT DOES THE 2ND ONE LOOK LIKE

Browse[2]> writeLines(add_columns_sql)

      select 
          sr.company_id  -- REQUIRED SO I ONLY UPSERT specific OPS/DIVISION member COMPANIES
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
           , sector_desc  
          , count(1)                          count_sp500_sector_desc
          , count(now_inbnd_stmtstat_mktcap)  count_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(now_inbnd_stmtstat_mktcap)      sum_sp500_sector_desc_now_inbnd_stmtstat_mktcap
          , count(last_inbnd_stmtstat_mktcap) count_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)     sum_sp500_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                         sum_sp500_sector_desc_mktcap
        from si_finecon2 where dateindex = 17409 and 
                               sp in ('500') and  
                              adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text 
        group by dateindex , sector_desc ) sq1 
      on sr.dateindex = sq1.dateindex and 
          sr.sp in ('500') and  
          sr.sector_desc = sq1.sector_desc and  
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and 
         sr.dateindex = 17409
      
Browse[2]> 
-- GOOD

Browse[2]> writeLines(add_columns_sql)

      select 
          sr.company_id  -- REQUIRED SO I ONLY UPSERT specific OPS/DIVISION member COMPANIES
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex 
             
          , count(1)                          count
          , count(now_inbnd_stmtstat_mktcap)  count_now_inbnd_stmtstat_mktcap
          , sum(now_inbnd_stmtstat_mktcap)      sum_now_inbnd_stmtstat_mktcap
          , count(last_inbnd_stmtstat_mktcap) count_last_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)     sum_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                         sum_mktcap
        from si_finecon2 where dateindex = 17409 and 
                               sp in ('500') and  
                              adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text 
        group by dateindex  ) sq1 
      on sr.dateindex = sq1.dateindex and 
          sr.sp in ('500') and  
          
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and 
         sr.dateindex = 17409
      
Browse[2]> 

  DATEINDEX         <- dateindex

  DIVISION          <- c("", "sector_desc", "industry_desc")
  
  SP_OPS_WHAT       <- c("('500','400','600')", "('500')")
  SP_OPS_WHAT_SHORT <- c("sp"                 , "sp500"  ) 

  combo_grid   <- expand.grid(DIVISION=DIVISION, SP_OPS_WHAT=SP_OPS_WHAT, stringsAsFactors = FALSE)
  combo_grid_f <- seq_along(row.names(combo_grid))

Browse[2]> split(combo_grid, combo_grid_f)
$`1`
  DIVISION SP_OPS_WHAT
1              ('500')

$`2`
     DIVISION SP_OPS_WHAT
2 sector_desc     ('500')

$`3`
       DIVISION SP_OPS_WHAT
3 industry_desc     ('500')

$`4`
  DIVISION         SP_OPS_WHAT
4          ('500','400','600')

$`5`
     DIVISION         SP_OPS_WHAT
5 sector_desc ('500','400','600')

$`6`
       DIVISION         SP_OPS_WHAT
6 industry_desc ('500','400','600')


      select 
          sr.company_id  -- REQUIRED SO I ONLY UPSERT specific OPS/DIVISION member COMPANIES
        , sq1.*                     
      from si_finecon2 sr inner join
      ( -- sq1
        select 
            dateindex
          , sector_desc
          , count(1)                          count_sp_sector_desc
          , count(now_inbnd_stmtstat_mktcap)  count_sp_sector_desc_now_inbnd_stmtstat_mktcap
          , sum(now_inbnd_stmtstat_mktcap)      sum_sp_sector_desc_now_inbnd_stmtstat_mktcap
          , count(last_inbnd_stmtstat_mktcap) count_sp_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(last_inbnd_stmtstat_mktcap)     sum_sp_sector_desc_last_now_inbnd_stmtstat_mktcap
          , sum(mktcap)                         sum_sp_sector_desc_mktcap
        from si_finecon2 where dateindex = 17409 and 
                               sp in ('500','400','600') and  
                              adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text 
        group by dateindex , sector_desc ) sq1 
      on sr.dateindex = sq1.dateindex and 
         sr.sp in ('500','400','600') and  
         sr.sector_desc = sq1.sector_desc and 
         sr.adr = 0 AND sr.exchange <> 'O'::text  AND sr.company !~~ '%iShares%'::text AND sr.company !~~ '%Vanguard%'::text AND sr.company !~~ 'SPDR'::text AND sr.company !~~ '%PowerShares%'::text AND sr.company !~~ '%Fund%'::text AND sr.company !~~ '%Holding%'::text AND sr.industry_desc !~~ '%Investment Service%'::text and 
         sr.dateindex = 17409

----

select
    dateindex
  , dateindexlwd
  , dateindexeom
  , dateindexeom::text dateindexeom_fct
  , 'sp_desc'::text collection_name01_fct
  ,     case when sp in ('500') then 'sp500'::text else 'notsp500'::text end sp_desc_fct

  
  , sum(now_inbnd_stmtstat_sales_q1)  / nullif(sum(now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
  , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_mktcap), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
  , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_sales_q1), 0) * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
  , count(now_inbnd_stmtid_dateindex)::numeric                                                                     count_now_inbnd_stmtstat_dateindex
  , count(now_inbnd_stmtid_dateindex)::numeric / nullif(count(last_inbnd_stmtid_dateindex)::numeric,0) * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
  , sum(mktcap) sum_mktcap
  , avg(pct_freeprice_ret_01m_ann * mktcap / nullif(sum_sp500_mktcap,0) )  avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM
  , sum(now_inbnd_stmtstat_mktcap)                                                   sum_now_inbnd_stmtstat_mktcap
  , sum(now_inbnd_stmtstat_mktcap) / nullif(sum(last_inbnd_stmtstat_mktcap), 0)  rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
from si_finecon2 where
      sp in ('500')

  and adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
  and dateindexeom = 17378
group by dateindex, dateindexlwd, dateindexeom, sp_desc_fct
order by dateindex, dateindexlwd, dateindexeom, sp_desc_fct

----

-- BEFORE
-- -- fe_data_store.si_finecon2_aggregates;
-- 
--   dateindex integer,
--   dateindexlwd integer,
--   dateindexeom integer,
--   dateindexeom_fct text,
--   sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
--   sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
--   sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap numeric(9,2),
--   sector_desc__energy____sum_now_inbnd_stmtstat_mktcap numeric(9,2)
-- 
-- select * from fe_data_store.si_finecon2_aggregates;
-- 17378;17378;17378;"17378";17503.10;12384.80;1002511.90;1720490.50

----

select
    dateindex
  , dateindexlwd
  , dateindexeom
  , dateindexeom::text dateindexeom_fct
  , 'sp_desc'::text collection_name01_fct
  ,     case when sp in ('500') then 'sp500'::text else 'notsp500'::text end sp_desc_fct
  , 'industry_desc'::text collection_name02_fct
  ,     industry_desc industry_desc_fct
  , sum(now_inbnd_stmtstat_sales_q1)  / nullif(sum(now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
  , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_mktcap), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
  , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_sales_q1), 0) * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
  , count(now_inbnd_stmtid_dateindex)::numeric                                                                     count_now_inbnd_stmtstat_dateindex
  , count(now_inbnd_stmtid_dateindex)::numeric / nullif(count(last_inbnd_stmtid_dateindex)::numeric,0) * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
  , sum(mktcap) sum_mktcap
  , avg(pct_freeprice_ret_01m_ann * mktcap / nullif(sum_sp500_industry_desc_mktcap,0) )  avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM
  , sum(now_inbnd_stmtstat_mktcap)                                                   sum_now_inbnd_stmtstat_mktcap
  , sum(now_inbnd_stmtstat_mktcap) / nullif(sum(last_inbnd_stmtstat_mktcap), 0)  rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
from si_finecon2 where
      sp in ('500') 
  and industry_desc in ('Gold & Silver', 'Furniture & Fixtures') 
  and adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
  and dateindexeom = 17378
group by dateindex, dateindexlwd, dateindexeom, sp_desc_fct, industry_desc_fct
order by dateindex, dateindexlwd, dateindexeom, sp_desc_fct, industry_desc_fct


--  # FROM
--  # load_division_aggregated_per_dateindex(dateindex = 17378) # data already in aggr
--  # SFS


    

-- AFTER
-- -- fe_data_store.si_finecon2_aggregates;
-- 

  dateindex integer,
  dateindexlwd integer,
  dateindexeom integer,
  dateindexeom_fct text,
  sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap numeric(9,2),
  sector_desc__energy____sum_now_inbnd_stmtstat_mktcap numeric(9,2),
  collection_name01_fct text,
  sp_desc_fct text,
  collection_name02_fct text,
  industry_desc_fct text,
  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  count_now_inbnd_stmtstat_dateindex numeric(8,2),
  rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sum_mktcap numeric(8,2),
  avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2)

-- 
-- select * from fe_data_store.si_finecon2_aggregates;
-- 

17378;17378;17378;"17378";17503.10;12384.80;1002511.90;1720490.50;"";"";"";"";;;;;;;;;;15.08;9.49;13.35;8.96;8.85;9.44;1.00;1.00;50.00;100.00;15151.50;19763.00;-9.30;-2.47;6559.80;19763.00;0.43;1.00

-- N
select distinct sector_desc from fe_data_store.si_finecon2 where dateindex = 17409;
"Basic Materials"
"Energy"


      select
          dateindex
        , dateindexlwd
        , dateindexeom
        , dateindexeom::text dateindexeom_fct
        , 'sp_desc'::text collection_name01_fct
        ,     case when sp in ('500','400','600') then 'sp'::text else 'notsp'::text end sp_desc_fct
        , 'industry_desc'::text collection_name02_fct
        ,     industry_desc industry_desc_fct
        , sum(now_inbnd_stmtstat_sales_q1)  / nullif(sum(now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_mktcap), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_sales_q1), 0) * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        , count(now_inbnd_stmtid_dateindex)::numeric                                                                     count_now_inbnd_stmtstat_dateindex
        , count(now_inbnd_stmtid_dateindex)::numeric / nullif(count(last_inbnd_stmtid_dateindex)::numeric,0) * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
        , sum(mktcap) sum_mktcap
        , avg(pct_freeprice_ret_01m_ann * mktcap / nullif(sum_sp_industry_desc_mktcap,0) )  avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM
        , sum(now_inbnd_stmtstat_mktcap)                                                   sum_now_inbnd_stmtstat_mktcap
        , sum(now_inbnd_stmtstat_mktcap) / nullif(sum(last_inbnd_stmtstat_mktcap), 0)  rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
      from si_finecon2 where dateindexeom = 17378 and
        sp in ('500','400','600') and 
        industry_desc in ('Gold & Silver', 'Furniture & Fixtures', 'Oil & Gas Operations') and 
        adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
      group by dateindex, dateindexlwd, dateindexeom, sp_desc_fct, industry_desc_fct 
      order by dateindex, dateindexlwd, dateindexeom, sp_desc_fct, industry_desc_fct 



      select
          dateindex
        , dateindexlwd
        , dateindexeom
        , dateindexeom::text dateindexeom_fct
        , 'sp_desc'::text collection_name01_fct
        ,     case when sp in ('500','400','600') then 'sp'::text else 'notsp'::text end sp_desc_fct
        , 'industry_desc'::text collection_name02_fct
        ,     industry_desc industry_desc_fct
        , sum(now_inbnd_stmtstat_sales_q1)  / nullif(sum(now_inbnd_stmtstat_mktcap), 0)   * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_mktcap), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_sales_q1), 0) * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        , count(now_inbnd_stmtid_dateindex)::numeric                                                                     count_now_inbnd_stmtstat_dateindex
        , count(now_inbnd_stmtid_dateindex)::numeric / nullif(count(last_inbnd_stmtid_dateindex)::numeric,0) * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
        , sum(mktcap) sum_mktcap
        , avg(pct_freeprice_ret_01m_ann * mktcap / nullif(sum_sp_industry_desc_mktcap,0) )  avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM
        , sum(now_inbnd_stmtstat_mktcap)                                                   sum_now_inbnd_stmtstat_mktcap
        , sum(now_inbnd_stmtstat_mktcap) / nullif(sum(last_inbnd_stmtstat_mktcap), 0)  rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
      from si_finecon2 where dateindexeom = 17284 -- and
        -- sp in ('500','400','600') and 
        -- industry_desc in ('Gold & Silver', 'Furniture & Fixtures', 'Oil & Gas Operations') and 
        -- adr = 0 AND exchange <> 'O'::text  AND company !~~ '%iShares%'::text AND company !~~ '%Vanguard%'::text AND company !~~ 'SPDR'::text AND company !~~ '%PowerShares%'::text AND company !~~ '%Fund%'::text AND company !~~ '%Holding%'::text AND industry_desc !~~ '%Investment Service%'::text
      group by dateindex, dateindexlwd, dateindexeom, sp_desc_fct, industry_desc_fct 
      order by dateindex, dateindexlwd, dateindexeom, sp_desc_fct, industry_desc_fct 


select max(netinc_q1) from query01

select * from query01 where netinc_q1 > (select max(netinc_q1) from query01) - 100000;
--typo
--"15856_A1F87";"15856_A1F87";15856;15856;15856;"A1F87";"A1F87";"AMCRY";"Amcor Limited (ADR)";"NA";242601.10

alter table query01 alter column netinc_q1 type numeric(7, 2);

create unique index query01_dateindex_company_id_both_unqpkidx on query01(dateindex_company_id);

alter table         query01 add primary key(dateindex_company_id) using index query01_dateindex_company_id_both_unqpkidx;






select count(*) 

select count(*) from query01 where sp = '500' and dateindex = 17409;
--500

  -- THIS ONE --
select to_timestamp(sqi.dateindex*3600*24)::date date, sqi.dateindex, sum(sqi.netinc_q1) 
from query01 sqi 
  where sqi.sp = '500' group by sqi.dateindex  
-- THIS ONE DOES IT ALL BUT THE CHANGE IS MORE EXTREME ( JUST ONE QUARTER --

-- sum(sqi.netinc_q1)
-- sum(sqi.netinc_q1)

CRASH

"2007-07-31";13725;207844.80  - 200
"2007-08-31";13756;204885.00
"2007-09-28";13784;204127.60
"2007-10-31";13817;196016.40
"2007-11-30";13847;153393.00
"2007-12-31";13878;144264.10  # 25% DROP
"2008-01-31";13909;96099.10
"2008-02-29";13938;83851.70
"2008-03-31";13969;87995.10
"2008-04-30";13999;122380.00
"2008-05-30";14029;146447.80
"2008-06-30";14060;146118.10
"2008-07-31";14091;140710.30
"2008-08-29";14120;126035.40
"2008-09-30";14152;131715.00
"2008-10-31";14183;100331.70
"2008-11-28";14211;97717.30
"2008-12-31";14244;125766.90

LATE 2015 and EARLY 2016

"2015-07-31";16647;218305.30
"2015-08-28";16675;209493.20
"2015-08-31";16678;209670.70
"2015-09-30";16708;210828.80
"2015-10-30";16738;225729.90
"2015-11-30";16769;227192.40
"2015-12-31";16800;228641.00 # REALLY NET_INCOME IS INCREASING
"2016-01-29";16829;227860.60
"2016-02-29";16860;199791.70
"2016-03-31";16891;187993.00

RECENT

"2017-03-31";17256;229336.70
"2017-04-28";17284;235510.40
"2017-05-31";17317;245860.30
"2017-06-30";17347;247059.80
"2017-07-31";17378;247028.70
"2017-08-31";17409;248060.90


select to_timestamp(sqi.dateindex*3600*24)::date date, sqi.dateindex, sum(sqi.sales_q1) 
from query01 sqi 
  where sqi.sp = '500' group by sqi.dateindex  

-- sum(sqi.sales_q1)

CRASH

"2007-06-29";13693;2220364.50
"2007-07-31";13725;2295889.80
"2007-08-31";13756;2341184.50
"2007-09-28";13784;2337901.90
"2007-10-31";13817;2340256.90
"2007-11-30";13847;2341794.60
"2007-12-31";13878;2327384.00
"2008-01-31";13909;2375328.20
"2008-02-29";13938;2451416.50
"2008-03-31";13969;2455486.60 -- PEAK
"2008-04-30";13999;2412448.70
"2008-05-30";14029;2381816.40
"2008-06-30";14060;2367961.60
"2008-07-31";14091;2454722.20
"2008-08-29";14120;2515605.50 -- ACTUALLY DOING BETTER
"2008-09-30";14152;2475466.50
"2008-10-31";14183;2472123.90
"2008-11-28";14211;2424354.80
"2008-12-31";14244;2384337.60

LATE 2015 EARLY 2016

"2015-06-30";16616;2525017.80
"2015-07-31";16647;2579465.90
"2015-08-28";16675;2609792.30
"2015-08-31";16678;2609998.50
"2015-09-30";16708;2608765.80
"2015-10-30";16738;2608526.10
"2015-11-30";16769;2614113.10
"2015-12-31";16800;2603888.40
"2016-01-29";16829;2638974.80
"2016-02-29";16860;2726126.80
"2016-03-31";16891;2650235.60

RECENT

"2017-03-31";17256;2776622.60
"2017-04-28";17284;2666061.60
"2017-05-31";17317;2654173.80
"2017-06-30";17347;2662589.50
"2017-07-31";17378;2719591.10
"2017-08-31";17409;2737043.20



select to_timestamp(sqi.dateindex*3600*24)::date date, sqi.dateindex,  sum(sqi.netinc_q1) /  sum(sqi.sales_q1) * 100
from query01 sqi 
  where sqi.sp = '500' group by sqi.dateindex  

-- efficiences
-- sum(sqi.netinc_q1) /  sum(sqi.sales_q1) * 100


CRASH

LATE 2015 and EARLY 2016

"2016-01-29";16829;8.63443637279143400700
"2016-02-29";16860;7.32877502249711935600
"2016-03-31";16891;7.09344482430165831300
"2016-04-29";16920;6.81853012088782875600
"2016-05-31";16952;8.38492915612552136900
"2016-06-30";16982;8.30490799941533732500
"2016-07-29";17011;8.34394720596449843200
"2016-08-31";17044;8.12582852693722430100
"2016-09-30";17074;8.40416373722930569900
"2016-10-31";17105;8.81919141440904125200
"2016-11-30";17135;9.44548186668341245500
"2016-12-30";17165;9.44237401849661407500
"2017-01-31";17197;9.44591021984753720900
"2017-02-28";17225;8.20602739861340409000 DROP HERE BUT NO MAJOR PROBLEM
"2017-03-31";17256;8.25955605201801642000

RECENT

"2017-03-31";17256;8.25955605201801642000
"2017-04-28";17284;8.83364435390390079500
"2017-05-31";17317;9.26315752193771184100
"2017-06-30";17347;9.27892940312428934300
"2017-07-31";17378;9.08330300095481265500
"2017-08-31";17409;9.06309772531175247800

-- overpriced or not ( LOWER IS BETTER )

select to_timestamp(sqi.dateindex*3600*24)::date date, sqi.dateindex,  sum(sqi.mktcap) / sum(sqi.netinc_q1) 
from query01 sqi 
  where sqi.sp = '500' group by sqi.dateindex 

-- TRENDIG LOWER IS BETTER
-- sum(sqi.mktcap) / sum(sqi.netinc_q1)

"2007-07-31";13725;64.2426560587515300
"2007-08-31";13756;65.9450696732313249
"2007-09-28";13784;68.5811497318344016
"2007-10-31";13817;71.4272933285174098
"2007-11-30";13847;87.7721871271831179
"2007-12-31";13878;92.4715691568449808 DEAL GETTING WORSE AND VERY FAST
"2008-01-31";13909;130.3122401770672150
"2008-02-29";13938;143.5326522896971677
"2008-03-31";13969;134.6177025766207437
"2008-04-30";13999;102.2272495505801602
"2008-05-30";14029;86.2245503175875636
"2008-06-30";14060;79.3153148035732739
"2008-07-31";14091;81.7672771645003955
"2008-08-29";14120;92.3882790073265130
"2008-09-30";14152;80.6101841096306419
"2008-10-31";14183;88.0784338349694065
"2008-11-28";14211;83.0707735477750613
"2008-12-31";14244;64.8164771493930438

LATE 2015 EARLY 2016 ( DEAL GETTING BETTER: BUT PEOPLE WHERE SHOPPING )

"2015-06-30";16616;94.3769398581496338
"2015-07-31";16647;90.2133342616968072
"2015-08-28";16675;88.9111727731496774
"2015-08-31";16678;88.0740489729847804
"2015-09-30";16708;85.3368856626798616
"2015-10-30";16738;86.0554729346887586
"2015-11-30";16769;85.1840651359816614
"2015-12-31";16800;83.1847748216636561
"2016-01-29";16829;79.2168777752713721
"2016-02-29";16860;89.6217395417327146  DEAL GETTING BETTER THROUGH HERE
"2016-03-31";16891;101.6715308548722559 DEAL BEGINS NOT TO BE GOO
"2016-04-29";16920;110.4771796466496211
"2016-05-31";16952;92.9684675566215054

RECENT ( IMPROVING - BUT FLUTTERING MOMENTUM )

"2017-03-31";17256;94.2902562040877016
"2017-04-28";17284;92.6999780052176040
"2017-05-31";17317;89.6234154111094797
"2017-06-30";17347;89.4259369593920176
"2017-07-31";17378;91.2975318252494548
"2017-08-31";17409;90.2005713113191156

explain
select to_timestamp(qu.dateindex*3600*24)::date date, qu.dateindex, count(*) from query01 qu where qu.sp = '500' group by qu.dateindex
left join lateral (
  select to_timestamp(sqi.dateindex*3600*24)::date date, sqi.dateindex, sum(sqi.netinc_q1) 
  from query01 sqi 
    where sqi.sp = '500' group by sqi.dateindex  
) sq on true;

-- LEFT_OFF



select (lofactor3c(array_agg(price), array_agg(mktcap), array_agg(sales_q1), array_agg(netinc_q1))).*, row_number() over() 
from query01 where sp = '500';




select row_number()over(), dateindex_company_id  from query01 limit 100;

select row_number()over(), dateindex_company_id  from query01 where sp = '500';

select * from lofactor3c(price, mktcap, netinc_q1, sales_q1, k := 5)

select row_number() over (), lofactor3c(price, mktcap, netinc_q1, sales_q1, k := 5) from query01;


select * from si_finecon2 where company ~~ 'SPDR'::text order by dateindex;
select * from si_finecon2 where ticker  ~~ 'SPDR'::text order by dateindex;
select * from si_finecon2 where company_id  ~~ 'SPDR'::text order by dateindex;
-- nothing found
select * from si_finecon2 where company ~~ 'Microsoft'::text order by dateindex;
-- nohting found
select * from si_finecon2 where ticker  ~~ 'MSFT'::text order by dateindex;
-- YES "Microsoft Corporation"

select * from si_finecon2 where company like '%Microsoft%'::text order by dateindex;
-- YES

select * from si_finecon2 where company ilike '%microsoft%'::text order by dateindex;
-- YES


select * from si_finecon2 where company ilike '%spdr%'::text order by dateindex, company;
-- 1308 = approx 15to16 per MONTH

The key word ILIKE can be used instead of LIKE to make the match case-insensitive according to the active locale. 
This is not in the SQL standard but is a PostgreSQL extension.

  The operator ~~ is equivalent to LIKE, and ~~* corresponds to ILIKE. 
  There are also !~~ and !~~* operators that represent NOT LIKE and NOT ILIKE, respectively. All of these operators are PostgreSQL-specific.


-- look like have fullish "income statements" / "ballance sheets"
-- industry_desc: "Misc. Financial Services"
-- first entered 14911 last entered  15856(2013-05-31)

company !~~ 'SPDR'::text 
-- should have been -- FIX 
company !~~ '%SPDR%'

select * from si_finecon2 where company ~~ '%SPDR%'::text order by dateindex, company;
-- 4.7 seconds + 10 seconds to return to the pgAdimn

create index temp_si_finecon2_company on si_finecon2(company);
-- 4.1 seconds
explain
select * from si_finecon2 where company ~~ '%SPDR%'::text order by dateindex, company;
-- 4.7 seconds + 10 seconds to return to the pgAdimn ( not use index ... parallet sequential scan "  Sort Key: dateindex, company")
-- drop index temp_si_finecon2_company
create index temp_si_finecon2_index_company_spdr_idx on si_finecon2(dateindex, company) where company ~~ '%SPDR%'::text;
-- create time 1-second **** (IF I RUN AN SELECT dateindex, compay QUERY 2TWICE, then the TIME to create the indiex is worth IT)
-- 4.1 seconds + 10 seconds to return to the pgAdimn explain - "Index Scan using temp_si_finecon2_index_company_spdr_idx on si_finecon2  (cost=0.28..258.45 rows=64 width=1740)"
select * from si_finecon2 where company ~~ '%SPDR%'::text order by dateindex, company;
-- 
explain -- "Index Only Scan using temp_si_finecon2_index_company_spdr_idx on si_finecon2  (cost=0.28..108.13 rows=64 width=27)"
select dateindex, company from si_finecon2 where company ~~ '%SPDR%'::text order by dateindex, company;
-- instantanious (59ms)

-- drop index temp_si_finecon2_index_company_spdr_idx;
select dateindex, company from si_finecon2 where company ~~ '%SPDR%'::text order by dateindex, company;
-- (590ms)

-- last ONE: others after: ONE: 15856 ("2013-05-31")

"XLY";"Consumer Discretionary SPDR (E"
"XLE";"Energy Select Sector SPDR (ETF"
"XLF";"Financial Select Sector SPDR ("
"XLV";"Health Care SPDR (ETF)"
"XLI";"Sector Spdr Trust Sbi"
"DGT";"SPDR DJ Global Titans (ETF)"
"RWX";"SPDR DJ International Real Est"
"DIA";"SPDR Dow Jones Industrial Aver"
"ELR";"SPDR Dow Jones Large Cap ETF"
"EMM";"SPDR Dow Jones Mid Cap ETF"
"RWR";"SPDR Dow Jones REIT ETF"
"TMW";"SPDR Dow Jones Total Market (E"
"FEZ";"SPDR EURO STOXX 50 ETF"
"GLD";"SPDR Gold Trust (ETF)"
"KBE";"SPDR KBW Bank (ETF)"
"KCE";"SPDR KBW Capital Markets (ETF)"
"KRE";"SPDR KBW Regional Banking (ETF"
"MTK";"SPDR Morgan Stanley Technology"
"MDYG";"SPDR S&P 400 Mid Cap Growth ET"
"MDYV";"SPDR S&P 400 Mid Cap Value ETF"
"SPY";"SPDR S&P 500 ETF Trust"           ****
"SPYG";"SPDR S&P 500 Growth ETF"
"SPYV";"SPDR S&P 500 Value ETF"
"SLY";"SPDR S&P 600 Small Cap ETF"
"SLYG";"SPDR S&P 600 Small Cap Growth"
"SLYV";"SPDR S&P 600 Small Cap Value E"
"XBI";"SPDR S&P Biotech (ETF)"
"SDY";"SPDR S&P Dividend (ETF)"
"XHB";"SPDR S&P Homebuilders (ETF)"
"KIE";"SPDR S&P Insurance ETF"
"XME";"SPDR S&P Metals and Mining (ET"
"XES";"SPDR S&P Oil & Gas Equipt & Se"
"XOP";"SPDR S&P Oil & Gas Explore & P"
"XPH";"SPDR S&P Pharmaceuticals (ETF)"
"XRT";"SPDR S&P Retail (ETF)"
"XSD";"SPDR S&P Semiconductor (ETF)"
"FEU";"SPDR STOXX Europe 50 ETF"
"XLK";"Technology SPDR (ETF)"
"XLU";"Utilities SPDR (ETF)"



select distinct company from si_finecon2 where industry_desc = 'Misc. Financial Services' order by 1;

industry_desc = 'Misc. Financial Service' (since fall of 2010) -- TODO [ ] 2003-2010
---------------------------------------------------------------------------------------

-- mistake or (later) rename
"iGen Networks Corp"
"IGEN Networks Corp"
"Jupiter Enterprises Inc"
"Jupiter Enterprises, Inc."
"Komodo, Inc"
"Komodo, Inc."
"Peregrine Industries Inc"
"Peregrine Industries Inc(NDA)"
"Rahaxi Inc"
"Rahaxi, Inc."
"Resource America Inc"
"Resource America, Inc."
"Spi Energy Co Ltd"
"Spi Energy Co Ltd (ADR)"
"Star Energy Corp(NDA)"
"Star Energy Corporation"
"Stellar Resources Ltd"
"Stellar Resources Ltd(NDA)"
"Thrive World Wide Inc"
"Thrive World Wide, Inc."
"Tintic Gold Mining Co"
"Tintic Gold Mining Co(NDA)"
"Zaxis International Inc"
"Zaxis International, Inc."

--industry_desc = 'Misc. Financial Services'
--si_finecon2(late 2010) -- TODO [ ] 2003-2010
--%XXX% unless otherise noted

Capital
BlackRock
BLDRS
Holding
Cohen & Steers%
"Consumer Staples Select Sect."
Fund
Investment
"Direxion"
Dow%
Dreyfus
DWS
Dividen
  Dividend
Fnd
  Fund
Eaton Vance
Ventures
Empire Global ?
Everyware Global ?
Technologies
  Technology
FinTech Acquisition
  Acquisition
    Acquisit
First Trust
  Trust
Merger
Global%
Guggenheim
Helios
ING%
Invesco%
iShares%
John Hancock%
Kayne Anderson%
Total Return
Leg Mason% (just one)
Market Vectors
ETF
Merrill Lynch
Trus
"Montgomery Street Income Secur"
Morgan Stanley%
Holdi
  Holdin
"Income & Growth"
Neuberger Ber%
Income Fund
Nuveen%
Investment
  Investors
"Peoples Federal Bancshares, In"
Pimco%
PIMCO%
Pioneer%
PowerShares%
ProShares%
Putnam%
RevenueShares%
RMK%
ROI%
Royale%
Royce%
Rydex% (S&P derivatives)
Resource Corp
Investment Corp
SPDR%
Sunrise%
Templeton%
Tortoise%
United States%
Vanguard%
High Div
Wells Fargo%
Western Asset%
Whiting USA%
WisdomTree
Zweig

select distinct company 
from si_finecon2 
  where industry_desc = 'Misc. Financial Services' 
    and sp in ('500','400','600')
order by 1;
-- just 7
"Apollo Investment Corp."
"Broadridge Financial Solutions"
"Cash America International Inc"
"Cash America International, In"
"Encore Capital Group, Inc."
"FactSet Research Systems Inc."
"Prospect Capital Corporation"


select company from sipro_stage.si_ci_17074 where company ~~ '%SPDR%';
-- none
-- last ONE: others after: ONE: 15856 ("2013-05-31")
select company from sipro_stage.si_ci_14911 where company ~~ '%SPDR%';
-- 41 entries
select company from sipro_stage.si_ci_15856 where company ~~ '%SPDR%';
-- 38 entries


--query01
select dateindex,to_timestamp(dateindex*3600*24)::date dateindex_dt, sum(netinc_q1) 
from query01
where sp = '500'
group by dateindex 
order by dateindex;
 



--query01
select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt
  , count(netinc_q1)
  , sum(  netinc_q1) sum_netinc_q1
from query01
where sp = '500'
group by dateindex 
order by dateindex;
 

 dateindex | dateindex_dt | count | sum_netinc_q1
-----------+--------------+-------+---------------

     13544 | 2007-01-31   |   500 |     214253.70 -- base 100%
     13572 | 2007-02-28   |   500 |     210769.40
     13602 | 2007-03-30   |   501 |     212677.50
     13633 | 2007-04-30   |   500 |     212262.50
     13665 | 2007-06-01   |   499 |     203487.60
     13693 | 2007-06-29   |   500 |     200875.60
     13725 | 2007-07-31   |   500 |     207844.80
     13756 | 2007-08-31   |   500 |     204885.00
     13784 | 2007-09-28   |   501 |     204127.60 -- deep slope begins
     13817 | 2007-10-31   |   502 |     196016.40
     13847 | 2007-11-30   |   500 |     153393.00 -- less than base 25%
     13878 | 2007-12-31   |   500 |     144264.10
     13909 | 2008-01-31   |   501 |      96099.10 -- volitility begins
     13938 | 2008-02-29   |   500 |      83851.70
     13969 | 2008-03-31   |   499 |      87995.10
     13999 | 2008-04-30   |   500 |     122380.00
     14029 | 2008-05-30   |   500 |     146447.80
     14060 | 2008-06-30   |   499 |     146118.10
     14091 | 2008-07-31   |   501 |     140710.30 -- 5 months of improvement ( did not make 6th month ) -- what does the stress index SAY?
     14120 | 2008-08-29   |   501 |     126035.40 -- SO WITHOUT A 'STRESS INDEX' to PULL ME BACK I MAY HAVE INVESTED HERE
     14152 | 2008-09-30   |   500 |     131715.00
     14183 | 2008-10-31   |   500 |     100331.70
     14211 | 2008-11-28   |   499 |      97717.30
     14244 | 2008-12-31   |   499 |     125766.90
     14274 | 2009-01-30   |   501 |      -8082.70
     14302 | 2009-02-27   |   502 |    -108719.60
     14334 | 2009-03-31   |   500 |    -168475.40
     14364 | 2009-04-30   |   500 |     -20288.40
     14393 | 2009-05-29   |   500 |      72000.30
     14425 | 2009-06-30   |   500 |      81701.70
     14456 | 2009-07-31   |   500 |     102284.50
     14487 | 2009-08-31   |   502 |     123634.10
     14517 | 2009-09-30   |   500 |     125231.30
     14547 | 2009-10-30   |   501 |     134793.80 -- 6 months of improvement begns
     14578 | 2009-11-30   |   493 |     135620.50
     14609 | 2009-12-31   |   488 |     134276.10
     14638 | 2010-01-29   |   491 |     145711.00
     14666 | 2010-02-26   |   500 |     162585.50

     14790 | 2010-06-30   |   499 |     169745.50
     14820 | 2010-07-30   |   499 |     188715.60
     14852 | 2010-08-31   |   500 |     189183.40
     14882 | 2010-09-30   |   500 |     189311.00
     14911 | 2010-10-29   |   500 |     187074.60 -- NEVER needed
     14943 | 2010-11-30   |   500 |     186211.70 -- Quantitative Easing 2 (QE2, November 2010 to June 2011 )
     14974 | 2010-12-31   |   499 |     185564.70
     15005 | 2011-01-31   |   499 |     190213.10
     15033 | 2011-02-28   |   500 |     207694.60
     15064 | 2011-03-31   |   499 |     208617.10
     15093 | 2011-04-29   |   497 |     213393.70

-- late 2015 and early 2016

     16435 | 2014-12-31   |   497 |     253450.30 -- base 100%
     16465 | 2015-01-30   |   497 |     235686.00
     16493 | 2015-02-27   |   494 |     218375.40
     16525 | 2015-03-31   |   500 |     218151.60
     16555 | 2015-04-30   |   500 |     213007.60
     16584 | 2015-05-29   |   500 |     207649.80
     16616 | 2015-06-30   |   498 |     204074.20
     16647 | 2015-07-31   |   500 |     218305.30
     16675 | 2015-08-28   |   500 |     209493.20
     16678 | 2015-08-31   |   500 |     209670.70
     16708 | 2015-09-30   |   500 |     210828.80 -- loss from base 25%
     16738 | 2015-10-30   |   500 |     225729.90
     16769 | 2015-11-30   |   499 |     227192.40
     16800 | 2015-12-31   |   499 |     228641.00 -- improvement ( but no onw saw it)
     16829 | 2016-01-29   |   498 |     227860.60
     16860 | 2016-02-29   |   500 |     199791.70
     16891 | 2016-03-31   |   499 |     187993.00
     16920 | 2016-04-29   |   488 |     173652.00 -- yet low point (improvement begins)
     16952 | 2016-05-31   |   496 |     207973.10 
     16982 | 2016-06-30   |   500 |     207160.10
     17011 | 2016-07-29   |   482 |     214189.30
     17044 | 2016-08-31   |   497 |     211818.80
     17074 | 2016-09-30   |   499 |     219097.70
     17105 | 2016-10-31   |   489 |     230955.90
     17135 | 2016-11-30   |   497 |     252394.10

-- recent (mergeable with multpl - I hope so)

     17135 | 2016-11-30   |   497 |     252394.10
     17165 | 2016-12-30   |   499 |     252472.00
     17197 | 2017-01-31   |   480 |     254933.30
     17225 | 2017-02-28   |   494 |     225964.10 -- down
     17256 | 2017-03-31   |   495 |     229336.70 -- back up ( improvement begins )
     17284 | 2017-04-28   |   485 |     235510.40
     17317 | 2017-05-31   |   499 |     245860.30
     17347 | 2017-06-30   |   498 |     247059.80
     17378 | 2017-07-31   |   492 |     247028.70 -- IF I WHERE A BETTING MAN ( I would say GET IN)
     17409 | 2017-08-31   |   497 |     248060.90 -- but what about COMPETITION

-- anysmoother: last 2 quarters (6 months)?

select dateindex,to_timestamp(dateindex*3600*24)::date dateindex_dt
  , sum(netinc_q1) + sum(netinc_q2) sum_netinc_q1_p_q2

--query01
select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt
  , count(netinc_q1 + netinc_q2)
  , sum(  netinc_q1 + netinc_q2) sum_netinc_q1_p_q2
from query01
where sp = '500'
group by dateindex 
order by dateindex;

 dateindex | dateindex_dt | count | sum_netinc_q1_p_q2
-----------+--------------+-------+--------------------

     13544 | 2007-01-31   |   500 |          435072.10
     13572 | 2007-02-28   |   500 |          444041.40
     13602 | 2007-03-30   |   501 |          441921.60
     13633 | 2007-04-30   |   500 |          427558.10
     13665 | 2007-06-01   |   499 |          412374.30
     13693 | 2007-06-29   |   500 |          411656.60
     13725 | 2007-07-31   |   500 |          418044.40
     13756 | 2007-08-31   |   500 |          410469.60
     13784 | 2007-09-28   |   501 |          408131.20
     13817 | 2007-10-31   |   502 |          416331.60
     13847 | 2007-11-30   |   500 |          387833.50
     13878 | 2007-12-31   |   500 |          376663.70
     13909 | 2008-01-31   |   501 |          320691.40 -- 25 % drop is here
     13938 | 2008-02-29   |   500 |          277043.20
     13969 | 2008-03-31   |   499 |          269777.50
     13999 | 2008-04-30   |   500 |          265318.30
     14029 | 2008-05-30   |   500 |          238381.70 -- minimum
     14060 | 2008-06-30   |   499 |          248026.00 -- improvement begins
     14091 | 2008-07-31   |   501 |          271579.80
     14120 | 2008-08-29   |   501 |          279923.90
     14152 | 2008-09-30   |   500 |          288077.00 -- (4th) long term improvement up to here 
     14183 | 2008-10-31   |   500 |          271328.80 -- down
     14211 | 2008-11-28   |   499 |          246212.80 -- down
     14244 | 2008-12-31   |   499 |          288373.10 -- up
     14274 | 2009-01-30   |   501 |          156261.50 -- down
     14302 | 2009-02-27   |   502 |           41900.50 -- way down
     14334 | 2009-03-31   |   500 |          -40950.60 -- way down
     14364 | 2009-04-30   |   500 |          -94001.10
     14393 | 2009-05-29   |   500 |          -98672.00 -- max down
     14425 | 2009-06-30   |   500 |          -77924.60
     14456 | 2009-07-31   |   500 |          112002.30
     14487 | 2009-08-31   |   502 |          207688.10
     14517 | 2009-09-30   |   500 |          209621.20
     14547 | 2009-10-30   |   501 |          238516.20
     14578 | 2009-11-30   |   493 |          262445.10 -- six months of continual improvement
     14609 | 2009-12-31   |   488 |          261506.30 
     14638 | 2010-01-29   |   491 |          281346.30 
     14666 | 2010-02-26   |   500 |          316282.90

     14790 | 2010-06-30   |   499 |          336509.80
     14820 | 2010-07-30   |   499 |          352988.50
     14852 | 2010-08-31   |   500 |          359657.00
     14882 | 2010-09-30   |   500 |          361589.90 -- NEVER needed
     14911 | 2010-10-29   |   500 |          379489.70 -- Quantitative Easing 2 (QE2, November 2010 to June 2011 )
     14943 | 2010-11-30   |   500 |          381200.60
     14974 | 2010-12-31   |   499 |          380524.40
     15005 | 2011-01-31   |   499 |          406202.00
     15033 | 2011-02-28   |   500 |          421121.60
     15064 | 2011-03-31   |   499 |          418794.70
     15093 | 2011-04-29   |   497 |          407545.30

-- late 2015 and early 2016

     16435 | 2014-12-31   |   497 |          529370.60
     16465 | 2015-01-30   |   497 |          515125.60
     16493 | 2015-02-27   |   494 |          521047.90
     16525 | 2015-03-31   |   500 |          482591.20
     16555 | 2015-04-30   |   500 |          458069.60 -- 25% drop is already here ( earlier than the q1 alone conbination )
     16584 | 2015-05-29   |   500 |          426415.70
     16616 | 2015-06-30   |   498 |          425322.60
     16647 | 2015-07-31   |   500 |          438194.70
     16675 | 2015-08-28   |   500 |          419314.80
     16678 | 2015-08-31   |   500 |          419072.50 -- max loss
     16708 | 2015-09-30   |   500 |          421100.50
     16738 | 2015-10-30   |   500 |          453024.30
     16769 | 2015-11-30   |   499 |          456055.80
     16800 | 2015-12-31   |   499 |          456947.50 -- 6 months of improvement
     16829 | 2016-01-29   |   498 |          459037.60 -- max improvelment
     16860 | 2016-02-29   |   500 |          408825.30
     16891 | 2016-03-31   |   499 |          414409.40
     16920 | 2016-04-29   |   488 |          389244.60
     16952 | 2016-05-31   |   496 |          399738.60
     16982 | 2016-06-30   |   500 |          401788.30
     17011 | 2016-07-29   |   482 |          395855.40 -- 6 months of not improvement
     17044 | 2016-08-31   |   497 |          419246.60
     17074 | 2016-09-30   |   499 |          428624.10
     17105 | 2016-10-31   |   488 |          446939.00
     17135 | 2016-11-30   |   496 |          477091.30
     17165 | 2016-12-30   |   498 |          479170.00
     17197 | 2017-01-31   |   480 |          492987.10 -- max improvement ( after 6 months)

-- recently

     17197 | 2017-01-31   |   480 |          492987.10
     17225 | 2017-02-28   |   494 |          478068.10
     17256 | 2017-03-31   |   494 |          482650.30
     17284 | 2017-04-28   |   484 |          467629.80
     17317 | 2017-05-31   |   499 |          475079.80 -- improvement begins again
     17347 | 2017-06-30   |   498 |          478853.40
     17378 | 2017-07-31   |   492 |          479593.50
     17409 | 2017-08-31   |   497 |          491659.70 -- max improvement (only seen 3 mo)


-- adjusted by count to equal 500

--query01
select dateindex, to_timestamp(dateindex*3600*24)::date dateindex_dt
  , count(netinc_q1)     netinc_q1_ct
  , sum(  netinc_q1) sum_netinc_q1
  , sum(  netinc_q1)* 500.00 / count(netinc_q1) sum_adjct_netinc_q1
from query01
where sp = '500'
group by dateindex 
order by dateindex;

dateindex | dateindex_dt | netinc_q1_ct | sum_netinc_q1 |  sum_adjct_netinc_q1
-----------+--------------+--------------+---------------+------------------------
     12055 | 2003-01-03   |          500 |      77136.60 |     77136.600000000000
     12083 | 2003-01-31   |          500 |      26602.20 |     26602.200000000000
     12111 | 2003-02-28   |          500 |     -18051.60 |    -18051.600000000000
     12146 | 2003-04-04   |          500 |     -21003.70 |    -21003.700000000000
     12174 | 2003-05-02   |          500 |      52279.30 |     52279.300000000000
 ...
     17165 | 2016-12-30   |          499 |     252472.00 |    252977.955911823647
     17197 | 2017-01-31   |          480 |     254933.30 |    265555.520833333333
     17225 | 2017-02-28   |          494 |     225964.10 |    228708.603238866397 -- begin improve
     17256 | 2017-03-31   |          495 |     229336.70 |    231653.232323232323
     17284 | 2017-04-28   |          485 |     235510.40 |    242794.226804123711
     17317 | 2017-05-31   |          499 |     245860.30 |    246353.006012024048
     17347 | 2017-06-30   |          498 |     247059.80 |    248052.008032128514
     17378 | 2017-07-31   |          492 |     247028.70 |    251045.426829268293 -- apex here  ( 5th month )
     17409 | 2017-08-31   |          497 |     248060.90 |    249558.249496981891 -- going down  
(178 rows)

select count(1) over (partition by company_id) from query01; 
-- 27 seoncs

select count(sq.netinc_q1) over ( partition by sq.company_id) from
  (
  select company_id, dateindex, netinc_q1 
  from query01 
  order by company_id, dateindex ) sq
;
-- 25 seconds

-- BEGIN: RE-ORDRED FROM ABOVE

create index query01_partial_company_id_lt_15184_idx on query01(dateindex) where dateindex < 15184;

             drop function r_browser();
create or replace function r_browser()
  returns void as
$body$
  browser()
$body$
  language plr;

             drop function r_browser_win(in stat1 float8);
create or replace function r_browser_win(in stat1 float8)
  returns void as
$body$
  browser()
$body$
  language plr window;


             drop function r_browser_win4(in stat1 float8, in stat2 int, in stat3 text, in stat4 text);
create or replace function r_browser_win4(in stat1 float8, in stat2 int, in stat3 text, in stat4 text)
  returns void as
$body$
  browser()
$body$
  language plr window;


select r_browser_win4(netinc_q1, dateindex, industry_desc, state) over (partition by dateindex, industry_desc, state) 
from si_finecon2 where sp = '500' and dateindex = 17409;




Browse[1]> str(formals())
Dotted pair list of 10
 $ stat1   : symbol
 $ stat2   : symbol
 $ stat3   : symbol
 $ stat4   : symbol
 $ farg1   : symbol
 $ farg2   : symbol
 $ farg3   : symbol
 $ farg4   : symbol
 $ fnumrows: symbol
 $ prownum : symbol
Browse[1]>

Browse[1]> args(match.call)
function (definition = sys.function(sys.parent()), call = sys.call(sys.parent()),
    expand.dots = TRUE, envir = parent.frame(2L))

Browse[1]> match.call()
(PLR259845 <- function(stat1, stat2, stat3, stat4, farg1, farg2,
    farg3, farg4, fnumrows, prownum) {
    browser()
})(stat1 = 94.7, stat2 = 17409L, stat3 = "Advertising", stat4 = "NY",
    farg1 = c(94.7, 328.1), farg2 = c(17409L, 0L), farg3 = c("Advertising",
    "Advertising"), farg4 = c("NY", "NY"), fnumrows = 2, prownum = 1)
Browse[1]>

Browse[1]> ls.str(envir = environment())
farg1 :  num [1:2(1d)] 94.7 328.1
farg2 :  int [1:2(1d)] 17409 0
farg3 :  chr [1:2(1d)] "Advertising" "Advertising"
farg4 :  chr [1:2(1d)] "NY" "NY"
fnumrows :  num 2
prownum :  num 1
stat1 :  num 94.7
stat2 :  int 17409
stat3 :  chr "Advertising"
stat4 :  chr "NY"


select r_browser_win4(netinc_q1, dateindex, sector_desc, sp) over (partition by sp order by netinc_q1 rows 3 preceding) 
from si_finecon2 where  dateindex = 17409;

ALWAYS at fnumrowsMAX = prownumMAX

Browse[1]> ls.str(envir = environment())
farg1 :  num [1:2(1d)] -1690 -1396
farg2 :  int [1:2(1d)] 17409 0
farg3 :  chr [1:2(1d)] "Services" "Health Care"
farg4 :  chr [1:2(1d)] "400" "400"
fnumrows :  num 2
prownum :  num 2
stat1 :  num -1396
stat2 :  int 17409
stat3 :  chr "Health Care"
stat4 :  chr "400"
Browse[1]> c

  ...
Browse[1]> ls.str(envir = environment())
farg1 :  num [1:3(1d)] -1690 -1396 -715
farg2 :  int [1:3(1d)] 17409 0 17409
farg3 :  chr [1:3(1d)] "Services" "Services" "Services"
farg4 :  chr [1:3(1d)] "400" "400" "400"
fnumrows :  num 3
prownum :  num 3
stat1 :  num -715
stat2 :  int 17409
stat3 :  chr "Services"
stat4 :  chr "400"
Browse[1]>


select r_browser_win4(netinc_q1, dateindex, sector_desc, sp) over (partition by sp order by netinc_q1 ROWS BETWEEN 2 PRECEDING AND 3 FOLLOWING ) 
from si_finecon2 where  dateindex = 17409;


2 PRECEDING AND 3 FOLLOWING

num [1:4(1d)] -1690 -1396 -715 -386

fnumrows :  num 4
prownum :  num 1

ls.str(envir = environment())

num [1:5(1d)] -1690 -1396 -715 -386 -230
fnumrows :  num 5
prownum :  num 2

select avg(netinc_q1) over (partition by sp order by netinc_q1 ROWS BETWEEN 2 PRECEDING AND 3 FOLLOWING ) 
from si_finecon2 where  dateindex = 17409;


             drop function r_browser_avg_win1(in stat1 float8);
create or replace function r_browser_avg_win1(in stat1 float8)
  returns float8 as
$body$
  pg.thrownotice(paste0(farg1,collapse = "_"))
  mean(farg1, na.rm = TRUE)
$body$
  language plr window;


select r_browser_avg_win1(netinc_q1) over (partition by sp order by netinc_q1 ROWS BETWEEN 2 PRECEDING AND 3 FOLLOWING ) 
from si_finecon2 where  dateindex = 17409;





             drop function fe_data_store.r_lof2c(in col1 anyarray, in col2 anyarray, in k int);
create or replace function fe_data_store.r_lof2c(in col1 anyarray, in col2 anyarray, in k int)
  returns setof int as
$body$
  res <- Rlof::lof(data.frame(col1, col2), k = k)
  res <- order(res, decreasing = TRUE)
  return(res)
$body$
  language plr;


select row_number() over () from query01 where sp = '500' and dateindex = 17409;
  
select r_lof2c(array_agg(price), array_agg(mktcap), k := 5) from query01 where sp = '500'

ERROR: R interpreter expression evaluation error
SQL state: 22000
Detail: Error: cannot allocate vector of size 29.4 Gb
Context: In PL/R function r_lof4c


-- LONG TERM SAVE [ ] 
             drop function fe_data_store.r_lof4c(in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int);
create or replace function fe_data_store.r_lof4c(in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int)
  returns setof int as
$body$
  res <- Rlof::lof(data.frame(col1, col2, col3, col4), k = k)
  res <- order(res, decreasing = TRUE)
  return(res)
$body$
  language plr;

select * from (
select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap, netinc_q1, sales_q1
from query01 where sp = '500' and dateindex = 17409
) sq where rn in (
  -- 3.7 seconds
  select r_lof4c(array_agg(price), array_agg(mktcap), array_agg(netinc_q1), array_agg(sales_q1), k := 5) 
  from query01 where sp = '500' and dateindex = 17409 limit 19 -- top 5 worse
)


             drop function fe_data_store.r_lof4c(in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int);
create or replace function fe_data_store.r_mywindow(in col1 float8)
  returns setof int as
$body$
  res <- Rlof::lof(data.frame(col1, col2, col3, col4), k = k)
  res <- order(res, decreasing = TRUE)
  return(res)
$body$
  language plr window;



-- BUT UNSAFE? -- I WOULD ORER IT ... SEE LATER
select * from (
select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap, netinc_q1, sales_q1
from query01 where sp = '500' and dateindex = 17409
) sq where rn in (
  -- 3.7 seconds
  select r_lof4c(array_agg(price), array_agg(mktcap), array_agg(netinc_q1), array_agg(sales_q1), k := 5) 
  from query01 where sp = '500' and dateindex = 17409 limit 19 -- top 5 worse
) 

 rn  | dateindex | company_id | sp  | ticker |            company             |   price   |  mktcap   | netinc_q1 | sales_q1
-----+-----------+------------+-----+--------+--------------------------------+-----------+-----------+-----------+-----------
  36 |     17409 | 1118N      | 500 | BRK.A  | Berkshire Hathaway Inc.        | 271450.00 | 444831.10 |   4262.00 |  57518.00
  43 |     17409 | 15150      | 500 | CAH    | Cardinal Health Inc            |     67.46 |  21041.00 |    274.00 |  32966.00 # sales_q1 > mktcap (1of4)
  79 |     17409 | 2799N      | 500 | DRE    | Duke Realty Corp               |     29.72 |  10483.40 |   1210.00 |    189.40 # netinc_q1 > sales_q1
 137 |     17409 | 5181N      | 500 | KR     | Kroger Co                      |     21.87 |  19921.10 |    303.00 |  36285.00 # sales_q1 > mktcap (2of4)
 160 |     17409 | 5904N      | 500 | CVS    | CVS Health Corp                |     77.34 |  77645.20 |   1094.00 |  45685.00
 173 |     17409 | 6453N      | 500 | NBL    | Noble Energy, Inc.             |     23.77 |  11540.90 |  -1512.00 |   1059.00
 229 |     17409 | 8728N      | 500 | ANDV   | Andeavor                       |    100.15 |  15787.40 |     40.00 |   7849.00
 236 |     17409 | 90680      | 500 | TSN    | Tyson Foods, Inc.              |     63.30 |  22451.30 |    447.00 |   9850.00
 250 |     17409 | 9556N      | 500 | WMT    | Wal-Mart Stores Inc            |     78.07 | 238098.40 |   2899.00 | 123355.00
 251 |     17409 | 9560N      | 500 | WBA    | Walgreens Boots Alliance Inc   |     81.50 |  87512.50 |   1162.00 |  30118.00
 291 |     17409 | A05E1      | 500 | COST   | Costco Wholesale Corporation   |    156.74 |  67736.40 |    700.00 |  28860.00
 296 |     17409 | A0656      | 500 | XRAY   | DENTSPLY SIRONA Inc            |     56.57 |  12776.40 |  -1050.00 |    992.70
 310 |     17409 | A0827      | 500 | INCY   | Incyte Corporation             |    137.41 |  28442.80 |    -12.50 |    326.40
 316 |     17409 | A0A5A      | 500 | ABC    | AmerisourceBergen Corp.        |     80.25 |  17163.00 |     50.40 |  38707.10 # sales_q1 > mktcap (2of4)
 345 |     17409 | A168F      | 500 | MTD    | Mettler-Toledo International I |    605.09 |  15359.80 |    101.60 |    653.70
 383 |     17409 | A2144      | 500 | XOM    | Exxon Mobil Corporation        |     76.33 | 322443.80 |   3350.00 |  60825.00
 402 |     17409 | A2827      | 500 | JPM    | JPMorgan Chase & Co.           |     90.89 | 321316.60 |   6555.00 |  15650.00
 414 |     17409 | A2BE9      | 500 | JNJ    | Johnson & Johnson              |    132.37 | 351791.80 |   3827.00 |  18839.00
 425 |     17409 | A2E5B      | 500 | CNC    | Centene Corp                   |     88.85 |  14907.10 |    254.00 |  11954.00
(19 rows)




select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap, netinc_q1, sales_q1
from query01 where sp = '500' and netinc_q1 > sales_q1 and dateindex = 17409
1;17409;"2799N";"500";"DRE";"Duke Realty Corp";29.72;10483.40;1210.00;189.40
JUST ONE

select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap, netinc_q1, sales_q1
from query01 where sp = '500' and ( netinc_q1 > sales_q1 or  netinc_q1 > mktcap or sales_q1 > mktcap );
-- 3000 rows

select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap, netinc_q1, sales_q1
from query01 where sp = '500' and netinc_q1 > mktcap and dateindex = 17409
NONE

select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap, netinc_q1, sales_q1
from query01 where sp = '500' and sales_q1 > mktcap and dateindex = 17409
FOUR OF THEM
1;17409;"15150";"500";"CAH";"Cardinal Health Inc";67.46;21041.00;274.00;32966.00
2;17409;"5181N";"500";"KR";"Kroger Co";21.87;19921.10;303.00;36285.00  # DI NOT CATCH
3;17409;"5853N";"500";"MCK";"McKesson Corporation";149.31;30845.20;309.00;51051.00
4;17409;"A0A5A";"500";"ABC";"AmerisourceBergen Corp.";80.25;17163.00;50.40;38707.10

--- END RE-ORDERED FROM ABOVE




-- KEEP
             drop function fe_data_store.r_lof4c(in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int);
create or replace function fe_data_store.r_lof4c(in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int)
  returns setof int as
$body$
  set.seed(1L)
  res <- Rlof::lof(data.frame(col1, col2, col3, col4), k = k)
  if(length(res) > 0L) {
    res <- order(res, decreasing = TRUE)[seq(1,min(5,length(res)),1)]
  } else {
    res <- integer()
  }
  return(res)
$body$
  language plr;

-- KEEP
select * from 
(
select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap, netinc_q1, sales_q1
from query01 
  where sp = '500' and dateindex = 17409 order by company_id
) sq 
  where rn in 
  (
    select r_lof4c(array_agg(price), array_agg(mktcap), array_agg(netinc_q1), array_agg(sales_q1), k := 5) 
    from ( select price, mktcap, netinc_q1, sales_q1 from  query01 where sp = '500' and dateindex = 17409 order by company_id ) sq_r limit 19 -- top 5 worse
  )
43;17409;"15150";"500";"CAH";"Cardinal Health Inc";67.46;21041.00;274.00;32966.00
79;17409;"2799N";"500";"DRE";"Duke Realty Corp";29.72;10483.40;1210.00;189.40
137;17409;"5181N";"500";"KR";"Kroger Co";21.87;19921.10;303.00;36285.00
250;17409;"9556N";"500";"WMT";"Wal-Mart Stores Inc";78.07;238098.40;2899.00;123355.00
316;17409;"A0A5A";"500";"ABC";"AmerisourceBergen Corp.";80.25;17163.00;50.40;38707.10



-- BETTER and EXCELLENT --  mktcap/nullif(price,0) mktcap_o_price
select * from 
(
select row_number() over () rn, dateindex, company_id, sp, ticker, company, price, mktcap/nullif(price,0) mktcap_o_price, netinc_q1, sales_q1
from query01 
  where sp = '500' and dateindex = 17409 order by company_id
) sq 
  where rn in 
  (
    select r_lof4c(array_agg(price), array_agg(mktcap/nullif(price,0)), array_agg(netinc_q1), array_agg(sales_q1), k := 5) 
    from ( select price, mktcap, netinc_q1, sales_q1 from  query01 where sp = '500' and dateindex = 17409 order by company_id ) sq_r limit 19 -- top 5 worse
  )
18;17409;"05680";"500";"AAPL";"Apple Inc.";164.00;5144.7560975609756098;8717.00;45408.00
36;17409;"1118N";"500";"BRK.A";"Berkshire Hathaway Inc.";271450.00;1.6387220482593479;4262.00;57518.00
79;17409;"2799N";"500";"DRE";"Duke Realty Corp";29.72;352.7388963660834455;1210.00;189.40
122;17409;"4366N";"500";"HD";"Home Depot Inc";149.87;1179.8398612130513111;2672.00;28108.00
137;17409;"5181N";"500";"KR";"Kroger Co";21.87;910.8870598994055784;303.00;36285.00                  # DID CATCH
158;17409;"5853N";"500";"MCK";"McKesson Corporation";149.31;206.5849574710334204;309.00;51051.00
160;17409;"5904N";"500";"CVS";"CVS Health Corp";77.34;1003.9462115334884924;1094.00;45685.00
198;17409;"7060N";"500";"COP";"ConocoPhillips";43.66;1209.9816765918460834;-3440.00;6777.00
217;17409;"8181N";"500";"SO";"Southern Co";48.26;998.4376295068379610;-1381.00;5430.00
250;17409;"9556N";"500";"WMT";"Wal-Mart Stores Inc";78.07;3049.8065838350198540;2899.00;123355.00
251;17409;"9560N";"500";"WBA";"Walgreens Boots Alliance Inc";81.50;1073.7730061349693252;1162.00;30118.00
301;17409;"A069E";"500";"UNH";"UnitedHealth Group Inc";198.90;952.1794871794871795;2284.00;50053.00
316;17409;"A0A5A";"500";"ABC";"AmerisourceBergen Corp.";80.25;213.8691588785046729;50.40;38707.10
336;17409;"A13EF";"500";"AMZN";"Amazon.com, Inc.";980.60;474.0061187028349990;197.00;37955.00
371;17409;"A1E44";"500";"PCLN";"Priceline Group Inc";1852.08;48.0238434624854218;720.20;3024.60
383;17409;"A2144";"500";"XOM";"Exxon Mobil Corporation";76.33;4224.3390541071662518;3350.00;60825.00
424;17409;"A2E5A";"500";"CVX";"Chevron Corporation";107.62;1896.9392306262776436;1450.00;32761.00
441;17409;"A997D";"500";"GOOGL";"Alphabet Inc";955.24;679.3400611364683221;3524.00;26010.00
470;17409;"C8804";"500";"GM";"General Motors Company";36.54;1428.4948002189381500;1660.00;36984.00


select 
  sqo.* 
from 
  ( -- sqo
    select row_number() over () rn, sqi.* from query01 sqi
  ) sqo where sqo.rn < 1001 -- rn == query01_rn 
                            -- 518 != 2799 -- NO: SEEMS FAULT OF THE BUGGY row_number
                            --                so order IS *SLIGHTLY* randomized


                          --
select 
  sqo.* 
from 
  ( -- sqo           # order by query01_rn ( I get back all ones)
    select row_number() over () rn, sqi.* from query01 sqi order by query01_rn
  ) sqo where sqo.rn < 1001 
-- rn   query01_rn -- BAZAAR ENDING
-- 519 != 518    
-- 1000;998 -- bazzar ending
-- 518;2799
-- 592;2911



select query01_rn from query01 where query01_rn < 1000

517
518
519

591
592
593

-- so STORE row number ( created by R on si_ci load_order) AND QUERY it load_rn EXACTLY

-- also see
Simulating Row Number in PostgreSQL Pre 8.4 
http://www.postgresonline.com/article_pfriendly/79.html
  works
  select generate_series(1,(select count(1) from query01));
        
-- FIX inbound dbf ORDER BY ( new column load_order )

-- DIRECT
select 
  sqo.* 
from 
  ( -- sqo           
    select sqi.* from query01 sqi
  ) sqo where sqo.query01_rn < 1001 order by sqo.query01_rn
-- LONG SCAN OF 1.5 MILLION RECORDS .. 49 SECONDS 
-- MANY query01_rn perh each partition (datendex


select 
  sqo.* 
from 
  ( -- sqo                                                        # manual made
    select row_number() over (order by sqi.query01_rn) rn, sqi.* 
    from query01 sqi 
    where sqi.dateindex = 17409 and sp = '500' -- gaps because query01_rnOVER ENTIRE SET 
    order by sqi.dateindex, sqi.query01_rn
  ) sqo 
-- WORKS

-- final CORRECT ANSWER ( SO NEVER trust over() )
select 
  sqo.* 
from 
  ( -- sqo
    select row_number() over (order by sqi.query01_rn) rn, sqi.* 
    from query01 sqi where sqi.dateindex = 17409
  ) sqo where sqo.rn < 1001 


-- [X] ALREADY IN SIFINECON.01
-- BEGIN OUTLIER DETECTION --

-- KEEP 
  drop type r_lof4c_type;
create type r_lof4c_type as (rn int, val int);

-- KEEP  -- NON PARALLEL VERSION
             drop function fe_data_store.r_lof4c(in rn bigint[], in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int, in retcount int);
create or replace function fe_data_store.r_lof4c(in rn bigint[], in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int, in retcount int)
  returns setof r_lof4c_type as
$body$

  Rlof___f.dist.to.knn <- function(dataset, neighbors, ...){

      m.dist <- as.matrix( Rlof:::distmc(dataset, ...))
      num.col <- dim(m.dist)[2]
      l.knndist <- lapply(c(1:num.col), function(i) {
          order.x <- order(m.dist[, i])
          kdist <- m.dist[, i][order.x[neighbors + 1]]
          numnei <- sum(m.dist[, i] <= kdist)
          data.frame(v.order = order.x[2:numnei], v.dist = m.dist[,
              i][order.x[2:numnei]])
      })
      rm(m.dist)
      maxnum <- max(unlist(lapply(l.knndist, function(x) {
          dim(x)[1]
      })))
      i <- numeric()
      knndist <- NULL
      for(i in 1:num.col)
          {
              len <- dim(l.knndist[[i]])[1]
              RES <- c(l.knndist[[i]]$v.order, rep(NA, (maxnum - len)),
                  l.knndist[[i]]$v.dist, rep(NA, (maxnum - len)))
              knndist <- cbind(knndist,RES)
          }
      knndist
  }

  Rlof__lof <- function(data, k, ...){

      if (is.null(k))
          stop("k is missing")
      if (!is.numeric(k))
          stop("k is not numeric")
      data <- as.matrix(data)
      if (!is.numeric(data))
          stop("the data contains non-numeric data type")
      v.k <- as.integer(k)
      if (max(v.k) >= dim(data)[1])
          stop("the maximum k value has to be less than the length of the data")
      distdata <- Rlof___f.dist.to.knn(data, max(v.k), ...)
      p <- dim(distdata)[2L]
      dist.start <- as.integer((dim(distdata)[1])/2)
      dist.end <- dim(distdata)[1]
      ik <- numeric()
      m.lof <- NULL
      for(ik in v.k) 
      {
          lrddata <- Rlof:::f.reachability(distdata, ik)
          v.lof <- rep(0, p)
          for (i in 1:p) {
              nneigh <- sum(!is.na(distdata[c((dist.start + 1):dist.end),
                  i]) & (distdata[c((dist.start + 1):dist.end),
                  i] <= distdata[(dist.start + ik), i]))
              v.lof[i] <- sum(lrddata[distdata[(1:nneigh), i]]/lrddata[i])/nneigh
          }
          m.lof <- cbind(m.lof, v.lof)
      }
      if (length(v.k) > 1)
          colnames(m.lof) <- v.k
      return(m.lof)
  }

  set.seed(1L)
  res <- Rlof__lof(data = data.frame(col1, col2, col3, col4), k = k)
  if(length(res) > 0L) {
    res <- order(res, decreasing = TRUE)[seq(1,min(retcount,length(res)),1)]
  } else {
    res <- integer()
  }
  return(data.frame(rn[seq(1,min(retcount,length(res)),1)], res))
$body$
  language plr;


-- [?] ADD netinc < sales ( BUT caught Duke Realty: netinc > sales : SEE BELOW)
-- [?] ADD MUST have all entries(mktcap, netinc, sales, ncc, assets) over range search e.g. if go back 6 months THAT company MUST pass all filters
-- -- --- not refound in this one: 'AB NAMARO' missng data AND one ENTRY wrong MKTCAP ( seems NOT in 500: is s european bank (ABN Amro))
-- [X] ALREADY IN SIFINECON.01
-- KEEP
-- all s&p500 members ranked (most outlier-ish)(#1) to (least outlier-ish(#~500)
select 
    sq.dateindex, sq.company_id
  -- -- required in join ( not necessary to view )
  -- -- orderer ( not necessary to view ) ( but very useful in portability )
  , sq_r3.rn  sq_r3_rn
  , sq_r3.val sq_r3_val 
  ---- redundant of above
  --, sq.rn sq_rn
  -- -- redundant payload ( not necessary at all ) 
  , sq.sp, sq.ticker, sq.company, sq.price, sq.mktcap, sq.netinc_q1, sq.sales_q1 
from 
  ( -- sq, sq.rn(1,2,3,...)
  select 
      row_number() over (partition by qu2.dateindex order by qu2.company_id) rn
    , qu2.dateindex, qu2.company_id
    -- -- redundant payload
    , qu2.sp, qu2.ticker, qu2.company, qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1
  from query01 qu2
  where qu2.sp = '500' and qu2.dateindex = 17409 order by qu2.company_id
  ) sq, 
  ( -- sq_r3 -- sq_r3.rn(1,2,3,...) sq_r3.val(137,316,43,...)
    select (sq_r2.out).* --sq_r2.rn(NEED TO CARRY), sq_r2.val 
    from
    ( -- sq_r2
      select r_lof4c(array_agg(sq_r.rn), array_agg(sq_r.price), array_agg(sq_r.mktcap), array_agg(sq_r.netinc_q1), array_agg(sq_r.sales_q1), k := 5, retcount := 20000) as out 
      from 
      ( -- sq_r
        select 
            row_number() over (partition by qu.dateindex order by qu.company_id) rn -- random
          ,                                           qu.price,              qu.mktcap,              qu.netinc_q1,              qu.sales_q1 
        from  query01 qu
        where qu.sp = '500' and qu.dateindex = 17409 order by qu.company_id 
      ) sq_r  
    ) sq_r2
  ) sq_r3
where sq.rn = sq_r3.val
order by sq_r3.rn
-- ... k:= 20000(max rows of rowcount is returned)

-- TECHNOLOGY:WORKS
-- LATERAL TO GET OUTLIER SP RECORDS IN THE DATABASE
select                quo2.dateindex,      sro2.company_id ,sro2.query_rnk, sro2.sr_val
from ( select distinct quo.dateindex from query01 quo where quo.sp = '500' order by quo.dateindex ) quo2
join lateral 
  ( -- sro2
    -- COPY AND PASTE FROM ABOVE ( replace ".dateindex = 17409" using ".dateindex = quo2.dateindex"
    -- WORKS
  ) sro2 on true
--30 seconds -> 88,000 rows (GOOD)

-- KEEP [ ] 
-- all s&p500 members except these (20) ones ( and I do not care about tracking 'rank' )
select 
    qu3.dateindex, qu3.company_id
  , qu3.sp, qu3.ticker, qu3.company, qu3.price, qu3.mktcap, qu3.netinc_q1, qu3.sales_q1
from query01 qu3
where qu3.sp = '500' and qu3.dateindex = 17409 and ( qu3.dateindex, qu3.company_id ) not in 
  ( 
  -- retcount:= 20
  ) -- xyz: must not be an alias 'here'
order by qu3.dateindex, qu3.company_id
-- 480 rows









-- END OUTLIER DETECTION (THIS WORKS) --

-- is date_eq0 actually useful?
select dateindex, company_id, company, date_eq0, date_eq0 - perend_q1 diff ,perend_q1, perend_q2 
from fe_data_store.si_finecon2
where ticker = 'AAPL'
order by dateindex, company_id, company;
-- traditionally date_eq0 was 5 to 0 days later than perend_q1
-- but sometimes missded -90, -87
-- last 4 months -1
--   previous 4 months: 0



select sq.* from 
(
  select dateindex, company_id, ticker, company, mktcap, sp, adr
    , split_date
    , to_timestamp(split_date * 3600 *24 )::date split_date_dt
    , extract(dow from to_timestamp(split_date * 3600 *24 )) split_date_dow
    , date_eq0
    , to_timestamp(date_eq0  * 3600 *24 )::date date_eq0_dt
    , extract(dow from to_timestamp(date_eq0 * 3600 *24 )) date_eq0_dow
    , perend_q1
    , to_timestamp(perend_q1  * 3600 *24 )::date perend_q1_dt
    , extract(dow from to_timestamp(perend_q1 * 3600 *24 )) perend_q1_dow
  from fe_data_store.si_finecon2
  order by dateindex, company_id, company
) sq where sq.split_date_dow in (0,6) order by sq.split_date_dt
limit 1000;
-- any Sunday (0) to Saturday (6) split ( and at the end of the month )

-- through 2010 oct
"DFCO";"Dalrada Financial Corporation";0.00;"NA";0;13407;"2006-09-16";6
"CHTL";"China Tel Group, Inc";81.70;"NA";0;13484;"2006-12-02";6  (close to eom )
"VELA";"VelaTel Global Communications";71.70;"NA";0;13484;"2006-12-02" (close to eom )
"DSEN";"Datascension, Inc.";0.10;"NA";0;14464;"2009-08-08"
"ADFI";"Asian Development Frontier Inc";5.70;"NA";0;15619;"2012-10-06";6
"BSKY";"Blue SKY Petroleum Inc";51.10;"NA";0;15619;"2012-10-06";6
"PRDC";"Alternative Investment Corp";0.40;"NA";0;16774;"2015-12-05";6
"EMITF";"Elbit Imaging Ltd";31.20;"NA";0;16978;"2016-06-26";0

select sq.* from 
(
  select dateindex, company_id, ticker, company, mktcap, sp, adr
    , split_date
    , to_timestamp(split_date * 3600 *24 )::date split_date_dt
    , extract(dow from to_timestamp(split_date * 3600 *24 )) split_date_dow
    , date_eq0
    , to_timestamp(date_eq0  * 3600 *24 )::date date_eq0_dt
    , extract(dow from to_timestamp(date_eq0 * 3600 *24 )) date_eq0_dow
    , perend_q1
    , to_timestamp(perend_q1  * 3600 *24 )::date perend_q1_dt
    , extract(dow from to_timestamp(perend_q1 * 3600 *24 )) perend_q1_dow
  from fe_data_store.si_finecon2
  order by dateindex, company_id, company
) sq where sq.date_eq0_dow in (0,6) order by sq.date_eq0_dt
limit 10000; -- MANY AT THE LAST CALENDAR DAY ON A SATURDAY/SUNDAY
-- any Sunday (0) to Saturday (6) split ( and at the end of the month )
--80,000 of 1.5 million (7%)

select count(*) from 
(
  select dateindex, company_id, ticker, company, mktcap, sp, adr
    , split_date
    , to_timestamp(split_date * 3600 *24 )::date split_date_dt
    , extract(dow from to_timestamp(split_date * 3600 *24 )) split_date_dow
    , date_eq0
    , to_timestamp(date_eq0  * 3600 *24 )::date date_eq0_dt
    , extract(dow from to_timestamp(date_eq0 * 3600 *24 )) date_eq0_dow
    , perend_q1
    , to_timestamp(perend_q1  * 3600 *24 )::date perend_q1_dt
    , extract(dow from to_timestamp(perend_q1 * 3600 *24 )) perend_q1_dow
  from fe_data_store.si_finecon2
  order by dateindex, company_id, company
) sq where sq.perend_q1_dow in (0,6) order by sq.perend_q1_dt
limit 10000;
-- 197,000 of 1.5 million


-- relative size mktcap vs assets

select avg(mktcap/nullif(assets_q1,0)) from fe_data_store.si_finecon2
where sp ='500';


select '--alter table fe_data_store.si_finecon2 drop column ' || column_name || ';' from information_schema.columns where table_schema = 'fe_data_store' and table_name = 'si_finecon2' and column_name like '%_now_last%';

 
         drop table fe_data_store.instruments;
         

         create table if not exists fe_data_store.instruments
         (
         dateindex integer,
         dateindexyear integer,
         dateindexyearmonth integer,
         dateindexmonthsincebirth integer,
         dateindexmonth integer,
         dateindexlbd integer,
         dateindexlwd integer,
         dateindexeom integer,
         instrument text,
         instrument_value numeric(8,2),
         prchg_04w_ann numeric(8,2),
         prchg_13w_ann numeric(8,2),
         prchg_26w_ann numeric(8,2),
         prchg_52w_ann numeric(8,2)
         );

select distinct dateindexlbd from fe_data_store.si_finecon2;
17317
17347 # M
17378 # M
17409 # M


select distinct dateindexp01lbd from fe_data_store.si_finecon2;
17284
17378 # M
17347 # M
17317 # M



select distinct dateindexmonthsincebirth from fe_data_store.si_finecon2;

select distinct dateindexeom from fe_data_store.si_finecon2;

zoo::as.Date

select * from fe_data_store.si_finecon2 where ticker = 'AAPL' order by dateindex;



select dateindex
   sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 
,  sp_desc__sp500____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 
,  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 
,  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 
,  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 
,  sp_desc__sp500____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 
,  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 
,  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 
,  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 
from fe_data_store.si_finecon2_aggregates order by dateindex;


-- FIGURE OUT TOMORROW WHY? 
explain
select distinct dateindex from si_finecon2 order by dateindex desc;

--delete from si_finecon2 where dateindex = 16675;
--delete from si_finecon2_aggregates where dateindex = 16675;

--delete from si_finecon2 where dateindex > 12384 
-- only did after: 15184 17347 17378 17409
--delete from si_finecon2_aggregates where dateindex > 12384 

-- 
--YES: THAT IS HOW TO DO IT repair_compact/coalesce_reindex--
--
--vacuum analyze verbose  fe_data_store.si_finecon2;
--reindex (verbose) table fe_data_store.si_finecon2;

--SOLUTION: MACHINE IS HOSED AND INDEXES NOT WORKING---
--check indexes if not working
--  explain
--  select distinct dateindex from si_finecon2 order by dateindex desc;
--TYPICALLY LESS THAN 20 MINTUES ( BUT 'AS LONG AS IT TAKES' )
--vacuum analyze verbose fe_data_store.si_finecon2;reindex (verbose) table fe_data_store.si_finecon2;
--check indexes if not working
--explain
--select distinct dateindex from si_finecon2 order by dateindex desc;


select relid::regclass, * from pg_stat_progress_vacuum;




-- TEMPORARY.sql

-- EXPERIMENT
set effective_cache_size to '6144MB';  -- os + 'shared_buffers'

set search_path to fe_data_store;
-- et search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'






-- KEEP  -- NON PARALLEL VERSION
             drop function fe_data_store.r_lof4c(in rn bigint[], in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int, in retcount int);
create or replace function fe_data_store.r_lof4c(in rn bigint[], in col1 anyarray, in col2 anyarray, in col3 anyarray, in col4 anyarray, in k int, in retcount int)
             drop function r_lof(in rn bigint[], in cols numeric[][], in k int, in retcount int);
create or replace function r_lof(in rn bigint[], in cols numeric[][], in k int, in retcount int)
  returns table(rn int, val int) as
$body$

  Rlof___f.dist.to.knn <- function(dataset, neighbors, ...){

      m.dist <- as.matrix( Rlof:::distmc(dataset, ...))
      num.col <- dim(m.dist)[2]
      l.knndist <- lapply(c(1:num.col), function(i) {
          order.x <- order(m.dist[, i])
          kdist <- m.dist[, i][order.x[neighbors + 1]]
          numnei <- sum(m.dist[, i] <= kdist)
          data.frame(v.order = order.x[2:numnei], v.dist = m.dist[,
              i][order.x[2:numnei]])
      })
      rm(m.dist)
      maxnum <- max(unlist(lapply(l.knndist, function(x) {
          dim(x)[1]
      })))
      i <- numeric()
      knndist <- NULL
      for(i in 1:num.col)
          {
              len <- dim(l.knndist[[i]])[1]
              RES <- c(l.knndist[[i]]$v.order, rep(NA, (maxnum - len)),
                  l.knndist[[i]]$v.dist, rep(NA, (maxnum - len)))
              knndist <- cbind(knndist,RES)
          }
      knndist
  }

  Rlof__lof <- function(data, k, ...){

      if (is.null(k))
          stop("k is missing")
      if (!is.numeric(k))
          stop("k is not numeric")
      data <- as.matrix(data)
      if (!is.numeric(data))
          stop("the data contains non-numeric data type")
      v.k <- as.integer(k)
      if (max(v.k) >= dim(data)[1])
          stop("the maximum k value has to be less than the length of the data")
      distdata <- Rlof___f.dist.to.knn(data, max(v.k), ...)
      p <- dim(distdata)[2L]
      dist.start <- as.integer((dim(distdata)[1])/2)
      dist.end <- dim(distdata)[1]
      ik <- numeric()
      m.lof <- NULL
      for(ik in v.k) 
      {
          lrddata <- Rlof:::f.reachability(distdata, ik)
          v.lof <- rep(0, p)
          for (i in 1:p) {
              nneigh <- sum(!is.na(distdata[c((dist.start + 1):dist.end),
                  i]) & (distdata[c((dist.start + 1):dist.end),
                  i] <= distdata[(dist.start + ik), i]))
              v.lof[i] <- sum(lrddata[distdata[(1:nneigh), i]]/lrddata[i])/nneigh
          }
          m.lof <- cbind(m.lof, v.lof)
      }
      if (length(v.k) > 1)
          colnames(m.lof) <- v.k
      return(m.lof)
  }

  set.seed(1L)
  res <- Rlof__lof(data = as.data.frame(t(cols)), k = k)
  if(length(res) > 0L) {
    res <- order(res, decreasing = TRUE)[seq(1,min(retcount,length(res)),1)]
  } else {
    res <- integer()
  }
  return(data.frame(rn[seq(1,min(retcount,length(res)),1)], res))
$body$
  language plr;


  # res <- Rlof__lof(data = data.frame(col1, col2, col3, col4), k = k)

                                                                                                                          -- , array_agg(sq_r.sales_q1)
            select r_lof4c(array_agg(sq_r.rn), array_agg(sq_r.price), array_agg(sq_r.mktcap), array_agg(sq_r.netinc_q1), k := 5, retcount := 2147483647) as out 
            from 
            ( -- sq_r
              select 
                  row_number() over (partition by qu.dateindex order by qu.company_id) rn -- random
                ,                                           qu.price,              qu.mktcap,              qu.netinc_q1 -- ,              qu.sales_q1 
              from  si_finecon2 qu
              where qu.sp = '500' and qu.dateindex = 12055 order by qu.company_id 
            ) sq_r  




select array[array_agg(sq.cyl),array_agg(sq.hp)] from ( select mt.* from mtcars mt limit 3) sq;
         array
------------------------
 {{6,6,4},{110,110,93}}
(1 row)


select array[[1,2,3],[4,5,6]];
       array
-------------------
 {{1,2,3},{4,5,6}}
(1 row)


select array[array_agg(sq.cyl),array_agg(sq.hp)]::numeric[] from ( select mt.* from mtcars mt limit 3) sq;

         array
------------------------
 {{6,6,4},{110,110,93}}
(1 row)




create or replace function fe_data_store.r_lof4c(in rn bigint[], in cols numeric[][], in k int, in retcount int)

res <- Rlof__lof(data = as.data.frame(t(cols)), k = k)


                                                                                                                          -- , array_agg(sq_r.sales_q1)
            select r_lof4c( rn := array_agg(sq_r.rn), cols := array[array_agg(sq_r.price), array_agg(sq_r.mktcap), array_agg(sq_r.netinc_q1), array_agg(sq_r.sales_q1)], k := 5, retcount := 2147483647) as out 
            from 
            ( -- sq_r
              select 
                  row_number() over (partition by qu.dateindex order by qu.company_id) rn -- random
                ,                                           qu.price,              qu.mktcap,              qu.netinc_q1    ,              qu.sales_q1 
              from  si_finecon2 qu
              where qu.sp = '500' and qu.dateindex = 12055 order by qu.company_id 
            ) sq_r  





-- NO: IT DOES NOT GARANTEE ODRER
--Does UNION ALL guarantee the order of the result set [duplicate]
--https://stackoverflow.com/questions/15766359/does-union-all-guarantee-the-order-of-the-result-set

-- WORKING CODE KEEP [ ]


-- CURRENTLY NOT USED ANYWHWERE(YET) --

             drop function r_lof(in rn bigint[], in cols float8[][], in k int, in retcount int);
create or replace function r_lof(in rn bigint[], in cols float8[][], in k int, in retcount int)
  returns table(rn int, val int) as
$body$

  # finds outliers using k neighbors

  # Slightly re-written by Andre Mikulec: pl/r author removed parallelism
  # based entirely on 
  # Rlof
  # Version:	1.1.1
  # Published:	2015-09-17
  # https://cran.r-project.org/web/packages/Rlof/index.html

  # args

  # rn       - unique integer id for the row: currently: only tested: row_number(): 1,2,3, . . . , n
  # cols     - numeric columns of the data.frame
  #          - can be sent by array[array_agg(col1),array_agg(col2), ... ]
  # k        - nearest neighbors
  # retcount - number of ranked rows to return
  #          - to return 'all' of the data, enter the PostgreSQL 'integer max'  
  #          - i.e.  retcount := 2147483647

  # return
  
  # table(rn int, val int) -- array of pairs 
  # rn                     -- final ranking (lower values are more outlier-ish (goal))
  # val                    -- the original input rn of "r_lof( rn := array_agg(.), ... )"

  # example

  # 
  # select s.rn_mtcars, s.car, st.rn car_outlier_rnk, s.mpg, s.cyl, s.disp
  # from ( -- s 
  #   select sq.car, sq.mpg, sq.cyl, sq.disp, row_number() over(order by sq.car) rn_mtcars
  #   from ( -- sq
  #     select mt."row.names" car, mt.mpg, mt.cyl, mt.disp from mtcars mt order by car 
  #   ) sq
  # ) s
  # , 
  # ( --st
  #   select rn(r_lof_res.out), val(r_lof_res.out)
  #   from ( -- r_lof_res
  #     select r_lof( rn := array_agg(rd.rn_mtcars), cols := array[array_agg(rd.mpg), array_agg(rd.cyl), array_agg(rd.disp)], k := 5, retcount := 2147483647) as out
  #     from ( -- rd
  #       select sq.car, sq.mpg, sq.cyl, sq.disp, row_number() over(order by sq.car) rn_mtcars
  #       from ( -- sq
  #         select mt."row.names" car, mt.mpg, mt.cyl, mt.disp from mtcars mt order by car 
  #       ) sq
  #     ) rd
  #   ) r_lof_res
  # ) st
  # where s.rn_mtcars = st.val
  # order by car_outlier_rnk;
  # 
  #  rn_mtcars |         car         | car_outlier_rnk | mpg  | cyl | disp
  # -----------+---------------------+-----------------+------+-----+-------
  #         31 | Valiant             |               1 | 18.1 |   6 |   225 -- outlier: only 6 cyl over 168
  #          2 | Cadillac Fleetwood  |               2 | 10.4 |   8 |   472
  #         15 | Lincoln Continental |               3 | 10.4 |   8 |   460
  # ...

  Rlof___f.dist.to.knn <- function(dataset, neighbors, ...){

      m.dist <- as.matrix( Rlof:::distmc(dataset, ...))
      num.col <- dim(m.dist)[2]
      l.knndist <- lapply(c(1:num.col), function(i) {
          order.x <- order(m.dist[, i])
          kdist <- m.dist[, i][order.x[neighbors + 1]]
          numnei <- sum(m.dist[, i] <= kdist)
          data.frame(v.order = order.x[2:numnei], v.dist = m.dist[,
              i][order.x[2:numnei]])
      })
      rm(m.dist)
      maxnum <- max(unlist(lapply(l.knndist, function(x) {
          dim(x)[1]
      })))
      i <- numeric()
      knndist <- NULL
      for(i in 1:num.col)
          {
              len <- dim(l.knndist[[i]])[1]
              RES <- c(l.knndist[[i]]$v.order, rep(NA, (maxnum - len)),
                  l.knndist[[i]]$v.dist, rep(NA, (maxnum - len)))
              knndist <- cbind(knndist,RES)
          }
      knndist
  }

  Rlof__lof <- function(data, k, ...){

      if (is.null(k))
          stop("k is missing")
      if (!is.numeric(k))
          stop("k is not numeric")
      data <- as.matrix(data)
      if (!is.numeric(data))
          stop("the data contains non-numeric data type")
      v.k <- as.integer(k)
      if (max(v.k) >= dim(data)[1])
          stop("the maximum k value has to be less than the length of the data")
      distdata <- Rlof___f.dist.to.knn(data, max(v.k), ...)
      p <- dim(distdata)[2L]
      dist.start <- as.integer((dim(distdata)[1])/2)
      dist.end <- dim(distdata)[1]
      ik <- numeric()
      m.lof <- NULL
      for(ik in v.k) 
      {
          lrddata <- Rlof:::f.reachability(distdata, ik)
          v.lof <- rep(0, p)
          for (i in 1:p) {
              nneigh <- sum(!is.na(distdata[c((dist.start + 1):dist.end),
                  i]) & (distdata[c((dist.start + 1):dist.end),
                  i] <= distdata[(dist.start + ik), i]))
              v.lof[i] <- sum(lrddata[distdata[(1:nneigh), i]]/lrddata[i])/nneigh
          }
          m.lof <- cbind(m.lof, v.lof)
      }
      if (length(v.k) > 1)
          colnames(m.lof) <- v.k
      return(m.lof)
  }

  set.seed(1L)
  res <- Rlof__lof(data = as.data.frame(t(cols)), k = k)
  if(length(res) > 0L) {
    res <- order(res, decreasing = TRUE)[seq(1,min(retcount,length(res)),1)]
  } else {
    res <- (data.frame(rn = integer(), val = integer()))[FALSE,,drop = FALSE]
    return(res)
  }
  return(data.frame(rn[seq(1,min(retcount,length(res)),1)], res))
$body$
  language plr;


-- WORKING IMPLEMENTATION (KEEP) [ ]

-- 12055;"1748N";11;92;"500";"CTL";"CenturyTel, Inc.";31.04;4426.60;607.60;524.50;-7.80
-- but if I add ncc_q1 then it falls from 11 to 19

select distinct on (r_lof_and_bads_ordered.dateindex, r_lof_and_bads_ordered.company_id)  
    r_lof_and_bads_ordered.dateindex
  , r_lof_and_bads_ordered.company_id
  , r_lof_and_bads_ordered_rn sp500_outlier_rnk
from ( -- r_lof_and_bads_ordered
  select r_lof_and_bads.*, row_number() over (partition by r_lof_and_bads.dateindex order by r_lof_and_bads.sq_r3_rn) r_lof_and_bads_ordered_rn
  from ( -- r_lof_and_bads
    select *
    from ( -- r_lof
      -- # (price, mktcap, netinc_q1, sales_q1)
      -- # qu2.sp = '500' and qu2.dateindex = 12055  # 1748N # sq_r2_rn == 11, sq_r3_rn == 92 # "CenturyTel, Inc." ( netinc_q1 > sales_q1 )
      select
          sq.dateindex, sq.company_id
        -- -- required in join ( not necessary to view )
        -- -- orderer ( not necessary to view ) ( but very useful in portability )
        , sq_r3.rn  sq_r3_rn
        , sq_r3.val sq_r3_val 
        ---- redundant of above
        --, sq.rn sq_rn
        -- -- redundant payload ( not necessary at all ) 
        , sq.sp, sq.ticker, sq.company, sq.price, sq.mktcap, sq.netinc_q1, sq.sales_q1, sq.ncc_q1, sq.assets_q1
      from 
        ( -- sq, sq.rn(1,2,3,...)
        select 
            row_number() over (partition by qu2.dateindex order by qu2.company_id) rn
          , qu2.dateindex, qu2.company_id
          -- -- redundant payload
          , qu2.sp, qu2.ticker, qu2.company, qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1, qu2.ncc_q1, qu2.assets_q1
        from si_finecon2 qu2                  -- 12055
        where qu2.sp = '500' and qu2.dateindex = 17409 order by qu2.company_id
        ) sq, 
        ( -- sq_r3 -- sq_r3.rn(1,2,3,...) sq_r3.val(137,316,43,...)
          select (sq_r2.out).* --sq_r2.rn(NEED TO CARRY), sq_r2.val 
          from
          ( -- sq_r2                                                                                                                                               -- testing: retcount := 20 xor 'all of the data'
            select 
                r_lof( rn := array_agg(sq_r.rn)
              , cols := 
                  array[
                      array_agg(sq_r.price),    array_agg(sq_r.mktcap), array_agg(sq_r.netinc_q1)
                    , array_agg(sq_r.sales_q1), array_agg(sq_r.ncc_q1)
                  ]
              , k := 5, retcount := 2147483647) as out 
            from 
            ( -- sq_r
              select 
                  row_number() over (partition by qu.dateindex order by qu.company_id) rn -- random
                , case when qu.price     is not null then qu.price::float8  else '-Infinity'::float8 end price
                , case when qu.mktcap    is not null then qu.mktcap::float8 else '-Infinity'::float8 end mktcap
                , case when qu.netinc_q1 is not null then qu.netinc_q1      else '-Infinity'::float8 end netinc_q1
                , case when qu.sales_q1  is not null then qu.sales_q1       else '-Infinity'::float8 end sales_q1
                , case when qu.ncc_q1    is not null then qu.ncc_q1         else '-Infinity'::float8 end ncc_q1
                -- NOTE: Bear Stearns  assets_q1 == 0.00
                , case when qu.assets_q1 is not null then qu.assets_q1      else '-Infinity'::float8 end assets_q1
              from  si_finecon2 qu
              where qu.sp = '500' and qu.dateindex = 12055 order by qu.company_id 
            ) sq_r  
          ) sq_r2
        ) sq_r3
      where sq.rn = sq_r3.val
      order by sq_r3.rn -- limit 10 -- also works 
    ) r_lof
    union all
    select * 
    from ( -- bads
      select 
            sq2_r.dateindex,  sq2_r.company_id
          , sq2_r.sq_r3_rn,  sq2_r.sq_r3_val
          , sq2_r.sp,    sq2_r.ticker,  sq2_r.company
          , sq2_r.price, sq2_r.mktcap,  sq2_r.netinc_q1,  sq2_r.sales_q1, sq2_r.ncc_q1, sq2_r.assets_q1
      from ( -- sq2_r
        select qu2.dateindex, qu2.company_id
          -- later -- distinct on -- so if lof detects and bads detects then lof is taken as the single record
          -- 2147483647 -- float to the bottom ( zero(0): float to the top )
          , 0 sq_r3_rn, row_number() over (partition by qu2.dateindex order by qu2.company_id) sq_r3_val
          , qu2.sp,    qu2.ticker, qu2.company
          , qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1, qu2.ncc_q1, qu2.assets_q1
        from si_finecon2 qu2                  -- 12055 
        where qu2.sp = '500' and qu2.dateindex = 17409 
      ) sq2_r
      where ( 
          -- tester
          -- sq2_r.price > 20.00
          --
          -- shuld be never
             sq2_r.price is null or sq2_r.mktcap is null or sq2_r.netinc_q1 is null or sq2_r.sales_q1 is null or sq2_r.ncc_q1 is null
             -- # (price, mktcap, netinc_q1, sales_q1)
             -- # qu2.sp = '500' and qu2.dateindex = 12055 # 1748N # sq_r2_rn == 0, sq_r3_rn == 92 # "CenturyTel, Inc." ( netinc_q1 > sales_q1 )
             or ( sq2_r.netinc_q1 > sq2_r.sales_q1 )
          )
      order by sq2_r.company_id 
    ) bads
  ) r_lof_and_bads
  order by r_lof_and_bads.dateindex, r_lof_and_bads.sq_r3_rn 
) r_lof_and_bads_ordered
order by r_lof_and_bads_ordered.dateindex, r_lof_and_bads_ordered.company_id, r_lof_and_bads_ordered_rn

-- https://finance.yahoo.com/quote/FL/cash-flow?p=FL
--https://www.sec.gov/Archives/edgar/data/850209/000085020917000009/fl-20170429x10q.htm#CF_10Q_PART01_ITEM01_CF
-- # 17409;"9905N";0;259;"500";"FL";"Foot Locker, Inc.";35.23;4627.10;51.00;1701.00;;3946.00 -- MISSING ncc_q1
--




-- DO NOT THINK FULLY RIGHT [ ]

-- 12055;"1748N";11;92;"500";"CTL";"CenturyTel, Inc.";31.04;4426.60;607.60;524.50;-7.80
-- but if I add ncc_q1 then it falls from 11 to 19

-- TOO HARD ... USE AN UPDATE FOLLOWED BY AN 'UPDATED ANTI-JION

select 
    dateindex
  , first_value(r_lof_and_bads_ordered.company_id) over w company_id
  , first_value(r_lof_and_bads_ordered.sq_r3_rn) over w   sq_r3_rn
  , first_value(r_lof_and_bads_ordered.sq_r3_val) over w  sq_r3_val
  , first_value(r_lof_and_bads_ordered.sp) over w         sp
  , first_value(r_lof_and_bads_ordered.ticker) over w     ticker
  , first_value(r_lof_and_bads_ordered.company) over w    company
  , first_value(r_lof_and_bads_ordered.price) over w      price
  , first_value(r_lof_and_bads_ordered.mktcap) over w     mktcap
  , first_value(r_lof_and_bads_ordered.netinc_q1) over w  netinc_q1
  , first_value(r_lof_and_bads_ordered.sales_q1) over w   sales_q1
  , first_value(r_lof_and_bads_ordered.ncc_q1) over w     ncc_q1
  , first_value(r_lof_and_bads_ordered.assets_q1) over w  assets_q1
  , r_lof_and_bads_ordered_rn sp500_outlier_rnk
from ( -- r_lof_and_bads_ordered
  select r_lof_and_bads.*, row_number() over () r_lof_and_bads_ordered_rn -- IS MY ORDER GARANTEE LOST?
  from ( -- r_lof_and_bads
    select * 
    from ( -- bads



-- MANUAL DETERMINATION OF OUTLIERS ( RESULT COLUMNS MATCH THE 'AUTOMATIC METHOD'(SEE ABOVE) --

select 
      sq2_r.dateindex,  sq2_r.company_id
    , sq2_r.sq_r3_rn   outlier_rank -- lower is more 'outlier-ish' ( ALWAYS ZERO(0) IN THIS SQL )
    , sq2_r.sq_r3_val  rn_id        -- row number position ( from the original (rn := ) of where the outlier occurs  
    , sq2_r.sp,    sq2_r.ticker,  sq2_r.company
    , sq2_r.price, sq2_r.mktcap,  sq2_r.netinc_q1,  sq2_r.sales_q1, sq2_r.ncc_q1, sq2_r.assets_q1
from ( -- sq2_r
  select qu2.dateindex, qu2.company_id
    -- later -- distinct on -- so if lof detects and bads detects then lof is taken as the single record
    -- 2147483647 -- float to the bottom ( zero(0): float to the top )
    , 0 sq_r3_rn, row_number() over (partition by qu2.dateindex order by qu2.company_id) sq_r3_val
    , qu2.sp,    qu2.ticker, qu2.company
    , qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1, qu2.ncc_q1, qu2.assets_q1
  from si_finecon2 qu2                  -- 12055 
  where qu2.sp = '500' and qu2.dateindex = 17409 
) sq2_r
where ( 
    -- tester
    -- sq2_r.price > 20.00
    --
    -- shuld be never
       sq2_r.price is null    or sq2_r.mktcap is null or sq2_r.netinc_q1 is null or 
       sq2_r.sales_q1 is null or sq2_r.ncc_q1 is null
       -- # (price, mktcap, netinc_q1, sales_q1)
       -- # qu2.sp = '500' and qu2.dateindex = 12055 # 1748N # sq_r2_rn == 0, sq_r3_rn == 92 # "CenturyTel, Inc." ( netinc_q1 > sales_q1 )
       or ( sq2_r.netinc_q1 > sq2_r.sales_q1 )
    )
order by sq2_r.company_id 


      
    ) bads

    union all

    select *
    from ( -- r_lof
      -- # (price, mktcap, netinc_q1, sales_q1)
      -- # qu2.sp = '500' and qu2.dateindex = 12055  # 1748N # sq_r2_rn == 11, sq_r3_rn == 92 # "CenturyTel, Inc." ( netinc_q1 > sales_q1 )


-- AUTOMATIC DETERMINATION OF OUTLIERS ( USES pl/r function: r_lof )
      
select
    sq.dateindex, sq.company_id
  -- -- required in join ( not necessary to view )
  -- -- orderer ( not necessary to view ) ( but very useful in portability )
  , sq_r3.rn  outlier_rank -- lower is more 'outlier-ish'
  , sq_r3.val rn_id       -- row number position ( from the original (rn := ) of where the outlier occurs     
  ---- redundant of above
  --, sq.rn sq_rn
  -- -- redundant payload ( not necessary at all ) 
  , sq.sp, sq.ticker, sq.company, sq.price, sq.mktcap, sq.netinc_q1, sq.sales_q1, sq.ncc_q1, sq.assets_q1
from 
  ( -- sq, sq.rn(1,2,3,...)
  select 
      row_number() over (partition by qu2.dateindex order by qu2.company_id) rn
    , qu2.dateindex, qu2.company_id
    -- -- redundant payload
    , qu2.sp, qu2.ticker, qu2.company, qu2.price, qu2.mktcap, qu2.netinc_q1, qu2.sales_q1, qu2.ncc_q1, qu2.assets_q1
  from si_finecon2 qu2                  -- 12055
  where qu2.sp = '500' and qu2.dateindex = 17409 order by qu2.company_id
  ) sq, 
  ( -- sq_r3 -- sq_r3.rn(1,2,3,...) sq_r3.val(137,316,43,...)
    select (sq_r2.out).* --sq_r2.rn(NEED TO CARRY), sq_r2.val 
    from
    ( -- sq_r2                                                                                                                                               -- testing: retcount := 20 xor 'all of the data'
      select 
          r_lof( rn := array_agg(sq_r.rn)
        , cols := 
            array[
                -- array_agg(sq_r.price),    array_agg(sq_r.mktcap), 
                array_agg(sq_r.netinc_q1 / nullif(sq_r.sales_q1,0))
              , array_agg(sq_r.ncc_q1    / nullif(sq_r.sales_q1,0))
            ]
        , k := 5, retcount := 2147483647) as out 
      from 
      ( -- sq_r
        select 
            row_number() over (order by qu.company_id) rn -- random
          , case when qu.price     is not null then qu.price::float8  else '-Infinity'::float8 end price
          , case when qu.mktcap    is not null then qu.mktcap::float8 else '-Infinity'::float8 end mktcap
          , case when qu.netinc_q1 is not null then qu.netinc_q1      else '-Infinity'::float8 end netinc_q1
          , case when qu.sales_q1  is not null then qu.sales_q1       else '-Infinity'::float8 end sales_q1
          , case when qu.ncc_q1    is not null then qu.ncc_q1         else '-Infinity'::float8 end ncc_q1
          -- NOTE: Bear Stearns  assets_q1 == 0.00
          , case when qu.assets_q1 is not null then qu.assets_q1      else '-Infinity'::float8 end assets_q1
        from  si_finecon2 qu
        where qu.sp = '500' and qu.dateindex = 12055 order by qu.company_id 
      ) sq_r  
    ) sq_r2
  ) sq_r3
where sq.rn = sq_r3.val
order by sq_r3.rn -- limit 10 -- also works 



    ) r_lof -- where company_id = '7065N'
    order by sq_r3_rn  -- union all --
  ) r_lof_and_bads
) r_lof_and_bads_ordered window w as (partition by r_lof_and_bads_ordered.company_id order by r_lof_and_bads_ordered.sq_r3_rn)
order by r_lof_and_bads_ordered.sq_r3_rn

-- https://finance.yahoo.com/quote/FL/cash-flow?p=FL
--https://www.sec.gov/Archives/edgar/data/850209/000085020917000009/fl-20170429x10q.htm#CF_10Q_PART01_ITEM01_CF
-- # 17409;"9905N";0;259;"500";"FL";"Foot Locker, Inc.";35.23;4627.10;51.00;1701.00;;3946.00 -- MISSING ncc_q1
--




> vect        <- sort(as.integer(dir(from_dir)))
> names(vect) <- zoo::as.Date(sort(as.integer(dir(from_dir))))
> cbind(dateindex = seq(1,dim(data.frame(vect))[1],1),data.frame(vect))
           dateindex  vect
2003-01-03         1 12055
2003-01-31         2 12083
2003-02-28         3 12111
2003-04-04         4 12146
2003-05-02         5 12174
2003-05-30         6 12202
2003-07-03         7 12236
2003-08-01         8 12265
2003-08-29         9 12293
2003-10-03        10 12328
2003-10-31        11 12356
2003-11-28        12 12384
2004-01-02        13 12419
2004-01-30        14 12447
2004-02-27        15 12475
2004-04-02        16 12510
2004-04-30        17 12538
2004-06-04        18 12573
2004-07-02        19 12601
2004-07-30        20 12629
2004-09-03        21 12664
2004-09-30        22 12691
2004-10-29        23 12720
2004-12-03        24 12755
2004-12-31        25 12783
2005-01-31        26 12814
2005-02-28        27 12842
2005-03-31        28 12873
2005-04-29        29 12902
2005-05-31        30 12934
2005-06-30        31 12964
2005-07-29        32 12993
2005-08-31        33 13026
2005-09-30        34 13056
2005-10-31        35 13087
2005-11-30        36 13117
2005-12-30        37 13147
2006-01-31        38 13179
2006-02-28        39 13207
2006-03-31        40 13238
2006-04-28        41 13266
2006-05-31        42 13299
2006-06-30        43 13329
2006-07-31        44 13360
2006-08-31        45 13391
2006-09-29        46 13420
2006-10-31        47 13452
2006-11-30        48 13482
2006-12-29        49 13511
2007-01-31        50 13544
2007-02-28        51 13572
2007-03-30        52 13602
2007-04-30        53 13633
2007-06-01        54 13665
2007-06-29        55 13693
2007-07-31        56 13725
2007-08-31        57 13756
2007-09-28        58 13784
2007-10-31        59 13817
2007-11-30        60 13847
2007-12-31        61 13878
2008-01-31        62 13909
2008-02-29        63 13938
2008-03-31        64 13969
2008-04-30        65 13999
2008-05-30        66 14029
2008-06-30        67 14060
2008-07-31        68 14091
2008-08-29        69 14120
2008-09-30        70 14152
2008-10-31        71 14183
2008-11-28        72 14211
2008-12-31        73 14244
2009-01-30        74 14274
2009-02-27        75 14302
2009-03-31        76 14334
2009-04-30        77 14364
2009-05-29        78 14393
2009-06-30        79 14425
2009-07-31        80 14456
2009-08-31        81 14487
2009-09-30        82 14517
2009-10-30        83 14547
2009-11-30        84 14578
2009-12-31        85 14609
2010-01-29        86 14638
2010-02-26        87 14666
2010-03-31        88 14699
2010-04-30        89 14729
2010-05-28        90 14757
2010-06-30        91 14790
2010-07-30        92 14820
2010-08-31        93 14852
2010-09-30        94 14882
2010-10-29        95 14911
2010-11-30        96 14943
2010-12-31        97 14974
2011-01-31        98 15005
2011-02-28        99 15033
2011-03-31       100 15064
2011-04-29       101 15093
2011-05-31       102 15125
2011-06-30       103 15155
2011-07-29       104 15184
2011-08-31       105 15217
2011-09-30       106 15247
2011-10-31       107 15278
2011-11-30       108 15308
2011-12-30       109 15338
2012-01-31       110 15370
2012-02-29       111 15399
2012-03-30       112 15429
2012-04-30       113 15460
2012-05-31       114 15491
2012-06-29       115 15520
2012-07-31       116 15552
2012-08-31       117 15583
2012-09-28       118 15611
2012-10-31       119 15644
2012-11-30       120 15674
2012-12-31       121 15705
2013-01-31       122 15736
2013-02-28       123 15764
2013-03-29       124 15793
2013-04-30       125 15825
2013-05-31       126 15856
2013-06-28       127 15884
2013-07-31       128 15917
2013-08-30       129 15947
2013-09-30       130 15978
2013-10-31       131 16009
2013-11-29       132 16038
2013-12-31       133 16070
2014-01-31       134 16101
2014-02-28       135 16129
2014-03-31       136 16160
2014-04-30       137 16190
2014-05-30       138 16220
2014-06-30       139 16251
2014-07-31       140 16282
2014-08-29       141 16311
2014-09-30       142 16343
2014-10-31       143 16374
2014-11-28       144 16402
2014-12-31       145 16435
2015-01-30       146 16465
2015-02-27       147 16493
2015-03-31       148 16525
2015-04-30       149 16555
2015-05-29       150 16584
2015-06-30       151 16616
2015-07-31       152 16647
2015-08-31       153 16678
2015-09-30       154 16708
2015-10-30       155 16738
2015-11-30       156 16769
2015-12-31       157 16800
2016-01-29       158 16829
2016-02-29       159 16860
2016-03-31       160 16891
2016-04-29       161 16920
2016-05-31       162 16952
2016-06-30       163 16982
2016-07-29       164 17011
2016-08-31       165 17044
2016-09-30       166 17074
2016-10-31       167 17105
2016-11-30       168 17135
2016-12-30       169 17165
2017-01-31       170 17197
2017-02-28       171 17225
2017-03-31       172 17256
2017-04-28       173 17284
2017-05-31       174 17317
2017-06-30       175 17347
2017-07-31       176 17378
2017-08-31       177 17409
>


-- TEMPORARY.sql

-- EXPERIMENT
set effective_cache_size to '6144MB';  -- os + 'shared_buffers'

set search_path to fe_data_store;
-- et search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'

-- TUES(DAY): FIX

-- THUES(EVE): RUN ( VISIT BRIAN'S PARTY )

-- FIX1 [ ]
-- each argument needs specifics
-- case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_ncc_q1 else null end
-- case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap else null end

-- FIX2 [ ] ( SHOULD BE NOW OVER NOW AND LAST OVER LAST
-- last_inbnd_stmtstat_price * mktcap
-- 
-- TEST RUN
-- upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = 17409, exactly_only_aggregates = TRUE)


      select
          dateindex
        , dateindexlbd
        , dateindexeom
        , dateindexeom::text dateindexeom_fct
        , 'sp_desc'::text collection_name01_fct
        ,     case when sp in ('500','400','600') then 'sp'::text else 'notsp'::text end sp_desc_fct
        , 'industry_desc'::text collection_name02_fct
        ,     industry_desc industry_desc_fct
        , sum(now_inbnd_stmtstat_ncc_q1)    / nullif(sum(now_inbnd_stmtstat_mktcap), 0)    * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100
        , sum(now_inbnd_stmtstat_sales_q1)  / nullif(sum(now_inbnd_stmtstat_mktcap), 0)    * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_mktcap), 0)   * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_sales_q1), 0)  * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        , sum(now_inbnd_stmtstat_ncc_q1)    / nullif(sum(now_inbnd_stmtstat_assets_q1), 0)    * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10
        , sum(now_inbnd_stmtstat_sales_q1)  / nullif(sum(now_inbnd_stmtstat_assets_q1), 0)    * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        , sum(now_inbnd_stmtstat_netinc_q1) / nullif(sum(now_inbnd_stmtstat_assets_q1), 0)   * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        , sum(now_inbnd_stmtstat_ncc_q1)    / nullif(sum(now_inbnd_stmtstat_sales_q1), 0)     * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100
        , sum(last_inbnd_stmtstat_ncc_q1)    / nullif(sum(last_inbnd_stmtstat_mktcap), 0)    * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100
        , sum(last_inbnd_stmtstat_sales_q1)  / nullif(sum(last_inbnd_stmtstat_mktcap), 0)    * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        , sum(last_inbnd_stmtstat_netinc_q1) / nullif(sum(last_inbnd_stmtstat_mktcap), 0)   * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        , sum(last_inbnd_stmtstat_netinc_q1) / nullif(sum(last_inbnd_stmtstat_sales_q1), 0)  * 100.00  rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        , sum(last_inbnd_stmtstat_ncc_q1)    / nullif(sum(last_inbnd_stmtstat_assets_q1), 0)    * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10
        , sum(last_inbnd_stmtstat_sales_q1)  / nullif(sum(last_inbnd_stmtstat_assets_q1), 0)    * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        , sum(last_inbnd_stmtstat_netinc_q1) / nullif(sum(last_inbnd_stmtstat_assets_q1), 0)   * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        , sum(last_inbnd_stmtstat_ncc_q1)    / nullif(sum(last_inbnd_stmtstat_sales_q1), 0)     * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100
        , count(now_inbnd_stmtid_dateindex)::numeric                                                                           count_now_inbnd_stmtstat_dateindex
        , count(now_inbnd_stmtid_dateindex)::numeric    / nullif(count(last_inbnd_stmtid_dateindex)::numeric,0)    * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
        , count(now_inbnd_stmtid_dateindexlbd)::numeric                                                                        count_now_inbnd_stmtstat_dateindexlbd
        , count(now_inbnd_stmtid_dateindexlbd)::numeric / nullif(count(last_inbnd_stmtid_dateindexlbd)::numeric,0) * 100.0 rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100
        , sum(sales_q1)  sum_sales_q1
        , sum(netinc_q1) sum_netinc_q1
        , sum(ncc_q1)    sum_ncc_q1
        , sum(assets_q1) sum_assets_q1
        , sum(mktcap)    sum_mktcap
        , sum(now_inbnd_stmtstat_sales_q1)  sum_now_inbnd_stmtstat_sales_q1
        , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1
        , sum(now_inbnd_stmtstat_ncc_q1)    sum_now_inbnd_stmtstat_ncc_q1
        , sum(now_inbnd_stmtstat_assets_q1) sum_now_inbnd_stmtstat_assets_q1
        , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
        , sum(now_inbnd_stmtstat_mktcap) / nullif(sum(last_inbnd_stmtstat_mktcap), 0)  rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
        , sum(last_inbnd_stmtstat_sales_q1)  sum_last_inbnd_stmtstat_sales_q1
        , sum(last_inbnd_stmtstat_netinc_q1) sum_last_inbnd_stmtstat_netinc_q1
        , sum(last_inbnd_stmtstat_ncc_q1)    sum_last_inbnd_stmtstat_ncc_q1
        , sum(last_inbnd_stmtstat_assets_q1) sum_last_inbnd_stmtstat_assets_q1
        , sum(last_inbnd_stmtstat_mktcap)    sum_last_inbnd_stmtstat_mktcap
        , avg(now_inbnd_stmtstat_price)      avg_now_inbnd_stmtstat_price
        , avg(last_inbnd_stmtstat_price)     avg_last_inbnd_stmtstat_price

-- case when last_inbnd_stmtstat_price is not null and
--             mktcap -> last_inbnd_stmtstat_mktcap (FIX) is not null and
--             sum_sp_industry_desc_last_inbnd_stmtstat_mktcap -> last_inbnd_stmtstat_mktcap ( NEW TEST ) is not null
-- then      last_inbnd_stmtstat_price
        
        , avg(last_inbnd_stmtstat_price * mktcap    / nullif(sum_sp_industry_desc_last_inbnd_stmtstat_mktcap,0) )    avg_mktcap_wdt_last_inbnd_stmtstat_price
        , avg( now_inbnd_stmtstat_price * mktcap    / nullif(sum_sp_industry_desc_now_inbnd_stmtstat_mktcap, 0) )    avg_mktcap_wdt_now_inbnd_stmtstat_price
        , avg(last_inbnd_stmtstat_price * assets_q1 / nullif(sum_sp_industry_desc_last_inbnd_stmtstat_assets_q1,0) ) avg_assets_q1_wdt_last_inbnd_stmtstat_price
        , avg( now_inbnd_stmtstat_price * assets_q1 / nullif(sum_sp_industry_desc_now_inbnd_stmtstat_assets_q1, 0) ) avg_assets_q1_wdt_now_inbnd_stmtstat_price
        , avg(pct_freeprice_ret_01m_ann * mktcap    / nullif(sum_sp_industry_desc_mktcap,0) )                        avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM
        , avg(pct_freeprice_ret_01m_ann * assets_q1 / nullif(sum_sp_industry_desc_assets_q1,0) )                     avg_assets_q1_wdt_pct_freeprice_ret_01m_ann 
      from si_finecon2 where dateindex = 17409 and
        sp in ('500','400','600')
         and industry_desc in ('Gold & Silver', 'Furniture & Fixtures', 'Oil & Gas Operations')     
      group by dateindex, dateindexlbd, dateindexeom, sp_desc_fct, industry_desc_fct 
      order by dateindex, dateindexlbd, dateindexeom, sp_desc_fct, industry_desc_fct 
 
-- FIX 0 OR FIX 3
-- ALSO LOAD COMPANY IDS (CHANGE MONTH) INTO A DIFFERENT STRUCTURE

-- FIX 4

explain -- 506070 (40 SECONDS)
select count(1) over (partition by company_id order by dateindex ) from si_finecon2 order by dateindex, company_id 

explain -- 946577 (75 SECONDS)
select count(1) 
from si_finecon2 now left outer join si_finecon2 p01lbd on now.dateindexp01lbd  = p01lbd.dateindexlbd and now.company_id = p01lbd.company_id

explain -- 2656828 ( ?? SECONDS ... ? 160 SECONDS - 2:40 MINUTES ... sequential scans ...
select now.dateindex, now.company_id
  , first_value(now.dateindex) over (partition by now.company_id, (now.perend_q1 != p01lbd.perend_q1) order by now.dateindex ) last_inbnd_stmtstat_dateindex
from si_finecon2 now left outer join si_finecon2 p01lbd on now.dateindexp01lbd  = p01lbd.dateindexlbd and now.company_id = p01lbd.company_id

-- WOULD HELP + ( NOT SHOWN 'AN INDEX' )

explain -- 2405973 ( ?? SECONDS ... ? 145 SECONDS - 2:24 MINTUES )
select case when now.perend_q1 != p01lbd.perend_q1 then 1 else 0 end is_first_inbnd_stmtstat_dateindex
from si_finecon2 now left outer join si_finecon2 p01lbd on now.dateindexp01lbd  = p01lbd.dateindexlbd and now.company_id = p01lbd.company_id

explain -- 11112194
select case when now.perend_q1 != p01lbd.perend_q1 then 1 else 0 end is_first_inbnd_stmtstat_dateindex
from si_finecon2 now left outer join si_finecon2 p01lbd on 
    now.dateindexp01lbd    =  p01lbd.dateindexlbd and now.company_id = p01lbd.company_id
and now.dateindex = 17409 and p01lbd.dateindex = ( select distinct on (ins.dateindexp01lbd) ins.dateindexp01lbd from si_finecon2 ins where ins.dateindex = 17409 )

-- NO HELP

explain -- --> exact 11112194 -> 120539(join)
select case when now.perend_q1 != p01lbd.perend_q1 then 1 else 0 end is_first_inbnd_stmtstat_dateindex
from si_finecon2 now join si_finecon2 p01lbd on 
    now.dateindexp01lbd     = p01lbd.dateindexlbd and now.company_id = p01lbd.company_id
and now.dateindex = 17409 and p01lbd.dateindex = ( select distinct on (ins.dateindexp01lbd) ins.dateindexp01lbd from si_finecon2 ins where ins.dateindex = 17409 )



explain -- --> exact 11112194 -> 120539(join) .. took 1/4 second ... 1/2 second
select now.dateindex, now.company_id, now.company, now.perend_q1 now_perend_q1, p01lbd.perend_q1 p01lbd_perend_q1
  , case when now.perend_q1 = p01lbd.perend_q1 then 0 else 1 end is_first_inbnd_stmtstat_dateindex
from si_finecon2 now join si_finecon2 p01lbd on  -- JUST LIKE 'with' FOOLS THE QUERY PLANNER --
    now.dateindex = 17378 and p01lbd.dateindex = ( select distinct on (ins.dateindexp01lbd) ins.dateindexp01lbd from si_finecon2 ins where ins.dateindex = 17378 )
and now.company_id = p01lbd.company_id


select ETC
from ( -- sq
select now.dateindex, now.company_id, now.company, now.perend_q1 now_perend_q1, p01lbd.perend_q1 p01lbd_perend_q1
  , case when now.perend_q1 = p01lbd.perend_q1 then 0 else 1 end is_first_inbnd_stmtstat_dateindex
from si_finecon2 now join si_finecon2 p01lbd on  -- JUST LIKE 'with' FOOLS THE QUERY PLANNER --
    now.dateindex = 17378 and p01lbd.dateindex = ( select distinct on (ins.dateindexp01lbd) ins.dateindexp01lbd from si_finecon2 ins where ins.dateindex = 17378 )
and now.company_id = p01lbd.company_id
) sq









      select
          dateindex
        , dateindexlbd
        , dateindexeom
        , dateindexeom::text dateindexeom_fct
        , 'sp_desc'::text collection_name01_fct
        ,     case when sp in ('500','400','600') then 'sp'::text else 'notsp'::text end sp_desc_fct
        , 'industry_desc'::text collection_name02_fct
        ,     industry_desc industry_desc_fct
        -- NOWS
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap else null end), 0)  * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100
        ,        sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap   else null end), 0) * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap    else null end), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_netinc_q1  else null end)   /
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_sales_q1  else null end), 0) * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_ncc_q1    else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10
        ,        sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_sales_q1  else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_netinc_q1  else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1  else null end), 0) * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_ncc_q1  else null end )    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_sales_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100
        -- LASTS
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap else null end), 0)  * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100
        ,        sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap   else null end), 0) * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap    else null end), 0)  * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_netinc_q1  else null end)   /
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_sales_q1  else null end), 0) * 100.00  rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_ncc_q1    else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10
        ,        sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_sales_q1  else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_netinc_q1  else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1  else null end), 0) * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_ncc_q1  else null end )    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_sales_q1 else null end), 0) * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100
        -- ERROR(ENDGE CASE)SANITY CHECKING
        , count(now_inbnd_stmtid_dateindex)::numeric                                                                                                                                     count_now_inbnd_stmtstat_dateindex
        ,        count(case when now_inbnd_stmtid_dateindex is not null and last_inbnd_stmtid_dateindex is not null then now_inbnd_stmtid_dateindex  else null end)::numeric    / 
          nullif(count(case when now_inbnd_stmtid_dateindex is not null and last_inbnd_stmtid_dateindex is not null then last_inbnd_stmtid_dateindex else null end)::numeric, 0) * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
        , count(now_inbnd_stmtid_dateindexlbd)::numeric                                                                                                                                      count_now_inbnd_stmtstat_dateindexlbd
        ,        count(case when now_inbnd_stmtid_dateindexlbd is not null and last_inbnd_stmtid_dateindexlbd is not null then now_inbnd_stmtid_dateindexlbd  else null end)::numeric    / 
          nullif(count(case when now_inbnd_stmtid_dateindexlbd is not null and last_inbnd_stmtid_dateindexlbd is not null then last_inbnd_stmtid_dateindexlbd else null end)::numeric, 0) * 100.0 rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100
        , sum(sales_q1)  sum_sales_q1
        , sum(netinc_q1) sum_netinc_q1
        , sum(ncc_q1)    sum_ncc_q1
        , sum(assets_q1) sum_assets_q1
        , sum(assets_q2) sum_assets_q2
        , sum(mktcap)    sum_mktcap
        , sum(now_inbnd_stmtstat_sales_q1)  sum_now_inbnd_stmtstat_sales_q1
        , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1
        , sum(now_inbnd_stmtstat_ncc_q1)    sum_now_inbnd_stmtstat_ncc_q1
        , sum(now_inbnd_stmtstat_assets_q1) sum_now_inbnd_stmtstat_assets_q1
        , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
        ,        sum(case when now_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap  else null end)     / 
          nullif(sum(case when now_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap else null end), 0) * 100.0 rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
        , sum(last_inbnd_stmtstat_sales_q1)  sum_last_inbnd_stmtstat_sales_q1
        , sum(last_inbnd_stmtstat_netinc_q1) sum_last_inbnd_stmtstat_netinc_q1
        , sum(last_inbnd_stmtstat_ncc_q1)    sum_last_inbnd_stmtstat_ncc_q1
        , sum(last_inbnd_stmtstat_assets_q1) sum_last_inbnd_stmtstat_assets_q1
        , sum(last_inbnd_stmtstat_mktcap)    sum_last_inbnd_stmtstat_mktcap
        , avg(now_inbnd_stmtstat_price)      avg_now_inbnd_stmtstat_price
        , avg(last_inbnd_stmtstat_price)     avg_last_inbnd_stmtstat_price
        -- RELATIVE CHANGES
        , (case when now_inbnd_stmtstat_assets_q1 is not null and now_inbnd_stmtstat_assets_q2 is not null then 1 else null end) * ( sum(now_inbnd_stmtstat_assets_q1) - sum(now_inbnd_stmtstat_assets_q2) / sum(abs(now_inbnd_stmtstat_assets_q2)) ) * 100                   pctchg_now_inbnd_stmtstat_assets_q1_from_q2
        , case when last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q2 is not null then 1 else null end *
          ( last_inbnd_stmtstat_assets_q1 - last_inbnd_stmtstat_assets_q2 / abs(last_inbnd_stmtstat_assets_q2) ) * 100                pctchg_last_inbnd_stmtstat_assets_q1_from_q2
        -- 
        -- THESE STILL ARE NOT RIGHT ORIGINAL WEIGHTS MUST INCLUDE BOTH PARTS OF THE DENOMINTOR ( FOR FUTURE INTENTIONS )
        -- FOR EXAMPLE: AGG_MKTCAP_WRT_MKTCAP_AND_PRICE MUST INCLUDE AGGREGATE_OF_MKTCAP_*AND*_WRT__PRICE_IS_NOT_NULL_AND_MKTCAP_IS_NOT_NULL
        -- 
        ,    avg(case when last_inbnd_stmtstat_price is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_price * last_inbnd_stmtstat_mktcap else null end    / 
          nullif(sum_sp_industry_desc_last_inbnd_stmtstat_mktcap,0) )    avg_mktcap_wdt_last_inbnd_stmtstat_price

        ,    avg(case when now_inbnd_stmtstat_price is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_price * now_inbnd_stmtstat_mktcap else null end        / 
          nullif(sum_sp_industry_desc_now_inbnd_stmtstat_mktcap, 0) )    avg_mktcap_wdt_now_inbnd_stmtstat_price

        ,    avg(case when last_inbnd_stmtstat_price is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_price * last_inbnd_stmtstat_assets_q1 else null end / 
          nullif(sum_sp_industry_desc_last_inbnd_stmtstat_assets_q1,0) ) avg_assets_q1_wdt_last_inbnd_stmtstat_price

        ,    avg(case when now_inbnd_stmtstat_price is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_price * now_inbnd_stmtstat_assets_q1 else null end     / 
          nullif(sum_sp_industry_desc_now_inbnd_stmtstat_assets_q1, 0) ) avg_assets_q1_wdt_now_inbnd_stmtstat_price

        ,    avg(case when pct_freeprice_ret_01m_ann is not null and mktcap is not null then pct_freeprice_ret_01m_ann *  mktcap  else null end    / 
          nullif(sum_sp_industry_desc_mktcap,0) )       avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM

        ,    avg(case when pct_freeprice_ret_01m_ann is not null and assets_q1 is not null then pct_freeprice_ret_01m_ann * assets_q1 else null end / 
          nullif(sum_sp_industry_desc_assets_q1,0) )    avg_assets_q1_wdt_pct_freeprice_ret_01m_ann 

      from si_finecon2 where dateindex = 12055 and
        sp in ('500','400','600')
         and industry_desc in ('Gold & Silver', 'Furniture & Fixtures', 'Oil & Gas Operations')     
      group by dateindex, dateindexlbd, dateindexeom, sp_desc_fct, industry_desc_fct 
      order by dateindex, dateindexlbd, dateindexeom, sp_desc_fct, industry_desc_fct 



      select
          dateindex
        , dateindexlbd
        , dateindexeom
        , dateindexeom::text dateindexeom_fct
        , 'sp_desc'::text collection_name01_fct
        ,     case when sp in ('500','400','600') then 'sp'::text else 'notsp'::text end sp_desc_fct
        , 'industry_desc'::text collection_name02_fct
        ,     industry_desc industry_desc_fct
        , (case when now_inbnd_stmtstat_assets_q1 is not null and now_inbnd_stmtstat_assets_q2 is not null then 1 else null end) * sum( now_inbnd_stmtstat_assets_q1 - now_inbnd_stmtstat_assets_q2 / abs(now_inbnd_stmtstat_assets_q2) ) * 100                   pctchg_now_inbnd_stmtstat_assets_q1_from_q2
      from si_finecon2 where dateindex = 12055 and
        sp in ('500','400','600')
         and industry_desc in ('Gold & Silver', 'Furniture & Fixtures', 'Oil & Gas Operations')     
      group by dateindex, dateindexlbd, dateindexeom, sp_desc_fct, industry_desc_fct 
      order by dateindex, dateindexlbd, dateindexeom, sp_desc_fct, industry_desc_fct 




      select
          dateindex
        , dateindexlbd
        , dateindexeom
        , dateindexeom::text dateindexeom_fct
        <%= {if(SP_OPS_WHAT_I != '') { ', ' %S+% SQuote('sp_desc') %S+% '::text collection_name01_fct' }} %>
        <%= {if(SP_OPS_WHAT_I != '') { ',     case when sp in ' %S+% SP_OPS_WHAT_I %S+% ' then ' %S+%  SQuote(SP_OPS_WHAT_SHORT_I)  %S+% '::text else ' %S+% SQuote('not' %S+% SP_OPS_WHAT_SHORT_I)  %S+% '::text end sp_desc_fct'  }} %>
        <%= {if(DIVISION_I != '') { ', ' %S+% SQuote(DIVISION_I) %S+% '::text collection_name02_fct' }} %>
        <%= {if(DIVISION_I != '') { ',     ' %S+% DIVISION_I %S+% ' ' %S+% DIVISION_I %S+% '_fct' }} %>
        -- NOWS
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap else null end), 0)  * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100
        ,        sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap   else null end), 0) * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap    else null end), 0)  * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_netinc_q1  else null end)   /
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_sales_q1  else null end), 0) * 100.00  rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_ncc_q1    else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10
        ,        sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_sales_q1  else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_sales_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        ,        sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_netinc_q1  else null end)    / 
          nullif(sum(case when now_inbnd_stmtstat_netinc_q1 is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_assets_q1  else null end), 0) * 1000.00  rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        ,        sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_ncc_q1  else null end )    / 
          nullif(sum(case when now_inbnd_stmtstat_ncc_q1 is not null and now_inbnd_stmtstat_sales_q1 is not null then now_inbnd_stmtstat_sales_q1 else null end), 0) * 100.00  rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100
        -- LASTS
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_ncc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap else null end), 0)  * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100
        ,        sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_sales_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap   else null end), 0) * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_netinc_q1 else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap    else null end), 0)  * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_netinc_q1  else null end)   /
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_sales_q1  else null end), 0) * 100.00  rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_ncc_q1    else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10
        ,        sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_sales_q1  else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_sales_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1 else null end), 0) * 100.00  rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10
        ,        sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_netinc_q1  else null end)    / 
          nullif(sum(case when last_inbnd_stmtstat_netinc_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_assets_q1  else null end), 0) * 1000.00  rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100
        ,        sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_ncc_q1  else null end )    / 
          nullif(sum(case when last_inbnd_stmtstat_ncc_q1 is not null and last_inbnd_stmtstat_sales_q1 is not null then last_inbnd_stmtstat_sales_q1 else null end), 0) * 100.00  rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100
        -- ERROR(ENDGE CASE)SANITY CHECKING
        , count(now_inbnd_stmtid_dateindex)::numeric                                                                                                                                     count_now_inbnd_stmtstat_dateindex
        ,        count(case when now_inbnd_stmtid_dateindex is not null and last_inbnd_stmtid_dateindex is not null then now_inbnd_stmtid_dateindex  else null end)::numeric    / 
          nullif(count(case when now_inbnd_stmtid_dateindex is not null and last_inbnd_stmtid_dateindex is not null then last_inbnd_stmtid_dateindex else null end)::numeric, 0) * 100.0 rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100
        , count(now_inbnd_stmtid_dateindexlbd)::numeric                                                                                                                                      count_now_inbnd_stmtstat_dateindexlbd
        ,        count(case when now_inbnd_stmtid_dateindexlbd is not null and last_inbnd_stmtid_dateindexlbd is not null then now_inbnd_stmtid_dateindexlbd  else null end)::numeric    / 
          nullif(count(case when now_inbnd_stmtid_dateindexlbd is not null and last_inbnd_stmtid_dateindexlbd is not null then last_inbnd_stmtid_dateindexlbd else null end)::numeric, 0) * 100.0 rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100
        , sum(sales_q1)  sum_sales_q1
        , sum(netinc_q1) sum_netinc_q1
        , sum(ncc_q1)    sum_ncc_q1
        , sum(assets_q1) sum_assets_q1
        , sum(assets_q2) sum_assets_q2
        , sum(mktcap)    sum_mktcap
        , sum(now_inbnd_stmtstat_sales_q1)  sum_now_inbnd_stmtstat_sales_q1
        , sum(now_inbnd_stmtstat_netinc_q1) sum_now_inbnd_stmtstat_netinc_q1
        , sum(now_inbnd_stmtstat_ncc_q1)    sum_now_inbnd_stmtstat_ncc_q1
        , sum(now_inbnd_stmtstat_assets_q1) sum_now_inbnd_stmtstat_assets_q1
        , sum(now_inbnd_stmtstat_mktcap)    sum_now_inbnd_stmtstat_mktcap
        ,        sum(case when now_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_mktcap  else null end)     / 
          nullif(sum(case when now_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_mktcap else null end), 0) * 100.0 rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100
        , sum(last_inbnd_stmtstat_sales_q1)  sum_last_inbnd_stmtstat_sales_q1
        , sum(last_inbnd_stmtstat_netinc_q1) sum_last_inbnd_stmtstat_netinc_q1
        , sum(last_inbnd_stmtstat_ncc_q1)    sum_last_inbnd_stmtstat_ncc_q1
        , sum(last_inbnd_stmtstat_assets_q1) sum_last_inbnd_stmtstat_assets_q1
        , sum(last_inbnd_stmtstat_mktcap)    sum_last_inbnd_stmtstat_mktcap
        , avg(now_inbnd_stmtstat_price)      avg_now_inbnd_stmtstat_price
        , avg(last_inbnd_stmtstat_price)     avg_last_inbnd_stmtstat_price
--       -- OUT OF TIME WED - FIX AGGREGATES [ ]
--       -- RELATIVE CHANGES
--       -- must appear in the GROUP BY clause or be used in an aggregate function
--       , case when now_inbnd_stmtstat_assets_q1 is not null and now_inbnd_stmtstat_assets_q2 is not null then 1 else null end *
--         ( now_inbnd_stmtstat_assets_q1 - now_inbnd_stmtstat_assets_q2 / abs(now_inbnd_stmtstat_assets_q2) ) * 100                   pctchg_now_inbnd_stmtstat_assets_q1_from_q2
--       , case when last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q2 is not null then 1 else null end *
--         ( last_inbnd_stmtstat_assets_q1 - last_inbnd_stmtstat_assets_q2 / abs(last_inbnd_stmtstat_assets_q2) ) * 100                pctchg_last_inbnd_stmtstat_assets_q1_from_q2
--       -- 
--       -- THESE STILL ARE NOT RIGHT ORIGINAL WEIGHTS MUST INCLUDE BOTH PARTS OF THE DENOMINTOR ( FOR FUTURE INTENTIONS )
--       -- FOR EXAMPLE: AGG_MKTCAP_WRT_MKTCAP_AND_PRICE MUST INCLUDE AGGREGATE_OF_MKTCAP_*AND*_WRT__PRICE_IS_NOT_NULL_AND_MKTCAP_IS_NOT_NULL
--       -- 
--       -- OUT OF TIME WED - FIX AGGREGATES [ ]
--       ,    avg(case when last_inbnd_stmtstat_price is not null and last_inbnd_stmtstat_mktcap is not null then last_inbnd_stmtstat_price * last_inbnd_stmtstat_mktcap else null end    / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_last_inbnd_stmtstat_mktcap,0) )    avg_mktcap_wdt_last_inbnd_stmtstat_price
-- 
--       ,    avg(case when now_inbnd_stmtstat_price is not null and now_inbnd_stmtstat_mktcap is not null then now_inbnd_stmtstat_price * now_inbnd_stmtstat_mktcap else null end        / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_now_inbnd_stmtstat_mktcap, 0) )    avg_mktcap_wdt_now_inbnd_stmtstat_price
-- 
--       ,    avg(case when last_inbnd_stmtstat_price is not null and last_inbnd_stmtstat_assets_q1 is not null then last_inbnd_stmtstat_price * last_inbnd_stmtstat_assets_q1 else null end / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_last_inbnd_stmtstat_assets_q1,0) ) avg_assets_q1_wdt_last_inbnd_stmtstat_price
-- 
--       ,    avg(case when now_inbnd_stmtstat_price is not null and now_inbnd_stmtstat_assets_q1 is not null then now_inbnd_stmtstat_price * now_inbnd_stmtstat_assets_q1 else null end     / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_now_inbnd_stmtstat_assets_q1, 0) ) avg_assets_q1_wdt_now_inbnd_stmtstat_price
-- 
--       ,    avg(case when pct_freeprice_ret_01m_ann is not null and mktcap is not null then pct_freeprice_ret_01m_ann *  mktcap  else null end    / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_mktcap,0) )       avg_mktcap_wdt_pct_freeprice_ret_01m_ann  -- FROM *** load_division_aggregated_now_last_mktcap_per_company_id *** FROM
-- 
--       ,    avg(case when pct_freeprice_ret_01m_ann is not null and assets_q1 is not null then pct_freeprice_ret_01m_ann * assets_q1 else null end / 
--         nullif(sum<%= {if(SP_OPS_WHAT_I != ''){'_' %S+% SP_OPS_WHAT_SHORT_I}} %><%= {if(DIVISION_I != ''){'_' %S+% DIVISION_I}} %>_assets_q1,0) )    avg_assets_q1_wdt_pct_freeprice_ret_01m_ann 

      from si_finecon2 where dateindex = <%= DATEINDEX %> and
        <%= {if(SP_OPS_WHAT_I != '') { 'sp in ' %S+% SP_OPS_WHAT_I }} %>
        <%= {if(!is.null(DIVISION_ITEMS_I)) { ' and ' %S+% DIVISION_I %S+% ' in (' %S+% stringi::stri_c(sapply(DIVISION_ITEMS_I, SQuote), collapse = ', ')  %S+% ')     ' }} %>
      group by dateindex, dateindexlbd, dateindexeom<%= {if(SP_OPS_WHAT_I != '') { ', sp_desc_fct' }} %><%= {if(DIVISION_I != '') { ', ' %S+% DIVISION_I %S+% '_fct' }} %> 
      order by dateindex, dateindexlbd, dateindexeom<%= {if(SP_OPS_WHAT_I != '') { ', sp_desc_fct' }} %><%= {if(DIVISION_I != '') { ', ' %S+% DIVISION_I %S+% '_fct' }} %> 


-- seems TO work ( PRODUCTION [ ] [ ] SEND IT IN DB.Q )
-- every 60 minutes
dbGetQuery(con,"vacuum analyze verbose  fe_data_store.si_finecon2;")
dbGetQuery(con,"reindex (verbose) table fe_data_store.si_finecon2;")

-- two places
[ ] [ ] 
spnot500 in ('400','600')

-- WHY WAS 2007-2008 A RECESSION AND 2015-2016 NOT A RECESSION

select  sum(ncc_q1) / sum(mktcap) * 10000.00 from si_finecon2 where dateindex = 14911;
  
-- SAVE [ ] MASS RENAME COLUMN SUFFIXES
select 'alter table ' || table_name || ' rename column ' || column_name || ' to ' || 
  unnest(regexp_matches(column_name,'^.*____')) || 'rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000'
from information_schema.columns
where 
      table_schema = 'fe_data_store' 
  and table_name = 'si_finecon2_aggregates' 
  and column_name ~ '.*rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100$';

--LEFT_OFF
select to_timestamp(dateindex*3600*24)::date dateindex_dt,
  sum(employees) sum_employees,
  sum(netinc_q1) sum_netinc_q1,
  sum(cgs_q1)   / nullif(sum(netinc_q1),0) rat_cgs_q1_o_netinc_q1,
  sum(cgs_q1)    sum_cgs_q1,  
  sum(sales_q1) / nullif(sum(cgs_q1),0)    rat_sales_q1_o_cgs_q1,
  sum(sales_q1)  sum_sales_q1,
  sum(mktcap)    sum_mktcap_q1,
  sum(price)     sum_price_q1,
  sum(cash_q1)   sum_cash_q1,
  sum(ncc_q1)    sum_ncc_q1,
  sum(tcf_q1 + tci_q1 + tco_q1 +  ere_q1) sum_ncc_q1_est,
  sum(tcf_q1)    sum_tcf_q1,
  sum(tci_q1)    sum_tci_q1,
  sum(tco_q1)    sum_tco_q1,
  sum(ere_q1)    sum_ere_q1
from query01
where sp = '500'
group by dateindex
order by dateindex





--drop view recession500_search;
--drop view recession_search;

create view recession500_search as 
 select
 
  dateindex,
  dateindexlbd,
  dateindexeom,
  to_timestamp(dateindexeom*3600*24)::date dateindexeom_dt,

  sp_desc__sp500____count_now_inbnd_stmtstat_dateindex,
  sp_desc__sp500____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100,
  sp_desc__sp500____count_now_inbnd_stmtstat_dateindexlbd,
  sp_desc__sp500____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100,
  sp_desc__sp500____sum_sales_q1 ,
  sp_desc__sp500____sum_netinc_q1 ,
  sp_desc__sp500____sum_ncc_q1 ,
  sp_desc__sp500____sum_assets_q1 ,
  sp_desc__sp500____sum_assets_q2,
  sp_desc__sp500____sum_mktcap ,

  sp_desc__sp500____avg_now_inbnd_stmtstat_price ,
  sp_desc__sp500____avg_last_inbnd_stmtstat_price ,

  sp_desc__sp500____sum_now_inbnd_stmtstat_sales_q1 ,
  sp_desc__sp500____sum_now_inbnd_stmtstat_netinc_q1 ,
  sp_desc__sp500____sum_now_inbnd_stmtstat_ncc_q1 ,
  sp_desc__sp500____sum_now_inbnd_stmtstat_assets_q1 ,
  sp_desc__sp500____sum_now_inbnd_stmtstat_mktcap ,
  sp_desc__sp500____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 ,
  sp_desc__sp500____sum_last_inbnd_stmtstat_sales_q1 ,
  sp_desc__sp500____sum_last_inbnd_stmtstat_netinc_q1 ,
  sp_desc__sp500____sum_last_inbnd_stmtstat_ncc_q1 ,
  sp_desc__sp500____sum_last_inbnd_stmtstat_assets_q1 ,
  sp_desc__sp500____sum_last_inbnd_stmtstat_mktcap ,

  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 ,
  sp_desc__sp500____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 ,
  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 ,
  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 ,
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 ,
  sp_desc__sp500____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 ,
  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 ,
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 ,
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 

from si_finecon2_aggregates order by dateindexeom;



select * from recession_search order by dateindexeom; 


--alter table si_finecon2_aggregates 
--rename column rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 to rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000;


-- last month ( look for 4 week loaded returns ) # [x] months_only_back = 13, exactly_only_future_returns = TRUE
select * from fe_data_store.si_finecon2 where dateindex = 17409 and ticker in ('MSFT','AAPL')

-- current(this) month ( look for general data )
select * from fe_data_store.si_finecon2 where dateindex = 17438 and ticker in ('MSFT','AAPL')
  -- MISSING count/now/last

-- curent(this) month ( look to see that the aggregates have been ) processed
-- FIX [ ]- MISSING *_last*_ ( prbably also *_now_* ) AND _*sum*_derivatives)
select * from fe_data_store.si_finecon2_aggregates where dateindex = 17438 
-- FIX -[ ] - DID NOT RETURN ANY RECORDS

 { upload_lwd_sipro_dbfs_to_db(exact_near_month_end_dbf_dirs = c(17438,17409), exactly_only_aggregates = NULL, exactly_only_aggregates_group_bys_only = NULL,  decreasing_sort_order = FALSE)  
   upload_lwd_sipro_dbfs_to_db(                                                months_only_back = 14, exactly_only_future_returns = TRUE) 
 }
 
select * from fe_data_store.si_finecon2 where ticker in ('MSFT','AAPL') order by ticker, dateindex;
--looking good

select * from fe_data_store.si_finecon2_aggregates order by dateindex;
--looging good



-- TEMPORARY.sql

-- EXPERIMENT
set effective_cache_size to '6144MB';  -- os + 'shared_buffers'

set search_path to fe_data_store;
-- et search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'






- DROP TABLE fe_data_store.si_finecon2_aggregates;

CREATE TABLE fe_data_store.si_finecon2_aggregates
(
  dateindex integer,
  dateindexlbd integer,
  dateindexeom integer,
  dateindexeom_fct text,
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_assets_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_assets_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_assets_q1 numeric(9,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_mktcap numeric(9,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_assets_q1 numeric(9,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_assets_q1 numeric(9,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_mktcap numeric(9,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp__sector_desc__energy____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp__sector_desc__energy____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_sales_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_sales_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_netinc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_netinc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_ncc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_ncc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_assets_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_assets_q1 numeric(9,2),
  sp_desc__sp__sector_desc__basic_materials____sum_mktcap numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_mktcap numeric(9,2),
  sp_desc__sp__sector_desc__basic_materials____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_now_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_now_inbnd_stmtstat_assets_q1 numeric(9,2),
  sp_desc__sp__sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_now_inbnd_stmtstat_mktcap numeric(9,2),
  sp_desc__sp__sector_desc__basic_materials____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____sum_last_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_last_inbnd_stmtstat_assets_q1 numeric(9,2),
  sp_desc__sp__sector_desc__basic_materials____sum_last_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_last_inbnd_stmtstat_mktcap numeric(9,2),
  sp_desc__sp__sector_desc__basic_materials____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__sector_desc__energy____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp____sum_sales_q1 numeric(9,2),
  sp_desc__sp____sum_netinc_q1 numeric(8,2),
  sp_desc__sp____sum_ncc_q1 numeric(8,2),
  sp_desc__sp____sum_assets_q1 numeric(10,2),
  sp_desc__sp____sum_mktcap numeric(10,2),
  sp_desc__sp____sum_now_inbnd_stmtstat_sales_q1 numeric(9,2),
  sp_desc__sp____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp____sum_now_inbnd_stmtstat_assets_q1 numeric(10,2),
  sp_desc__sp____sum_now_inbnd_stmtstat_mktcap numeric(10,2),
  sp_desc__sp____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp____sum_last_inbnd_stmtstat_sales_q1 numeric(9,2),
  sp_desc__sp____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp____sum_last_inbnd_stmtstat_assets_q1 numeric(10,2),
  sp_desc__sp____sum_last_inbnd_stmtstat_mktcap numeric(10,2),
  sp_desc__sp____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_assets_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_assets_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_assets_q1 numeric(9,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_mktcap numeric(9,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_assets_q1 numeric(9,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_last_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_last_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_last_inbnd_stmtstat_mktcap numeric(9,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500__sector_desc__energy____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp500__sector_desc__energy____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_sales_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_sales_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_netinc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_netinc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_ncc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_ncc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_assets_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_assets_q1 numeric(9,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_mktcap numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_mktcap numeric(9,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_now_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_now_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_now_inbnd_stmtstat_assets_q1 numeric(9,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_now_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_now_inbnd_stmtstat_mktcap numeric(9,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_last_inbnd_stmtstat_sales_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_last_inbnd_stmtstat_assets_q1 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_last_inbnd_stmtstat_assets_q1 numeric(9,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_last_inbnd_stmtstat_mktcap numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_last_inbnd_stmtstat_mktcap numeric(9,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500__sector_desc__energy____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_sales_q1_o_mktcap_x_100 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_sales_q1_o_assets_q1_x_10 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_assets_q1_x_100 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_100 numeric(8,2),
  sp_desc__sp500____count_now_inbnd_stmtstat_dateindex numeric(8,2),
  sp_desc__sp500____rat_count_now_inbnd_stmtstat_dateindex_o_last_x_100 numeric(8,2),
  sp_desc__sp500____count_now_inbnd_stmtstat_dateindexlbd numeric(8,2),
  sp_desc__sp500____rat_count_now_inbnd_stmtstat_dateindexlbd_o_last_x_100 numeric(8,2),
  sp_desc__sp500____sum_sales_q1 numeric(9,2),
  sp_desc__sp500____sum_netinc_q1 numeric(8,2),
  sp_desc__sp500____sum_ncc_q1 numeric(8,2),
  sp_desc__sp500____sum_assets_q1 numeric(10,2),
  sp_desc__sp500____sum_mktcap numeric(10,2),
  sp_desc__sp500____sum_now_inbnd_stmtstat_sales_q1 numeric(9,2),
  sp_desc__sp500____sum_now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500____sum_now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500____sum_now_inbnd_stmtstat_assets_q1 numeric(10,2),
  sp_desc__sp500____sum_now_inbnd_stmtstat_mktcap numeric(10,2),
  sp_desc__sp500____rat_sum_now_inbnd_stmtstat_mktcap_o_last_x_100 numeric(8,2),
  sp_desc__sp500____sum_last_inbnd_stmtstat_sales_q1 numeric(9,2),
  sp_desc__sp500____sum_last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  sp_desc__sp500____sum_last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  sp_desc__sp500____sum_last_inbnd_stmtstat_assets_q1 numeric(10,2),
  sp_desc__sp500____sum_last_inbnd_stmtstat_mktcap numeric(10,2),
  sp_desc__sp500____avg_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500____avg_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500____avg_mktcap_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500____avg_mktcap_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500____avg_assets_q1_wdt_last_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500____avg_assets_q1_wdt_now_inbnd_stmtstat_price numeric(8,2),
  sp_desc__sp500____avg_mktcap_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp500____avg_assets_q1_wdt_pct_freeprice_ret_01m_ann numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____sum_assets_q2 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____sum_assets_q2 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____sum_assets_q2 numeric(9,2),
  sp_desc__sp__sector_desc__basic_materials____sum_assets_q2 numeric(8,2),
  sp_desc__sp__sector_desc__energy____sum_assets_q2 numeric(9,2),
  sp_desc__sp____sum_assets_q2 numeric(10,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____sum_assets_q2 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____sum_assets_q2 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____sum_assets_q2 numeric(9,2),
  sp_desc__sp500__sector_desc__basic_materials____sum_assets_q2 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____sum_assets_q2 numeric(9,2),
  sp_desc__sp500____sum_assets_q2 numeric(10,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp__industry_desc__furniture_and_fixtures____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__gold_and_silver____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp__industry_desc__oil_and_gas_operations____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp__sector_desc__basic_materials____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp__sector_desc__energy____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp500__industry_desc__furniture_and_fixtures____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__gold_and_silver____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp500__industry_desc__oil_and_gas_operations____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp500__sector_desc__basic_materials____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp500__sector_desc__energy____rat_assets_q1_o_mktcap numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500____rat_now_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_curr_mktcap_x_10000 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_sales_q1_o_curr_mktcap_x_100 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_netinc_q1_o_curr_mktcap_x_1000 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_assets_q1_x_10000 numeric(8,2),
  sp_desc__sp500____rat_last_inbnd_stmtstat_ncc_q1_o_sales_q1_x_10000 numeric(8,2),
  sp_desc__sp500____rat_netinc_q1_o_ncc_x100 numeric(8,2),
  sp_desc__sp500____rat_ncc_q1_o_mktcap_x100 numeric(8,2),
  sp_desc__sp500____rat_assets_q1_o_mktcap numeric(8,2)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE fe_data_store.si_finecon2_aggregates
  OWNER TO postgres;

-- Index: fe_data_store.si_finecon2_aggregates_dateindex_key

-- DROP INDEX fe_data_store.si_finecon2_aggregates_dateindex_key;

CREATE UNIQUE INDEX si_finecon2_aggregates_dateindex_key
  ON fe_data_store.si_finecon2_aggregates
  USING btree
  (dateindex);

-----------

data.frame(
  id = c(1, 1, 1, 1),
  id_fct = c("10", "10", "10", "10"),
  col1_fct = c('A1', 'A1', 'A2', 'A2'),
  col2_fct = c('B1', 'B2', 'B1', 'B2'),
  aggr1 = c(8, 16, 32, 64),
  aggr2 = c(10008, 10016, 10032, 10064)
, stringsAsFactors = FALSE
)  -> DFS

# > DFS
#   id id_fct col1_fct col2_fct aggr1 aggr2
# 1  1     10       A1       B1     8 10008
# 2  1     10       A1       B2    16 10016
# 3  1     10       A2       B1    32 10032
# 4  1     10       A2       B2    64 10064

# 
# liquifyDF(x = DF, const_cols_regexpr = "^id", fctr_cols_rexpr = "_fct$")
# 
#   id id_fct a1__b1____aggr1 a1__b2____aggr1 a2__b1____aggr1 a2__b2____aggr1 a1__b1____aggr2 a1__b2____aggr2
# 1  1     10               8              16              32              64           10008           10016
#   a2__b1____aggr2 a2__b2____aggr2
# 1           10032           10064

# PRODUCTION USE
# liquifyDF(si_all_df, const_cols_regexpr = "^dateindex.*", fctr_cols_rexpr = ".*_fct$")


data.frame(
  id = c(1, 1, 1, 1),
  id_fct = c("10", "10", "10", "10"),
  col1_fct = c('A1', NA_character_  , 'A2', 'A2'),
  col2_fct = c('B1', 'B2', 'B1', 'B2'),
  aggr1 = c(8, 16, 32, 64),
  aggr2 = c(10008, 10016, 10032, 10064)
, stringsAsFactors = FALSE
)  -> DFSMISSNG  # GOOD

> DFSMISSNG
  id id_fct col1_fct col2_fct aggr1 aggr2
1  1     10       A1       B1     8 10008
2  1     10     <NA>       B2    16 10016
3  1     10       A2       B1    32 10032
4  1     10       A2       B2    64 10064

# liquifyDF(x = DFSMISSNG, const_cols_regexpr = "^id", fctr_cols_rexpr = "_fct$")             # HERE
#                             # HERE                                                          # HERE 
#   id id_fct a1__b1____aggr1 na__b2____aggr1 a2__b1____aggr1 a2__b2____aggr1 a1__b1____aggr2 na__b2____aggr2
# 1  1     10               8              16              32              64           10008           10016
#   a2__b1____aggr2 a2__b2____aggr2 # 
# 1           10032           10064
# > 
#
# [ ] HIGH [ ] 
# [ ] MAY ADD PARAMETER 'NA' col_fct_NA_replace = NULL, "any" "all" "each"



-----------



'if makes sense'
REGULAR EXPESSION ON INBOUND COLUMNS ~ '_inbnd_stmtstat_STAT(CAPTURED)'


CUBE 'if makes sense'


AGG(...) FILTER(<where clause>) OVER
...
OVER (PARTITION BY )'if makes sense'
  industry_desc over (partition by industry_desc) where industry_desc in ('','')
  sector_desc   over
  

CUBE                                              where sp in ('500','400','600')
  sp500
  notsp500
  --
  sp(all)

rat_[{now/last}_inbnd_stmtstat_]STATn_o_STATd_x_10\d*

COME BACK
avg/sum/count/rat_count

wdt(SPELLED WRONG) -> wtd
  assets_q1/mktcap


-- VANILLA/NOW/LAST

  now_inbnd_stmtid_dateindex integer,
  now_inbnd_stmtid_dateindexlbd integer,
  now_inbnd_stmtstat_sales_q1 numeric(8,2),
  now_inbnd_stmtstat_netinc_q1 numeric(8,2),
  now_inbnd_stmtstat_ncc_q1 numeric(8,2),
  now_inbnd_stmtstat_assets_q1 numeric(9,2),
  now_inbnd_stmtstat_mktcap numeric(8,2),
  now_inbnd_stmtstat_price numeric(8,2)
  
  last_inbnd_stmtid_dateindex integer,
  last_inbnd_stmtid_dateindexlbd integer,
  last_inbnd_stmtstat_sales_q1 numeric(8,2),
  last_inbnd_stmtstat_netinc_q1 numeric(8,2),
  last_inbnd_stmtstat_ncc_q1 numeric(8,2),
  last_inbnd_stmtstat_assets_q1 numeric(9,2),
  last_inbnd_stmtstat_mktcap numeric(8,2),
  last_inbnd_stmtstat_price numeric(8,2),



select count(*) from fe_data_store.si_finecon2;

explain
select to_timestamp(dateindex*3600*24)::date, dateindex, sum(last_inbnd_stmtstat_mktcap) 
filter(where last_inbnd_stmtstat_mktcap is not null) 
from fe_data_store.si_finecon2 
where sp = '500'
group by dateindex
order by dateindex;

select to_timestamp(dateindex*3600*24)::date, dateindex, sum(last_inbnd_stmtstat_mktcap) 
filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
from fe_data_store.si_finecon2 
where sp = '500'
group by dateindex
order by dateindex;


--- [ ]  NEW IN DATABASE
-------------------------
-- cash from investing
-- cash from operations
-- case from investing
-- cash from financing
-- current assets
-- current liablities
-- liabilities

explain
select
    dateindex 
  , to_timestamp(dateindex*3600*24)::date dateindex_dt
  , sum(last_inbnd_stmtstat_mktcap) 
      filter(where last_inbnd_stmtstat_mktcap    is not null and last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(last_inbnd_stmtstat_assets_q1) 
      filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(last_inbnd_stmtstat_mktcap) 
      filter(where last_inbnd_stmtstat_mktcap    is not null and last_inbnd_stmtstat_assets_q1 is not null) /
    sum(last_inbnd_stmtstat_assets_q1) 
      filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1
from fe_data_store.si_finecon2 
where sp = '500' 
group by dateindex
order by dateindex;
"Sort  (cost=281363.97..281364.41 rows=178 width=104)"
"  Sort Key: dateindex"
"  ->  HashAggregate  (cost=281350.64..281357.32 rows=178 width=104)"
"        Group Key: dateindex"
"        ->  Bitmap Heap Scan on si_finecon2  (cost=964.33..280165.89 rows=94780 width=16)"
"              Recheck Cond: (sp = '500'::text)"
"              ->  Bitmap Index Scan on si_finecon2_finecon_sp_500_partial_idx  (cost=0.00..940.64 rows=94780 width=0)"


explain
select
    dateindex 
  , to_timestamp(dateindex*3600*24)::date dateindex_dt
  , 'sp500' col1_fct
  , sum(last_inbnd_stmtstat_mktcap) 
      filter(where last_inbnd_stmtstat_mktcap    is not null and last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(last_inbnd_stmtstat_assets_q1) 
      filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(last_inbnd_stmtstat_mktcap) 
      filter(where last_inbnd_stmtstat_mktcap    is not null and last_inbnd_stmtstat_assets_q1 is not null) /
    sum(last_inbnd_stmtstat_assets_q1) 
      filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1
from fe_data_store.si_finecon2 
where sp = '500' 
group by sp, dateindex
order by sp, dateindex;

"Sort  (cost=281648.02..281649.80 rows=712 width=107)"
"  Sort Key: dateindex"
"  ->  HashAggregate  (cost=281587.59..281614.29 rows=712 width=107)"
"        Group Key: sp, dateindex"
"        ->  Bitmap Heap Scan on si_finecon2  (cost=964.33..280165.89 rows=94780 width=19)"
"              Recheck Cond: (sp = '500'::text)"
"              ->  Bitmap Index Scan on si_finecon2_finecon_sp_500_partial_idx  (cost=0.00..940.64 rows=94780 width=0)"

-- DANGEROUSLY WRONG
-- denominitor of the weighted ( JUST ADD: over (partition by dateindex) sum_last_inbnd_stmtstat_mktcap )
explain
select
    dateindex , company_id
  , to_timestamp(dateindex*3600*24)::date dateindex_dt
  , sum(last_inbnd_stmtstat_mktcap) 
      filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
        over (partition by dateindex) sum_last_inbnd_stmtstat_mktcap
from fe_data_store.si_finecon2 
where sp = '500' 
order by dateindex, company_id;
--88,000 rows

-- PROTECT data from beging written to cells where ( where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null )
explain
select
    dateindex , company_id
  , to_timestamp(dateindex*3600*24)::date dateindex_dt
  , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
      sum(last_inbnd_stmtstat_mktcap) 
        filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
          over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
    else null end sum_last_inbnd_stmtstat_mktcap
from fe_data_store.si_finecon2 
where sp = '500' 
order by dateindex, company_id;
--88,000 rows
"Sort  (cost=299152.50..299389.45 rows=94780 width=48)"
"        ->  Sort  (cost=288000.55..288237.50 rows=94780 width=24)"
"              Sort Key: dateindex, ((last_inbnd_stmtstat_mktcap IS NOT NULL)), ((last_inbnd_stmtstat_assets_q1 IS NOT NULL))"
"              ->  Bitmap Heap Scan on si_finecon2  (cost=964.33..280165.89 rows=94780 width=24)"
"                    Recheck Cond: (sp = '500'::text)"
"                    ->  Bitmap Index Scan on si_finecon2_finecon_sp_500_partial_idx  (cost=0.00..940.64 rows=94780 width=0)"

explain
with index_wieghts as (
  select
      dateindex , company_id
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
  from fe_data_store.si_finecon2 
  where sp = '500' 
  order by dateindex, company_id
)
select * from index_wieghts;
"CTE Scan on index_wieghts  (cost=299389.45..301285.05 rows=94780 width=72)"
"  CTE index_wieghts"
"    ->  Sort  (cost=299152.50..299389.45 rows=94780 width=48)"
"          Sort Key: si_finecon2.dateindex, si_finecon2.company_id"
"          ->  WindowAgg  (cost=288000.55..291317.85 rows=94780 width=48)"
"                ->  Sort  (cost=288000.55..288237.50 rows=94780 width=24)"
"                      Sort Key: si_finecon2.dateindex, ((si_finecon2.last_inbnd_stmtstat_mktcap IS NOT NULL)), ((si_finecon2.last_inbnd_stmtstat_assets_q1 IS NOT NULL))"
"                      ->  Bitmap Heap Scan on si_finecon2  (cost=964.33..280165.89 rows=94780 width=24)"
"                            Recheck Cond: (sp = '500'::text)"
"                            ->  Bitmap Index Scan on si_finecon2_finecon_sp_500_partial_idx  (cost=0.00..940.64 rows=94780 width=0)"

-- begin try
-- average mktcap wieighted by assets

explain
with index_wieghts as (
  select
      dateindex , company_id
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
  from fe_data_store.si_finecon2 
  where sp = '500' 
  order by dateindex, company_id
) --88,000 rows
select 1 
from  fe_data_store.si_finecon2 fe, index_wieghts w
where     fe.dateindex  = w.dateindex 
      and fe.company_id = w.company_id;
"Merge Join  (cost=309120.13..351773.25 rows=41705 width=4)"
"  Merge Cond: ((fe.dateindex = w.dateindex) AND (fe.company_id = w.company_id))"
"  CTE index_wieghts"
"    ->  Sort  (cost=299152.50..299389.45 rows=94780 width=48)"
"          Sort Key: si_finecon2.dateindex, si_finecon2.company_id"
"          ->  WindowAgg  (cost=288000.55..291317.85 rows=94780 width=48)"
"                ->  Sort  (cost=288000.55..288237.50 rows=94780 width=24)"
"                      Sort Key: si_finecon2.dateindex, ((si_finecon2.last_inbnd_stmtstat_mktcap IS NOT NULL)), ((si_finecon2.last_inbnd_stmtstat_assets_q1 IS NOT NULL))"
"                      ->  Bitmap Heap Scan on si_finecon2  (cost=964.33..280165.89 rows=94780 width=24)"
"                            Recheck Cond: (sp = '500'::text)"
"                            ->  Bitmap Index Scan on si_finecon2_finecon_sp_500_partial_idx  (cost=0.00..940.64 rows=94780 width=0)"
"  ->  Index Only Scan using si_finecon2_dateindex_company_id_key on si_finecon2 fe  (cost=0.43..34019.34 rows=1501261 width=10)"
"  ->  Sort  (cost=9730.25..9967.20 rows=94780 width=36)"
"        Sort Key: w.dateindex, w.company_id"
"        ->  CTE Scan on index_wieghts w  (cost=0.00..1895.60 rows=94780 width=36)"

explain
with index_wieghts as (
  select
      dateindex , company_id
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
  from fe_data_store.si_finecon2 
  where sp = '500' 
  order by dateindex, company_id
) --88,000 rows
select 1 
from  fe_data_store.si_finecon2 fe, index_wieghts w
where     fe.dateindex  = w.dateindex 
      and fe.company_id = w.company_id
      and sp = '500'; --redundant ( but safe ) becuase WITH also RETURNS 88,000 rows ( ADD )
"Merge Join  (cost=597120.25..598568.28 rows=2633 width=4)"
"  Merge Cond: ((fe.dateindex = w.dateindex) AND (fe.company_id = w.company_id))"
"  CTE index_wieghts"
"    ->  Sort  (cost=299152.50..299389.45 rows=94780 width=48)"
"          Sort Key: si_finecon2.dateindex, si_finecon2.company_id"
"          ->  WindowAgg  (cost=288000.55..291317.85 rows=94780 width=48)"
"                ->  Sort  (cost=288000.55..288237.50 rows=94780 width=24)"
"                      Sort Key: si_finecon2.dateindex, ((si_finecon2.last_inbnd_stmtstat_mktcap IS NOT NULL)), ((si_finecon2.last_inbnd_stmtstat_assets_q1 IS NOT NULL))"
"                      ->  Bitmap Heap Scan on si_finecon2  (cost=964.33..280165.89 rows=94780 width=24)"
"                            Recheck Cond: (sp = '500'::text)"
"                            ->  Bitmap Index Scan on si_finecon2_finecon_sp_500_partial_idx  (cost=0.00..940.64 rows=94780 width=0)"
"  ->  Sort  (cost=288000.55..288237.50 rows=94780 width=10)"
"        Sort Key: fe.dateindex, fe.company_id"
"        ->  Bitmap Heap Scan on si_finecon2 fe  (cost=964.33..280165.89 rows=94780 width=10)"
"              Recheck Cond: (sp = '500'::text)"
"              ->  Bitmap Index Scan on si_finecon2_finecon_sp_500_partial_idx  (cost=0.00..940.64 rows=94780 width=0)"
"  ->  Sort  (cost=9730.25..9967.20 rows=94780 width=36)"
"        Sort Key: w.dateindex, w.company_id"
"        ->  CTE Scan on index_wieghts w  (cost=0.00..1895.60 rows=94780 width=36)"

-- want average mktcap weighted by assets_q1

with index_wieghts as (
  select
      dateindex , company_id
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
  from fe_data_store.si_finecon2 
  where sp = '500' 
  order by dateindex, company_id
) --88,000 rows
select 1 
from  fe_data_store.si_finecon2 fe, index_wieghts w
where     fe.dateindex  = w.dateindex 
      and fe.company_id = w.company_id
      and sp = '500'; 

-- want
-- ave mktcap -- weighted BY assets
-- AVG( mktcap * MY_ASSETS * SUM_ALL_ASSETS )
-- want average mktcap weighted by assets_q1
--
-- TO DO: COMBINE WITH ABOVE
--

with index_wieghts as (
  select
      dateindex , company_id
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
  from fe_data_store.si_finecon2 
  where sp = '500' 
  order by dateindex, company_id
)
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , 'sp500' col1_fct
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1
from  fe_data_store.si_finecon2 fe, index_wieghts w
where     fe.dateindex  = w.dateindex 
      and fe.company_id = w.company_id
      and sp = '500'
group by fe.sp, fe.dateindex
order by fe.sp, fe.dateindex;
-- TECHICALLY WORKS


-- GOOD
-- A SOLUTION ( 'WITH PART' SINGLE FACTOR 'sp500' )
with index_weights as (
  select
      dateindex , company_id
    , 'sp500' col1_fct
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
    , case when last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_assets_q1) 
          filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_assets_q1 is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_assets_q1
  from fe_data_store.si_finecon2 
  where sp = '500' 
    and last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null -- for safety (filters: elim, over: not_elim)
  order by dateindex, company_id
-- 88,000 rows
)
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , 'sp500' col1_fct
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) avg_last_inbnd_stmtstat_mktcap_wtd_by_assets_q1
from  fe_data_store.si_finecon2 fe, index_weights w
where     fe.dateindex  = w.dateindex 
      and fe.company_id = w.company_id
      and sp = '500'
      and last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null -- for safety (filters: elim, over: not_elim)
group by fe.sp, fe.dateindex
order by fe.sp, fe.dateindex;
-- 177 records



-- GOOD
-- A SOLUTION ( 'WITH PART' SINGLE FACTOR 'sp500' )
with index_weights as (
  select
      dateindex , company_id
    , 'sp500' col1_fct
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
    , case when last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_assets_q1) 
          filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_assets_q1 is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_assets_q1
  from fe_data_store.si_finecon2 
  where sp = '500' 
    and last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null -- for safety (filters: elim, over: not_elim)
  order by dateindex, company_id
-- 88,000 rows
)
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , 'sp500' col1_fct
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) avg_last_inbnd_stmtstat_mktcap_wtd_by_assets_q1
from  fe_data_store.si_finecon2 fe, index_weights w
where     fe.dateindex  = w.dateindex 
      and fe.company_id = w.company_id
      and sp = '500'
      and last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null -- for safety (filters: elim, over: not_elim)
group by fe.sp, fe.dateindex
order by fe.sp, fe.dateindex;
-- 177 records


-- BETTER ( 1 of 2 TO KEEP )
-- A SOLUTION ( 'WITH PART' SINGLE FACTOR 'sp500' )
with index_weights as (
  select
      dateindex , company_id
    , to_timestamp(dateindex*3600*24)::date dateindex_dt
    , case when last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_mktcap) 
          filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_mktcap is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_mktcap
    , case when last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null then
        sum(last_inbnd_stmtstat_assets_q1) 
          filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null) 
            over (partition by dateindex, last_inbnd_stmtstat_assets_q1 is not null, last_inbnd_stmtstat_assets_q1 is not null) 
      else null end sum_last_inbnd_stmtstat_assets_q1
  from fe_data_store.si_finecon2 
  where sp = '500' 
    and last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null -- for safety (filters: elim, over: not_elim)
  order by dateindex, company_id
-- 88,000 rows
)
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , 'whatsp' col1_fct -- dummy
  , 'sp500'  col2_fct -- dummy
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) avg_last_inbnd_stmtstat_mktcap_wtd_by_assets_q1
from  fe_data_store.si_finecon2 fe, index_weights w
where     fe.dateindex  = w.dateindex 
      and fe.company_id = w.company_id
      and sp = '500'
      and last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null -- for safety (filters: elim, over: not_elim)
group by fe.dateindex
order by fe.dateindex;
-- 177 records








-- BEGIN TRY ...  A SOLUTION WITH MULTIPLE FACTORS ( 'WITH' PART SHOULD/MUST RETURN 177 RECORDS )
-- WILL JOIN ON DATEINDEX ( ELIM PARTITIONS ... ADD 'GROUP BY DATEINEX' )
select
    dateindex --, company_id
  , 'sp500' col1_fct
  , to_timestamp(dateindex*3600*24)::date dateindex_dt
  , 
      sum(last_inbnd_stmtstat_mktcap) 
        filter(where last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null) 
                  sum_last_inbnd_stmtstat_mktcap
  , 
      sum(last_inbnd_stmtstat_assets_q1) 
        filter(where last_inbnd_stmtstat_assets_q1 is not null and last_inbnd_stmtstat_assets_q1 is not null) 
                   sum_last_inbnd_stmtstat_assets_q1
from fe_data_store.si_finecon2 
where sp = '500' 
  and last_inbnd_stmtstat_mktcap is not null and last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
group by dateindex
order by dateindex--, company_id
-- TECHNICALLY WORKS

select
    fe.dateindex --, company_id
  , 'sp500' col1_fct
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , 
      sum(fe.last_inbnd_stmtstat_mktcap) 
        filter(where fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                  sum_last_inbnd_stmtstat_mktcap
  , 
      sum(fe.last_inbnd_stmtstat_assets_q1) 
        filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                   sum_last_inbnd_stmtstat_assets_q1
from ( select fei.* from fe_data_store.si_finecon2 fei ) fe 
where fe.sp = '500' 
  and fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
group by fe.dateindex
order by fe.dateindex--, company_id
-- ONLY ADDED aliases



-- GOOD
-- A SOLUTION ( 'WITH PART' MULTPLE FACTOR 'sp500'/'spnot500' )
--drop index si_finecon2_finecon_sp_partial_idx
CREATE INDEX si_finecon2_finecon_sp_partial_idx
  ON fe_data_store.si_finecon2 (dateindex)
  WHERE sp in ('500','400','600');
-- 78 seconds
select
    fe.dateindex --, company_id
  , col1_fct
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , 
      sum(fe.last_inbnd_stmtstat_mktcap) 
        filter(where fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                  sum_last_inbnd_stmtstat_mktcap
  , 
      sum(fe.last_inbnd_stmtstat_assets_q1) 
        filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                   sum_last_inbnd_stmtstat_assets_q1
from ( -- fe
       -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
       select
           fei.dateindex 
         , case when fei.sp = '500' then 'sp500' else 'spnot500' end col1_fct
         , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
       from fe_data_store.si_finecon2 fei 
       where fei.sp in('500','400','600') 
         and fei.last_inbnd_stmtstat_mktcap is not null and fei.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
     ) fe -- 261,000 records
--where fe.sp in('500','400','600') 
--  and fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
group by   fe.dateindex
         , fe.col1_fct
order by   fe.col1_fct 
         , fe.dateindex --, company_id
-- 354 ROWS


CREATE INDEX si_finecon2_finecon_sp_partial_idx
  ON fe_data_store.si_finecon2 (dateindex)
  WHERE sp in ('500','400','600');
with index_weights as (
  -- KEEPISH
  -- A SOLUTION ( 'WITH' PART MULTPLE FACTOR 'sp500'/'spnot500' )
  select
      fe.dateindex --, company_id
    , col1_fct
    , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
    , 
        sum(fe.last_inbnd_stmtstat_mktcap) 
          filter(where fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                    sum_last_inbnd_stmtstat_mktcap
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                     sum_last_inbnd_stmtstat_assets_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , case when fei.sp = '500' then 'sp500' else 'spnot500' end col1_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
         from fe_data_store.si_finecon2 fei 
         where fei.sp in('500','400','600') 
           and fei.last_inbnd_stmtstat_mktcap is not null and fei.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
       ) fe -- 261,000 records
  --where fe.sp in('500','400','600') 
  --  and fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
  group by   fe.dateindex
           , fe.col1_fct
  order by   fe.col1_fct 
           , fe.dateindex --, company_id
) -- 354 ROWS
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , fe.col1_fct
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) avg_last_inbnd_stmtstat_mktcap_wtd_by_assets_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , case when fei.sp = '500' then 'sp500' else 'spnot500' end col1_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
         from fe_data_store.si_finecon2 fei 
         where fei.sp in('500','400','600') 
           and fei.last_inbnd_stmtstat_mktcap is not null and fei.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
       ) fe -- 261,000 records
     , index_weights w
where     fe.dateindex  = w.dateindex 
   -- and fe.company_id = w.company_id
   -- and sp = '500'
      and fe.col1_fct = w.col1_fct
group by   fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
         , fe.col1_fct
order by   fe.col1_fct 
         , fe.dateindex;
-- 354 ROWS


-- select r_version();
-- 
-- select * from information_schema.table_constraints;
-- 
-- select constraint_name 
--                 from information_schema.table_constraints 
--                   where constraint_schema   = 'fe_data_store' 
--                   and   table_name          = 'si_finecon2' 
--                   and   constraint_type     = 'PRIMARY KEY';
-- 
-- 
-- ALTER TABLE fe_data_store.si_finecon2 DROP CONSTRAINT si_finecon2_pkey;
-- 
-- ALTER TABLE fe_data_store.si_finecon2
--   ADD CONSTRAINT si_finecon2_pkey PRIMARY KEY(dateindex_company_id);
-- 
-- create unique index if not exists si_finecon2_pkey on si_finecon2(dateindex_company_id);
-- 
-- alter table if exists si_finecon2 add primary key using index si_finecon2_pkey;
-- 
-- 
--   drop table upsert_temp;
-- create table upsert_temp(dateindex_company_id text);
-- alter table upsert_temp drop constraint upsert_temp_pkey;
-- 
-- 
-- select constraint_name 
--                   from information_schema.table_constraints 
--                     where table_name          = 'upsert_temp' 
--                     and   constraint_type     = 'PRIMARY KEY';
-- 
-- 
-- alter table if exists upsert_temp add primary key using index upsert_temp_pkey;
-- 
-- 
-- SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();




-- new ( ALREADY IN R PROGRAM [ ]  )
create index if not exists si_finecon2_finecon_sp_partial_idx on 
                  si_finecon2(dateindex,sp) where sp in ('500','400','600');

explain
"GroupAggregate  (cost=628923.38..628924.76 rows=19 width=200)"
"  Group Key: ('spwhat'::text), (CASE WHEN (fei.sp = '500'::text) THEN 'sp500'::text ELSE 'spnot500'::text END), fei.dateindex"
"  CTE index_weights"
"    ->  Sort  (cost=605071.94..605073.72 rows=712 width=136)"
"          Sort Key: (CASE WHEN (fei_1.sp = '500'::text) THEN 'sp500'::text ELSE 'spnot500'::text END), fei_1.dateindex"
"          ->  HashAggregate  (cost=605016.85..605038.21 rows=712 width=136)"
"                Group Key: 'spwhat'::text, CASE WHEN (fei_1.sp = '500'::text) THEN 'sp500'::text ELSE 'spnot500'::text END, fei_1.dateindex"
"                ->  Bitmap Heap Scan on si_finecon2 fei_1  (cost=2837.86..602599.56 rows=193383 width=80)"
"                      Recheck Cond: (sp = ANY ('{500,400,600}'::text[]))"
"                      Filter: ((last_inbnd_stmtstat_mktcap IS NOT NULL) AND (last_inbnd_stmtstat_assets_q1 IS NOT NULL))"
"                      ->  Bitmap Index Scan on si_finecon2_finecon_sp_partial_idx  (cost=0.00..2789.51 rows=271478 width=0)"
"  ->  Sort  (cost=23849.66..23849.71 rows=19 width=112)"
"        Sort Key: (CASE WHEN (fei.sp = '500'::text) THEN 'sp500'::text ELSE 'spnot500'::text END), fei.dateindex"
"        ->  Nested Loop  (cost=23.74..23849.26 rows=19 width=112)"
"              ->  CTE Scan on index_weights w  (cost=0.00..16.02 rows=4 width=68)"
"                    Filter: ('spwhat'::text = col1_fct)"
"              ->  Bitmap Heap Scan on si_finecon2 fei  (cost=23.74..5958.25 rows=5 width=19)"
"                    Recheck Cond: ((dateindex = w.dateindex) AND (sp = ANY ('{500,400,600}'::text[])))"
"                    Filter: ((last_inbnd_stmtstat_mktcap IS NOT NULL) AND (last_inbnd_stmtstat_assets_q1 IS NOT NULL) AND (w.col2_fct = CASE WHEN (sp = '500'::text) THEN 'sp500'::text ELSE 'spnot500'::text END))"
"                    ->  Bitmap Index Scan on si_finecon2_finecon_sp_partial_idx  (cost=0.00..23.73 rows=1525 width=0)"
"                          Index Cond: (dateindex = w.dateindex)"             
with index_weights as (
  -- KEEPISH
  -- A SOLUTION ( 'WITH' PART MULTPLE FACTOR 'sp500'/'spnot500' )
  select
      fe.dateindex --, company_id
    , fe.col1_fct
    , fe.col2_fct
    , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
    , 
        sum(fe.last_inbnd_stmtstat_mktcap) 
          filter(where fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                    sum_last_inbnd_stmtstat_mktcap
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                     sum_last_inbnd_stmtstat_assets_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'spwhat'::text col1_fct
           , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
         from fe_data_store.si_finecon2 fei 
         where fei.sp in('500','400','600') 
           and fei.last_inbnd_stmtstat_mktcap is not null and fei.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
       ) fe -- 261,000 records
  --where fe.sp in('500','400','600') 
  --  and fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
  group by   fe.dateindex
           , fe.col1_fct
           , fe.col2_fct
  order by   fe.col1_fct
           , fe.col2_fct 
           , fe.dateindex --, company_id
) -- 354 ROWS
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , fe.col1_fct
  , fe.col2_fct
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) avg_last_inbnd_stmtstat_mktcap_wtd_by_assets_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'spwhat'::text col1_fct 
           , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
         from fe_data_store.si_finecon2 fei 
         where fei.sp in('500','400','600') 
           and fei.last_inbnd_stmtstat_mktcap is not null and fei.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
       ) fe -- 261,000 records
     , index_weights w
where     fe.dateindex  = w.dateindex 
   -- and fe.company_id = w.company_id
   -- and sp = '500'
      and fe.col1_fct = w.col1_fct
      and fe.col2_fct = w.col2_fct
group by   fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
         , fe.col1_fct
         , fe.col2_fct
order by   fe.col1_fct
         , fe.col2_fct 
         , fe.dateindex;
-- 354 ROWS



-- TEMPORARY.sql

-- EXPERIMENT
set effective_cache_size to '6144MB';  -- os + 'shared_buffers'

set search_path to fe_data_store;
-- set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'



-- KEEPISH ( 2 of 2 TO KEEP )
-- A SOLUTION ( MULTPLE FACTOR 'sp500'/'spnot500' )
-- BETTER
--
--  old ( ALREADY IN THE PROGRAM )
-- CREATE INDEX si_finecon2_finecon_sp_partial_idx
--   ON fe_data_store.si_finecon2 (dateindex)
--   WHERE sp in ('500','400','600');
with index_weights as (
  -- KEEPISH
  -- A SOLUTION ( 'WITH' PART MULTPLE FACTOR 'sp500'/'spnot500' )
  select
      fe.dateindex --, company_id
    , fe.col1_fct
    , fe.col2_fct
    , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
    , 
        sum(fe.last_inbnd_stmtstat_mktcap) 
          filter(where fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                    sum_last_inbnd_stmtstat_mktcap
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
                     sum_last_inbnd_stmtstat_assets_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct
           , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
         from fe_data_store.si_finecon2 fei 
         where fei.sp in('500','400','600') 
           and fei.last_inbnd_stmtstat_mktcap is not null and fei.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
           -- -- and fei.dateindex = 17438 
       ) fe -- 261,000 records
  --where fe.sp in('500','400','600') 
  --  and fe.last_inbnd_stmtstat_mktcap is not null and fe.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
  group by   fe.dateindex
           , fe.col1_fct
           , fe.col2_fct
  order by   fe.col1_fct
           , fe.col2_fct 
           , fe.dateindex --, company_id
) -- 354 ROWS
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , fe.col1_fct
  , fe.col2_fct
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) avg_last_inbnd_stmtstat_mktcap_wtd_by_assets_q1
  -- NEW
  -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 10 sum_last_liab_q1_o_last_assets_q1_x10
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 10 sum_last_liab_q1_o_mktcap_x10
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
    (nullif(
      sum(fe.last_inbnd_stmtstat_assets_q1) 
        filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
      sum(fe.last_inbnd_stmtstat_liab_q1) 
        filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
    ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
  -- STAT / ASSETS     STAT / MKTCAP
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
  -- SUM OF THE CURRENTS CA + CL
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
    ) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
    ) /
    sum(fe.mktcap) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100
  -- ENDNEW
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct 
           , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
           -- NEW
           , fei.last_inbnd_stmtstat_liab_q1
           , fei.mktcap
           , fei.last_inbnd_stmtstat_netinc_q1
           , fei.last_inbnd_stmtstat_ncc_q1
           , fei.last_inbnd_stmtstat_tco_q1
           , fei.last_inbnd_stmtstat_tcf_q1
           , fei.last_inbnd_stmtstat_tci_q1
           , fei.last_inbnd_stmtstat_ca_q1
           , fei.last_inbnd_stmtstat_cl_q1
           -- ENDNEW
         from fe_data_store.si_finecon2 fei 
         where fei.sp in('500','400','600') 
           -- and fei.last_inbnd_stmtstat_mktcap is not null and fei.last_inbnd_stmtstat_assets_q1 is not null -- for safety (filter(): elim, over(): not_elim)
       ) fe -- 261,000 records
     , index_weights w
where     fe.dateindex  = w.dateindex 
      -- -- and fe.dateindex = 17438  
   -- and fe.company_id = w.company_id
   -- and sp = '500'
      and fe.col1_fct = w.col1_fct
      and fe.col2_fct = w.col2_fct
group by   fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
         , fe.col1_fct
         , fe.col2_fct
order by   fe.col1_fct
         , fe.col2_fct 
         , fe.dateindex;
-- 354 ROWS

-- 49-72
-- 153-159, 
-- 177 (17438)


--17438;"2017-09-29";"sp_desc";"spnot500";2389614.90;3949892.20;60.49823081247635062000;3.98269988709544191286;7.33105647387736765720;12.0036095052974190;27.4680089785478700;43.14984491559432770000;6.96518292545719310000;-7.72414794270400570000;-9.91379829652240190000;16.17865253064460648000;21.34945988480674454000;-0.099622852518275571271000;-0.30006527858821264000;-17.33714119564768152000;-22.79145481745775752000;15.86347029303897433300;26.05576879703526519800;9.08478457082373673300;14.92468534534691195600;24.94753115488521314900;39.26313253250970457400
-- -- -- and fe/fei.dateindex = 17438 
--17438;"2017-09-29";"sp_desc";"spnot500";2389614.90;3949892.20;60.49823081247635062000;3.98269988709544191286;7.33105647387736765720;12.0036095052974190;27.4680089785478700;43.14984491559432770000;6.96518292545719310000;-7.72414794270400570000;-9.91379829652240190000;16.17865253064460648000;21.34945988480674454000;-0.099622852518275571271000;-0.30006527858821264000;-17.33714119564768152000;-22.79145481745775752000;15.86347029303897433300;26.05576879703526519800;9.08478457082373673300;14.92468534534691195600;24.94753115488521314900;39.26313253250970457400

-------------------
-------------------


-- A SOLUTION ( 'WITH' PART MULTPLE FACTOR-ABLE: 'sp500'/'spnot500' )

-- CREATE INDEX si_finecon2_finecon_sp_partial_idx
--   ON fe_data_store.si_finecon2 (dateindex)
--   WHERE sp in ('500','400','600');
with index_weights as (
  select
      fe.dateindex --, company_id
    , fe.col1_fct
    , fe.col2_fct
    , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.price is not null) 
                     sum_wrt_price_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f52w_ann is not null) 
                     sum_wrt_pradchg_f52w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f26w_ann is not null) 
                     sum_wrt_pradchg_f26w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f13w_ann is not null) 
                     sum_wrt_pradchg_f13w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f04w_ann is not null) 
                     sum_wrt_pradchg_f04w_ann_mktcap
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                     sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.mktcap is not null) 
                     sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1
    , 
        sum(fe.last_inbnd_stmtstat_liab_q1) 
          filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                     sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1
    , 
        sum(fe.last_inbnd_stmtstat_liab_q1) 
          filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.mktcap is not null) 
                     sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct
        -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , case when fei.sp = '500' then 'sp500' else 'ERROR' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1, fei.last_inbnd_stmtstat_liab_q1, fei.mktcap
           , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
           , fei.price
         from fe_data_store.si_finecon2 fei 
        -- where fei.sp in('500','400','600') 
           where fei.sp in('500')
           -- and fei.dateindex = 17438 
       ) fe -- 261,000 records ( if 'sp500' and 'spnot500' )
  group by   fe.dateindex
           , fe.col1_fct
           , fe.col2_fct
  order by   fe.col1_fct
           , fe.col2_fct 
           , fe.dateindex 
) -- 354 ROWS ( if 'sp500' and 'spnot500' )
select
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , fe.col1_fct
  , fe.col2_fct
  -- THE RESPONSE VALUE
  , avg(price * fe.mktcap / nullif(w.sum_wrt_price_mktcap,0)  ) 
      filter(where fe.price    is not null and fe.mktcap is not null) * 1000                     avg_price_wtd_by_mktcap_x1000
  -- CANDIDATE RESPONSE VARIABLES
  , avg(fe.pradchg_f52w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f52w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f52w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f52w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f26w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f26w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f26w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f26w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f13w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f13w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f13w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f13w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f04w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f04w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f04w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f04w_ann_wtd_by_mktcap_x1000
  -- BEGIN NON-RESPONSE VARIABLES
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) * 10 avg_last_mktcap_wtd_by_assets_q1_x10
  , avg(fe.mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)                     * 10 avg_mktcap_wtd_by_assets_q1_x10
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null) * 10 avg_last_mktcap_wtd_by_liab_q1_x10
  , avg(fe.mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
      filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null)                     * 10 avg_mktcap_wtd_by_liab_q1_x10
  -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 100 sum_last_liab_q1_o_last_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 100 sum_last_liab_q1_o_mktcap_x100
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
    (nullif(
      sum(fe.last_inbnd_stmtstat_assets_q1) 
        filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
      sum(fe.last_inbnd_stmtstat_liab_q1) 
        filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
    ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
  -- STAT / ASSETS     STAT / MKTCAP
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
  -- SUM OF THE CURRENTS CA + CL
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
    ) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
    ) /
    sum(fe.mktcap) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct 
        -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , case when fei.sp = '500' then 'sp500' else 'ERROR' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
           , fei.last_inbnd_stmtstat_liab_q1
           , fei.mktcap
           , fei.last_inbnd_stmtstat_netinc_q1
           , fei.last_inbnd_stmtstat_ncc_q1
           , fei.last_inbnd_stmtstat_tco_q1
           , fei.last_inbnd_stmtstat_tcf_q1
           , fei.last_inbnd_stmtstat_tci_q1
           , fei.last_inbnd_stmtstat_ca_q1
           , fei.last_inbnd_stmtstat_cl_q1
           , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
           , fei.price
         from fe_data_store.si_finecon2 fei 
        -- where fei.sp in('500','400','600') -- 261,000 records
           where fei.sp in('500')
       ) fe 
     , index_weights w
where     fe.dateindex  = w.dateindex 
       -- and fe.dateindex = 17438  
      and fe.col1_fct = w.col1_fct
      and fe.col2_fct = w.col2_fct
group by   fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
         , fe.col1_fct
         , fe.col2_fct
order by   fe.col1_fct
         , fe.col2_fct 
         , fe.dateindex;
-- 354 ROWS ( if 'sp500' and 'spnot500' )

-- ROWS OF INTEREST
-- 49-72        (CRASH OF 2007/2008
-- 153-159      (LATE 2015/EARLY 2016)
-- 177 (17438)  (NOW)


--17438;"2017-09-29";"sp_desc";"spnot500";2389614.90;3949892.20;60.49823081247635062000;3.98269988709544191286;7.33105647387736765720;12.0036095052974190;27.4680089785478700;43.14984491559432770000;6.96518292545719310000;-7.72414794270400570000;-9.91379829652240190000;16.17865253064460648000;21.34945988480674454000;-0.099622852518275571271000;-0.30006527858821264000;-17.33714119564768152000;-22.79145481745775752000;15.86347029303897433300;26.05576879703526519800;9.08478457082373673300;14.92468534534691195600;24.94753115488521314900;39.26313253250970457400
-- -- -- and fe/fei.dateindex = 17438 
--17438;"2017-09-29";"sp_desc";"spnot500";2389614.90;3949892.20;60.49823081247635062000;3.98269988709544191286;7.33105647387736765720;12.0036095052974190;27.4680089785478700;43.14984491559432770000;6.96518292545719310000;-7.72414794270400570000;-9.91379829652240190000;16.17865253064460648000;21.34945988480674454000;-0.099622852518275571271000;-0.30006527858821264000;-17.33714119564768152000;-22.79145481745775752000;15.86347029303897433300;26.05576879703526519800;9.08478457082373673300;14.92468534534691195600;24.94753115488521314900;39.26313253250970457400


-- LEFT_OFF: FIGURE OUT WHY (full outer join dateindex in(values()), show: price, mktcap )
-- avg_price_wtd_by_mktcap_x1000
-- BIG GAP JUMPS ... --


14638;"2010-01-29";"sp_desc";"sp500"; 116.52049771681620833000
14666;"2010-02-26";"sp_desc";"sp500";4380.36687547857637787000

15491;"2012-05-31";"sp_desc";"sp500";3938.30972380523886773000
15520;"2012-06-29";"sp_desc";"sp500"; 190.38034974170349658000

16493;"2015-02-27";"sp_desc";"sp500"; 214.67842986519413895000
16525;"2015-03-31";"sp_desc";"sp500";8194.18502454483139611000


explain
select ticker, company, price, mktcap from fe_data_store.si_finecon2
where sp = '500' and dateindex  IN (VALUES(14638),(14666))
order by price desc;
"BRK.A";"Berkshire Hathaway Inc.";119800.00;186194.40
"GOOG";"Google Inc.";529.94;168135.50
"GOOG";"Google Inc.";526.80;167511.50
"WPO";"Washington Post Company, The";434.62;4085.40
"WPO";"Washington Post Company, The";420.31;3950.90

explain
select ticker, company, price, mktcap from fe_data_store.si_finecon2
where sp = '500' and dateindex  IN (VALUES(15491),(15520))
order by price desc;
"BRK.A";"Berkshire Hathaway Inc.";118850.00;196387.20
"PCLN";"Priceline.com Inc";664.52;33088.50
"PCLN";"Priceline.com Inc";625.49;31145.10
"AAPL";"Apple Inc.";584.00;546076.20
"GOOG";"Google Inc";580.86;189374.90
"GOOG";"Google Inc";580.07;189117.40
"AAPL";"Apple Inc.";577.73;540213.40

explain
select ticker, company, price, mktcap from fe_data_store.si_finecon2
where sp = '500' and dateindex  IN (VALUES(16493),(16525))
order by price desc;
"BRK.A";"Berkshire Hathaway Inc.";217500.00;356510.80
"PCLN";"Priceline Group Inc";1237.48;64273.70
"PCLN";"Priceline Group Inc";1164.15;60465.00
"AZO";"AutoZone, Inc.";682.16;21640.40
"CMG";"Chipotle Mexican Grill, Inc.";664.97;20628.90
"CMG";"Chipotle Mexican Grill, Inc.";650.54;20198.50
"AZO";"AutoZone, Inc.";642.68;20500.10





--NOTE: count and sum AUTOMATICALLY 'do not include NULLs'
--
-- global flexible: just add/subtract expression: xyz.dateindex = XYZ 
--
--What does the S&P 500 index measure and how is it calculated?
--sum of the adjusted market capitalization of all S&P 500 stocks 
--  and 
--then dividing it with an index divisor
--http://www.investopedia.com/ask/answers/040215/what-does-sp-500-index-measure-and-how-it-calculated.asp
--
-- A SOLUTION ( 'WITH' PART MULTPLE FACTOR-ABLE: 'sp500'/'spnot500' )
--
-- create index si_finecon2_finecon_sp_partial_idx
--   on fe_data_store.si_finecon2 (dateindex)
--   where sp in ('500','400','600');
--
with index_weights as (
  select
      fe.dateindex 
    , fe.col1_fct
    , fe.col2_fct
    , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f52w_ann is not null) 
                     sum_wrt_pradchg_f52w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f26w_ann is not null) 
                     sum_wrt_pradchg_f26w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f13w_ann is not null) 
                     sum_wrt_pradchg_f13w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f04w_ann is not null) 
                     sum_wrt_pradchg_f04w_ann_mktcap
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                     sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.mktcap is not null) 
                     sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1
    , 
        sum(fe.last_inbnd_stmtstat_liab_q1) 
          filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                     sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1
    , 
        sum(fe.last_inbnd_stmtstat_liab_q1) 
          filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.mktcap is not null) 
                     sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct
        -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , case when fei.sp = '500' then 'sp500' else 'ERROR' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1, fei.last_inbnd_stmtstat_liab_q1, fei.mktcap
           , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
           , fei.price
         from fe_data_store.si_finecon2 fei 
        -- where fei.sp in('500','400','600') 
           where fei.sp in('500')
          -- and fei.dateindex = 17438 
       ) fe -- 261,000 records ( if 'sp500' and 'spnot500' )
  group by   fe.dateindex
           , fe.col1_fct
           , fe.col2_fct
  order by   fe.col1_fct
           , fe.col2_fct 
           , fe.dateindex 
) -- 354 ROWS ( if 'sp500' and 'spnot500' )
select
  -- DISPLAY ORDER IS HERE
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , fe.col1_fct
  , fe.col2_fct
  -- THE RESPONSE VALUE
  , sum(fe.mktcap) filter(where fe.mktcap is not null) / nullif(count(fe.mktcap) filter(where fe.mktcap is not null),0) / 1000.0 sum_mktcap_o_count_d1000
  -- CANDIDATE RESPONSE VARIABLES
  , avg(fe.pradchg_f52w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f52w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f52w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f52w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f26w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f26w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f26w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f26w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f13w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f13w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f13w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f13w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f04w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f04w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f04w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f04w_ann_wtd_by_mktcap_x1000
  -- BEGIN NON-RESPONSE VARIABLES
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) * 10 avg_last_mktcap_wtd_by_assets_q1_x10
  , avg(fe.mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)                     * 10 avg_mktcap_wtd_by_assets_q1_x10
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null) * 10 avg_last_mktcap_wtd_by_liab_q1_x10
  , avg(fe.mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
      filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null)                     * 10 avg_mktcap_wtd_by_liab_q1_x10
  -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 100 sum_last_liab_q1_o_last_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 100 sum_last_liab_q1_o_mktcap_x100
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
    (nullif(
      sum(fe.last_inbnd_stmtstat_assets_q1) 
        filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
      sum(fe.last_inbnd_stmtstat_liab_q1) 
        filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
    ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
  -- STAT / ASSETS     STAT / MKTCAP
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
  -- SUM OF THE CURRENTS CA + CL
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
    ) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
    ) /
    sum(fe.mktcap) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct 
        -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , case when fei.sp = '500' then 'sp500' else 'ERROR' end col2_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
           , fei.last_inbnd_stmtstat_liab_q1
           , fei.mktcap
           , fei.last_inbnd_stmtstat_netinc_q1
           , fei.last_inbnd_stmtstat_ncc_q1
           , fei.last_inbnd_stmtstat_tco_q1
           , fei.last_inbnd_stmtstat_tcf_q1
           , fei.last_inbnd_stmtstat_tci_q1
           , fei.last_inbnd_stmtstat_ca_q1
           , fei.last_inbnd_stmtstat_cl_q1
           , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
           , fei.price
         from fe_data_store.si_finecon2 fei 
        -- where fei.sp in('500','400','600') -- 261,000 records
           where fei.sp in('500')
          -- and fei.dateindex = 17438
       ) fe 
     , index_weights w
where     fe.dateindex = w.dateindex  
      and fe.col1_fct = w.col1_fct
      and fe.col2_fct = w.col2_fct
group by   fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
         , fe.col1_fct
         , fe.col2_fct
order by   fe.col1_fct
         , fe.col2_fct 
         , fe.dateindex
;
-- 354 ROWS ( if 'sp500' and 'spnot500' )




-- global flexible: just add/subtract expression: xyz.dateindex = XYZ 
--
-- A SOLUTION ( 'WITH' PART MULTPLE FACTOR-ABLE: 'sp500'/'spnot500' )
-- BREAKDOWN of Financial/NotFinancial 
-- create index si_finecon2_finecon_sp_500_sector_desc_partial_idx on
--   fe_data_store.si_finecon2(dateindex, sp, sector_desc) where sp = '500';
--
with index_weights as (
  select
      fe.dateindex 
    , fe.col1_fct
    , fe.col2_fct
    , fe.col3_fct
    , fe.col4_fct
    , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f52w_ann is not null) 
                     sum_wrt_pradchg_f52w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f26w_ann is not null) 
                     sum_wrt_pradchg_f26w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f13w_ann is not null) 
                     sum_wrt_pradchg_f13w_ann_mktcap
    , 
        sum(fe.mktcap) 
          filter(where fe.mktcap is not null and fe.pradchg_f04w_ann is not null) 
                     sum_wrt_pradchg_f04w_ann_mktcap
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                     sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1
    , 
        sum(fe.last_inbnd_stmtstat_assets_q1) 
          filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.mktcap is not null) 
                     sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1
    , 
        sum(fe.last_inbnd_stmtstat_liab_q1) 
          filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                     sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1
    , 
        sum(fe.last_inbnd_stmtstat_liab_q1) 
          filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.mktcap is not null) 
                     sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct
        -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , case when fei.sp = '500' then 'sp500' else 'ERROR' end col2_fct
           , 'sector_fin_desc'::text col3_fct
           , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1, fei.last_inbnd_stmtstat_liab_q1, fei.mktcap
           , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
           , fei.price
         from fe_data_store.si_finecon2 fei 
        -- where fei.sp in('500','400','600') 
           where fei.sp in('500')
          -- and fei.dateindex = 17438 
       ) fe -- 261,000 records ( if 'sp500' and 'spnot500' )
  group by   fe.col3_fct
           , fe.col4_fct
           , fe.dateindex    
           , fe.col1_fct
           , fe.col2_fct
  order by   fe.col3_fct
           , fe.col4_fct
           , fe.dateindex
           , fe.col1_fct
           , fe.col2_fct 
) -- 354 ROWS ( if 'sp500' and 'spnot500' )
select
  -- DISPLAY ORDER IS HERE
    fe.dateindex 
  , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
  , fe.col1_fct
  , fe.col2_fct
  , fe.col3_fct
  , fe.col4_fct
  -- THE RESPONSE VALUE
  , sum(fe.mktcap) filter(where fe.mktcap is not null) / nullif(count(fe.mktcap) filter(where fe.mktcap is not null),0) / 1000.0 sum_mktcap_o_count_d1000
  -- CANDIDATE RESPONSE VARIABLES
  , avg(fe.pradchg_f52w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f52w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f52w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f52w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f26w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f26w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f26w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f26w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f13w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f13w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f13w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f13w_ann_wtd_by_mktcap_x1000
  , avg(fe.pradchg_f04w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f04w_ann_mktcap,0)  ) 
      filter(where fe.pradchg_f04w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f04w_ann_wtd_by_mktcap_x1000
  -- BEGIN NON-RESPONSE VARIABLES
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
  , sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
  , sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) * 10 avg_last_mktcap_wtd_by_assets_q1_x10
  , avg(fe.mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
      filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)                     * 10 avg_mktcap_wtd_by_assets_q1_x10
  , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
      filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null) * 10 avg_last_mktcap_wtd_by_liab_q1_x10
  , avg(fe.mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
      filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null)                     * 10 avg_mktcap_wtd_by_liab_q1_x10
  -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 100 sum_last_liab_q1_o_last_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 100 sum_last_liab_q1_o_mktcap_x100
  , sum(fe.last_inbnd_stmtstat_liab_q1) 
      filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
    (nullif(
      sum(fe.last_inbnd_stmtstat_assets_q1) 
        filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
      sum(fe.last_inbnd_stmtstat_liab_q1) 
        filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
    ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
  -- STAT / ASSETS     STAT / MKTCAP
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_netinc_q1) 
      filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
  , sum(fe.last_inbnd_stmtstat_ncc_q1) 
      filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tco_q1) 
      filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tcf_q1) 
      filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
  , sum(fe.last_inbnd_stmtstat_tci_q1) 
      filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_ca_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
  , sum(fe.last_inbnd_stmtstat_cl_q1) 
      filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
    sum(fe.last_inbnd_stmtstat_mktcap) 
      filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
  -- SUM OF THE CURRENTS CA + CL
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
    ) /
    sum(fe.last_inbnd_stmtstat_assets_q1) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
  , (
      sum(fe.last_inbnd_stmtstat_ca_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
      sum(fe.last_inbnd_stmtstat_cl_q1) 
        filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
    ) /
    sum(fe.mktcap) 
      filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100
  from ( -- fe
         -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
         select
             fei.dateindex 
           , 'sp_desc'::text col1_fct 
        -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
           , case when fei.sp = '500' then 'sp500' else 'ERROR' end col2_fct
           , 'sector_fin_desc'::text col3_fct
           , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
           , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
           , fei.last_inbnd_stmtstat_liab_q1
           , fei.mktcap
           , fei.last_inbnd_stmtstat_netinc_q1
           , fei.last_inbnd_stmtstat_ncc_q1
           , fei.last_inbnd_stmtstat_tco_q1
           , fei.last_inbnd_stmtstat_tcf_q1
           , fei.last_inbnd_stmtstat_tci_q1
           , fei.last_inbnd_stmtstat_ca_q1
           , fei.last_inbnd_stmtstat_cl_q1
           , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
           , fei.price
         from fe_data_store.si_finecon2 fei 
        -- where fei.sp in('500','400','600') -- 261,000 records
           where fei.sp in('500')
          -- and fei.dateindex = 17438
       ) fe 
     , index_weights w
where     fe.dateindex = w.dateindex
      and fe.col1_fct = w.col1_fct
      and fe.col2_fct = w.col2_fct
      and fe.col3_fct = w.col3_fct
      and fe.col4_fct = w.col4_fct
group by   fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
         , fe.col1_fct
         , fe.col2_fct
         , fe.col3_fct
         , fe.col4_fct
order by   fe.col1_fct
         , fe.col2_fct 
         , fe.col3_fct
         , fe.col4_fct
         , fe.dateindex
;


---------------------------------------------
-------- r code run implemenatation result --

-- EXPERIMENT
set effective_cache_size to '6144MB';  -- os + 'shared_buffers'

set search_path to fe_data_store;
-- set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
--set work_mem to  '1200MB';
set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'


select count(1) from fe_data_store.si_finecon2 fei where fei.dateindex in (values(12055), (12083));

select count(1) from fe_data_store.si_finecon2 fei where fei.dateindex in (values(12055), (12083))

explain
select count(1) from fe_data_store.si_finecon2 fei where fei.dateindex in (values(12055), (12083))


      -- global flexible: just add/subtract expression: xyz.dateindex = XYZ 
      --
      -- A SOLUTION ( 'WITH' PART MULTPLE FACTOR-ABLE: 'sp500'/'spnot500' )
      -- BREAKDOWN of Financial/NotFinancial 
      -- create index si_finecon2_finecon_sp_500_sector_desc_partial_idx on
      --   fe_data_store.si_finecon2(dateindex, sp, sector_desc) where sp = '500';
      --

      with index_weights as (
        select
            fe.dateindex 
          , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
          , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f52w_ann is not null) 
                           sum_wrt_pradchg_f52w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f26w_ann is not null) 
                           sum_wrt_pradchg_f26w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f13w_ann is not null) 
                           sum_wrt_pradchg_f13w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f04w_ann is not null) 
                           sum_wrt_pradchg_f04w_ann_mktcap
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'    end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 , 'sp_desc'::text col1_fct, case when fei.sp = '500' then 'sp500' else '_error_' end col2_fct
, 'sector_fin_desc'::text col3_fct, case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct

                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1, fei.last_inbnd_stmtstat_liab_q1, fei.mktcap
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') 
              -- and fei.sp in('500')
                 and fei.sp in('500')
              -- and fei.dateindex = 17438 
                 and fei.dateindex = 17409
             ) fe -- 261,000 records ( if 'sp500' and 'spnot500' )
        group by   fe.dateindex
                 , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
        order by   fe.dateindex
                 , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
      ) -- 354 ROWS ( if 'sp500' and 'spnot500' )
      select
        -- DISPLAY ORDER IS HERE
          fe.dateindex 
        , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
        , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
        -- THE RESPONSE VALUE
        , sum(fe.mktcap) filter(where fe.mktcap is not null) / nullif(count(fe.mktcap) filter(where fe.mktcap is not null),0) / 1000.0 sum_mktcap_o_count_d1000
        -- CANDIDATE RESPONSE VARIABLES
        , avg(fe.pradchg_f52w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f52w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f52w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f52w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f26w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f26w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f26w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f26w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f13w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f13w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f13w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f13w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f04w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f04w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f04w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f04w_ann_wtd_by_mktcap_x1000
        -- BEGIN NON-RESPONSE VARIABLES
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
        , sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) * 10 avg_last_mktcap_wtd_by_assets_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)                     * 10 avg_mktcap_wtd_by_assets_q1_x10
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null) * 10 avg_last_mktcap_wtd_by_liab_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null)                     * 10 avg_mktcap_wtd_by_liab_q1_x10
        -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 100 sum_last_liab_q1_o_last_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 100 sum_last_liab_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
          (nullif(
            sum(fe.last_inbnd_stmtstat_assets_q1) 
              filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
            sum(fe.last_inbnd_stmtstat_liab_q1) 
              filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
          ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
        -- STAT / ASSETS     STAT / MKTCAP
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
        -- SUM OF THE CURRENTS CA + CL
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
          ) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
          ) /
          sum(fe.mktcap) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct 
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'  end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 , 'sp_desc'::text col1_fct, case when fei.sp = '500' then 'sp500' else '_error_' end col2_fct
, 'sector_fin_desc'::text col3_fct, case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct

                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
                 , fei.last_inbnd_stmtstat_liab_q1
                 , fei.mktcap
                 , fei.last_inbnd_stmtstat_netinc_q1
                 , fei.last_inbnd_stmtstat_ncc_q1
                 , fei.last_inbnd_stmtstat_tco_q1
                 , fei.last_inbnd_stmtstat_tcf_q1
                 , fei.last_inbnd_stmtstat_tci_q1
                 , fei.last_inbnd_stmtstat_ca_q1
                 , fei.last_inbnd_stmtstat_cl_q1
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') -- 261,000 records
              -- and fei.sp in('500')
                 and fei.sp in('500')
              -- and fei.dateindex = 17438
                 and fei.dateindex = 17409
             ) fe 
           , index_weights w
      where     fe.dateindex = w.dateindex
              and fe.col1_fct = w.col1_fct  and fe.col2_fct = w.col2_fct  and fe.col3_fct = w.col3_fct  and fe.col4_fct = w.col4_fct
      group by  fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
                , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
      order by  fe.dateindex
                , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
      ;


      -- global flexible: just add/subtract expression: xyz.dateindex = XYZ 
      --
      -- A SOLUTION ( 'WITH' PART MULTPLE FACTOR-ABLE: 'sp500'/'spnot500' )
      -- BREAKDOWN of Financial/NotFinancial 
      -- create index si_finecon2_finecon_sp_500_sector_desc_partial_idx on
      --   fe_data_store.si_finecon2(dateindex, sp, sector_desc) where sp = '500';
      --

      with index_weights as (
        select
            fe.dateindex 
          , fe.col1_fct, fe.col2_fct
          , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f52w_ann is not null) 
                           sum_wrt_pradchg_f52w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f26w_ann is not null) 
                           sum_wrt_pradchg_f26w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f13w_ann is not null) 
                           sum_wrt_pradchg_f13w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f04w_ann is not null) 
                           sum_wrt_pradchg_f04w_ann_mktcap
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'    end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 , 'sp_desc'::text col1_fct, case when fei.sp = '500' then 'sp500' else '_error_' end col2_fct

                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1, fei.last_inbnd_stmtstat_liab_q1, fei.mktcap
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') 
              -- and fei.sp in('500')
                 and fei.sp in('500')
              -- and fei.dateindex = 17438 
                 and fei.dateindex = 17409
             ) fe -- 261,000 records ( if 'sp500' and 'spnot500' )
        group by   fe.dateindex
                 , fe.col1_fct, fe.col2_fct
        order by   fe.dateindex
                 , fe.col1_fct, fe.col2_fct
      ) -- 354 ROWS ( if 'sp500' and 'spnot500' )
      select
        -- DISPLAY ORDER IS HERE
          fe.dateindex 
     -- NOT SENT TO LIQUIFY (ONLY MEANT FOR SINGLE QUERY INTERACTIVE DEBUGGING)
     -- , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
        , fe.col1_fct, fe.col2_fct
        -- THE RESPONSE VALUE
        , sum(fe.mktcap) filter(where fe.mktcap is not null) / nullif(count(fe.mktcap) filter(where fe.mktcap is not null),0) / 1000.0 sum_mktcap_o_count_d1000
        -- CANDIDATE RESPONSE VARIABLES
        , avg(fe.pradchg_f52w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f52w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f52w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f52w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f26w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f26w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f26w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f26w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f13w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f13w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f13w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f13w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f04w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f04w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f04w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f04w_ann_wtd_by_mktcap_x1000
        -- BEGIN NON-RESPONSE VARIABLES
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
        , sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) * 10 avg_last_mktcap_wtd_by_assets_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)                     * 10 avg_mktcap_wtd_by_assets_q1_x10
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null) * 10 avg_last_mktcap_wtd_by_liab_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null)                     * 10 avg_mktcap_wtd_by_liab_q1_x10
        -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 100 sum_last_liab_q1_o_last_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 100 sum_last_liab_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
          (nullif(
            sum(fe.last_inbnd_stmtstat_assets_q1) 
              filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
            sum(fe.last_inbnd_stmtstat_liab_q1) 
              filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
          ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
        -- STAT / ASSETS     STAT / MKTCAP
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
        -- SUM OF THE CURRENTS CA + CL
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
          ) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
          ) /
          sum(fe.mktcap) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct 
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'  end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 , 'sp_desc'::text col1_fct, case when fei.sp = '500' then 'sp500' else '_error_' end col2_fct

                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
                 , fei.last_inbnd_stmtstat_liab_q1
                 , fei.mktcap
                 , fei.last_inbnd_stmtstat_netinc_q1
                 , fei.last_inbnd_stmtstat_ncc_q1
                 , fei.last_inbnd_stmtstat_tco_q1
                 , fei.last_inbnd_stmtstat_tcf_q1
                 , fei.last_inbnd_stmtstat_tci_q1
                 , fei.last_inbnd_stmtstat_ca_q1
                 , fei.last_inbnd_stmtstat_cl_q1
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') -- 261,000 records
              -- and fei.sp in('500')
                 and fei.sp in('500')
              -- and fei.dateindex = 17438
                 and fei.dateindex = 17409
             ) fe 
           , index_weights w
      where     fe.dateindex = w.dateindex
              and fe.col1_fct = w.col1_fct  and fe.col2_fct = w.col2_fct
      group by  fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
                , fe.col1_fct, fe.col2_fct
      order by  fe.dateindex
                , fe.col1_fct, fe.col2_fct
      ;



      -- global flexible: just add/subtract expression: xyz.dateindex = XYZ 
      --
      -- A SOLUTION ( 'WITH' PART MULTPLE FACTOR-ABLE: 'sp500'/'spnot500' )
      -- BREAKDOWN of Financial/NotFinancial 
      -- create index si_finecon2_finecon_sp_500_sector_desc_partial_idx on
      --   fe_data_store.si_finecon2(dateindex, sp, sector_desc) where sp = '500';
      --

      with index_weights as (
        select
            fe.dateindex 
          , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
          , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f52w_ann is not null) 
                           sum_wrt_pradchg_f52w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f26w_ann is not null) 
                           sum_wrt_pradchg_f26w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f13w_ann is not null) 
                           sum_wrt_pradchg_f13w_ann_mktcap
          , 
              sum(fe.mktcap) 
                filter(where fe.mktcap is not null and fe.pradchg_f04w_ann is not null) 
                           sum_wrt_pradchg_f04w_ann_mktcap
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_assets_q1) 
                filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.last_inbnd_stmtstat_mktcap is not null) 
                           sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1
          , 
              sum(fe.last_inbnd_stmtstat_liab_q1) 
                filter(where fe.last_inbnd_stmtstat_liab_q1 is not null and fe.mktcap is not null) 
                           sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'    end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 , 'sp_desc'::text col1_fct, case when fei.sp = '500' then 'sp500' else '_error_' end col2_fct
, 'sector_fin_desc'::text col3_fct, case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct

                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1, fei.last_inbnd_stmtstat_liab_q1, fei.mktcap
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') 
              -- and fei.sp in('500')
                 and fei.sp in('500')
              -- and fei.dateindex = 17438 
                 
             ) fe -- 261,000 records ( if 'sp500' and 'spnot500' )
        group by   fe.dateindex
                 , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
        order by   fe.dateindex
                 , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
      ) -- 354 ROWS ( if 'sp500' and 'spnot500' )
      select
        -- DISPLAY ORDER IS HERE
          fe.dateindex 
     -- NOT SENT TO LIQUIFY (ONLY MEANT FOR SINGLE QUERY INTERACTIVE DEBUGGING)
     -- , to_timestamp(fe.dateindex*3600*24)::date dateindex_dt
        , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
        -- THE RESPONSE VALUE
        , sum(fe.mktcap) filter(where fe.mktcap is not null) / nullif(count(fe.mktcap) filter(where fe.mktcap is not null),0) / 1000.0 sum_mktcap_o_count_d1000
        -- CANDIDATE RESPONSE VARIABLES
        , avg(fe.pradchg_f52w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f52w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f52w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f52w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f26w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f26w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f26w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f26w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f13w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f13w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f13w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f13w_ann_wtd_by_mktcap_x1000
        , avg(fe.pradchg_f04w_ann * fe.mktcap / nullif(w.sum_wrt_pradchg_f04w_ann_mktcap,0)  ) 
            filter(where fe.pradchg_f04w_ann    is not null and fe.mktcap is not null)         * 1000 avg_pradchg_f04w_ann_wtd_by_mktcap_x1000
        -- BEGIN NON-RESPONSE VARIABLES
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) sum_wrt_assets_q1_of_last_inbnd_stmtstat_mktcap
        , sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null) sum_wrt_mktcap_of_last_inbnd_stmtstat_assets_q1
        , sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_mktcap    is not null)    * 100 sum_last_mktcap_o_last_inbnd_stmtstat_assets_q1_x100
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) * 10 avg_last_mktcap_wtd_by_assets_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_assets_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_assets_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)                     * 10 avg_mktcap_wtd_by_assets_q1_x10
        , avg(fe.last_inbnd_stmtstat_mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_last_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.last_inbnd_stmtstat_mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null) * 10 avg_last_mktcap_wtd_by_liab_q1_x10
        , avg(fe.mktcap * fe.last_inbnd_stmtstat_liab_q1 / nullif(w.sum_wrt_mktcap_last_inbnd_stmtstat_liab_q1,0)  ) 
            filter(where fe.mktcap    is not null and fe.last_inbnd_stmtstat_liab_q1 is not null)                     * 10 avg_mktcap_wtd_by_liab_q1_x10
        -- ASSETS/ LIAB / OE(ASSETS-LIAB) 
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_liab_q1    is not null)    * 100 sum_last_liab_q1_o_last_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                        is not null and fe.last_inbnd_stmtstat_liab_q1  is not null)      * 100 sum_last_liab_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_liab_q1) 
            filter(where fe.last_inbnd_stmtstat_liab_q1   is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) / 
          (nullif(
            sum(fe.last_inbnd_stmtstat_assets_q1) 
              filter(where fe.last_inbnd_stmtstat_assets_q1  is not null and fe.last_inbnd_stmtstat_liab_q1   is not null) -    
            sum(fe.last_inbnd_stmtstat_liab_q1) 
              filter(where fe.last_inbnd_stmtstat_liab_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) 
          ,0))                                                                                                             * 10 sum_last_liab_q1_o_diff_assets_q1_less_liab_q1_x10
        -- STAT / ASSETS     STAT / MKTCAP
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_netinc_q1    is not null)    * 10000 sum_last_netinc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_netinc_q1) 
            filter(where fe.last_inbnd_stmtstat_netinc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_netinc_q1  is not null)    * 1000 sum_last_netinc_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ncc_q1    is not null)    * 10000 sum_last_ncc_q1_o_assets_q1_x10000
        , sum(fe.last_inbnd_stmtstat_ncc_q1) 
            filter(where fe.last_inbnd_stmtstat_ncc_q1   is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ncc_q1  is not null)    * 10000 sum_last_ncc_q1_o_mktcap_x10000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tco_q1    is not null)    * 1000 sum_last_tco_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tco_q1) 
            filter(where fe.last_inbnd_stmtstat_tco_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tco_q1  is not null)    * 1000 sum_last_tco_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tcf_q1    is not null)    * 1000 sum_last_tcf_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tcf_q1) 
            filter(where fe.last_inbnd_stmtstat_tcf_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tcf_q1  is not null)    * 1000 sum_last_tcf_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_tci_q1    is not null)    * 1000 sum_last_tci_q1_o_assets_q1_x1000
        , sum(fe.last_inbnd_stmtstat_tci_q1) 
            filter(where fe.last_inbnd_stmtstat_tci_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_tci_q1  is not null)    * 1000 sum_last_tci_q1_o_mktcap_x1000
        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_ca_q1    is not null)    * 100 sum_last_ca_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_ca_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_ca_q1  is not null)    * 100 sum_last_ca_q1_o_mktcap_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_assets_q1 is not null and fe.last_inbnd_stmtstat_cl_q1    is not null)    * 100 sum_last_cl_q1_o_assets_q1_x100
        , sum(fe.last_inbnd_stmtstat_cl_q1) 
            filter(where fe.last_inbnd_stmtstat_cl_q1    is not null and fe.mktcap is not null) /
          sum(fe.last_inbnd_stmtstat_mktcap) 
            filter(where fe.mktcap                          is not null and fe.last_inbnd_stmtstat_cl_q1  is not null)    * 100 sum_last_cl_q1_o_mktcap_x100
        -- SUM OF THE CURRENTS CA + CL
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)
          ) /
          sum(fe.last_inbnd_stmtstat_assets_q1) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.last_inbnd_stmtstat_assets_q1 is not null)    * 100 sum_last_ca_cl_q1_o_assets_q1_x100
        , (
            sum(fe.last_inbnd_stmtstat_ca_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null) +
            sum(fe.last_inbnd_stmtstat_cl_q1) 
              filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)
          ) /
          sum(fe.mktcap) 
            filter(where fe.last_inbnd_stmtstat_ca_q1 is not null and fe.last_inbnd_stmtstat_cl_q1 is not null and fe.mktcap is not null)                           * 100 sum_last_ca_cl_q1_o_mktcap_x100
        from ( -- fe
               -- explain  -- NO_INDEX+CASE ( over 60+ ... ) INDEX+CASE ( 25 seconds )
               select
                   fei.dateindex 
              -- , 'sp_desc'::text col1_fct 
              -- , case when fei.sp = '500' then 'sp500' else 'spnot500' end col2_fct
              -- , case when fei.sp = '500' then 'sp500' else '_error_'  end col2_fct
              -- , 'sector_fin_desc'::text col3_fct
              -- , case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct
                 , 'sp_desc'::text col1_fct, case when fei.sp = '500' then 'sp500' else '_error_' end col2_fct
, 'sector_fin_desc'::text col3_fct, case when fei.sector_desc = 'Financial' then 'sectfinfinancial' else 'sectfinnotfinancial' end col4_fct

                 , fei.last_inbnd_stmtstat_mktcap, fei.last_inbnd_stmtstat_assets_q1
                 , fei.last_inbnd_stmtstat_liab_q1
                 , fei.mktcap
                 , fei.last_inbnd_stmtstat_netinc_q1
                 , fei.last_inbnd_stmtstat_ncc_q1
                 , fei.last_inbnd_stmtstat_tco_q1
                 , fei.last_inbnd_stmtstat_tcf_q1
                 , fei.last_inbnd_stmtstat_tci_q1
                 , fei.last_inbnd_stmtstat_ca_q1
                 , fei.last_inbnd_stmtstat_cl_q1
                 , fei.pradchg_f04w_ann, fei.pradchg_f13w_ann, fei.pradchg_f26w_ann, fei.pradchg_f52w_ann
                 , fei.price
               from fe_data_store.si_finecon2 fei 
                 where 1 = 1
              -- and fei.sp in('500','400','600') -- 261,000 records
              -- and fei.sp in('500')
                 and fei.sp in('500')
              -- and fei.dateindex = 17438
                 
             ) fe 
           , index_weights w
      where     fe.dateindex = w.dateindex
              and fe.col1_fct = w.col1_fct  and fe.col2_fct = w.col2_fct  and fe.col3_fct = w.col3_fct  and fe.col4_fct = w.col4_fct
      group by  fe.dateindex    -- potential expansion to 'grouping sets','cubes' ( if makes sense )
                , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
      order by  fe.dateindex
                , fe.col1_fct, fe.col2_fct, fe.col3_fct, fe.col4_fct
      ;


-- [ USEFUL: KEEP ]
-- NOV 29 2017 -- 101.00
-- res <- Quandl('MULTPL/SP500_EARNINGS_MONTH') 
-- '2017-03-31'
-- NEED eom + INFLATION_ADJ CPI-U
select 
    co.* 
  , t4q.earnings_x10_4q
  , t4q.earnings_per_avg_share_x10_4q
  , t4q.earnings_per_avg_share_x10_bal_4q 
  , t3q.earnings_x13_3q
  , t3q.earnings_per_avg_share_x13_3q
  , t2q.earnings_x20_2q
  , t2q.earnings_per_avg_share_x20_2q
  , t1q.earnings_x40_1q
  , t1q.earnings_per_avg_share_x40_1q 
from (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_co
    , (date_trunc('month', to_timestamp((fe.dateindex -5)*3600*24)::date) + interval '1 month' - interval '1 day')::date  dateindexeom_dt_co
    , count(1) count_elig
    , sum( mktcap / price ) / count(1) avg_shares 
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
  and price   is not null
  group by dateindex
  order by dateindex asc
) co left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_4q
  , count(1) count_4q
  , sum( netinc_q1 + netinc_q2 + netinc_q3 + netinc_q4 ) / count(1) / 100 * 4/4 earnings_x10_4q
  , sum( mktcap / price ) / count(1) avg_shares                                                                                                        
  , sum( netinc_q1 + netinc_q2 + netinc_q3 + netinc_q4 ) / sum( mktcap / price ) * 10 * 4/4 earnings_per_avg_share_x10_4q                                    
  , sum( netinc_q1 + netinc_q2 + netinc_q3 + netinc_q4 ) / sum( mktcap / price ) / 29.8287359691438460 * 101.00 * 10 * 4/4 earnings_per_avg_share_x10_bal_4q 
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t4q on co.dateindex_dt_co = t4q.dateindex_dt_4q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_3q
  , sum( netinc_q1 + netinc_q2 + netinc_q3  ) / count(1) / 100 * 4/3 earnings_x13_3q                                                                                               
  , sum( netinc_q1 + netinc_q2 + netinc_q3  ) / sum( mktcap / price ) * 10 * 4/3 earnings_per_avg_share_x13_3q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t3q on co.dateindex_dt_co = t3q.dateindex_dt_3q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_2q
  , sum( netinc_q1 + netinc_q2 ) / count(1) / 100 * 4/2 earnings_x20_2q                                                                                               
  , sum( netinc_q1 + netinc_q2 ) / sum( mktcap / price ) * 10 * 4/2 earnings_per_avg_share_x20_2q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t2q on co.dateindex_dt_co = t2q.dateindex_dt_2q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_1q
  , sum( netinc_q1 ) / count(1) / 100 * 4/1 earnings_x40_1q                                                                                               
  , sum( netinc_q1 ) / sum( mktcap / price ) * 10 * 4/1 earnings_per_avg_share_x40_1q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t1q on co.dateindex_dt_co = t1q.dateindex_dt_1q 
order by co.dateindex_dt_co
;



-- [ USEFUL: KEEP ]
-- quandle equivalent ?? -- ???
-- ALREADY THERE AS goodsight01.R get_sipro_sp500_mktcap_o_netinc()
select 
    co.* 
  , t4q.mktcap_o_netinc_d10_x10_4q 
  , t3q.mktcap_o_netinc_d13_x10_3q 
  , t2q.mktcap_o_netinc_d20_x10_2q
  , t1q.mktcap_o_netinc_d40_x10_1q
from (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_co
    , (date_trunc('month', to_timestamp((fe.dateindex -5)*3600*24)::date) + interval '1 month' - interval '1 day')::date  dateindexeom_dt_co
    , count(1) count_elig_mktcap_o_netinc
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
  group by dateindex
  order by dateindex asc
) co left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_4q
  , sum( mktcap ) / ( ( sum( netinc_q1 ) + sum( netinc_q2 ) + sum( netinc_q3 ) + sum( netinc_q4 ) ) * 10 * 4/4 ) * 10 mktcap_o_netinc_d10_x10_4q                                   
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t4q on co.dateindex_dt_co = t4q.dateindex_dt_4q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_3q
  , sum( mktcap ) / ( ( sum( netinc_q1 ) + sum( netinc_q2 ) + sum( netinc_q3 ) ) * 10 * 4/3 ) * 10 mktcap_o_netinc_d13_x10_3q 
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t3q on co.dateindex_dt_co = t3q.dateindex_dt_3q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_2q
  , sum( mktcap ) / ( ( sum( netinc_q1 ) + sum( netinc_q2 ) ) * 10 * 4/2 ) * 10 mktcap_o_netinc_d20_x10_2q                                      
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t2q on co.dateindex_dt_co = t2q.dateindex_dt_2q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_1q                                                                                              
  , sum( mktcap ) / ( (sum( netinc_q1 ) ) * 10 * 4/1 ) * 10 mktcap_o_netinc_d40_x10_1q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.netinc_q1 is not null
  and fe.netinc_q2 is not null
  and fe.netinc_q3 is not null
  and fe.netinc_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t1q on co.dateindex_dt_co = t1q.dateindex_dt_1q 
order by co.dateindex_dt_co
;






-- not useful(not interpretable)
select 
    co.* 

  , t4q.mktcap_o_ncc_q_4q_ann  
  , t4q.mktcap_o_tco_q_4q_ann 
  , t4q.mktcap_o_tcf_q_4q_ann 
  , t4q.mktcap_o_tci_q_4q_ann 
  , t4q.mktcap_o_ere_q_4q_ann 

  , t3q.mktcap_o_ncc_q_3q_ann  
  , t3q.mktcap_o_tco_q_3q_ann 
  , t3q.mktcap_o_tcf_q_3q_ann 
  , t3q.mktcap_o_tci_q_3q_ann 
  , t3q.mktcap_o_ere_q_3q_ann 

  , t2q.mktcap_o_ncc_q_2q_ann  
  , t2q.mktcap_o_tco_q_2q_ann 
  , t2q.mktcap_o_tcf_q_2q_ann 
  , t2q.mktcap_o_tci_q_2q_ann 
  , t2q.mktcap_o_ere_q_2q_ann

  , t1q.mktcap_o_ncc_q_1q_ann  
  , t1q.mktcap_o_tco_q_1q_ann 
  , t1q.mktcap_o_tcf_q_1q_ann 
  , t1q.mktcap_o_tci_q_1q_ann 
  , t1q.mktcap_o_ere_q_1q_ann

  ----

  , t4q.mktcap_o_ncc_q_4q_ann  
  , t3q.mktcap_o_ncc_q_3q_ann  
  , t2q.mktcap_o_ncc_q_2q_ann 
  , t1q.mktcap_o_ncc_q_1q_ann 

  , t4q.mktcap_o_tco_q_4q_ann  
  , t3q.mktcap_o_tco_q_3q_ann  
  , t2q.mktcap_o_tco_q_2q_ann 
  , t1q.mktcap_o_tco_q_1q_ann 

  , t4q.mktcap_o_tcf_q_4q_ann  
  , t3q.mktcap_o_tcf_q_3q_ann  
  , t2q.mktcap_o_tcf_q_2q_ann 
  , t1q.mktcap_o_tcf_q_1q_ann 

  , t4q.mktcap_o_tci_q_4q_ann  
  , t3q.mktcap_o_tci_q_3q_ann  
  , t2q.mktcap_o_tci_q_2q_ann 
  , t1q.mktcap_o_tci_q_1q_ann 

  , t4q.mktcap_o_ere_q_4q_ann  
  , t3q.mktcap_o_ere_q_3q_ann  
  , t2q.mktcap_o_ere_q_2q_ann 
  , t1q.mktcap_o_ere_q_1q_ann 

from (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_co
    , (date_trunc('month', to_timestamp((fe.dateindex -5)*3600*24)::date) + interval '1 month' - interval '1 day')::date  dateindexeom_dt_co
    , count(1) count_elig_mktcap_o_statz
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.tco_q1 is not null
  and fe.tcf_q1 is not null
  and fe.tci_q1 is not null
  and fe.ere_q1 is not null
  and fe.ncc_q2 is not null
  and fe.tco_q2 is not null
  and fe.tcf_q2 is not null
  and fe.tci_q2 is not null
  and fe.ere_q2 is not null
  and fe.ncc_q3 is not null
  and fe.tco_q3 is not null
  and fe.tcf_q3 is not null
  and fe.tci_q3 is not null
  and fe.ere_q3 is not null
  and fe.ncc_q4 is not null
  and fe.tco_q4 is not null
  and fe.tcf_q4 is not null
  and fe.tci_q4 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
  group by dateindex
  order by dateindex asc
) co left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_4q
  , sum( mktcap ) / ( ( sum( ncc_q1 ) + sum( ncc_q2 ) + sum( ncc_q3 ) + sum( ncc_q4 ) ) * 10 * 4/4 ) * 10 mktcap_o_ncc_q_4q_ann  
  , sum( mktcap ) / ( ( sum( tco_q1 ) + sum( tco_q2 ) + sum( tco_q3 ) + sum( tco_q4 ) ) * 10 * 4/4 ) * 10 mktcap_o_tco_q_4q_ann 
  , sum( mktcap ) / ( ( sum( tcf_q1 ) + sum( tcf_q2 ) + sum( tcf_q3 ) + sum( tcf_q4 ) ) * 10 * 4/4 ) * 10 mktcap_o_tcf_q_4q_ann 
  , sum( mktcap ) / ( ( sum( tci_q1 ) + sum( tci_q2 ) + sum( tci_q3 ) + sum( tci_q4 ) ) * 10 * 4/4 ) * 10 mktcap_o_tci_q_4q_ann 
  , sum( mktcap ) / ( ( sum( ere_q1 ) + sum( ere_q2 ) + sum( ere_q3 ) + sum( ere_q4 ) ) * 10 * 4/4 ) * 10 mktcap_o_ere_q_4q_ann 
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.tco_q1 is not null
  and fe.tcf_q1 is not null
  and fe.tci_q1 is not null
  and fe.ere_q1 is not null
  and fe.ncc_q2 is not null
  and fe.tco_q2 is not null
  and fe.tcf_q2 is not null
  and fe.tci_q2 is not null
  and fe.ere_q2 is not null
  and fe.ncc_q3 is not null
  and fe.tco_q3 is not null
  and fe.tcf_q3 is not null
  and fe.tci_q3 is not null
  and fe.ere_q3 is not null
  and fe.ncc_q4 is not null
  and fe.tco_q4 is not null
  and fe.tcf_q4 is not null
  and fe.tci_q4 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t4q on co.dateindex_dt_co = t4q.dateindex_dt_4q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_3q
  , sum( mktcap ) / ( ( sum( ncc_q1 ) + sum( ncc_q2 ) + sum( ncc_q3 ) ) * 10 * 4/3 ) * 10 mktcap_o_ncc_q_3q_ann
  , sum( mktcap ) / ( ( sum( tco_q1 ) + sum( tco_q2 ) + sum( tco_q3 ) ) * 10 * 4/3 ) * 10 mktcap_o_tco_q_3q_ann
  , sum( mktcap ) / ( ( sum( tcf_q1 ) + sum( tcf_q2 ) + sum( tcf_q3 ) ) * 10 * 4/3 ) * 10 mktcap_o_tcf_q_3q_ann
  , sum( mktcap ) / ( ( sum( tci_q1 ) + sum( tci_q2 ) + sum( tci_q3 ) ) * 10 * 4/3 ) * 10 mktcap_o_tci_q_3q_ann
  , sum( mktcap ) / ( ( sum( ere_q1 ) + sum( ere_q2 ) + sum( ere_q3 ) ) * 10 * 4/3 ) * 10 mktcap_o_ere_q_3q_ann
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.tco_q1 is not null
  and fe.tcf_q1 is not null
  and fe.tci_q1 is not null
  and fe.ere_q1 is not null
  and fe.ncc_q2 is not null
  and fe.tco_q2 is not null
  and fe.tcf_q2 is not null
  and fe.tci_q2 is not null
  and fe.ere_q2 is not null
  and fe.ncc_q3 is not null
  and fe.tco_q3 is not null
  and fe.tcf_q3 is not null
  and fe.tci_q3 is not null
  and fe.ere_q3 is not null
  and fe.ncc_q4 is not null
  and fe.tco_q4 is not null
  and fe.tcf_q4 is not null
  and fe.tci_q4 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t3q on co.dateindex_dt_co = t3q.dateindex_dt_3q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_2q
  , sum( mktcap ) / ( ( sum( ncc_q1 ) + sum( ncc_q2 ) ) * 10 * 4/2 ) * 10 mktcap_o_ncc_q_2q_ann 
  , sum( mktcap ) / ( ( sum( tco_q1 ) + sum( tco_q2 ) ) * 10 * 4/2 ) * 10 mktcap_o_tco_q_2q_ann
  , sum( mktcap ) / ( ( sum( tcf_q1 ) + sum( tcf_q2 ) ) * 10 * 4/2 ) * 10 mktcap_o_tcf_q_2q_ann
  , sum( mktcap ) / ( ( sum( tci_q1 ) + sum( tci_q2 ) ) * 10 * 4/2 ) * 10 mktcap_o_tci_q_2q_ann
  , sum( mktcap ) / ( ( sum( ere_q1 ) + sum( ere_q2 ) ) * 10 * 4/2 ) * 10 mktcap_o_ere_q_2q_ann
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.tco_q1 is not null
  and fe.tcf_q1 is not null
  and fe.tci_q1 is not null
  and fe.ere_q1 is not null
  and fe.ncc_q2 is not null
  and fe.tco_q2 is not null
  and fe.tcf_q2 is not null
  and fe.tci_q2 is not null
  and fe.ere_q2 is not null
  and fe.ncc_q3 is not null
  and fe.tco_q3 is not null
  and fe.tcf_q3 is not null
  and fe.tci_q3 is not null
  and fe.ere_q3 is not null
  and fe.ncc_q4 is not null
  and fe.tco_q4 is not null
  and fe.tcf_q4 is not null
  and fe.tci_q4 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t2q on co.dateindex_dt_co = t2q.dateindex_dt_2q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_1q                                                                                              
  , sum( mktcap ) / ( ( sum( ncc_q1 ) ) * 10 * 4/1 ) * 10 mktcap_o_ncc_q_1q_ann  
  , sum( mktcap ) / ( ( sum( tco_q1 ) ) * 10 * 4/1 ) * 10 mktcap_o_tco_q_1q_ann
  , sum( mktcap ) / ( ( sum( tcf_q1 ) ) * 10 * 4/1 ) * 10 mktcap_o_tcf_q_1q_ann
  , sum( mktcap ) / ( ( sum( tci_q1 ) ) * 10 * 4/1 ) * 10 mktcap_o_tci_q_1q_ann
  , sum( mktcap ) / ( ( sum( ere_q1 ) ) * 10 * 4/1 ) * 10 mktcap_o_ere_q_1q_ann
  from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.tco_q1 is not null
  and fe.tcf_q1 is not null
  and fe.tci_q1 is not null
  and fe.ere_q1 is not null
  and fe.ncc_q2 is not null
  and fe.tco_q2 is not null
  and fe.tcf_q2 is not null
  and fe.tci_q2 is not null
  and fe.ere_q2 is not null
  and fe.ncc_q3 is not null
  and fe.tco_q3 is not null
  and fe.tcf_q3 is not null
  and fe.tci_q3 is not null
  and fe.ere_q3 is not null
  and fe.ncc_q4 is not null
  and fe.tco_q4 is not null
  and fe.tcf_q4 is not null
  and fe.tci_q4 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
group by dateindex
order by dateindex asc
) t1q on co.dateindex_dt_co = t1q.dateindex_dt_1q 
order by co.dateindex_dt_co
;



-- cash_flow per ave share ( not useful )
select 
    co.* 
  , t4q.cashflow_x10_4q
  , t4q.cashflow_per_avg_share_x10_4q
  , t3q.cashflow_x13_3q
  , t3q.cashflow_per_avg_share_x13_3q
  , t2q.cashflow_x20_2q
  , t2q.cashflow_per_avg_share_x20_2q
  , t1q.cashflow_x40_1q
  , t1q.cashflow_per_avg_share_x40_1q 
from (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_co
    , (date_trunc('month', to_timestamp((fe.dateindex -5)*3600*24)::date) + interval '1 month' - interval '1 day')::date  dateindexeom_dt_co
    , count(1) count_elig
    , sum( mktcap / price ) / count(1) avg_shares 
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.ncc_q2 is not null
  and fe.ncc_q3 is not null
  and fe.ncc_q4 is not null
  and mktcap is not null
  and price   is not null
  group by dateindex
  order by dateindex asc
) co left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_4q
  , count(1) count_4q
  , sum( ncc_q1 + ncc_q2 + ncc_q3 + ncc_q4 ) / count(1) / 100 * 4/4 cashflow_x10_4q
  , sum( mktcap / price ) / count(1) avg_shares                                                                                                        
  , sum( ncc_q1 + ncc_q2 + ncc_q3 + ncc_q4 ) / sum( mktcap / price ) * 10 * 4/4 cashflow_per_avg_share_x10_4q                                     
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.ncc_q2 is not null
  and fe.ncc_q3 is not null
  and fe.ncc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t4q on co.dateindex_dt_co = t4q.dateindex_dt_4q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_3q
  , sum( ncc_q1 + ncc_q2 + ncc_q3  ) / count(1) / 100 * 4/3 cashflow_x13_3q                                                                                               
  , sum( ncc_q1 + ncc_q2 + ncc_q3  ) / sum( mktcap / price ) * 10 * 4/3 cashflow_per_avg_share_x13_3q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.ncc_q2 is not null
  and fe.ncc_q3 is not null
  and fe.ncc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t3q on co.dateindex_dt_co = t3q.dateindex_dt_3q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_2q
  , sum( ncc_q1 + ncc_q2 ) / count(1) / 100 * 4/2 cashflow_x20_2q                                                                                               
  , sum( ncc_q1 + ncc_q2 ) / sum( mktcap / price ) * 10 * 4/2 cashflow_per_avg_share_x20_2q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.ncc_q2 is not null
  and fe.ncc_q3 is not null
  and fe.ncc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t2q on co.dateindex_dt_co = t2q.dateindex_dt_2q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_1q
  , sum( ncc_q1 ) / count(1) / 100 * 4/1 cashflow_x40_1q                                                                                               
  , sum( ncc_q1 ) / sum( mktcap / price ) * 10 * 4/1 cashflow_per_avg_share_x40_1q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ncc_q1 is not null
  and fe.ncc_q2 is not null
  and fe.ncc_q3 is not null
  and fe.ncc_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t1q on co.dateindex_dt_co = t1q.dateindex_dt_1q 
order by co.dateindex_dt_co
;



-- may be 'barely' useful
-- exchange rate effects per ave share 
select 
    co.* 
  , t4q.exchrateeffects_x10_4q
  , t4q.exchrateeffects_per_avg_share_x10_4q
  , t3q.exchrateeffects_x13_3q
  , t3q.exchrateeffects_per_avg_share_x13_3q
  , t2q.exchrateeffects_x20_2q
  , t2q.exchrateeffects_per_avg_share_x20_2q
  , t1q.exchrateeffects_x40_1q
  , t1q.exchrateeffects_per_avg_share_x40_1q 
from (
  select 
      to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_co
    , (date_trunc('month', to_timestamp((fe.dateindex -5)*3600*24)::date) + interval '1 month' - interval '1 day')::date  dateindexeom_dt_co
    , count(1) count_elig
    , sum( mktcap / price ) / count(1) avg_shares 
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ere_q1 is not null
  and fe.ere_q2 is not null
  and fe.ere_q3 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
  and price   is not null
  group by dateindex
  order by dateindex asc
) co left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_4q
  , count(1) count_4q
  , sum( ere_q1 + ere_q2 + ere_q3 + ere_q4 ) / count(1) / 100 * 4/4 exchrateeffects_x10_4q
  , sum( mktcap / price ) / count(1) avg_shares                                                                                                        
  , sum( ere_q1 + ere_q2 + ere_q3 + ere_q4 ) / sum( mktcap / price ) * 10 * 4/4 exchrateeffects_per_avg_share_x10_4q                                     
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ere_q1 is not null
  and fe.ere_q2 is not null
  and fe.ere_q3 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t4q on co.dateindex_dt_co = t4q.dateindex_dt_4q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_3q
  , sum( ere_q1 + ere_q2 + ere_q3  ) / count(1) / 100 * 4/3 exchrateeffects_x13_3q                                                                                               
  , sum( ere_q1 + ere_q2 + ere_q3  ) / sum( mktcap / price ) * 10 * 4/3 exchrateeffects_per_avg_share_x13_3q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ere_q1 is not null
  and fe.ere_q2 is not null
  and fe.ere_q3 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t3q on co.dateindex_dt_co = t3q.dateindex_dt_3q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_2q
  , sum( ere_q1 + ere_q2 ) / count(1) / 100 * 4/2 exchrateeffects_x20_2q                                                                                               
  , sum( ere_q1 + ere_q2 ) / sum( mktcap / price ) * 10 * 4/2 exchrateeffects_per_avg_share_x20_2q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ere_q1 is not null
  and fe.ere_q2 is not null
  and fe.ere_q3 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t2q on co.dateindex_dt_co = t2q.dateindex_dt_2q left join (
select 
    to_timestamp(fe.dateindex*3600*24)::date dateindex_dt_1q
  , sum( ere_q1 ) / count(1) / 100 * 4/1 exchrateeffects_x40_1q                                                                                               
  , sum( ere_q1 ) / sum( mktcap / price ) * 10 * 4/1 exchrateeffects_per_avg_share_x40_1q                                    
from fe_data_store.si_finecon2 fe
where sp = '500'
  and fe.ere_q1 is not null
  and fe.ere_q2 is not null
  and fe.ere_q3 is not null
  and fe.ere_q4 is not null
  and mktcap is not null
  and price  is not null
group by dateindex
order by dateindex asc
) t1q on co.dateindex_dt_co = t1q.dateindex_dt_1q 
order by co.dateindex_dt_co
;


-- KEEPISH ( BUT NOT USEFUL TO INVEST IN )
-- p03/f03 - very little better in top 1% with very high volitility ( i also did 06 12 - .... same conclusion )
-- at 1% ntiles are VERY NOISY
-- buyback yield and future returns in the s&p 500
-- play swap (03->06->09->12)
--
-- RUN OF DEC 21, 2017
-- 24.3288997514375727828629;  158.09250591005598519147611024763575849612115188;  0.85510875427705929837636728513938755179137951;  1
-- 
explain
select 
    avg       (sq2.f03_prchchg_ann) over (partition by sq2.ntile_100_p03_bby_ann)        avg_f03_prchchg_ann
  , stddev_pop(sq2.f03_prchchg_ann) over (partition by sq2.ntile_100_p03_bby_ann) stddev_pop_f03_prchchg_ann
  , (avg      (sq2.f03_prchchg_ann) over (partition by sq2.ntile_100_p03_bby_ann)  - 0) / nullif(stddev_pop(case when sq2.f03_prchchg_ann > 0 then 0 else sq2.f03_prchchg_ann end) over (partition by sq2.ntile_100_p03_bby_ann),0) tsortino_f03_prchchg_ann
  , sq2.* from 
  (
  select 
      ntile(100) over (order by sq1.p03_bby_ann desc) ntile_100_p03_bby_ann
    , sq1.* from 
    (
    select now.dateindex, now.ticker, now.company
      , (( p03.mktcap  / nullif(p03.price,0) ) - ( now.mktcap  / nullif(now.price,0) )) / nullif( now.mktcap  / nullif(now.price,0),0) * 4.0 * 100.0 p03_bby_ann
      , ((                      f03.price    ) - (                      now.price    )) / nullif(                         now.price,0) * 4.0 * 100.0 f03_prchchg_ann
    from    fe_data_store.si_finecon2 p03
          , fe_data_store.si_finecon2 now
          , fe_data_store.si_finecon2 f03
    where (now.dateindexeom = p03.dateindexf03eom and now.company_id = p03.company_id) and
          (now.dateindexeom = f03.dateindexp03eom and now.company_id = f03.company_id) and
          (now.split_date is null or now.split_date + 93 < now.dateindexeom) and -- did not recent split/combine
          now.sp = '500' and
          now.dateindex > 15000 -- and -- = -- now.dateindex = 17409 and -- > 15000 -- 2011+ ( over ~ last 7 years )
    ) sq1
  ) sq2
order by sq2.p03_bby_ann desc 



-- proportion of interest expense over liabilities and RETURN



-- JUST THIS ONE (excludes 2017 BECAUSE pradchg_f52w_ann is not KNOWN yet)
-- JUST THIS ONE
-- JUST THIS ONE TO LOOK AT (RECENT FUR YEARS) ( BUT HISTORICALLY *NOT* 'AT ALL' CONSISTENT )
-- NOT CONSITENT IF I INCLUDE THE OTHER SPS (400,600)
-- NOT COSISTENT IF I CHANGE THE ORDER BY TO (-1) AND/OR ASC
select 
    -- 19%
           avg(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)        avg_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    -- about global average
  , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10) stddev_pop_intno_o_liab_desc_ntile10_pradchg_f26w_ann
  , sq2.*
  from (
  select 
      -- must be EXACTLY THIS FORM "(-1) *" or "asc" NOT WORK (edge cases)
      -- 12 - 19.2629881422924901;37.0531544021566000
      -- 10 - 18.6881554677206851;36.2742053649500396 ( REST AVG 10 PERCENT )
      ntile(12) over (order by sq1.intno_o_liab desc) intno_o_liab_desc_ntile10
    , sq1.* 
    from (
    select 
        dateindex, ticker, company
      , pradchg_f04w_ann, pradchg_f13w_ann, pradchg_f26w_ann, pradchg_f52w_ann
      , intno_q1/nullif(liab_q1,0) intno_o_liab, intno_q1/nullif(assets_q1,0) intno_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
    from fe_data_store.si_finecon2 now 
    where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
      and pradchg_f04w_ann is not null and pradchg_f13w_ann is not null and pradchg_f26w_ann is not null and pradchg_f52w_ann is not null 
      and sp in ('500') and dateindex >= 16071 -- (to 17529) -- 2014-01-01 - 4 years and every month
      -- and sp in ('500') and dateindex >= 14610 and dateindex < 16071 -- 2010-01-01 -- 2014-01-01    (GOOD buy everything ALSO DID WELL)
      -- and sp in ('500') and dateindex >= 13514 and dateindex < 14610 -- 2007-01-01 -- 2010-01-01 -- POOR and everthing else did POOR
  ) sq1 
) sq2;



select 
           avg(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)        avg_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    -- about global average
  , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10) stddev_pop_intno_o_liab_desc_ntile10_pradchg_f26w_ann
  , sq2.*
  from (
  select 
      ntile(12) over (order by sq1.intno_o_liab desc) intno_o_liab_desc_ntile10
    , sq1.* 
    from (
    select 
        dateindex, dateindexyear, dateindexyear, ticker, company
      , pradchg_f26w_ann
      , intno_q1/nullif(liab_q1,0) intno_o_liab, intno_q1/nullif(assets_q1,0) intno_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
    from fe_data_store.si_finecon2 now 
    where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
      and pradchg_f26w_ann is not null
      and sp in ('500') and dateindex >= 17167 -- 2017-01-01-- dateindexyear = 2016 good year -- 2015 less worse performer in a bad year -- 2014 avg performer -- 2013 good performer (but all firms performed good) --
  ) sq1 -- including 2017 6-months( VERY GOOD )
) sq2;



select 
    -- 18%
           avg(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)        avg_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    -- about global average
  , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10) stddev_pop_intno_o_liab_desc_ntile10_pradchg_f26w_ann
  , sq2.*
  from (
  select 
      --  must be EXACTLY THIS FORM "(-1) *" or "asc" NOT WORK (edge cases)
      -- 12 - 19.2629881422924901;37.0531544021566000
      -- 10 - 18.6881554677206851;36.2742053649500396 ( REST AVG 10 PERCENT )
      ntile(12) over (order by sq1.intno_o_liab desc) intno_o_liab_desc_ntile10
    , sq1.* 
    from (
    select 
        dateindex, ticker, company
      , pradchg_f04w_ann, pradchg_f13w_ann, pradchg_f26w_ann, pradchg_f52w_ann
      , intno_q1/nullif(liab_q1,0) intno_o_liab, intno_q1/nullif(assets_q1,0) intno_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
    from fe_data_store.si_finecon2 now 
    where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
      and pradchg_f04w_ann is not null and pradchg_f13w_ann is not null and pradchg_f26w_ann is not null and pradchg_f52w_ann is not null 
      and sp in ('500') and dateindex >= 16071 -- (to 17529) -- 2014-01-01 - 4 years and every month
      -- and intno_q1 > 0 -- FAILS(12) "and intno_q1 > 0"  # 7.6706920077972710;29.7923442978375116
      -- and intno_q1 = 0 -- FAILS(1) "and intno_q1 = 0   # 12.6975705820843499;36.4565792408334827
      -- and intno_q1 < 0 # ( ONE RECORD )                 
      -- and sp in ('400','600') and dateindex >= 16071 # FAILS(12) # 10 best performer # 16.6209711620016964;48.7800513316692602 
      -- and sp in ('500','400','600') and dateindex >= 16071 # SUCESS(12) # 22.3839111233784157;45.5514235215664923(riskier)
      -- all time
  ) sq1 
) sq2;



select 
           avg(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)        avg_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    -- about global average
  , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10) stddev_pop_intno_o_liab_desc_ntile10_pradchg_f26w_ann
  , sq2.*
  from (
  select 
      ntile(12) over (order by sq1.intno_o_liab desc) intno_o_liab_desc_ntile10
    , sq1.* 
    from (
    select 
        dateindex, ticker, company
      , pradchg_f04w_ann, pradchg_f13w_ann, pradchg_f26w_ann, pradchg_f52w_ann
      , intno_q1/nullif(liab_q1,0) intno_o_liab, intno_q1/nullif(assets_q1,0) intno_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
    from fe_data_store.si_finecon2 now 
    where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
      and pradchg_f04w_ann is not null and pradchg_f13w_ann is not null and pradchg_f26w_ann is not null and pradchg_f52w_ann is not null 
      and sp in ('500')
  ) sq1 
) sq2
; -- moderate returns and LARGE risk


-- some idea of 6-months
select 
           avg(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)          avg_intno_o_liab_desc_ntile10_pradchg_f26w_ann
  , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)   stddev_pop_intno_o_liab_desc_ntile10_pradchg_f26w_ann
  ,        avg(pradchg_f26w_ann) over (partition by intno_o_assets_desc_ntile10)        avg_intno_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
  , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_assets_desc_ntile10) stddev_pop_intno_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
  ,        avg(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)         avg_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
  , stddev_pop(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)  stddev_pop_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
  , sq2.*
  from (
  select 
      ntile(10) over (order by sq1.intno_o_liab   desc) intno_o_liab_desc_ntile10
    , ntile(10) over (order by sq1.intno_o_assets desc) intno_o_assets_desc_ntile10
    , ntile(10) over (order by sq1.liab_o_assets  desc) liab_o_assets_desc_ntile10
    , sq1.* 
    from (
    select 
        dateindex, dateindexyear, dateindexyear, ticker, company
      , pradchg_f26w_ann
      , intno_q1/nullif(liab_q1,0) intno_o_liab, intno_q1/nullif(assets_q1,0) intno_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
    from fe_data_store.si_finecon2 now 
    where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
      and pradchg_f26w_ann is not null
      and sp in ('500') and dateindex >= 17167 
  ) sq1 
) sq2 order by liab_o_assets_desc_ntile10 desc, intno_o_liab_desc_ntile10 desc, intno_o_assets_desc_ntile10 desc


                                            --  avg: 22.0145263157894737;  stddev_pop  25.9931373035439188
-- return of first 6months of 2017 - best deal: lowest(best)    ratio of intno_o_liab
--                                              highest(worst)  ratio of intno_o_assets
--                                              not matter or : highest(worst)liab_o_assets AND NOT lowest(best)
-- this case: I want 'higher' ntile(good)_values
select intno_o_liab_desc_ntile10, intno_o_assets_desc_ntile10, liab_o_assets_desc_ntile10
  , avg(pradchg_f26w_ann) avg_pradchg_f26w_ann, stddev_pop(pradchg_f26w_ann) stddev_pop_pradchg_f26w_ann
  , count(pradchg_f26w_ann) count_pradchg_f26w_ann
  , avg(intno_o_liab) avg_intno_o_liab, avg(intno_o_assets) avg_intno_o_assets , avg(liab_o_assets) avg_liab_o_assets
from (
  select 
             avg(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)          avg_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)   stddev_pop_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    ,        avg(pradchg_f26w_ann) over (partition by intno_o_assets_desc_ntile10)        avg_intno_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_assets_desc_ntile10) stddev_pop_intno_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    ,        avg(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)         avg_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)  stddev_pop_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , sq2.*
    from (
    select 
                                                 -- this case: I want 'higher' ntile(good)_values
                                                 -- desc: higher the value, then the lower the ntile_value ( like 'rank' )
                               -- diff than usual: higher value is 'bad'
        ntile(2) over (order by sq1.intno_o_liab   desc) intno_o_liab_desc_ntile10
      , ntile(2) over (order by sq1.intno_o_assets desc) intno_o_assets_desc_ntile10
      , ntile(2) over (order by sq1.liab_o_assets  desc) liab_o_assets_desc_ntile10
      , sq1.* 
      from (
      select 
          dateindex, dateindexyear, dateindexyear, ticker, company
        , pradchg_f26w_ann
        , intno_q1/nullif(liab_q1,0) intno_o_liab, intno_q1/nullif(assets_q1,0) intno_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
      from fe_data_store.si_finecon2 now 
      where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
        and pradchg_f26w_ann is not null
        and sp in ('500') and dateindex >= 17167 -- (to 17529) -- pradchg_f26w_ann return of first 6months of 2017
    ) sq1 
  ) sq2 order by liab_o_assets_desc_ntile10 desc, intno_o_liab_desc_ntile10 desc, intno_o_assets_desc_ntile10 desc
) sq3 group by cube(intno_o_liab_desc_ntile10, intno_o_assets_desc_ntile10, liab_o_assets_desc_ntile10) order by avg_pradchg_f26w_ann desc




-- return of first 3.5years through first half of 2017 - best deal: 
--                                              
--                                              
-- this case: I want 'higher' ntile(good)_values
-- return of first 6months of 2017 - best deal: not matter or :lowest(best)    ratio of intno_o_liab
--                                              lowest(best)  ratio of intno_o_assets
--                                              highest(worst)liab_o_assets
select intno_o_liab_desc_ntile10, intno_o_assets_desc_ntile10, liab_o_assets_desc_ntile10
  , avg(pradchg_f26w_ann) avg_pradchg_f26w_ann, stddev_pop(pradchg_f26w_ann) stddev_pop_pradchg_f26w_ann
  , count(pradchg_f26w_ann) count_pradchg_f26w_ann
  , avg(intno_o_liab) avg_intno_o_liab, avg(intno_o_assets) avg_intno_o_assets , avg(liab_o_assets) avg_liab_o_assets
from (
  select 
             avg(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)          avg_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_liab_desc_ntile10)   stddev_pop_intno_o_liab_desc_ntile10_pradchg_f26w_ann
    ,        avg(pradchg_f26w_ann) over (partition by intno_o_assets_desc_ntile10)        avg_intno_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by intno_o_assets_desc_ntile10) stddev_pop_intno_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    ,        avg(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)         avg_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)  stddev_pop_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , sq2.*
    from (
    select 
                                                 -- this case: I want 'higher' ntile(good)_values
                                                 -- desc: higher the value, then the lower the ntile_value ( like 'rank' )
                               -- diff than usual: higher value is 'bad'
        ntile(2) over (order by sq1.intno_o_liab   desc) intno_o_liab_desc_ntile10
      , ntile(2) over (order by sq1.intno_o_assets desc) intno_o_assets_desc_ntile10
      , ntile(2) over (order by sq1.liab_o_assets  desc) liab_o_assets_desc_ntile10
      , sq1.* 
      from (
      select 
          dateindex, dateindexyear, dateindexyear, ticker, company
        , pradchg_f26w_ann
        , intno_q1/nullif(liab_q1,0) intno_o_liab, intno_q1/nullif(assets_q1,0) intno_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
      from fe_data_store.si_finecon2 now 
      where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
        and pradchg_f26w_ann is not null
        and sp in ('500') and dateindex >= 16071 -- 2004-01-01 - (to 17529) -- pradchg_f26w_ann return of first 6months of 2017
    ) sq1 
  ) sq2 order by liab_o_assets_desc_ntile10 desc, intno_o_liab_desc_ntile10 desc, intno_o_assets_desc_ntile10 desc
) sq3 group by cube(intno_o_liab_desc_ntile10, intno_o_assets_desc_ntile10, liab_o_assets_desc_ntile10) order by avg_pradchg_f26w_ann desc



-- SAME AS ABOVE
-- inttot: intno_q1 + int_q1
-- return of first 3.5years through first half of 2017 - best deal: 
--                                              
--                                              
-- this case: I want 'higher' ntile(good)_values
-- return of first 6months of 2017 - best deal: not matter or :lowest(best)    ratio of intno_o_liab
--                                              lowest(best)  ratio of intno_o_assets
--                                              highest(worst)liab_o_assets                                          
select inttot_o_liab_desc_ntile10, inttot_o_assets_desc_ntile10, liab_o_assets_desc_ntile10
  , avg(pradchg_f26w_ann) avg_pradchg_f26w_ann, stddev_pop(pradchg_f26w_ann) stddev_pop_pradchg_f26w_ann
  , count(pradchg_f26w_ann) count_pradchg_f26w_ann
  , avg(inttot_o_liab) avg_inttot_o_liab, avg(inttot_o_assets) avg_inttot_o_assets , avg(liab_o_assets) avg_liab_o_assets
from (
  select 
             avg(pradchg_f26w_ann) over (partition by inttot_o_liab_desc_ntile10)          avg_inttot_o_liab_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by inttot_o_liab_desc_ntile10)   stddev_pop_inttot_o_liab_desc_ntile10_pradchg_f26w_ann
    ,        avg(pradchg_f26w_ann) over (partition by inttot_o_assets_desc_ntile10)        avg_inttot_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by inttot_o_assets_desc_ntile10) stddev_pop_inttot_o_assets_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    ,        avg(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)         avg_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , stddev_pop(pradchg_f26w_ann) over (partition by liab_o_assets_desc_ntile10)  stddev_pop_liab_o_assets_desc_ntile10_desc_ntile10_desc_ntile10_pradchg_f26w_ann
    , sq2.*
    from (
    select 
                                                 -- this case: I want 'higher' ntile(good)_values
                                                 -- desc: higher the value, then the lower the ntile_value ( like 'rank' )
                               -- diff than usual: higher value is 'bad'
        ntile(2) over (order by sq1.inttot_o_liab   desc) inttot_o_liab_desc_ntile10
      , ntile(2) over (order by sq1.inttot_o_assets desc) inttot_o_assets_desc_ntile10
      , ntile(2) over (order by sq1.liab_o_assets  desc)  liab_o_assets_desc_ntile10
      , sq1.* 
      from (
      select 
          dateindex, dateindexyear, dateindexyear, ticker, company
        , pradchg_f26w_ann
        , (intno_q1 + int_q1)/nullif(liab_q1,0) inttot_o_liab, (intno_q1 + int_q1)/nullif(assets_q1,0) inttot_o_assets, liab_q1/nullif(assets_q1,0) liab_o_assets
      from fe_data_store.si_finecon2 now 
      where intno_q1 is not null and liab_q1 is not null and assets_q1 is not null 
        and pradchg_f26w_ann is not null
        and sp in ('500') and dateindex >= 16071 -- 2004-01-01 - (to 17529) -- pradchg_f26w_ann return of first 6months of 2017
    ) sq1 
  ) sq2 order by liab_o_assets_desc_ntile10 desc, inttot_o_liab_desc_ntile10 desc, inttot_o_assets_desc_ntile10 desc
) sq3 group by cube(inttot_o_liab_desc_ntile10, inttot_o_assets_desc_ntile10, liab_o_assets_desc_ntile10) order by avg_pradchg_f26w_ann desc



-- MISSING VALUES so LESS OBS - NEED ONE QUERY PER ITEM ( OR BELOW SEE AVG ) 
-- bad times and good times how does intno_q1 stack up
-- 
select mktcap, assets_q1, liab_q1, intno_q1, int_q1 from fe_data_store.si_finecon2 where dateindex = 16860 and sp = '500'; -- "2016-02-29"
-- STARTED TO PUT IN ZEROS where intno_q1 and int_q1 ARE NULL

select mktcap, assets_q1, liab_q1, intno_q1, int_q1 from fe_data_store.si_finecon2 where dateindex = 16891 and sp = '500'; -- 2016-03-31



--WHEN I HAVE TIME SEARCH FOR OUTLIERS
--NEED AN R FUNCTION [ ] -- dateindex by dateindex, column_by_column looper
-- outlier comparer
select 
      le.company_id le_company_id, le.ticker le_ticker, le.company le_company
    , le.int_q1 le_int_q1, re.int_q1 re_int_q1, re.int_q1/nullif(le.int_q1,0) re_o_le_multiplyer
    , re.company re_company, re.ticker re_ticker, re.company_id re_company_id
from fe_data_store.si_finecon2 le full outer join fe_data_store.si_finecon2 re on le.company_id = re.company_id 
where le.dateindex = 16860 and re.dateindex = 16891 and le.sp = '500' and re.sp = '500' and le.adr = 0 and re.adr = 0
order by re_o_le_multiplyer desc nulls last
-- left(le) and right(re) compare dateindexes                 -- sp(le and re) filter_expr
-- dir_i on "W:\AAIISIProDBFs" to to get the before and after dateindexes


get_sipro_sp500_various_int_expenses_eom_xts 

-- SAVE TO AN R FUNCTION [ ]

-- "2016-03-31" 
-- Stock Investor Pro 4.5 (3/31/2016 release)
-- Null Handling
-- With some database services, 
-- missing data may simply be converted to a zero. While this is an 
-- easy solution, it may lead to the end user drawing incorrect 
-- conclusions about the company’s financial state or it may lead to 
-- incorrect screening results. Stock Investor 4.5 now only 
-- deliberately converts missing data to zeros in those cases where 
-- the company has not reported a line item across its entire time 
-- series. For example, if a company has never reported long-term 
-- debt on its balance sheet or dividends on its income statement, 
-- null (NA) values for these fields are converted to zeros. 
-- Otherwise, if there is a missing piece of data for a given data 
-- field within a time series, this null value remains a null (NA).

-- Formula Changes for Existing Data Fields
-- in some cases, that we had to start calculating the data ourselves.
-- Interest expense (INT_Q1-Q8, 12M, Y1-Y7)

select 
    to_timestamp(dateindex*3600*24)::date dateindex_dt
  , dateindex
  , count(mktcap)    
  , sum(mktcap)    / count(mktcap)    a_mktcap
  , sum(assets_q1) / count(assets_q1) a_assets
  , sum(liab_q1)   / count(liab_q1)   a_liab
  , (sum(intno_q1) + sum(int_q1))  / count(1)                               a_inttot -- best that I can do
  , sum(mktcap)    / nullif(sum(intno_q1) + sum(int_q1),0)                  s_mktcap_o_s_inttot
  , (sum(assets_q1) - sum(liab_q1)) / nullif(sum(intno_q1) + sum(int_q1),0) s_equity_o_s_inttot
  , sum(assets_q1) / nullif(sum(intno_q1) + sum(int_q1),0)                  s_assets_o_s_inttot
  , sum(liab_q1)   / nullif(sum(intno_q1) + sum(int_q1),0)                  s_liab_o_s_inttot
  , sum(mktcap)    / nullif(sum(liab_q1),0)                                 s_mktcap_o_s_liab
from ( 
       select dateindex, sp, mktcap, assets_q1, liab_q1
       -- Stock Investor Pro 4.5 (3/31/2016 release) -- Null Handling -- calculating the data ourselves -- Interest expense (INT_Q1-Q8, 12M, Y1-Y7)
       -- I do the entire history because it makes sense
       , case when intno_q1 is null then 0 else intno_q1 end intno_q1  -- typically before 2016-03-31
       , case when int_q1   is null then 0 else                        -- typically before 2016-03-31
       -- errors at -- Stock Investor Pro 4.5 (3/31/2016 release)
         case when dateindex = 16891 and int_q1 >= 5071.00 and adr = 0 then int_q1 / 1000.00 else -- MANY errors
         case when dateindex = 17225 and ticker = 'AVB' then int_q1 / 1000.00 else         -- seems outlier error (fixable?)
         case when dateindex = 17470 and ticker = 'JPM' then null             else int_q1  -- seems outlier error (not fixable) -- ONE case
         end end end end int_q1
       from fe_data_store.si_finecon2
     ) sq1
      -- outlier ONE case
where int_q1 is not null and mktcap is not null and assets_q1 is not null and liab_q1 is not null
and sp in ('500') -- 
group by dateindex order by dateindex

-- COME BACK [ ] after reindex
-- outlier comparer a_cash to wa_mktcap_cash jumps by double to next point

select 
      le.company_id le_company_id, le.ticker le_ticker, le.company le_company
    , le.mktcap le_mktcap, re.mktcap re_mktcap, re.mktcap/nullif(le.mktcap,0) re_o_le_multiplyer
    , re.company re_company, re.ticker re_ticker, re.company_id re_company_id
from fe_data_store.si_finecon2 le full outer join fe_data_store.si_finecon2 re on le.company_id = re.company_id 
where le.dateindex = 12783 and re.dateindex = 12814 and le.sp = '500' and re.sp = '500' and le.adr = 0 and re.adr = 0
order by re_o_le_multiplyer desc nulls last
-- O.K.

select le.mktcap,
      le.company_id le_company_id, le.ticker le_ticker, le.company le_company
    , le.cash_q1 le_cash_q1, re.cash_q1 re_cash_q1, re.cash_q1/nullif(le.cash_q1,0) re_o_le_multiplyer
    , re.company re_company, re.ticker re_ticker, re.company_id re_company_id
from fe_data_store.si_finecon2 le full outer join fe_data_store.si_finecon2 re on le.company_id = re.company_id 
where le.dateindex = 12783 and re.dateindex = 12814 and le.sp = '500' and re.sp = '500' and le.adr = 0 and re.adr = 0
order by re_o_le_multiplyer desc nulls last
--NOT OK
385882.90;"3737N";"GE";"General Electric Company";10075.00;150800.00;14.9677419354838710;"General Electric Company";"GE";"3737N"
39416.40;"7884N";"SLB";"Schlumberger Limited";201.20;2997.40;14.8976143141153082;"Schlumberger Limited";"SLB";"7884N"
9034.30;"CE04D";"KMI";"Kinder Morgan, Inc.";12.20;177.00;14.5081967213114754;"Kinder Morgan, Inc.";"KMI";"CE04D"
3898.70;"64702";"NVLS";"Novellus Systems, Inc.";51.90;583.00;11.2331406551059730;"Novellus Systems, Inc.";"NVLS";"64702"


select le.mktcap,
      le.company_id le_company_id, le.ticker le_ticker, le.company le_company
    , le.cash_q1 le_cash_q1, re.cash_q1 re_cash_q1, re.cash_q1/nullif(le.cash_q1,0) re_o_le_multiplyer
    , re.company re_company, re.ticker re_ticker, re.company_id re_company_id
from fe_data_store.si_finecon2 le full outer join fe_data_store.si_finecon2 re on le.company_id = re.company_id 
where le.dateindex = 12783 and re.dateindex = 12842 and le.sp = '500' and re.sp = '500' and le.adr = 0 and re.adr = 0
order by re_o_le_multiplyer desc nulls last
7093.70;"47507010";"JP";"Jefferson-Pilot Corporation";25.00;27655.00;1106.2000000000000000;"Jefferson-Pilot Corporation";"JP";"47507010"
1613.40;"2284N";"CTB";"Cooper Tire & Rubber Company";24.90;938.60;37.6947791164658635;"Cooper Tire & Rubber Company";"CTB";"2284N"
385882.90;"3737N";"GE";"General Electric Company";10075.00;150800.00;14.9677419354838710;"General Electric Company";"GE";"3737N"
39416.40;"7884N";"SLB";"Schlumberger Limited";201.20;2997.40;14.8976143141153082;"Schlumberger Limited";"SLB";"7884N"
9034.30;"CE04D";"KMI";"Kinder Morgan, Inc.";12.20;177.00;14.5081967213114754;"Kinder Morgan, Inc.";"KMI";"CE04D"
3898.70;"64702";"NVLS";"Novellus Systems, Inc.";51.90;583.00;11.2331406551059730;"Novellus Systems, Inc.";"NVLS";"64702"



-- 2007-2008 recession
-- seems most noticable s_mktcap_o_cash, s_equity_o_cash - strong downtroddent before 2007-2008 recession
--
-- short way
-- long way (very slow, does not scale - does not use pre-calculated pradchg_f13w_ann)
--
-- 2007-2008 recession
-- inbound loosing value: seems most: s_mktcap_o_cash, s_equity_o_cash
---late 2015-early 2016 false recession
-- slight inbount loosing value: s_mktcap_o_cash, 'no loosing value': s_equity_o_cash
-- 
explain
select 
    to_timestamp(dateindex*3600*24)::date dateindex_dt
  , dateindex
  , count(mktcap)                     c_mktcap
  , sum(mktcap)    / count(mktcap)    a_mktcap
  , sum(assets_q1) / count(assets_q1) a_assets
  , sum(liab_q1)   / count(liab_q1)   a_liab
  , sum(cash_q1)   / count(cash_q1)                                                                 a_cash
  , sum(cash_q1*mktcap)   / sum(mktcap)                                                             wa_mktcap_cash
  , sum(mktcap)    / nullif(sum(cash_q1),0)                                                         s_mktcap_o_s_cash
  , (sum(assets_q1) - sum(liab_q1)) / nullif(sum(cash_q1),0)                                        s_equity_o_s_cash
  , sum(assets_q1) / nullif(sum(cash_q1),0)                                                         s_assets_o_s_cash
  , sum(liab_q1)   / nullif(sum(cash_q1),0)                                                         s_liab_o_s_cash
  , sum(mktcap)    / nullif(sum(liab_q1),0)                                                         s_mktcap_o_s_liab
  , sum( (pradchg_f13w_ann) * 1      ) / count(pradchg_f13w_ann)     a_pradchg_f13w_ann   
  , sum( (pradchg_f13w_ann) * mktcap ) / sum(mktcap)         wa_mktcap_pctchg_f03_price_ann 
from ( 
       select now.dateindex, now.sp, now.mktcap, now.assets_q1, now.liab_q1, now.cash_q1
            , now.pradchg_f13w_ann
       from fe_data_store.si_finecon2 now
       where
             now.sp in(values('500'))
         and now.assets_q1 is not null and now.liab_q1 is not null  
         and now.cash_q1 is not null 
         and now.mktcap is not null 
         and now.pradchg_f13w_ann is not null
         and ((now.split_date < now.dateindex - 93) or now.split_date is null) -- no split within the last 93 days ( or never a split in history )
     ) sq1
group by dateindex order by dateindex



-- SAME AS ABOVE ( but I also care about interest payments)

--r_s_cash_o_s_intno_int
--means something or ( somthing else means something)
--
--uptrending? seems better than downtrending
--but downtrending does not (often) hurt(recently/toward 2008 recession)
--slope flattens (high)"2004-12-31" 21.2660864028290493
--slope flattens  (low)"2008-04-30" 6.1922659177590551
--slope flattens (high)"2015-04-30" 47.8389552292651250
--                     "2017-09-29" 26.3586874116369448
--comment out --now.pradchg_f13w_ann
--                      "2017-12-29" 27.3959806633438585
explain
select 
    to_timestamp(dateindex*3600*24)::date dateindex_dt
  , dateindex
  , count(mktcap)                     c_mktcap
  , sum(mktcap)    / count(mktcap)    a_mktcap
  , sum(assets_q1) / count(assets_q1) a_assets
  , sum(liab_q1)   / count(liab_q1)   a_liab
  , sum(cash_q1)   / (sum(intno_q1) + sum(int_q1))                r_s_cash_o_s_intno_int
  -- , sum( (pradchg_f13w_ann) * 1      ) / count(pradchg_f13w_ann)          a_pradchg_f13w_ann   
  -- , sum( (pradchg_f13w_ann) * mktcap ) / sum(mktcap)              wa_mktcap_pctchg_f03_price_ann 
from ( 
       select now.dateindex, now.sp, now.mktcap, now.assets_q1, now.liab_q1, now.cash_q1
           -- , now.pradchg_f13w_ann
           -- Stock Investor Pro 4.5 (3/31/2016 release) -- Null Handling -- calculating the data ourselves -- Interest expense (INT_Q1-Q8, 12M, Y1-Y7)
           -- I do the entire history because it makes sense
           , case when now.intno_q1 is null then 0 else now.intno_q1 end intno_q1  -- typically before 2016-03-31
           , case when now.int_q1   is null then 0 else                        -- typically before 2016-03-31
           -- errors at -- Stock Investor Pro 4.5 (3/31/2016 release)
             case when now.dateindex = 16891 and now.int_q1 >= 5071.00 and now.adr = 0 then now.int_q1 / 1000.00 else -- MANY errors
             case when now.dateindex = 17225 and now.ticker = 'AVB' then now.int_q1 / 1000.00 else         -- seems outlier error (fixable?)
             case when now.dateindex = 17470 and now.ticker = 'JPM' then null             else now.int_q1  -- seems outlier error (not fixable) -- ONE case
             end end end end int_q1
       from fe_data_store.si_finecon2 now
       where
             now.sp in(values('500'))
         and now.assets_q1 is not null and now.liab_q1 is not null  
         and now.cash_q1 is not null 
         and now.mktcap is not null 
         -- and now.pradchg_f13w_ann is not null
         and now.int_q1 is not null -- ONE CASE
         and ((now.split_date < now.dateindex - 93) or now.split_date is null) -- no split within the last 93 days ( or never a split in history )
     ) sq1
group by dateindex order by dateindex



--SKIP THIS
-- long way (very slow, does not scale - does not use pre-calculated pradchg_f13w_ann)
-- 2007-2008 recession
-- inbound loosing value: seems most s_mktcap_o_cash, s_equity_o_cash
-- 
--
explain
select 
    to_timestamp(dateindex*3600*24)::date dateindex_dt
  , dateindex
  , count(mktcap)                     c_mktcap
  , sum(mktcap)    / count(mktcap)    a_mktcap
  , sum(assets_q1) / count(assets_q1) a_assets
  , sum(liab_q1)   / count(liab_q1)   a_liab
  , sum(cash_q1)   / count(cash_q1)                                                                 a_cash
  , sum(cash_q1*mktcap)   / sum(mktcap)                                                             wa_mktcap__cash
  , sum(mktcap)    / nullif(sum(cash_q1),0)                                                         s_mktcap_o_s_cash
  , (sum(assets_q1) - sum(liab_q1)) / nullif(sum(cash_q1),0)                                        s_equity_o_s_cash
  , sum(assets_q1) / nullif(sum(cash_q1),0)                                                         s_assets_o_s_cash
  , sum(liab_q1)   / nullif(sum(cash_q1),0)                                                         s_liab_o_s_cash
  , sum(mktcap)    / nullif(sum(liab_q1),0)                                                         s_mktcap_o_s_liab
  , sum( ((f03_price - now_price) / nullif(abs(now_price),0) * 4 * 100) * 1      ) / count(now_price)            a_pctchg_f03_price_ann   
  , sum( ((f03_price - now_price) / nullif(abs(now_price),0) * 4 * 100) * mktcap ) / sum(mktcap)         wa_mktcap_pctchg_f03_price_ann 
from ( 
       select now.dateindex, now.sp, now.mktcap, now.assets_q1, now.liab_q1, now.cash_q1
            , now.price now_price, f03.price f03_price
       from fe_data_store.si_finecon2 now, fe_data_store.si_finecon2 f03
       where 
             now.sp in(values('500'))
         and now.assets_q1 is not null and now.liab_q1 is not null  
         and now.cash_q1 is not null 
         and now.mktcap is not null 
         and now.price is not null and f03.price is not null is not null 
         and ((f03.split_date < f03.dateindex - 93) or f03.split_date is null) -- no split within the last 93 days ( or never a split in history )
         and now.dateindexf03eom = f03.dateindexeom and now.company_id = f03.company_id
         and now.dateindex <= 12510 -- just first 12 months -- 15491 -- may2012 --  --17074 -- just recent 12 months
     ) sq1
group by dateindex order by dateindex
-- 12(15)months 1:38 -- VERY LONG





