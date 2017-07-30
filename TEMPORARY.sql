

set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
--set work_mem to  '1200MB';
   set work_mem to '2047MB';
set constraint_exclusion = on;
-- postgresql 9.6
set max_parallel_workers_per_gather to 4; -- not 'written in docs'



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


[ ] pl/r TRY THIS
fuzzy matching by Levenshtein
------------------------------

agrepl(pattern = , x = , max.distance = 0.1, ignore.case = TRUE, fixed = FALSE) # .Internal(agrepl(pattern

'1 Infinite Loop'
'1 Infinite Loop 1 Infinite Loop'
'ONE INFINITE LOOP'

# Apple address looks thorugout the years ( human entry & errors )

-- 15184 -- AAII - update_from_future_new_company_ids
agrepl(pattern = '1 Infinite Loop', x = '1 Infinite Loop 1 Infinite Loop', max.distance = 0.1, ignore.case = TRUE, fixed = FALSE)
[1] TRUE

agrepl(pattern = '1 Infinite Loop', x = 'ONE INFINITE LOOP', max.distance = 0.1, ignore.case = TRUE, fixed = FALSE)
[1] TRUE

> agrepl(pattern = '1 Infinite Loop', x = 'ONE', max.distance = 0.1, ignore.case = TRUE, fixed = FALSE)
[1] FALSE
> agrepl(pattern = '1 Infinite Loop', x = 'INFINITE', max.distance = 0.1, ignore.case = TRUE, fixed = FALSE)
[1] FALSE
> agrepl(pattern = '1 Infinite Loop', x = 'LOOP', max.distance = 0.1, ignore.case = TRUE, fixed = FALSE)
[1] FALSE

Andre Experience



Where is a temporary table created?
-----------------------------------

Temporary tables get put into a schema called

   "pg_temp_NNN", 

where "NNN" indicates which server backend you're connected to. 
This is implicitly added to your search path in the session that creates them.

Note that you can't access one connection's temp tables via another connection

pg_my_temp_schema()	oid	OID of session's temporary schema, or 0 if none
WRONG: 'if none, then no records returned'

postgres=# SELECT  pg_my_temp_schema();
 pg_my_temp_schema
-------------------
                 0
(1 row)

# upone first TEMPORARY object creation, the 'temp schema' will be created.
# ALSO, the 'new temp schema' will STAY after all temp objects are destroyed

create temporary table xxx(); # ACCESS by xxx # drop by xxx  ( NOT pg_temp_2.xxx : will not work )

CREATE TABLE
SELECT  pg_my_temp_schema();
 pg_my_temp_schema
-------------------
            199757
(1 row)

alter table xxx add xx int;

truncate table xxx; -- PostgreSQL 9.6 ( no 'if exists' in language syntax )
TRUNCATE TABLE

select nspname from pg_namespace where oid = pg_my_temp_schema();

  nspname
-----------
 pg_temp_2
(1 row)

# ZERO in ARTICLE
# NOTE: pg_stat_activity NO LONGER HAS procpid COLUMN and HAS radically changed
# SO STICK WITH THE STANDARD: information_schema

# list all your temporary tables ( and columns ):

postgres=# select pg_backend_pid();
 pg_backend_pid
----------------
          54120
(1 row)

select table_catalog, table_schema, table_name, column_name, ordinal_position, data_type
  from information_schema.columns where table_schema = 'pg_temp_2';


 table_catalog | table_schema | table_name | column_name | ordinal_position | data_type
---------------+--------------+------------+-------------+------------------+-----------
 postgres      | pg_temp_2    | xxx        | xx          |                1 | integer
(1 row)

Where is temporary table created?
2010
https://stackoverflow.com/questions/3037160/where-is-temporary-table-created






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

--       ABOVE
--       [ ] ( FIGURE OUT HOW TO ACCUMULATE: prchg_free_01m ( like netinc ) # WINDOWS FUNCTION LIKE?

--       ABOVE
--       [ ] replace    case when pertyp_q1 = 'W' then 7.0 * perlen_q1  BY  perlen_q1 - perlen_q2
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



