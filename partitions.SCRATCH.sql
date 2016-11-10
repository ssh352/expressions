


set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;


-- > zoo::as.Date(16525)
-- [1] "2015-03-31"
-- > zoo::as.Date(16129)
-- [1] "2014-02-28"

set search_path=sipro_stage;
set time zone 'utc';

-- > as.integer(zoo::as.Date("2016-02-29")) # monday # last day of month
-- [1] 16860
-- > as.integer(zoo::as.Date("2017-02-28")) # tuesday
-- [1] 17225


-- > zoo::as.Date("2013-05-31") # friday # [1] 15856
-->  zoo::as.Date("2014-05-31") # saturday # [[1] 16221 ---> gets pulled back to friday

select case 
  when extract(dow from dd.near_trg_f12m_dt) = 0 
    then dd.near_trg_f12m_dt - interval '2 day'
  when extract(dow from dd.near_trg_f12m_dt) = 6 
    then dd.near_trg_f12m_dt - interval '1 day'
  else 
    dd.near_trg_f12m_dt
end
from (
select date_trunc('month', to_timestamp(5856*3600*24)::date) 
+ interval '13 month' - interval '1 day'  near_trg_f12m_dt
) dd;
-- GOOD
  -- . . . eventually want an index timeindexf12mlwd- future 12m last week day ( integer )
  -- . . . create index

--  extract( 'epoch' from  ## ) / ( 3600*24 ) 

-- if intervals are reversable

select 
timeindexf12meom,
case 
  when extract(dow from mldom.f12mldom) = 0  then (extract('epoch' from mldom.f12mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f12mldom) = 6  then (extract('epoch' from mldom.f12mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f12mldom) /(3600*24))::int end timeindexf12mlwd,
timeindexf09meom,
case 
  when extract(dow from mldom.f09mldom) = 0  then (extract('epoch' from mldom.f09mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f09mldom) = 6  then (extract('epoch' from mldom.f09mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f09mldom) /(3600*24))::int end timeindexf09mlwd,
timeindexf06meom,
case 
  when extract(dow from mldom.f06mldom) = 0  then (extract('epoch' from mldom.f06mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f06mldom) = 6  then (extract('epoch' from mldom.f06mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f06mldom) /(3600*24))::int end timeindexf06mlwd,
timeindexf03meom,
case 
  when extract(dow from mldom.f03mldom) = 0  then (extract('epoch' from mldom.f03mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f03mldom) = 6  then (extract('epoch' from mldom.f03mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f03mldom) /(3600*24))::int end timeindexf03mlwd
from (
select 
                        f1m_and_b1d.f1m_and_b1d + interval '12 month'                             f12mldom, -- future12m last day of month
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '12 month')  /(3600*24))::int timeindexf12meom,
                        f1m_and_b1d.f1m_and_b1d + interval '09 month'                             f09mldom,
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '09 month')  /(3600*24))::int timeindexf09meom,
                        f1m_and_b1d.f1m_and_b1d + interval '06 month'                             f06mldom,
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '06 month')  /(3600*24))::int timeindexf06meom,
                        f1m_and_b1d.f1m_and_b1d + interval '03 month'                             f03mldom,
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '03 month')  /(3600*24))::int timeindexf03meom
from (
select date_trunc('month', to_timestamp(15856*3600*24)::date) 
 + interval '1 month' - interval '1 day'  f1m_and_b1d -- forward 1 month and back 1 day
) f1m_and_b1d
) mldom; -- month last day of month
--WORKS KEEP

-- NOTE: SOME CASE: HAVE TO GO BACKWARD 12 MONTHS   massAAIISIProIterMktSheets

-- alter table your_table add timeindexf12meom integer;
-- alter table your_table add timeindexf12mlwd integer;
-- alter table your_table add timeindexf09meom integer;
-- alter table your_table add timeindexf09mlwd integer;
-- alter table your_table add timeindexf06meom integer;
-- alter table your_table add timeindexf06mlwd integer;
-- alter table your_table add timeindexf03meom integer;
-- alter table your_table add timeindexf03mlwd integer;
--SHOULD WORK

-- update your_table tbl
-- set timeindexf12meom = n_dis.timeindexf12meom,
-- set timeindexf12mlwd = n_dis.timeindexf12mlwd,
-- set timeindexf09meom = n_dis.timeindexf09meom,
-- set timeindexf09mlwd = n_dis.timeindexf09mlwd,
-- set timeindexf06meom = n_dis.timeindexf06meom,
-- set timeindexf06mlwd = n_dis.timeindexf06mlwd,
-- set timeindexf03meom = n_dis.timeindexf03meom,
-- set timeindexf03mlwd = n_dis.timeindexf03mlwd
-- from( ABOVE: WORKS KEEP ) n_dis  where n_dis.dateindex = tbl.dateindex -- n_dis; new date indexes
--SHOULD WORK

--MAY BE BETTER TO EXTRACT THE 52W,26W,13W AND PULL BACK INTO CURRENT TIME


-- 
-- explain
-- select ci.dateindex
-- from si_ci ci join lateral ( 
--   select psd.dateindex  
--   from si_psd psd 
--   where psd.dateindex = 16070 and
--   ci.dateindex = 15705 
-- ) inline_view on true; 

-- > 16070 - 15705
-- [1] 365

explain
select ci2.dateindex
from ( select ci.*  from si_ci ci where  ci.dateindex = 15705 ) ci2 join lateral ( 
  select psd2.dateindex
  from ( select psd.* from si_psd psd where psd.dateindex = 16070 ) psd2
  where ci2.company_id = psd2.company_id
) iw on true; 
-- GOOD - SPEED 9000 ( 1 SECOND )

explain
select ci2.dateindex, iv.prchg_52w_n  -- NEW(EXTRA COLUMN) IS HERE
from ( select ci.*  from si_ci ci where  ci.dateindex = 15705 ) ci2 join lateral ( 
  select psd2.dateindex, psd2.prchg_52w::numeric(15,2) prchg_52w_n
  from ( select psd.* from si_psd psd where psd.dateindex = 16070 ) psd2
  where ci2.company_id = psd2.company_id
) iv on true; 
-- WORKS ( SAME AS ABOVE )
-- GOOD - SPEED 9000 ( 1 SECOND )

explain
select ci2.dateindex, ci2.company_id, ci2.ticker,  iv.f_prchg_52w_n
from ( select ci.*  from si_ci ci where  ci.dateindex = 15705 ) ci2 join lateral ( 
  select psd2.dateindex, psd2.prchg_52w::numeric(15,2) f_prchg_52w_n -- future price change 52w numeric
  from ( select psd.* from si_psd psd where psd.dateindex = 16070) psd2 
  where ci2.company_id = psd2.company_id
) iv on true; 

explain
select ci2.dateindex, ci2.company_id, ci2.ticker,  iv.f_prchg_52w_n
from ( select ci.*  from si_ci ci where  ci.dateindex = 15705 ) ci2 join lateral ( 
  select psd2.dateindex, psd2.prchg_52w::numeric(15,2) f_prchg_52w_n -- future price change 52w numeric
  from ( select psd.* from si_psd psd where psd.dateindex = 16070 + ci2.dateindex/ci2.dateindex -1) psd2 -- test get to ci2
  where ci2.company_id = psd2.company_id
) iv on true; 
-- SLOW (OVER 60 SECONDS) ( BUT WORKS ) - BUT MAY BE GOOD FOR A 'ONE TIME PERMANENT COLUMN UPDATE' *** HERE - THIS ONE ***

-- BUT IF I CAN DO, ci.timeindexf12mlwd - psd.timeindex, THEN I CAN SKIP ALL OF THAT ***

explain
select ci2.dateindex
from ( select ci.*  from si_ci ci where  ci.dateindex = 15705 ) ci2 join lateral ( 
  select psd2.dateindex  
  from ( select psd.* from si_psd psd where psd.dateindex = ci2.dateindex + 365 ) psd2
  where ci2.company_id = psd2.company_id
) inline_view on true;
-- WORKS BUT SPEED - 166624 ( 23 SECONDS ) ( OR '66 SECONDS' )


select psd2.dateindex -- CAN NOT SEE CI
from ( select psd.* from si_psd psd where psd.dateindex = 16070 ) psd2
where company_id in (  select  ci.company_id from  si_ci ci where psd2.company_id = ci.company_id  
and ci.dateindex = 15705 ) -- CORRECT SECONDS 4, 1.4


explain
select psd2.dateindex  
from ( select psd.* from si_psd psd where psd.dateindex = 16070 ) psd2
where company_id in (  select  ci.company_id from  si_ci ci where psd2.company_id = ci.company_id  
and ci.dateindex = ( psd2.dateindex - 365 ) ) -- FOREVER, SECONDS 



-- (MOST DIRECT)DIRECT
explain
select ci2.dateindex dateindex_c, ci2.ticker, psd2.prchg_52w::numeric(15,2)
from ( select ci.*  from si_ci  ci  where  ci.dateindex = 15705 )  ci2
join ( select psd.* from si_psd psd where psd.dateindex = 16070 ) psd2   -- MAY NOT? reach ci2 TO DO math?
on ci2.company_id = psd2.company_id;
-- GOOD SPEED 12000 ( 1 SECOND ) ( FIRST RUN IS '3 SECONDS' ) ( BUT 'CAN' GO UP TO 17 SECONDS ) ( 60 SECONDS )
-- ONE SECOND, ONE SECOND AFTER MEMORY LOAD

-- select dateindex from si_ci  ci  where  ci.dateindex = 15705
-- PRROB JUST WANT TO PULL CI.SPECIFIC
-- select count(*) from si_ci  ci  where  ci.dateindex = 15705 
-- select count(*) from si_ci  ci  where  ci.dateindex = 16070 

explain
select count(*) 
from si_ci ci1, si_ci ci2 
where ci1.dateindex = 15705 and ci2.dateindex = 16070 and ci1.company_id = ci2.company_id ;
-- CORRECT


explain
select ci2.dateindex dateindex_c, ci2.ticker, psd2.prchg_52w::numeric(15,2)
from ( select ci.*  from si_ci  ci  where  ci.dateindex   = 15705  )   ci2,
     ( select ci.*  from si_ci  ci  where  ci.dateindex   = 16070  )   ci2_f,
     ( select psd.*  from si_psd psd where  psd.dateindex = 16070 )    psd2 
where
ci2.company_id   =  ci2_f.company_id and
ci2_f.company_id =  psd2.company_id;
-- CORRECT AND FAST 1.1

explain
select ci2.dateindex dateindex_c, ci2.ticker, psd2.prchg_52w::numeric(15,2)
from ( select ci.*  from si_ci  ci  where  ci.dateindex = 15705 )  ci2 
join ( select ci.*  from si_ci  ci  where  ci.dateindex = 16070 )  ci2_f
on        ci2.company_id = ci2_f.company_id 
join  ( select psd.*  from si_psd psd where  psd.dateindex = 16070 ) psd2   -- MAY NOT? reach ci2 TO DO math?
on      ci2_f.company_id =  psd2.company_id order by 2; 
-- CORRECT AND FAST 4.7, 1.1 ( ALSO 'check constraint WILL no longer SCAN each INDEX )

explain
select ci2.dateindex dateindex_c, ci2.ticker, psd2.prchg_52w::numeric(15,2)
from ( select ci.*  from si_ci  ci  where  ci.dateindex = (select 15705) )  ci2 
join ( select ci.*  from si_ci  ci  where  ci.dateindex = (select 16070) )  ci2_f
on        ci2.company_id = ci2_f.company_id 
join  ( select psd.*  from si_psd psd where  psd.dateindex = (select 16070 ) ) psd2   -- MAY NOT? reach ci2 TO DO math?
on      ci2_f.company_id =  psd2.company_id order by 2; 
-- SAME AS ABOVE: INFINITY PAGE COST -- CAN NOT OPTIMIZE: ( select )

-- NEED A LOOKUP_TABLE ( I THINK ) ( BETTER OFF ? si_returns with REAL returns



-- 32 seconds
select distinct(dateindex) from si_ci;

-- 8.7 seconds; 564000 rows
select first_value(dateindex) over (partition by dateindex) from si_ci;

-- and
--select distinct on (va) 16070 va;

-- INSTANT RESPONSE
select distinct on (dateindex) dateindex from si_ci order by 1;



select date_trunc('month', to_timestamp(dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  f1m_and_b1d 
from ( select distinct on (dateindex) dateindex from si_ci) f1m_and_b1d


-- BEGIN VERY KEEP --
-- BEGIN VERY KEEP --

create table sipro_data_store.si_retdate
as
select 
dateindex,
dateindexf12meom,
case 
  when extract(dow from mldom.f12mldom) = 0  then (extract('epoch' from mldom.f12mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f12mldom) = 6  then (extract('epoch' from mldom.f12mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f12mldom) /(3600*24))::int end dateindexf12mlwd,
dateindexf09meom,
case 
  when extract(dow from mldom.f09mldom) = 0  then (extract('epoch' from mldom.f09mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f09mldom) = 6  then (extract('epoch' from mldom.f09mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f09mldom) /(3600*24))::int end dateindexf09mlwd,
dateindexf06meom,
case 
  when extract(dow from mldom.f06mldom) = 0  then (extract('epoch' from mldom.f06mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f06mldom) = 6  then (extract('epoch' from mldom.f06mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f06mldom) /(3600*24))::int end dateindexf06mlwd,
dateindexf03meom,
case 
  when extract(dow from mldom.f03mldom) = 0  then (extract('epoch' from mldom.f03mldom - interval '2 day') /(3600*24))::int
  when extract(dow from mldom.f03mldom) = 6  then (extract('epoch' from mldom.f03mldom - interval '1 day') /(3600*24))::int
else (extract('epoch' from mldom.f03mldom) /(3600*24))::int end dateindexf03mlwd
from (
select 
  dateindex,
                        f1m_and_b1d.f1m_and_b1d + interval '12 month'                             f12mldom, -- future12m last day of month
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '12 month')  /(3600*24))::int dateindexf12meom,
                        f1m_and_b1d.f1m_and_b1d + interval '09 month'                             f09mldom,
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '09 month')  /(3600*24))::int dateindexf09meom,
                        f1m_and_b1d.f1m_and_b1d + interval '06 month'                             f06mldom,
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '06 month')  /(3600*24))::int dateindexf06meom,
                        f1m_and_b1d.f1m_and_b1d + interval '03 month'                             f03mldom,
  (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '03 month')  /(3600*24))::int dateindexf03meom
from (
-- forward 1 month and back 1 day
select date_trunc('month', to_timestamp(dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  f1m_and_b1d, 
       dateindex 
from ( select distinct on (dateindex) dateindex from si_ci) f1m_and_b1d -- PostgreSQL proprietary - returns just one of each
) f1m_and_b1d -- where dateindex = 16952 -- WORKS - just load the (current) month 
) mldom; 
-- WORKS - INSTANTANEIOUS

set search_path to sipro_stage,sipro_data_store;
set time zone 'utc';

select * from sipro_data_store.si_retdate;

-- END VERY KEEP --
-- END VERY KEEP --

--ERROR: VACUUM cannot be executed from a function or multi-command string -- SO SENT DBGETQUERY ALONE
--vacuum analyze sipro_data_store.si_retdate2;


-- WORKS -- ALREADY IN R [X]

set search_path to sipro_stage;
set work_mem to '1200MB';
set time zone 'utc';

insert into sipro_data_store.si_retdate2
  select 
  dateindex,
  dateindexf12meom,
  case 
    when extract(dow from mldom.f12mldom) = 0  then (extract('epoch' from mldom.f12mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f12mldom) = 6  then (extract('epoch' from mldom.f12mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f12mldom) /(3600*24))::int end dateindexf12mlwd,
  dateindexf09meom,
  case 
    when extract(dow from mldom.f09mldom) = 0  then (extract('epoch' from mldom.f09mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f09mldom) = 6  then (extract('epoch' from mldom.f09mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f09mldom) /(3600*24))::int end dateindexf09mlwd,
  dateindexf06meom,
  case 
    when extract(dow from mldom.f06mldom) = 0  then (extract('epoch' from mldom.f06mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f06mldom) = 6  then (extract('epoch' from mldom.f06mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f06mldom) /(3600*24))::int end dateindexf06mlwd,
  dateindexf03meom,
  case 
    when extract(dow from mldom.f03mldom) = 0  then (extract('epoch' from mldom.f03mldom - interval '2 day') /(3600*24))::int
    when extract(dow from mldom.f03mldom) = 6  then (extract('epoch' from mldom.f03mldom - interval '1 day') /(3600*24))::int
  else (extract('epoch' from mldom.f03mldom) /(3600*24))::int end dateindexf03mlwd
  from (
  select 
    dateindex,
                          f1m_and_b1d.f1m_and_b1d + interval '12 month'                             f12mldom, -- future12m last day of month
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '12 month')  /(3600*24))::int dateindexf12meom,
                          f1m_and_b1d.f1m_and_b1d + interval '09 month'                             f09mldom,
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '09 month')  /(3600*24))::int dateindexf09meom,
                          f1m_and_b1d.f1m_and_b1d + interval '06 month'                             f06mldom,
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '06 month')  /(3600*24))::int dateindexf06meom,
                          f1m_and_b1d.f1m_and_b1d + interval '03 month'                             f03mldom,
    (extract('epoch' from f1m_and_b1d.f1m_and_b1d + interval '03 month')  /(3600*24))::int dateindexf03meom
  from (
  -- forward 1 month and back 1 day
  select date_trunc('month', to_timestamp(dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  f1m_and_b1d, 
         dateindex 
  from ( select distinct on (dateindex) dateindex from si_ci where dateindex = 16952 ) f1m_and_b1d -- PostgreSQL proprietary - returns just one of each
  ) f1m_and_b1d -- where dateindex = 16952 -- WORKS - just load the (current) month 
  ) mldom; 

-- END WORKS


sipro_data_store.si_retdate

-- COPY FROM ABOVE
explain
select ci2.dateindex dateindex_c, ci2.ticker, psd2.prchg_52w::numeric(15,2)
from ( select ci.*  from si_ci  ci  where  ci.dateindex = 15705 )  ci2 
join ( select ci.*  from si_ci  ci  where  ci.dateindex = 16070 )  ci2_f
on        ci2.company_id = ci2_f.company_id 
join  ( select psd.*  from si_psd psd where  psd.dateindex = 16070 ) psd2   -- MAY NOT? reach ci2 TO DO math?
on      ci2_f.company_id =  psd2.company_id order by 2; 
-- CORRECT AND FAST 4.7, 1.1, 32.0

-- ABOVE: DO NOT USE ci.* IT WILL LITERALLY PULL ALL OF THE COLUMNS BACK ( VERY SLOW )

--select retd.dateindexf12mlwd from sipro_data_store.si_retdate retd where retd.dateindex  = 15705
--16070

select ci2.dateindex dateindex_c, ci2.ticker, psd2.prchg_52w::numeric(15,2)
from ( select ci.dateindex, ci.company_id, ci.ticker_unq ticker from si_ci ci  where  ci.dateindex = 15705 )  ci2 
join ( select ci.dateindex, ci.company_id, ci.ticker_unq ticker from si_ci ci  where  ci.dateindex = 16070 )  ci2_f
on        ci2.company_id = ci2_f.company_id 
join  ( select psd.*  from si_psd psd where  psd.dateindex = 16070 ) psd2   -- MAY NOT? reach ci2 TO DO math?
on      ci2_f.company_id =  psd2.company_id order by 2; 



select * 
from si_psd psd
where psd.dateindex = 16070
-- 14,10.8 SECONDS TO PULL BACK 16070 ROWS

explain analyze
select psd.dateindex, psd.company_id, psd.ticker_unq, psd.prchg_52w::numeric(15,2)
from si_psd psd, sipro_data_store.si_retdate retd
where psd.dateindex = retd.dateindexf12mlwd order by 2
-- 428,00 ROWS = 7.1,10,13.9,21,63 SECONDS ( 20 MB but MAX CPUs - DATA IS BESING FLUSHED OUT )
--GOOD ( BUT IS SCANNING THE INDEXES OF ALL OF THE PSD PARTITIONS )

explain
select ci.dateindex dateindex_c, ci.ticker_unq ticker_unq_c, ci.company, psd.dateindex dateindex_f, 
psd.ticker_unq ticker_unq_f, 
psd.prchg_52w::numeric(15,2)
from si_ci ci, si_psd psd, sipro_data_store.si_retdate retd 
where
retd.dateindex = ci.dateindex and
retd.dateindex = psd.dateindex and
ci.dateindex  = psd.dateindex and
ci.company_id = psd.company_id 
-- 58 SECONDS PRESENT DATA ( PSD is NOT USEFUL   300,000 cost = 60 SECONDS


select ci.dateindex dateindex_c, ci.ticker_unq ticker_unq_c, ci.company,
isq.ticker_unq ticker_unq_f, 
isq.netinc_q1::numeric(15,2)
from si_ci ci, si_isq isq, sipro_data_store.si_retdate retd 
where
retd.dateindex = ci.dateindex and
retd.dateindex = isq.dateindex and
ci.dateindex  = isq.dateindex and
ci.company_id = isq.company_id;
-- 180 SECONDS  -- 156 SECONDS(set work_mem to '30MB')

explain analyze
select ci.dateindex dateindex_c, ci.ticker_unq ticker_unq_c, ci.company,
cfq.fcfps_q1::numeric(15,2),
isq.netinc_q1::numeric(15,2)
from 
si_ci ci, 
si_isq isq, 
si_cfq cfq,
sipro_data_store.si_retdate retd 
where
retd.dateindex = ci.dateindex and
retd.dateindex = isq.dateindex and
retd.dateindex = cfq.dateindex and
ci.dateindex  = isq.dateindex and
ci.company_id = isq.company_id and
ci.company_id = cfq.company_id;
-- 592,000 
-- set work_mem to '30MB'; -- 192 seconds

explain
select ci.dateindex, ci.company_id, ci.ticker_unq ticker_unq_c, ci.company
from si_ci ci left join lateral ( 
select isq.dateindex, isq.company_id
from si_isq isq
where isq.dateindex = ci.dateindex 
and isq.company_id = ci.company_id
) inline_view on true;
-- COST 407,000 ( 102 SECONDS )      LATERSL
--                              JOIN LATERAL ( 179 SECONDS ) 407,000

explain
select ci.dateindex, ci.company_id, ci.ticker_unq ticker_unq_c, ci.company
from si_ci ci left join lateral ( 
select isq.dateindex, isq.company_id
from si_isq isq, si_bsq bsq
where isq.dateindex = ci.dateindex and
      bsq.dateindex = ci.dateindex and
  isq.company_id = ci.company_id and
  bsq.company_id = ci.company_id
) inline_view on true;
-- COST 9,000,000 - NEVER FINISH


explain
select ci.dateindex, ci.company_id, ci.ticker_unq ticker_unq_c, ci.company
from si_ci ci left join lateral ( 
select isq.dateindex, isq.company_id
from si_isq isq
where isq.dateindex = ci.dateindex 
and isq.company_id = ci.company_id
) isq_view on true left join lateral ( 
select isq.dateindex, isq.company_id
from si_isq isq
where isq.dateindex = ci.dateindex 
and isq.company_id = ci.company_id
) bsq_view on true;
-- COST 688,000

with recursive cte(n,m) 
  as (select 0,11, 'a' 
       union all 
      select n+1,m+1, 'b'
        from cte 
       where n < 3) 
select * from cte;

select dateindex from sipro_data_store.si_retdate offset 0 limit 1 
-- GOOD

select dateindex from sipro_data_store.si_retdate offset 1 limit 1 
-- GOOD

select count(*) from sipro_data_store.si_retdate;
-- WORKS

with recursive cte(n,m)  -- just output(whatever) postion column aliases
  as (select 0,11,    ( select dateindex from sipro_data_store.si_retdate offset 0 limit 1  )
       union all 
      select n+1,m+1, ( select dateindex from sipro_data_store.si_retdate offset n+1 limit 1  )
        from cte 
       where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select * from cte order by 3 desc;

 n  | m  | dateindex
----+----+-----------
 67 | 78 |     16952
 66 | 77 |     16920
 65 | 76 |     16891
 64 | 75 |     16860
 63 | 74 |     16829
 62 | 73 |     16800
 ...
  2 | 13 |     14974
  1 | 12 |     14943
  0 | 11 |     14911
(68 rows)



set search_path to sipro_stage,sipro_data_store;
set time zone 'utc';
set constraint_exclusion = on;


explain
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd from cte order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select ci.dateindex, ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    -- dateindex = 15705 and -- 4.3 seconds(for ONE month ( at its fastest ) )
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select psd.prchg_52w from
  ( select dateindex, company_id_unq, prchg_52w::numeric(15,2) from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) psd 
) psds on (true) order by cis.ticker_unq;
-- COST 238,000 -- one index scan aon each ci partition, one index scan on each psd partition
-- RETURNS IN ( no unq ): 60 SECONDS
-- RETURNS IN (    unq ): 134,165 SECONDS
-- STILL DOES THOSE *DAMN* INDEX SCANS

  -- NEED TO SWICH to ticker join for a certain TIME RANGE [ ]


--HOW FAST IS 'JUST ONE' ( OF 68)
explain
select ci2.dateindex dateindex_c, ci2.ticker, psd2.prchg_52w::numeric(15,2)
from ( select ci.dateindex, ci.company_id, ci.ticker_unq ticker from si_ci ci  where  ci.dateindex = 15705 )  ci2 
join ( select ci.dateindex, ci.company_id, ci.ticker_unq ticker from si_ci ci  where  ci.dateindex = 16070 )  ci2_f
on        ci2.company_id = ci2_f.company_id 
join  ( select psd.*  from si_psd psd where  psd.dateindex = 16070 ) psd2   -- MAY NOT? reach ci2 TO DO math?
on      ci2_f.company_id =  psd2.company_id order by 2; 
-- COST 3420 -- * 68 -- 232,560 JUST A LITTLE FASTER: IT DID NOT SCAN THE PARTITION BUT IT LOST
--   MUCH TIME IN *HASH* AND *APPEND*


explain
select ci2.dateindex, ci2.ticker_unq, ci2.company, psd2.prchg_52w
from (      select ci.dateindex, ci.company_id_unq, ci.ticker_unq, ci.company from si_ci ci  where  ci.dateindex = 15705 )  ci2 
left join ( select ci.dateindex, ci.company_id_unq, ci.ticker_unq             from si_ci ci  where  ci.dateindex = 16070 )  ci2_f
on        ci2.company_id_unq = ci2_f.company_id_unq 
left join  ( select psd.company_id_unq, psd.prchg_52w::numeric(15,2)  from si_psd psd where  psd.dateindex = 16070 ) psd2  
on      ci2_f.company_id_unq =  psd2.company_id_unq order by ci2.ticker_unq; 
-- COST 4212 ( with unq )  * 68 = 286416 ( WORST PERFORMANCE )
--   MUCH TIME IN *HASH* AND *APPEND*

---------
---------

-- single month w 52w
explain
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd from cte order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
     ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    dateindex = 15705 and 
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select psd.prchg_52w from
  ( select dateindex, company_id_unq, prchg_52w::numeric(15,2) from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) psd 
) psds on (true) 
order by cis.ticker_unq;




-- single month w 52w and 26w
explain
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd, dateindexf06mlwd from cte order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    dateindex = 15705 and 
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select psd.prchg_52w from
  ( select dateindex, company_id_unq, prchg_52w::numeric(15,2) from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) psd 
) psds on (true) 
left join lateral (
  select psd2.prchg_26w from
  ( select dateindex, company_id_unq, prchg_26w::numeric(15,2) from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) psd2 
) psds2 on (true) -- 10,7,7 RUN SPEED
order by cis.ticker_unq;





-- single month w 52w and 26w and 13w
explain
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf03mlwd from cte order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    dateindex = 15705 and 
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select psd52.prchg_52w from
  ( select dateindex, company_id_unq, prchg_52w::numeric(15,2)  from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) psd52
) psds52 on (true) 
left join lateral (
  select psd26.prchg_26w from
  ( select dateindex, company_id_unq, prchg_26w::numeric(15,2)  from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) psd26
) psds26 on (true) -- 10,7,7 RUN SPEED
left join lateral (
  select psd13.prchg_13w from
  ( select dateindex, company_id_unq, prchg_13w::numeric(15,2)  from si_psd 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) psd13 
) psds13 on (true) --  11,10 RUN SPEED
order by cis.ticker_unq;



-- single month w 52w and 09m and 26w and 13w . . .  annualized
explain
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    dateindex = 15705 and  -- 16952 -- 15705
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select psd52.prchg_52w_ann from
  ( select dateindex, company_id_unq, prchg_52w::numeric(15,2)    prchg_52w_ann from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) psd52
) psds52 on (true) 
left join lateral (
  select psd26.prchg_26w_ann from
  ( select dateindex, company_id_unq, prchg_26w::numeric(15,2) * 2 prchg_26w_ann from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) psd26
) psds26 on (true) -- 10,7,7 RUN SPEED
left join lateral (
  select psd13.prchg_13w_ann from
  ( select dateindex, company_id_unq, prchg_13w::numeric(15,2) * 4 prchg_13w_ann from si_psd 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) psd13 
) psds13 on (true) --  11,10 RUN SPEED
order by cis.ticker_unq;





-- single month w 52w and 09m and 26w and 13w . . .  annualized  
-- and priceb52w
explain analyze
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    dateindex = 15705 and  -- 16952 -- 15705
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select priceb52w, pricef52w, b52w.prchg_52w_ann from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb52w,
    price::numeric(15,2) pricef52w, 
    prchg_52w::numeric(15,2)    prchg_52w_ann 
    from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b52w
) b52ws on (true) 
left join lateral (
  select b26w.prchg_26w_ann from
  ( select dateindex, company_id_unq, prchg_26w::numeric(15,2) * 2 prchg_26w_ann from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b26w
) b26ws on (true) -- 10,7,7 RUN SPEED
left join lateral (
  select b13w.prchg_13w_ann from
  ( select dateindex, company_id_unq, prchg_13w::numeric(15,2) * 4 prchg_13w_ann from si_psd 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b13w 
) b13ws on (true) --  11,10 RUN SPEED
order by cis.ticker_unq;






-- single month w 52w and 09m and 26w and 13w . . .  annualized  
-- and priceb52w

-- ALL OF IT

with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    -- dateindex = 15705 and  -- 16952 -- 15705 ALL OF IT
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select priceb52w, pricef52w, b52w.prchg_52w_ann from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb52w,
    price::numeric(15,2) pricef52w, 
    prchg_52w::numeric(15,2)    prchg_52w_ann 
    from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b52w
) b52ws on (true) 
left join lateral (
  select b26w.prchg_26w_ann from
  ( select dateindex, company_id_unq, prchg_26w::numeric(15,2) * 2 prchg_26w_ann from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b26w
) b26ws on (true) -- 10,7,7 RUN SPEED
left join lateral (
  select b13w.prchg_13w_ann from
  ( select dateindex, company_id_unq, prchg_13w::numeric(15,2) * 4 prchg_13w_ann from si_psd 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b13w 
) b13ws on (true) --  11,10 RUN SPEED
order by cis.ticker_unq;
-- WHOLE THING 2 MINUTES AND 34 SECONDS


-- ALL TIME MONTHS
-- all futures and annualized ( without ticker change time )

explain analyze
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    -- dateindex = 15705 and  -- 16952 -- 15705
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select divaccmf4q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q
    from si_isq 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b4qisq
) b4qsisq on (true) 
left join lateral (
  select priceb52w, pricef52w, prchg_52w, prchg_52w_ann,
  prchg_52w + case when divaccmf4q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf4q / nullif(priceb52w,0.00::numeric(15,2))) * 100 
      end pradiv_chg52w,
  prchg_52w + case when divaccmf4q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf4q / nullif(priceb52w,0.00::numeric(15,2))) * 100 
      end pradiv_chg52w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb52w,
    price::numeric(15,2) pricef52w, 
    prchg_52w::numeric(15,2),
    prchg_52w::numeric(15,2)    prchg_52w_ann 
    from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b52wpsd
) b52wspsd on (true)  -- one div and price combo session is only cost 482000
left join lateral (
  select divaccmf2q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf2q
    from si_isq 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b2qisq
) b2qsisq on (true)  
left join lateral (
  select priceb26w, pricef26w, prchg_26w, prchg_26w_ann,
  prchg_26w + case when divaccmf2q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf2q / nullif(priceb26w,0.00::numeric(15,2))) * 100 
      end     pradiv_chg26w,
  prchg_26w + case when divaccmf2q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf2q / nullif(priceb26w,0.00::numeric(15,2))) * 100 
      end * 2 pradiv_chg26w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_26w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb26w,
    price::numeric(15,2) pricef26w, 
    prchg_26w::numeric(15,2),
    prchg_26w::numeric(15,2) * 2 prchg_26w_ann 
    from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b26wpsd
) b26wspsd on (true)
left join lateral (
  select divaccmf1q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf1q
    from si_isq 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b1qisq
) b1qsisq on (true)  
left join lateral (
  select priceb13w, pricef13w, prchg_13w, prchg_13w_ann,
  prchg_13w + case when divaccmf1q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf1q / nullif(priceb13w,0.00::numeric(15,2))) * 100 
      end     pradiv_chg13w,
  prchg_13w + case when divaccmf1q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf1q / nullif(priceb13w,0.00::numeric(15,2))) * 100 
      end * 4 pradiv_chg13w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_13w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb13w,
    price::numeric(15,2) pricef13w, 
    prchg_13w::numeric(15,2),
    prchg_13w::numeric(15,2) * 2 prchg_13w_ann 
    from si_psd 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b13wpsd
) b13wspsd on (true) 
order by cis.ticker_unq;
-- si_pro_data_store
-- HOW MUCH TIME EST _473MB(taskmgr)___ ( ( below) 41 * 68 = > 41 * 68 / 60 = 46 minutes ( FASTER OR NOT ? )
-- UNKNOWN - BEST DO IT *PERIOD BY PERIOD*


set search_path to sipro_stage,sipro_data_store;
set time zone 'utc';
set constraint_exclusion = on;
set work_mem to '200MB' -- more

-- all futures and annualized ( without ticker change time )

explain analyze
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select n, dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    dateindex = 15705 and  -- 16952 -- 15705
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select divaccmf4q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q
    from si_isq 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b4qisq
) b4qsisq on (true) 
left join lateral (
  select priceb52w, pricef52w, prchg_52w, prchg_52w_ann,
  prchg_52w + case when divaccmf4q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf4q / nullif(priceb52w,0.00::numeric(15,2))) * 100 
      end pradiv_chg52w,
  prchg_52w + case when divaccmf4q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf4q / nullif(priceb52w,0.00::numeric(15,2))) * 100 
      end pradiv_chg52w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb52w,
    price::numeric(15,2) pricef52w, 
    prchg_52w::numeric(15,2),
    prchg_52w::numeric(15,2)    prchg_52w_ann 
    from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b52wpsd
) b52wspsd on (true)  -- one div and price combo session is only cost 482000
left join lateral (
  select divaccmf2q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf2q
    from si_isq 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b2qisq
) b2qsisq on (true)  
left join lateral (
  select priceb26w, pricef26w, prchg_26w, prchg_26w_ann,
  prchg_26w + case when divaccmf2q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf2q / nullif(priceb26w,0.00::numeric(15,2))) * 100 
      end     pradiv_chg26w,
  prchg_26w + case when divaccmf2q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf2q / nullif(priceb26w,0.00::numeric(15,2))) * 100 
      end * 2 pradiv_chg26w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_26w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb26w,
    price::numeric(15,2) pricef26w, 
    prchg_26w::numeric(15,2),
    prchg_26w::numeric(15,2) * 2 prchg_26w_ann 
    from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b26wpsd
) b26wspsd on (true)
left join lateral (
  select divaccmf1q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf1q
    from si_isq 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b1qisq
) b1qsisq on (true)  
left join lateral (
  select priceb13w, pricef13w, prchg_13w, prchg_13w_ann,
  prchg_13w + case when divaccmf1q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf1q / nullif(priceb13w,0.00::numeric(15,2))) * 100 
      end     pradiv_chg13w,
  prchg_13w + case when divaccmf1q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf1q / nullif(priceb13w,0.00::numeric(15,2))) * 100 
      end * 4 pradiv_chg13w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_13w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb13w,
    price::numeric(15,2) pricef13w, 
    prchg_13w::numeric(15,2),
    prchg_13w::numeric(15,2) * 2 prchg_13w_ann 
    from si_psd 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b13wpsd
) b13wspsd on (true) 
order by cis.ticker_unq;
-- sipro_data_store COST 446808   dateindex = 15705
-- THINKS: "Execution time: 40238.996 ms" ( ACTAULT TIME 41.1 )

--  n  | dateindex | dateindexf12mlwd | dateindexf06mlwd | dateindexf09mlwd | dateindexf03mlwd | company_id_unq | ticker_unq |            company             | divaccmf4q |          priceb52w          | pricef52w | prchg_52w | prchg_52w_ann |      pradiv_chg
-- 52w       |    pradiv_chg52w_ann     | divaccmf2q |          priceb26w          | pricef26w | prchg_26w | prchg_26w_ann |      pradiv_chg26w       |    pradiv_chg26w_ann     | divaccmf1q |          priceb13w          | pricef13w | prchg_13w | prchg_13w_an
-- n |       pradiv_chg13w       |     pradiv_chg13w_ann




set search_path to sipro_stage,sipro_data_store;
set time zone 'utc';
set constraint_exclusion = on;
set work_mem to '200MB' -- more

-- all futures and annualized ( WITH ticker change time ) -- IN PROGRESS ( CURR - SAME AS ABOVE )

explain analyze
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select -- n,
dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select * from tq 
left join lateral (
  select --ci.dateindex, 
    ci.company_id_unq, ci.ticker_unq, ci.company from 
  ( select dateindex, company_id_unq, ticker_unq, company from si_ci  
    where 
    dateindex = 15705 and  -- 16952 -- 15705
    dateindex = tq.dateindex  ) ci
) cis on (true)
left join lateral (
  select divaccmf4q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q
    from si_isq 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b4qisq
) b4qsisq on (true) 
left join lateral (
  select priceb52w, pricef52w, prchg_52w_ann,
  prchg_52w + case when divaccmf4q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf4q / nullif(priceb52w,0.00::numeric(15,2))) * 100 
      end pradiv_chg52w,
  prchg_52w + case when divaccmf4q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf4q / nullif(priceb52w,0.00::numeric(15,2))) * 100 
      end pradiv_chg52w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb52w,
    price::numeric(15,2) pricef52w, 
    prchg_52w::numeric(15,2),
    prchg_52w::numeric(15,2)    prchg_52w_ann 
    from si_psd 
    where dateindex = tq.dateindexf12mlwd and company_id_unq = cis.company_id_unq) b52wpsd
) b52wspsd on (true)  -- one div and price combo session is only cost 482000
left join lateral (
  select divaccmf2q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf2q
    from si_isq 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b2qisq
) b2qsisq on (true)  
left join lateral (
  select priceb26w, pricef26w, prchg_26w_ann,
  prchg_26w + case when divaccmf2q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf2q / nullif(priceb26w,0.00::numeric(15,2))) * 100 
      end     pradiv_chg26w,
  prchg_26w + case when divaccmf2q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf2q / nullif(priceb26w,0.00::numeric(15,2))) * 100 
      end * 2 pradiv_chg26w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_26w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb26w,
    price::numeric(15,2) pricef26w, 
    prchg_26w::numeric(15,2),
    prchg_26w::numeric(15,2) * 2 prchg_26w_ann 
    from si_psd 
    where dateindex = tq.dateindexf06mlwd and company_id_unq = cis.company_id_unq) b26wpsd
) b26wspsd on (true)
left join lateral (
  select divaccmf1q from
  ( select dateindex, company_id_unq, 
    coalesce(dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf1q
    from si_isq 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b1qisq
) b1qsisq on (true)  
left join lateral (
  select priceb13w, pricef13w, prchg_13w_ann,
  prchg_13w + case when divaccmf1q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf1q / nullif(priceb13w,0.00::numeric(15,2))) * 100 
      end     pradiv_chg13w,
  prchg_13w + case when divaccmf1q = 0.00::numeric(15,2) then 0.00 
    else (divaccmf1q / nullif(priceb13w,0.00::numeric(15,2))) * 100 
      end * 4 pradiv_chg13w_ann
  from
  ( select dateindex, company_id_unq, 
    price::numeric(15,2)/(nullif(prchg_13w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) priceb13w,
    price::numeric(15,2) pricef13w, 
    prchg_13w::numeric(15,2),
    prchg_13w::numeric(15,2) * 2 prchg_13w_ann 
    from si_psd 
    where dateindex = tq.dateindexf03mlwd and company_id_unq = cis.company_id_unq) b13wpsd
) b13wspsd on (true) 
order by cis.ticker_unq;






-- experiment

with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select    dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select tq.dateindex tq_dateindex, ci.dateindex ci_dateindex, psd.dateindex psd_dateindex, isq.dateindex dateindex
from 
tq, si_ci ci, si_psd psd, si_isq isq
where 
tq.dateindex = 15705 
and tq.dateindex         = ci.dateindex 
and tq.dateindexf12mlwd = psd.dateindex 
and tq.dateindexf12mlwd = isq.dateindex
;
-- 2:45+ ( NOT A CORRECT QUERY )

show search_path; -- set search_path to sipro_stage,sipro_data_store
-- set search_path to sipro_data_store,sipro_stage;

explain
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select    dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select tq.dateindex tq_dateindex, ci.dateindex ci_dateindex, psd.dateindex psd_dateindex, isq.dateindex dateindex
from 
tq, si_ci ci, si_psd psd, si_isq isq
where 
tq.dateindex = 15705 
and tq.dateindex         =  ci.dateindex 
and tq.dateindexf12mlwd  = psd.dateindex 
and ci.company_id_unq    = psd.company_id_unq
and tq.dateindexf12mlwd  = isq.dateindex
and ci.company_id_unq    = isq.company_id_unq
;
-- 2:45 (2:53 exactly)
-- 1:20 ( sipro_data_store ), next query 1.7 seconds  COST 9000


-- select * from sipro_data_store.si_retdate order by dateindex desc; -- DOES THE SAME AS THE RECURS

set time zone 'utc';
set constraint_exclusion = on;
set work_mem to '1200MB' 

explain analyze
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select    dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select tq.dateindex tq_dateindex, ci.dateindex ci_dateindex, psd.dateindex psd_dateindex, isq.dateindex dateindex
from 
tq, si_ci ci, si_psd psd, si_isq isq
where 
-- tq.dateindex = 15705 
-- and 
    tq.dateindex         =  ci.dateindex 
and tq.dateindexf12mlwd  = psd.dateindex 
and ci.company_id_unq    = psd.company_id_unq
and tq.dateindexf12mlwd  = isq.dateindex
and ci.company_id_unq    = isq.company_id_unq
;

-- ( sipro_data_store ), __SECONDS ( MEMORY SHOT UP TO 200MB
-- ESTIMATED: COST 382174
-- "Execution time: 57210.323 ms"
-- ACTUAL RUN _33_SECONDS_ ( 320 THOUSAND ROWS ) 


    -- select * from sipro_data_store.si_retdate order by dateindex desc; 
    -- DOES THE SAME AS THE RECURSIVE  -- SO GET RID OF THE RECURSIVE [X]

   -- NEED FUTURE ticker_ and FUTURE company ( BECUASE NAMES CHANGE ) [PATCHED_SEE_BELOW]

   -- NEED *MORE* COLUMNS ( SEE ABOVE ) [X]
   -- NEED LEFT OUTER JOINS - MISSING DATA IS BEING ELIMINATED [THINK A HAVE]
   
   -- NEED IF IN RANGE JOIN BY TICKER INSTEAD [ ]
   
   --   NOTE: (IF ORACLE/MSSQL/SQLITE - WOULD ALSO NEED COMPONSITE INDEXES 
   --    (dateindex,company_id_unq) (dateindex,ticker_unq)
   --   But PostgreSQL - can do multiple index scans - [I THINK I HAVE] 

show search_path; -- sipro_data_store, sipro_stage

explain analyze
select retdate.dateindex tq_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
si_retdate retdate, si_ci ci, si_psd psd, si_isq isq,  si_ci cif
where 
retdate.dateindex        =  ci.dateindex 
--
and ci.company_id_unq    = psd.company_id_unq
and ci.company_id_unq    = isq.company_id_unq
--
and ci.company_id_unq    = cif.company_id_unq
--
and retdate.dateindexf12mlwd  = cif.dateindex
and retdate.dateindexf12mlwd  = isq.dateindex
and retdate.dateindexf12mlwd  = psd.dateindex 
; -- 398,000 COST and EST 72 seconds -- 2ND EST 26 SECONDS -- ACT RUN: 1ST; 39 SECONDS  2ND; 40 SECS

-- DOES order MATTER? - BELOW PRODUCTED EXACT SAME PLAN - NOT MATTER 320,000 RECORDS RETURND

explain analyze
select retdate.dateindex tq_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
si_retdate retdate, si_ci ci, si_psd psd, si_isq isq,  si_ci cif
where 
retdate.dateindex        =  ci.dateindex 
--
and retdate.dateindexf12mlwd  = cif.dateindex
and retdate.dateindexf12mlwd  = isq.dateindex
and retdate.dateindexf12mlwd  = psd.dateindex 
--
and ci.company_id_unq    = psd.company_id_unq
and ci.company_id_unq    = isq.company_id_unq
--
and ci.company_id_unq    = cif.company_id_unq
; -- 398,000 COST and EST 25 seconds, 30 SECONDS


-- NEED LEFT JOINS

explain analyze
select retdate.dateindex tq_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
si_retdate retdate, si_ci ci, si_psd psd, si_isq isq,  si_ci cif
where 
    retdate.dateindex        =  ci.dateindex 
--
and ci.company_id_unq    = psd.company_id_unq
and ci.company_id_unq    = isq.company_id_unq
--
and ci.company_id_unq    = cif.company_id_unq
--
and retdate.dateindexf12mlwd  = cif.dateindex
and retdate.dateindexf12mlwd  = isq.dateindex
and retdate.dateindexf12mlwd  = psd.dateindex 

-- seems equivalent
     si_retdate retdate
join si_ci ci 
  on retdate.dateindex         = ci.dateindex 
join si_psd psd
  on ci.company_id_unq         = psd.company_id_unq
 and retdate.dateindexf12mlwd  = psd.dateindex 
join si_isq isq
  on ci.company_id_unq         = isq.company_id_unq
 and retdate.dateindexf12mlwd  = isq.dateindex
join si_ci cif
  on ci.company_id_unq         = cif.company_id_unq
 and retdate.dateindexf12mlwd  = cif.dateindex

-- written in BELOW

explain analyze
select retdate.dateindex tq_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
     si_retdate retdate
join si_ci ci 
  on retdate.dateindex         = ci.dateindex 
join si_psd psd
  on ci.company_id_unq         = psd.company_id_unq
 and retdate.dateindexf12mlwd  = psd.dateindex 
join si_isq isq
  on ci.company_id_unq         = isq.company_id_unq
 and retdate.dateindexf12mlwd  = isq.dateindex
join si_ci cif
  on ci.company_id_unq         = cif.company_id_unq
 and retdate.dateindexf12mlwd  = cif.dateindex;
-- COST 398,000 EST: 44 SECONDS -- ACTUAL RUN: 41 SECONDS 320,000 ROWS

-- WHAT IS THIS ( as long as my date is sparse )

-- LEFT_OFF: THIS SEEMS TO WORK: RESTRICT LATER?
explain analyze
select retdate.dateindex retdate_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
                si_retdate retdate
full outer join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 
full outer join si_psd psd
              on ci.company_id_unq         = psd.company_id_unq
             and retdate.dateindexf12mlwd  = psd.dateindex 
full outer join si_isq isq
              on ci.company_id_unq         = isq.company_id_unq
             and retdate.dateindexf12mlwd  = isq.dateindex
full outer join si_ci cif
              on ci.company_id_unq         = cif.company_id_unq
             and retdate.dateindexf12mlwd  = cif.dateindex;
-- COST _521,000_  EST _65 SECONDS_ ACTUAL 104 SECONDS AND 1.3 MILLION ROWS
-- LATER RESTRICT WHERE TQ_DATEINDEX IS NOT NULL THEN CI_DATEINDEX IS NOT NULL
-- INCLUDES NEW COMPANIES THAT DID NOT EXIST 12 MONTHS AGO


explain analyze
select retdate.dateindex retdate_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 
full outer join si_psd psd
              on ci.company_id_unq         = psd.company_id_unq
             and retdate.dateindexf12mlwd  = psd.dateindex 
full outer join si_isq isq
              on ci.company_id_unq         = isq.company_id_unq
             and retdate.dateindexf12mlwd  = isq.dateindex
full outer join si_ci cif
              on ci.company_id_unq         = cif.company_id_unq
             and retdate.dateindexf12mlwd  = cif.dateindex;
-- 105 SECONDS AND 1.3 MILLION ROWS

explain analyze
select retdate.dateindex retdate_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 
left outer join si_ci cif
              on ci.company_id_unq         = cif.company_id_unq
             and retdate.dateindexf12mlwd  = cif.dateindex
left outer join si_psd psd
              on ci.company_id_unq         = psd.company_id_unq
             and retdate.dateindexf12mlwd  = psd.dateindex 
full outer join si_isq isq
              on ci.company_id_unq         = isq.company_id_unq
             and retdate.dateindexf12mlwd  = isq.dateindex
;
-- 60 SECONDS AND 809,000 RECORDS



explain analyze
select retdate.dateindex retdate_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 
left outer join si_ci cif
              on ci.company_id_unq         = cif.company_id_unq
             and retdate.dateindexf12mlwd  = cif.dateindex
left outer join si_psd psd
              on ci.company_id_unq         = psd.company_id_unq
             and retdate.dateindexf12mlwd  = psd.dateindex 
left outer join si_isq isq
              on ci.company_id_unq         = isq.company_id_unq
             and retdate.dateindexf12mlwd  = isq.dateindex
;
-- 50 SECONDS AND 564,681 RECORDS


explain analyze
select retdate.dateindex retdate_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 
left outer join si_ci cif
              on ci.company_id_unq         = cif.company_id_unq
             and retdate.dateindexf12mlwd  = cif.dateindex
left outer join si_isq isq
              on ci.company_id_unq         = isq.company_id_unq
             and retdate.dateindexf12mlwd  = isq.dateindex
left outer join si_psd psd
              on ci.company_id_unq         = psd.company_id_unq
             and retdate.dateindexf12mlwd  = psd.dateindex 
;
-- 50 SECONDS AND 564,681 RECORDS


-- BEGIN VERY KEEP -- **** LEFT_OFF(SEE OTHTER(LATERALS) L E F T O F F BELOW ) ***
explain analyze
select retdate.dateindex retdate_dateindex, 
 ci.dateindex ci_dateindex,
cif.dateindex cif_dateindex, 
fut.dateindex fut_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 
left outer join si_ci cif
              on ci.company_id_unq         = cif.company_id_unq
             and retdate.dateindexf12mlwd  = cif.dateindex
left outer join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq
;
-- 47 SECONDS AND 564,681 RECORDS ( CORRECT ANSWER )  **** LEFT_OFF *** 
-- END VERY KEEP --


-- NEED 26W AND 13W

explain analyze
select retdate.dateindex retdate_dateindex, 
 ci.dateindex ci_dateindex,
-- cif.dateindex cif_dateindex, 
-- fut.dateindex fut_dateindex
   w52.dateindex w52_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 

       left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)
;
-- 115 SECONDS 564681 RECORDS



explain analyze
select retdate.dateindex retdate_dateindex, 
 ci.dateindex ci_dateindex,
-- cif.dateindex cif_dateindex, 
-- fut.dateindex fut_dateindex
   w52.dateindex w52_dateindex,
   w26.dateindex w26_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf06mlwd  = cif.dateindex

         ) w26 on (true)
         
;
-- EXPECTED TIME 166 SECONDS ( ACTUAL 6 MINUTE AND 14 SECONDS - 374 SECONDS )
--                           (                                  241 SECONDS - 2ND TIME )


set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set constraint_exclusion = on;
set work_mem to '1200MB';


-- VERY VERY KEEP --  LEFT_OFF
explain analyze
select retdate.dateindex retdate_dateindex, 
 ci.dateindex ci_dateindex,
-- cif.dateindex cif_dateindex, 
-- fut.dateindex fut_dateindex
   w52.dateindex w52_dateindex,
   w26.dateindex w26_dateindex,
   w13.dateindex w13_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf06mlwd  = cif.dateindex

         ) w26 on (true)

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf03mlwd  = cif.dateindex

         ) w13 on (true)
         
;
-- TYPICAL RUN: 98 SECONDS

show search_path;

-- same as above ( but just ONE month ) -- KEEP 

explain analyze
select retdate.dateindex retdate_dateindex, 
 ci.dateindex ci_dateindex,
-- cif.dateindex cif_dateindex, 
-- fut.dateindex fut_dateindex
   w52.dateindex w52_dateindex,
   w26.dateindex w26_dateindex,
   w13.dateindex w13_dateindex
from 
                ( select * from si_retdate where dateindex = 15705 ) retdate

           join ( select * from si_ci where dateindex = 15705 ) ci 
              on retdate.dateindex         =   ci.dateindex 

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)

      left join lateral ( select cif.dateindex from
              
                 si_ci cif

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf06mlwd  = cif.dateindex

         ) w26 on (true)

      left join lateral ( select cif.dateindex from
              
                 si_ci cif

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf03mlwd  = cif.dateindex

         ) w13 on (true)
         
;


-- NOT A 'KEEP' TRY TO REDUCE FURTHER

explain analyze
select retdate.dateindex retdate_dateindex, 
 ci.dateindex ci_dateindex,
-- cif.dateindex cif_dateindex, 
-- fut.dateindex fut_dateindex
   w52.dateindex w52_dateindex--,
   --w26.dateindex w26_dateindex,
   --w13.dateindex w13_dateindex
from 
                ( select * from si_retdate where dateindex = 15705 ) retdate

           join ( select * from si_ci where dateindex = 15705 ) ci 
              on retdate.dateindex         =   ci.dateindex 

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join  (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex
         

         ) w52 on (true)
         
;


explain analyze
select retdate.dateindex retdate_dateindex, 
 ci.dateindex ci_dateindex,
-- cif.dateindex cif_dateindex, 
-- fut.dateindex fut_dateindex
   w52.dateindex w52_dateindex --,
   --w26.dateindex w26_dateindex,
   --w13.dateindex w13_dateindex
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex         =   ci.dateindex 

      left join lateral ( select cif.dateindex from
              
                 si_ci cif 

      left join (
         select psd.dateindex, psd.company_id_unq 
         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut on cif.dateindex = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)
;


-- NOTE: MAY FURTHER BE OPTIMIZED BY USING RANGES  : dateindex within 366 fuure days
-- I PRABLAY WILL NOT DO

-- EXPECED EXECUTION TIME: _65 SECONDS - 9875 ROWS
-- ACTUAL 

-- END OF VERY VERY KEEP --








explain
select 1 from
si_ci cif left      outer join si_psd psd on cif.dateindex  = psd.dateindex  and cif.company_id = psd.company_id 
               full outer join si_isq isq on psd.company_id = isq.company_id and psd.dateindex  = isq.dateindex
and cif.dateindex = 16070;
-- NOT RIGHT 65 SECONDS 1.1 MILLION ROWS ARE RETURNED

select 1 from
si_ci cif left outer join ( si_psd psd full outer join si_isq isq on psd.company_id = isq.company_id and psd.dateindex = isq.dateindex ) future on cif.dateindex  = future.dateindex  and cif.company_id = future.company_id 
and cif.dateindex = 16070;
-- = future.dat..
--   ^
-- ERROR: column reference "dateindex" is ambiguous


select 1 from
si_ci cif left outer join ( select isq.dateindex, isq.company_id from 
                             si_psd psd full outer join si_isq isq 
                             on psd.company_id = isq.company_id and psd.dateindex = isq.dateindex ) 
                            future 
                             on cif.dateindex  = future.dateindex  and cif.company_id = future.company_id 
and cif.dateindex = 16070;
--  24 SECONDS AND  564 ROWS ARE RETURNED ( NOT RIGHT )

select 1 from
si_ci cif left outer join ( select isq.dateindex, isq.company_id from 
                             si_psd psd join si_isq isq 
                             on psd.company_id = isq.company_id and psd.dateindex = isq.dateindex ) 
                            future 
                             on cif.dateindex  = future.dateindex  and cif.company_id = future.company_id 
and cif.dateindex = 16070;
--  24 SECONDS AND  564 ROWS ARE RETURNED ( NOT RIGHT )


-- better? LEFT_OFF

explain analyze
select retdate.dateindex tq_dateindex, 
ci.dateindex ci_dateindex, 
  psd.dateindex psd_dateindex, 
  isq.dateindex isq_dateindex,
  cif.dateindex cif_dateindex
from 
si_retdate retdate
left outer join 
  ( si_ci ci left outer join 
    ( si_ci cif full outer join si_psd psd full outer join si_isq isq on ...  ) future on retdate.dateindexf12mlwd = future.dateindex and ci.company_id = future.company_id ) on retdate.dateindex = ci.dateindex




              on retdate.dateindex         = ci.dateindex 
left outer join (


)
si_psd psd full outer join 
              on ci.company_id_unq         = psd.company_id_unq
             and retdate.dateindexf12mlwd  = psd.dateindex 
full outer join si_isq isq
              on ci.company_id_unq         = isq.company_id_unq
             and retdate.dateindexf12mlwd  = isq.dateindex
full outer join si_ci cif
              on ci.company_id_unq         = cif.company_id_unq
             and retdate.dateindexf12mlwd  = cif.dateindex;






with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select    dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select tq.dateindex tq_dateindex, ci.dateindex ci_dateindex, psd.dateindex psd_dateindex, isq.dateindex dateindex
from 
tq, si_ci ci, si_psd psd, si_isq isq
where 
tq.dateindex = 15705 
and ci.dateindex = 15705
and tq.dateindex         =  ci.dateindex 
and tq.dateindexf12mlwd  = psd.dateindex 
and ci.company_id_unq    = psd.company_id_unq
and tq.dateindexf12mlwd  = isq.dateindex
and ci.company_id_unq    = isq.company_id_unq
;
-- 47 seconds



-- create table as  "si_returns52w_" dateindex 
-- 
-- create index  si_returns52w_ ticker_unq dateindex company_id_unq
-- 
-- create index 
-- 
-- case when (14819+1) < dateindex and dateindex < (15185-1) then      ticker_unq  else     company_id_unq end = 
-- case when (14819+1) < dateindex and dateindex < (15185-1) then  cis.ticker_unq  else cis.company_id_unq end 
-- 
-- si_returns52w_ticker
-- 
-- 2:30 is slow, is equijoins faster??
--  
-- select null from
-- 


with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select    dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select tq.dateindex tq_dateindex, ci.dateindex ci_dateindex, psd.dateindex psd_dateindex, isq.dateindex dateindex
from 
tq, si_ci ci, si_psd psd, si_isq isq
where 
    tq.dateindex = 15705 
and ci.dateindex = 15705
and tq.dateindexf12mlwd  = 16070
and psd.dateindex        = 16070
and isq.dateindex        = 16070
and tq.dateindex         =  ci.dateindex 
and tq.dateindexf12mlwd  = psd.dateindex 
and tq.dateindexf12mlwd  = isq.dateindex
and ci.company_id_unq    = psd.company_id_unq
and ci.company_id_unq    = isq.company_id_unq
;
-- 47,3 seconds



drop function foo();
create function foo()
returns table(f1 int, f2 int, f3 int, f4 int) as
$$
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select    dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select tq.dateindex tq_dateindex, ci.dateindex ci_dateindex, psd.dateindex psd_dateindex, isq.dateindex dateindex
from 
tq, si_ci ci, si_psd psd, si_isq isq
where 
    tq.dateindex = 15705 
and ci.dateindex = 15705
and tq.dateindexf12mlwd  = 16070
and psd.dateindex        = 16070
and isq.dateindex        = 16070
and tq.dateindex         =  ci.dateindex 
and tq.dateindexf12mlwd  = psd.dateindex 
and tq.dateindexf12mlwd  = isq.dateindex
and ci.company_id_unq    = psd.company_id_unq
and ci.company_id_unq    = isq.company_id_unq
$$ language sql;

-- not unwrap is 1.7 seconds
select * from foo();


-- unwrap is 5 seconds
select (foo()).* from ( select 1) sq;





drop function foo(cdateindex int,fdateindex int);
create function foo(cdateindex int,fdateindex int)
returns table(f1 int, f2 int, f3 int, f4 int) as
$$
with tq as (
select * from (
with recursive cte(n)  -- just output(whatever) postion column aliases
  as (select 0,    
        ( select dateindex        from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset 0 limit 1  ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset 0 limit 1  )
  union all 
      select n + (select 1),  
        ( select dateindex        from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf12mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf09mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf06mlwd from sipro_data_store.si_retdate offset n+1 limit 1 ),
        ( select dateindexf03mlwd from sipro_data_store.si_retdate offset n+1 limit 1 )
        from cte 
  where n < ( select count(*) from sipro_data_store.si_retdate ) - 1 ) 
select    dateindex, dateindexf12mlwd, dateindexf06mlwd, dateindexf09mlwd, dateindexf03mlwd from cte 
order by 2 desc  -- WORKS
) sq )
select tq.dateindex tq_dateindex, ci.dateindex ci_dateindex, psd.dateindex psd_dateindex, isq.dateindex dateindex
from 
tq, si_ci ci, si_psd psd, si_isq isq
where 
    tq.dateindex = $1 
and ci.dateindex = $1
and tq.dateindexf12mlwd  = $2
and psd.dateindex        = $2
and isq.dateindex        = $2
and tq.dateindex         =  ci.dateindex 
and tq.dateindexf12mlwd  = psd.dateindex 
and tq.dateindexf12mlwd  = isq.dateindex
and ci.company_id_unq    = psd.company_id_unq
and ci.company_id_unq    = isq.company_id_unq
$$ language sql stable;


select * from foo(15705,16070);
-- stable remembers
+
--somthing unknown -- WORKS FAST
select * from foo(15736,16101);

select * from foo(15736::int,16101::int);

select foo(a,b) from ( select 15705::int a, 16070::int b) sq;
-- 60 SECONDS RUN 1, 100 SECONDS ALL NEXT RUNS

-- unwrap is 5 seconds
select (foo(15736,16101)).* from ( select 1) sq; -- hang forever


select (foo(a,b)).* from ( select 15705::int a, 16070::int b) sq;  -- ::seems to make difference 
-- forever

select (foo(a,b)).* from ( select 15705 a, 16070 b) sq;
-- forever


select * from foo(15705,16070);

sipro_stage.si_ci

ALTER TABLE member CLUSTER ON member_name_idx;

CLUSTER member

show search_path;

create table sipro_data_store.si_ci ( like sipro_stage.si_ci including all );
-- 1.5 seconds
select count(*) from  sipro_data_store.si_ci
-- ZERO DOES NOT DO INHERITANCE




--  begin testing of updates ( below ) --

select ci.dateindex, ci.company_id_unq, ci_f.company_id_unq, ci_f.ticker_unq
from sipro_data_store.si_ci ci_f, sipro_data_store.si_ci ci
where ci.ticker_unq  = ci_f.ticker_unq
and   ci.dateindex   in (
                         14911,
                         14943,
                         14974,
                         15005,
                         15033,
                         15064,
                         15093,
                         15125,
                         15155
                        )
and   ci_f.dateindex = 15184  and ci_f.ticker_unq = 'AAPL';

----

select psd.dateindex, psd.company_id_unq, psd_f.company_id_unq, psd_f.ticker_unq
from sipro_data_store.si_psd psd_f, sipro_data_store.si_psd psd
where psd.ticker_unq  = psd_f.ticker_unq
and   psd.dateindex   in (
                         14911,
                         14943,
                         14974,
                         15005,
                         15033,
                         15064,
                         15093,
                         15125,
                         15155
                        )
and   psd_f.dateindex = 15184  and psd_f.ticker_unq = 'AAPL';

----

select isq.dateindex, isq.company_id_unq, isq_f.company_id_unq, isq_f.ticker_unq
from sipro_data_store.si_isq isq_f, sipro_data_store.si_isq isq
where isq.ticker_unq  = isq_f.ticker_unq
and   isq.dateindex   in (
                         14911,
                         14943,
                         14974,
                         15005,
                         15033,
                         15064,
                         15093,
                         15125,
                         15155
                        )
and   isq_f.dateindex = 15184  and isq_f.ticker_unq = 'AAPL';


--  end testing of updates ( below ) --



-------------------------
-------------------------

-----------
-----------


create table sipro_data_store.si_ci as select * from sipro_stage.si_ci;
-- 90 seconds


-- index: sipro_data_store.si_ci_adr_idx

-- drop index sipro_data_store.si_ci_adr_idx;

create index si_ci_adr_idx
  on sipro_data_store.si_ci
  using btree
  (adr);

-- index: sipro_data_store.si_ci_company_id_idx

-- drop index sipro_data_store.si_ci_company_id_idx;

create index si_ci_company_id_idx
  on sipro_data_store.si_ci
  using btree
  (company_id collate pg_catalog.default);

-- index: sipro_data_store.si_ci_company_id_unq_idx

-- drop index sipro_data_store.si_ci_company_id_unq_idx;

create index si_ci_company_id_unq_idx
  on sipro_data_store.si_ci
  using btree
  (company_id_unq collate pg_catalog.default);

-- index: sipro_data_store.si_ci_country_idx

-- drop index sipro_data_store.si_ci_country_idx;

create index si_ci_country_idx
  on sipro_data_store.si_ci
  using btree
  (country collate pg_catalog.default);

-- index: sipro_data_store.si_ci_dateindex_idx

-- drop index sipro_data_store.si_ci_dateindex_idx;

create index si_ci_dateindex_idx
  on sipro_data_store.si_ci
  using btree
  (dateindex);

-- index: sipro_data_store.si_ci_dateindexeom_idx

-- drop index sipro_data_store.si_ci_dateindexeom_idx;

create index si_ci_dateindexeom_idx
  on sipro_data_store.si_ci
  using btree
  (dateindexeom);

-- index: sipro_data_store.si_ci_exchange_idx

-- drop index sipro_data_store.si_ci_exchange_idx;

create index si_ci_exchange_idx
  on sipro_data_store.si_ci
  using btree
  (exchange collate pg_catalog.default);

-- index: sipro_data_store.si_ci_ind_2_dig_idx

-- drop index sipro_data_store.si_ci_ind_2_dig_idx;

create index si_ci_ind_2_dig_idx
  on sipro_data_store.si_ci
  using btree
  (ind_2_dig collate pg_catalog.default);

-- index: sipro_data_store.si_ci_ind_3_dig_idx

-- drop index sipro_data_store.si_ci_ind_3_dig_idx;

create index si_ci_ind_3_dig_idx
  on sipro_data_store.si_ci
  using btree
  (ind_3_dig collate pg_catalog.default);

-- index: sipro_data_store.si_ci_ticker_unq_idx

-- drop index sipro_data_store.si_ci_ticker_unq_idx;

create index si_ci_ticker_unq_idx
  on sipro_data_store.si_ci
  using btree
  (ticker_unq collate pg_catalog.default);

-- 52.3 seconds
-- ____

----------------
----------------

create table sipro_data_store.si_psd as select * from sipro_stage.si_psd;
-- 88 seconds


-- index: sipro_data_store.si_psd_company_id_idx

-- drop index sipro_data_store.si_psd_company_id_idx;

create index si_psd_company_id_idx
  on sipro_data_store.si_psd
  using btree
  (company_id collate pg_catalog.default);

-- index: sipro_data_store.si_psd_company_id_unq_idx

-- drop index sipro_data_store.si_psd_company_id_unq_idx;

create index si_psd_company_id_unq_idx
  on sipro_data_store.si_psd
  using btree
  (company_id_unq collate pg_catalog.default);

-- index: sipro_data_store.si_psd_dateindex_idx

-- drop index sipro_data_store.si_psd_dateindex_idx;

create index si_psd_dateindex_idx
  on sipro_data_store.si_psd
  using btree
  (dateindex);

-- index: sipro_data_store.si_psd_dateindexeom_idx

-- drop index sipro_data_store.si_psd_dateindexeom_idx;

create index si_psd_dateindexeom_idx
  on sipro_data_store.si_psd
  using btree
  (dateindexeom);

-- index: sipro_data_store.si_psd_mktcap_idx

-- drop index sipro_data_store.si_psd_mktcap_idx;

create index si_psd_mktcap_idx
  on sipro_data_store.si_psd
  using btree
  (mktcap collate pg_catalog.default);

-- index: sipro_data_store.si_psd_ticker_unq_idx

-- drop index sipro_data_store.si_psd_ticker_unq_idx;

create index si_psd_ticker_unq_idx
  on sipro_data_store.si_psd
  using btree
  (ticker_unq collate pg_catalog.default);

-- 37 seconds

----------------------
----------------------

create table sipro_data_store.si_isq as select * from sipro_stage.si_isq;
-- 145 seconds


-- index: sipro_data_store.si_isq_company_id_idx

-- drop index sipro_data_store.si_isq_company_id_idx;

create index si_isq_company_id_idx
  on sipro_data_store.si_isq
  using btree
  (company_id collate pg_catalog.default);

-- index: sipro_data_store.si_isq_company_id_unq_idx

-- drop index sipro_data_store.si_isq_company_id_unq_idx;

create index si_isq_company_id_unq_idx
  on sipro_data_store.si_isq
  using btree
  (company_id_unq collate pg_catalog.default);

-- index: sipro_data_store.si_isq_dateindex_idx

-- drop index sipro_data_store.si_isq_dateindex_idx;

create index si_isq_dateindex_idx
  on sipro_data_store.si_isq
  using btree
  (dateindex);

-- index: sipro_data_store.si_isq_dateindexeom_idx

-- drop index sipro_data_store.si_isq_dateindexeom_idx;

create index si_isq_dateindexeom_idx
  on sipro_data_store.si_isq
  using btree
  (dateindexeom);

-- index: sipro_data_store.si_isq_ticker_unq_idx

-- drop index sipro_data_store.si_isq_ticker_unq_idx;

create index si_isq_ticker_unq_idx
  on sipro_data_store.si_isq
  using btree
  (ticker_unq collate pg_catalog.default);

-- _38_ seconds


----------
----------


------------------------------------------
---- BEGIN company_id_unq patch  ---------
----

alter table sipro_data_store.si_ci add column company_id_unq_orig text;
-- instantaneously

update sipro_data_store.si_ci set company_id_unq_orig = company_id_unq;
-- 5 minutes

update sipro_data_store.si_ci ci 
set company_id_unq = ci_f.company_id_unq
from sipro_data_store.si_ci ci_f
where ci.ticker_unq = ci_f.ticker_unq
and   ci.dateindex   in (
                         14911,
                         14943,
                         14974,
                         15005,
                         15033,
                         15064,
                         15093,
                         15125,
                         15155
                        )
and ci_f.dateindex = 15184;
-- 21 seconds


---- GOOD one to COPY psd DOES NOT SHOW UP IN WORDS

alter table sipro_data_store.si_psd add column company_id_unq_orig text;

update sipro_data_store.si_psd set company_id_unq_orig = company_id_unq;

update sipro_data_store.si_psd psd 
set company_id_unq = psd_f.company_id_unq
from sipro_data_store.si_psd psd_f
where psd.ticker_unq = psd_f.ticker_unq
and   psd.dateindex   in (
                         14911,
                         14943,
                         14974,
                         15005,
                         15033,
                         15064,
                         15093,
                         15125,
                         15155
                        )
and psd_f.dateindex = 15184;

----

alter table sipro_data_store.si_isq add column company_id_unq_orig text;

update sipro_data_store.si_isq set company_id_unq_orig = company_id_unq;

update sipro_data_store.si_isq isq 
set company_id_unq = isq_f.company_id_unq
from sipro_data_store.si_isq isq_f
where isq.ticker_unq = isq_f.ticker_unq
and   isq.dateindex   in (
                         14911,
                         14943,
                         14974,
                         15005,
                         15033,
                         15064,
                         15093,
                         15125,
                         15155
                        )
and isq_f.dateindex = 15184;


vacuum analyze sipro_data_store.si_ci(company_id_unq); -- 1 second

vacuum analyze sipro_data_store.si_psd(company_id_unq);

vacuum analyze sipro_data_store.si_isq(company_id_unq);

-- NOTE
-- finance_econ=#
-- finance_econ=# set search_path to sipro_data_store,sipro_stage;
-- SET
-- finance_econ=# set time zone 'utc';
-- SET
-- finance_econ=# set work_mem to '1200MB';
-- SET
-- finance_econ=# set constraint_exclusion = on;
-- SET
-- finance_econ=#
-- finance_econ=# show search_path;
--           search_path
-- -------------------------------
--  sipro_data_store, sipro_stage
-- (1 row)
-- 
-- 
-- finance_econ=# select distinct length(now_company_id_unq) from sipro_data_store.si_returns;
--  length
-- --------
-- 
--       5
--       8
-- (3 rows)
-- 
-- 
-- finance_econ=# select count(1) from sipro_data_store.si_returns where length(now_company_id_unq) = 8;
--  count
-- -------
--   6031 # ( SLIGHTLY GREATER THAN 1% )
-- (1 row)
-- 
-- 
-- finance_econ=# select count(1) from sipro_data_store.si_returns where length(now_company_id_unq) = 5;
--  count
-- --------
--  558642
-- (1 row)
-- 
-- 
-- finance_econ=# select count(1) from sipro_data_store.si_returns where length(now_company_id_unq) is null;
--  count
-- -------
--      8
-- (1 row)
-- 
-- 
-- finance_econ=# select now_company from sipro_data_store.si_returns where length(now_company_id_unq) is null;
--      now_company
-- ----------------------
--  InterXion Holding NV
--  InterXion Holding NV
--  InterXion Holding NV
--  Tornier N.V.
--  InterXion Holding NV
--  InterXion Holding NV
--  InterXion Holding NV
--  InterXion Holding NV
-- (8 rows)
-- 
-- 
-- finance_econ=#



----
----  END company_id_unq patch  ---------
-----------------------------------------

------------------------------------
------------------------------------


-- ON sipro_data_store.si_ci
--
-- NEED: partial index company not like '%iShares%';            [X]
-- NEED: partial index psd.mktcap::numeric(15,2) >=   25        [X]
-- NEED: partial index psd.mktcap::numeric(15,2) >=  200        [X]
-- NEED: partial index psd.mktcap::numeric(15,2) >= 1600        [X]

-- NEED: partial index psd.mktcap::numeric(15,2)  < 1600 and psd.mktcap::numeric(15,2) >=  200 [X]

-- NEED: partial index psd.mktcap::numeric(15,2)  <  200 and psd.mktcap::numeric(15,2) >=   25 [X]


create index si_ci_company_not_lk_ishares_idx ON sipro_data_store.si_ci(company) WHERE company not like '%iShares%';
--DONE

create index si_psd_mktcap_gr_25_idx on sipro_data_store.si_psd(mktcap) where mktcap::numeric(15,2) >= 25;
--DONE

create index si_psd_mktcap_gr_200_idx on sipro_data_store.si_psd(mktcap) where mktcap::numeric(15,2) >= 200;
--DONE

create index si_psd_mktcap_gr_1600_idx on sipro_data_store.si_psd(mktcap) where mktcap::numeric(15,2) >= 1600;
--DONE

create index si_psd_mktcap_gr_200_lt_1600_idx on sipro_data_store.si_psd(mktcap) where 
  mktcap::numeric(15,2) >= 200 and mktcap::numeric(15,2) < 1600;
--DONE

create index si_psd_mktcap_gr_25_lt_200_idx on sipro_data_store.si_psd(mktcap) where 
  mktcap::numeric(15,2) >= 25 and mktcap::numeric(15,2) < 200;
--DONE


---- TEMP


  alter table sipro_data_store.si_psd add column company_id_unq_orig text;
--DONE

  update sipro_data_store.si_psd set company_id_unq_orig = company_id_unq;
--DONE
-- 23 MINUTES ( TOO TIME CONSUMING - NOT WORTH IT )

  update sipro_data_store.si_psd psd 
  set company_id_unq = psd_f.company_id_unq
  from sipro_data_store.si_psd psd_f
  where psd.ticker_unq = psd_f.ticker_unq
  and   psd.dateindex   in (
                           14911,
                           14943,
                           14974,
                           15005,
                           15033,
                           15064,
                           15093,
                           15125,
                           15155
                          )
  and psd_f.dateindex = 15184;
-- 1 MINUTE

vacuum analyze sipro_data_store.si_psd(company_id_unq);
-- 1 MINUTE
----


-- THIS WORKS: COULD 'KEEP AND STORE' [ ] 
-------------------------------------------------
----------- BEGIN fincon CREATE -----------------


create table sipro_data_store.si_finecon1
as
select  ci.dateindex           dateindex_ci,
        ci.company_id_unq company_id_unq_ci,
        ci.exchange,
        ci.ind_2_dig,
        ci.ind_3_dig,
        psd.dateindex           dateindex_psd,
        psd.company_id_unq company_id_unq_psd      
from 
sipro_data_store.si_ci  ci  full outer join 
sipro_data_store.si_psd psd 
on 
ci.dateindex      = psd.dateindex       and 
ci.company_id_unq = psd.company_id_unq;
-- 35 SECONDS ( USED 4 INDEXES )



create table sipro_data_store.si_finecon2
as
select  finecon1.*,
        isq.dateindex           dateindex_isq,
        isq.company_id_unq company_id_unq_isq      
from 
sipro_data_store.si_finecon1 finecon1 full outer join 
sipro_data_store.si_isq isq 
on 
finecon1.dateindex_ci      = isq.dateindex      and 
finecon1.company_id_unq_ci = isq.company_id_unq;
-- 57 SECONDS ( USED 2 INDEXES - ONLY ISQ HAS INDEXES )


drop table sipro_data_store.si_finecon1;
--

create table sipro_data_store.si_finecon3
as
select  finecon2.*,
        bsq.dateindex           dateindex_bsq,
        bsq.company_id_unq company_id_unq_bsq      
from 
sipro_data_store.si_finecon2 finecon2 full outer join 
sipro_data_store.si_bsq bsq 
on 
finecon2.dateindex_ci      = bsq.dateindex      and 
finecon2.company_id_unq_ci = bsq.company_id_unq;
-- 106 SECONDS

drop table sipro_data_store.si_finecon2;
--

create table sipro_data_store.si_finecon4
as
select  finecon3.*,
        cfq.dateindex           dateindex_cfq,
        cfq.company_id_unq company_id_unq_cfq      
from 
sipro_data_store.si_finecon3 finecon3 full outer join 
sipro_data_store.si_cfq cfq 
on 
finecon3.dateindex_ci      = cfq.dateindex       and 
finecon3.company_id_unq_ci = cfq.company_id_unq;


drop table sipro_data_store.si_finecon3;
--


create table sipro_data_store.si_finecon5
as
select  finecon4.*,
        rat.dateindex           dateindex_rat,
        rat.company_id_unq company_id_unq_rat      
from 
sipro_data_store.si_finecon4 finecon4 full outer join 
sipro_data_store.si_rat rat 
on 
finecon4.dateindex_ci      = rat.dateindex       and 
finecon4.company_id_unq_ci = rat.company_id_unq;


drop table sipro_data_store.si_finecon4;
--


create table sipro_data_store.si_finecon6
as
select  finecon5.*,
        mlt.dateindex           dateindex_mlt,
        mlt.company_id_unq company_id_unq_mlt      
from 
sipro_data_store.si_finecon5 finecon5 full outer join 
sipro_data_store.si_mlt mlt 
on 
finecon5.dateindex_ci      = mlt.dateindex       and 
finecon5.company_id_unq_ci = mlt.company_id_unq;


drop table sipro_data_store.si_finecon5;
--


create table sipro_data_store.si_finecon7
as
select  finecon6.*,
        date.dateindex           dateindex_date,
        date.company_id_unq company_id_unq_date      
from 
sipro_data_store.si_finecon6 finecon6 full outer join 
sipro_data_store.si_date date 
on 
finecon6.dateindex_ci      = date.dateindex       and 
finecon6.company_id_unq_ci = date.company_id_unq;


drop table sipro_data_store.si_finecon6;
--

create table sipro_data_store.si_finecon8
as
select  finecon7.*,
        psdc.dateindex           dateindex_psdc,
        psdc.company_id_unq company_id_unq_psdc      
from 
sipro_data_store.si_finecon7 finecon7 full outer join 
sipro_data_store.si_psdc psdc 
on 
finecon7.dateindex_ci      = psdc.dateindex       and 
finecon7.company_id_unq_ci = psdc.company_id_unq;

drop table sipro_data_store.si_finecon7;
--

create table sipro_data_store.si_finecon9
as
select  finecon8.*,
        psdd.dateindex           dateindex_psdd,
        psdd.company_id_unq company_id_unq_psdd      
from 
sipro_data_store.si_finecon8 finecon8 full outer join 
sipro_data_store.si_psdd psdd 
on 
finecon8.dateindex_ci      = psdd.dateindex       and 
finecon8.company_id_unq_ci = psdd.company_id_unq;

drop table sipro_data_store.si_finecon8;
--

create table sipro_data_store.si_finecon10
as
select  finecon9.*,
        returns.now_dateindex           dateindex_returns,
        returns.now_company_id_unq company_id_unq_returns      
from 
sipro_data_store.si_finecon9 finecon9 full outer join 
sipro_data_store.si_returns returns 
on 
finecon9.dateindex_ci      = returns.now_dateindex       and 
finecon9.company_id_unq_ci = returns.now_company_id_unq;

drop table sipro_data_store.si_finecon9;
--

create table sipro_data_store.si_finecon11
as
select  finecon10.*,
        exchg.dateindex           dateindex_exchg
from 
sipro_data_store.si_finecon10 finecon10 full outer join 
sipro_data_store.si_exchg exchg 
on 
finecon10.dateindex_ci      = exchg.dateindex and
finecon10.exchange          = exchg.exchg_code


drop table sipro_data_store.si_finecon10;
--

create table sipro_data_store.si_finecon12
as
select  finecon11.*,
        mgdsc.dateindex           dateindex_mgdsc_sect
from 
sipro_data_store.si_finecon11 finecon11 full outer join 
sipro_data_store.si_mgdsc mgdsc 
on 
finecon11.dateindex_ci      = mgdsc.dateindex and
finecon11.ind_2_dig         = mgdsc.mg_code;


drop table sipro_data_store.si_finecon11;
--

create table sipro_data_store.si_finecon13
as
select  finecon12.*,
        mgdsc.dateindex           dateindex_mgdsc_ind
from 
sipro_data_store.si_finecon12 finecon12 full outer join 
sipro_data_store.si_mgdsc mgdsc 
on 
finecon12.dateindex_ci      = mgdsc.dateindex and
finecon12.ind_3_dig         = mgdsc.mg_code;

drop table sipro_data_store.si_finecon12;
--

alter table sipro_data_store.si_finecon13 rename to si_finecon;


----------- END fincon CREATE -------------------
-------------------------------------------------

-- THIS WORKS: COULD 'KEEP AND STORE' [ ] ???
-- AFTER 41 MINTUES
--ERROR: row is too big: size 8224, maximum size 8160
--SQL state: 54000 

----------------------------------------------- 
--------- BEGIN onebig ------------------------ 

---------- begin column rename -------------

-- before query

-- ci

alter table sipro_data_store.si_ci rename column "row.names" to "row.names_ci";
alter table sipro_data_store.si_ci rename column dateindex   to dateindex_ci;
alter table sipro_data_store.si_ci rename column company_id  to company_id_ci;
-- DONE [X]

alter table sipro_data_store.si_ci rename column repno to repno_ci;
-- DONE [X]

alter table sipro_data_store.si_ci rename column lastmod to lastmod_ci;
-- DONE [X]

-- ONLY ci
alter table sipro_data_store.si_ci rename column ticker  to ticker_ci;
-- DONE [X]

alter table sipro_data_store.si_ci rename column company_id_unq      to company_id_unq_ci;
alter table sipro_data_store.si_ci rename column ticker_unq          to ticker_unq_ci;
alter table sipro_data_store.si_ci rename column dateindexeom        to dateindexeom_ci;
alter table sipro_data_store.si_ci rename column company_id_unq_orig to company_id_unq_orig_ci;
-- DONE [X]

-- bsq

alter table sipro_data_store.si_bsq rename column "row.names" to "row.names_bsq";
alter table sipro_data_store.si_bsq rename column dateindex   to dateindex_bsq;
alter table sipro_data_store.si_bsq rename column company_id  to company_id_bsq;
-- DONE [X]

alter table sipro_data_store.si_bsq rename column repno to repno_bsq;
-- DONE [X]

alter table sipro_data_store.si_bsq rename column lastmod to lastmod_bsq;
-- DONE [X]

alter table sipro_data_store.si_bsq rename column updated to updated_bsq;
-- DONE [X]

alter table sipro_data_store.si_bsq rename column company_id_unq      to company_id_unq_bsq;
alter table sipro_data_store.si_bsq rename column ticker_unq          to ticker_unq_bsq;
alter table sipro_data_store.si_bsq rename column dateindexeom        to dateindexeom_bsq;
alter table sipro_data_store.si_bsq rename column company_id_unq_orig to company_id_unq_orig_bsq;
-- DONE [X]

-- isq

alter table sipro_data_store.si_isq rename column "row.names" to "row.names_isq";
alter table sipro_data_store.si_isq rename column dateindex   to dateindex_isq;
alter table sipro_data_store.si_isq rename column company_id  to company_id_isq;
-- DONE [X]

alter table sipro_data_store.si_isq rename column repno to repno_isq;
-- DONE [X]

alter table sipro_data_store.si_isq rename column lastmod to lastmod_isq;
-- DONE [X]

alter table sipro_data_store.si_isq rename column updated to updated_isq;
-- DONE [X]

alter table sipro_data_store.si_isq rename column company_id_unq      to company_id_unq_isq;
alter table sipro_data_store.si_isq rename column ticker_unq          to ticker_unq_isq;
alter table sipro_data_store.si_isq rename column dateindexeom        to dateindexeom_isq;
alter table sipro_data_store.si_isq rename column company_id_unq_orig to company_id_unq_orig_isq;
-- DONE [X]

-- cfq

alter table sipro_data_store.si_cfq rename column "row.names" to "row.names_cfq";
alter table sipro_data_store.si_cfq rename column dateindex   to dateindex_cfq;
alter table sipro_data_store.si_cfq rename column company_id  to company_id_cfq;
-- DONE [X]

alter table sipro_data_store.si_cfq rename column repno to repno_cfq;
-- DONE [X]

alter table sipro_data_store.si_cfq rename column lastmod to lastmod_cfq;
-- DONE [X]

alter table sipro_data_store.si_cfq rename column updated to updated_cfq;
-- DONE [X]

alter table sipro_data_store.si_cfq rename column company_id_unq      to company_id_unq_cfq;
alter table sipro_data_store.si_cfq rename column ticker_unq          to ticker_unq_cfq;
alter table sipro_data_store.si_cfq rename column dateindexeom        to dateindexeom_cfq;
alter table sipro_data_store.si_cfq rename column company_id_unq_orig to company_id_unq_orig_cfq;
-- DONE [X]

-- mlt

alter table sipro_data_store.si_mlt rename column "row.names" to "row.names_mlt";
alter table sipro_data_store.si_mlt rename column dateindex   to dateindex_mlt;
alter table sipro_data_store.si_mlt rename column company_id  to company_id_mlt;
-- DONE [X]

alter table sipro_data_store.si_mlt rename column repno to repno_mlt;
-- DONE [X]

alter table sipro_data_store.si_mlt rename column company_id_unq      to company_id_unq_mlt;
alter table sipro_data_store.si_mlt rename column ticker_unq          to ticker_unq_mlt;
alter table sipro_data_store.si_mlt rename column dateindexeom        to dateindexeom_mlt;
alter table sipro_data_store.si_mlt rename column company_id_unq_orig to company_id_unq_orig_mlt;
-- DONE [X]

-- rat

alter table sipro_data_store.si_rat rename column "row.names" to "row.names_rat";
alter table sipro_data_store.si_rat rename column dateindex   to dateindex_rat;
alter table sipro_data_store.si_rat rename column company_id  to company_id_rat;
-- DONE [X]

alter table sipro_data_store.si_rat rename column repno to repno_rat;
-- DONE [X]

alter table sipro_data_store.si_rat rename column company_id_unq      to company_id_unq_rat;
alter table sipro_data_store.si_rat rename column ticker_unq          to ticker_unq_rat;
alter table sipro_data_store.si_rat rename column dateindexeom        to dateindexeom_rat;
alter table sipro_data_store.si_rat rename column company_id_unq_orig to company_id_unq_orig_rat;
-- DONE [X]

-- psdc

alter table sipro_data_store.si_psdc rename column "row.names" to "row.names_psdc";
alter table sipro_data_store.si_psdc rename column dateindex   to dateindex_psdc;
alter table sipro_data_store.si_psdc rename column company_id  to company_id_psdc;
-- DONE [X]

alter table sipro_data_store.si_psdc rename column company_id_unq      to company_id_unq_psdc;
alter table sipro_data_store.si_psdc rename column ticker_unq          to ticker_unq_psdc;
alter table sipro_data_store.si_psdc rename column dateindexeom        to dateindexeom_psdc;
alter table sipro_data_store.si_psdc rename column company_id_unq_orig to company_id_unq_orig_psdc;
-- DONE [X]

-- psdd

alter table sipro_data_store.si_psdd rename column "row.names" to "row.names_psdd";
alter table sipro_data_store.si_psdd rename column dateindex   to dateindex_psdd;
alter table sipro_data_store.si_psdd rename column company_id  to company_id_psdd;
-- DONE [X]

alter table sipro_data_store.si_psdd rename column company_id_unq      to company_id_unq_psdd;
alter table sipro_data_store.si_psdd rename column ticker_unq          to ticker_unq_psdd;
alter table sipro_data_store.si_psdd rename column dateindexeom        to dateindexeom_psdd;
alter table sipro_data_store.si_psdd rename column company_id_unq_orig to company_id_unq_orig_psdd;
-- DONE [X]

-- date

alter table sipro_data_store.si_date rename column "row.names" to "row.names_date";
alter table sipro_data_store.si_date rename column dateindex   to dateindex_date;
alter table sipro_data_store.si_date rename column company_id  to company_id_date;
-- DONE [X]

alter table sipro_data_store.si_date rename column repno to repno_date;
-- DONE [X]

alter table sipro_data_store.si_date rename column updated to updated_date;
-- DONE [X]

alter table sipro_data_store.si_date rename column company_id_unq      to company_id_unq_date;
alter table sipro_data_store.si_date rename column ticker_unq          to ticker_unq_date;
alter table sipro_data_store.si_date rename column dateindexeom        to dateindexeom_date;
alter table sipro_data_store.si_date rename column company_id_unq_orig to company_id_unq_orig_date;
-- DONE [X]

-- psd

alter table sipro_data_store.si_psd rename column "row.names" to "row.names_psd";
alter table sipro_data_store.si_psd rename column dateindex   to dateindex_psd;
alter table sipro_data_store.si_psd rename column company_id  to company_id_psd;
-- DONE [X]

alter table sipro_data_store.si_psd rename column repno to repno_psd;
-- DONE [X]

alter table sipro_data_store.si_psd rename column lastmod to lastmod_psd;
-- DONE [X]

alter table sipro_data_store.si_psd rename column updated to updated_psd;
-- DONE [X]

alter table sipro_data_store.si_psd rename column company_id_unq      to company_id_unq_psd;
alter table sipro_data_store.si_psd rename column ticker_unq          to ticker_unq_psd;
alter table sipro_data_store.si_psd rename column dateindexeom        to dateindexeom_psd;
alter table sipro_data_store.si_psd rename column company_id_unq_orig to company_id_unq_orig_psd;
-- DONE [X]

-- exchg

alter table sipro_data_store.si_exchg rename column "row.names" to "row.names_exchg";
alter table sipro_data_store.si_exchg rename column dateindex   to dateindex_exchg;
-- DONE [X]

alter table sipro_data_store.si_exchg rename column dateindexeom        to dateindexeom_exchg;
-- DONE [X]

-- mgdsc

alter table sipro_data_store.si_mgdsc rename column "row.names" to "row.names_mgdsc";
alter table sipro_data_store.si_mgdsc rename column dateindex   to dateindex_mgdsc;
-- DONE [X]

alter table sipro_data_store.si_mgdsc rename column dateindexeom        to dateindexeom_mgdsc;
-- DONE [X]

---------- end column rename ---------------



---------- begin query ---------------- 


set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;


create table sipro_data_store.si_onebig as
select ci.*, psd.*, bsq.*, isq.*, cfq.*, mlt.*, rat.*, psdc.*, psdd.*, date.*, exchg.*,
  mgdsc_s."row.names_mgdsc"  "row.names_mgdsc_sect", 
  mgdsc_s.dateindex_mgdsc    dateindex_mgdsc_sect,
  mgdsc_s.mg_code            mg_code_sect, 
  mgdsc_s.mg_desc            mg_desc_sect, 
  mgdsc_s.dateindexeom_mgdsc dateindexeom_mgdsc_sect,
  mgdsc_i."row.names_mgdsc"  "row.names_mgdsc_ind", 
  mgdsc_i.dateindex_mgdsc    dateindex_mgdsc_ind,
  mgdsc_i.mg_code            mg_code_ind, 
  mgdsc_i.mg_desc            mg_desc_ind, 
  mgdsc_i.dateindexeom_mgdsc dateindexeom_mgdsc_ind,
returns.*
from
sipro_data_store.si_ci  ci 
full outer join 
sipro_data_store.si_psd psd 
on
ci.company_id_unq_ci    = psd.company_id_unq_psd and
ci.dateindex_ci         = psd.dateindex_psd
full outer join 
sipro_data_store.si_bsq bsq 
on
ci.company_id_unq_ci    = bsq.company_id_unq_bsq and
ci.dateindex_ci         = bsq.dateindex_bsq
full outer join 
sipro_data_store.si_isq isq 
on
ci.company_id_unq_ci    = isq.company_id_unq_isq and
ci.dateindex_ci         = isq.dateindex_isq
full outer join 
sipro_data_store.si_cfq cfq 
on
ci.company_id_unq_ci    = cfq.company_id_unq_cfq and
ci.dateindex_ci         = cfq.dateindex_cfq
full outer join 
sipro_data_store.si_mlt mlt 
on
ci.company_id_unq_ci    = mlt.company_id_unq_mlt and
ci.dateindex_ci         = mlt.dateindex_mlt
full outer join 
sipro_data_store.si_rat rat 
on
ci.company_id_unq_ci    = rat.company_id_unq_rat and
ci.dateindex_ci         = rat.dateindex_rat
full outer join 
sipro_data_store.si_psdc psdc 
on
ci.company_id_unq_ci    = psdc.company_id_unq_psdc and
ci.dateindex_ci         = psdc.dateindex_psdc
full outer join 
sipro_data_store.si_psdd psdd 
on
ci.company_id_unq_ci    = psdd.company_id_unq_psdd and
ci.dateindex_ci         = psdd.dateindex_psdd
full outer join 
sipro_data_store.si_date date 
on
ci.company_id_unq_ci    = date.company_id_unq_date and
ci.dateindex_ci         = date.dateindex_date
full outer join 
sipro_data_store.si_exchg exchg 
on
ci.exchange             = exchg.exchg_code and
ci.dateindex_ci         = exchg.dateindex_exchg
full outer join 
sipro_data_store.si_mgdsc mgdsc_s 
on
ci.ind_2_dig            = mgdsc_s.mg_code         and
ci.dateindex_ci         = mgdsc_s.dateindex_mgdsc
full outer join 
sipro_data_store.si_mgdsc mgdsc_i 
on
ci.ind_3_dig            = mgdsc_i.mg_code         and
ci.dateindex_ci         = mgdsc_i.dateindex_mgdsc
full outer join
sipro_data_store.si_returns returns
on 
ci.company_id_unq_ci = returns.now_company_id_unq and
ci.dateindex_ci      = returns.now_dateindex;

-- AFTER 41 MINITUES - ERROR: row is too big: size 8224, maximum size 8160
--ERROR: row is too big: size 8224, maximum size 8160
--SQL state: 54000 

----------end query ------------------



---------- begin column rename -------------

-- put back after query

-- ci

alter table sipro_data_store.si_ci rename column "row.names_ci" to "row.names";
alter table sipro_data_store.si_ci rename column dateindex_ci   to dateindex;
alter table sipro_data_store.si_ci rename column company_id_ci  to company_id;

alter table sipro_data_store.si_ci rename column repno_ci to repno;

alter table sipro_data_store.si_ci rename column lastmod_ci to lastmod;

-- ONLY ci
alter table sipro_data_store.si_ci rename column ticker_ci to ticker;

alter table sipro_data_store.si_ci rename column company_id_unq_ci      to company_id_unq;
alter table sipro_data_store.si_ci rename column ticker_unq_ci          to ticker_unq;
alter table sipro_data_store.si_ci rename column dateindexeom_ci        to dateindexeom;

alter table sipro_data_store.si_ci rename column company_id_unq_orig_ci to company_id_unq_orig;

-- bsq

alter table sipro_data_store.si_bsq rename column "row.names_bsq" to "row.names";
alter table sipro_data_store.si_bsq rename column dateindex_bsq   to dateindex;
alter table sipro_data_store.si_bsq rename column company_id_bsq  to company_id;

alter table sipro_data_store.si_bsq rename column repno_bsq to repno;

alter table sipro_data_store.si_bsq rename column lastmod_bsq to lastmod;

alter table sipro_data_store.si_bsq rename column updated_bsq to updated;

alter table sipro_data_store.si_bsq rename column company_id_unq_bsq      to company_id_unq;
alter table sipro_data_store.si_bsq rename column ticker_unq_bsq          to ticker_unq;
alter table sipro_data_store.si_bsq rename column dateindexeom_bsq        to dateindexeom;

alter table sipro_data_store.si_bsq rename column company_id_unq_orig_bsq to company_id_unq_orig;

-- isq

alter table sipro_data_store.si_isq rename column "row.names_isq" to "row.names";
alter table sipro_data_store.si_isq rename column dateindex_isq   to dateindex;
alter table sipro_data_store.si_isq rename column company_id_isq  to company_id;

alter table sipro_data_store.si_isq rename column repno_isq to repno;

alter table sipro_data_store.si_isq rename column lastmod_isq to lastmod;

alter table sipro_data_store.si_isq rename column updated_isq to updated;

alter table sipro_data_store.si_isq rename column company_id_unq_isq      to company_id_unq;
alter table sipro_data_store.si_isq rename column ticker_unq_isq          to ticker_unq;
alter table sipro_data_store.si_isq rename column dateindexeom_isq        to dateindexeom;

alter table sipro_data_store.si_isq rename column company_id_unq_orig_isq to company_id_unq_orig;

-- cfq

alter table sipro_data_store.si_cfq rename column "row.names_cfq" to "row.names";
alter table sipro_data_store.si_cfq rename column dateindex_cfq   to dateindex;
alter table sipro_data_store.si_cfq rename column company_id_cfq  to company_id;

alter table sipro_data_store.si_cfq rename column repno_cfq to repno;

alter table sipro_data_store.si_cfq rename column lastmod_cfq to lastmod;

alter table sipro_data_store.si_cfq rename column updated_cfq to updated;

alter table sipro_data_store.si_cfq rename column company_id_unq_cfq      to company_id_unq;
alter table sipro_data_store.si_cfq rename column ticker_unq_cfq          to ticker_unq;
alter table sipro_data_store.si_cfq rename column dateindexeom_cfq        to dateindexeom;

alter table sipro_data_store.si_cfq rename column company_id_unq_orig_cfq to company_id_unq_orig;

-- mlt

alter table sipro_data_store.si_mlt rename column "row.names_mlt" to "row.names";
alter table sipro_data_store.si_mlt rename column dateindex_mlt   to dateindex;
alter table sipro_data_store.si_mlt rename column company_id_mlt  to company_id;

alter table sipro_data_store.si_mlt rename column repno_mlt to repno;

alter table sipro_data_store.si_mlt rename column company_id_unq_mlt      to company_id_unq;
alter table sipro_data_store.si_mlt rename column ticker_unq_mlt          to ticker_unq;
alter table sipro_data_store.si_mlt rename column dateindexeom_mlt        to dateindexeom;

alter table sipro_data_store.si_mlt rename column company_id_unq_orig_mlt to company_id_unq_orig;

-- rat

alter table sipro_data_store.si_rat rename column "row.names_rat" to "row.names";
alter table sipro_data_store.si_rat rename column dateindex_rat   to dateindex;
alter table sipro_data_store.si_rat rename column company_id_rat  to company_id;

alter table sipro_data_store.si_rat rename column repno_rat to repno;

alter table sipro_data_store.si_rat rename column company_id_unq_rat      to company_id_unq;
alter table sipro_data_store.si_rat rename column ticker_unq_rat          to ticker_unq;
alter table sipro_data_store.si_rat rename column dateindexeom_rat        to dateindexeom;

alter table sipro_data_store.si_rat rename column company_id_unq_orig_rat to company_id_unq_orig;

-- psdc

alter table sipro_data_store.si_psdc rename column "row.names_psdc" to "row.names";
alter table sipro_data_store.si_psdc rename column dateindex_psdc   to dateindex;
alter table sipro_data_store.si_psdc rename column company_id_psdc  to company_id;

alter table sipro_data_store.si_psdc rename column company_id_unq_psdc      to company_id_unq;
alter table sipro_data_store.si_psdc rename column ticker_unq_psdc          to ticker_unq;
alter table sipro_data_store.si_psdc rename column dateindexeom_psdc        to dateindexeom;

alter table sipro_data_store.si_psdc rename column company_id_unq_orig_psdc to company_id_unq_orig;

-- psdd

alter table sipro_data_store.si_psdd rename column "row.names_psdd" to "row.names";
alter table sipro_data_store.si_psdd rename column dateindex_psdd   to dateindex;
alter table sipro_data_store.si_psdd rename column company_id_psdd  to company_id;

alter table sipro_data_store.si_psdd rename column company_id_unq_psdd      to company_id_unq;
alter table sipro_data_store.si_psdd rename column ticker_unq_psdd          to ticker_unq;
alter table sipro_data_store.si_psdd rename column dateindexeom_psdd        to dateindexeom;

alter table sipro_data_store.si_psdd rename column company_id_unq_orig_psdd to company_id_unq_orig;

-- date

alter table sipro_data_store.si_date rename column "row.names_date" to "row.names";
alter table sipro_data_store.si_date rename column dateindex_date   to dateindex;
alter table sipro_data_store.si_date rename column company_id_date  to company_id;

alter table sipro_data_store.si_date rename column repno_date to repno;

alter table sipro_data_store.si_date rename column updated_date to updated;

alter table sipro_data_store.si_date rename column company_id_unq_date      to company_id_unq;
alter table sipro_data_store.si_date rename column ticker_unq_date          to ticker_unq;
alter table sipro_data_store.si_date rename column dateindexeom_date        to dateindexeom;

alter table sipro_data_store.si_date rename column company_id_unq_orig_date to company_id_unq_orig;

-- psd

alter table sipro_data_store.si_psd rename column "row.names_psd" to "row.names";
alter table sipro_data_store.si_psd rename column dateindex_psd   to dateindex;
alter table sipro_data_store.si_psd rename column company_id_psd  to company_id;

alter table sipro_data_store.si_psd rename column repno_psd to repno;

alter table sipro_data_store.si_psd rename column lastmod_psd to lastmod;

alter table sipro_data_store.si_psd rename column updated_psd to updated;

alter table sipro_data_store.si_psd rename column company_id_unq_psd      to company_id_unq;
alter table sipro_data_store.si_psd rename column ticker_unq_psd          to ticker_unq;
alter table sipro_data_store.si_psd rename column dateindexeom_psd        to dateindexeom;

alter table sipro_data_store.si_psd rename column company_id_unq_orig_psd to company_id_unq_orig;

-- exchg

alter table sipro_data_store.si_exchg rename column "row.names_exchg" to "row.names";
alter table sipro_data_store.si_exchg rename column dateindex_exchg   to dateindex;

alter table sipro_data_store.si_exchg rename column dateindexeom_exchg        to dateindexeom;

-- mgdsc

alter table sipro_data_store.si_mgdsc rename column "row.names_mgdsc" to "row.names";
alter table sipro_data_store.si_mgdsc rename column dateindex_mgdsc   to dateindex;

alter table sipro_data_store.si_mgdsc rename column dateindexeom_mgdsc        to dateindexeom;


---------- end column rename ---------------

--------- END onebig ------------------------ 
---------------------------------------------


-- R PROGRAM [X] createAAIIOneBigRealTable 
-------------------------------------------------
----------- BEGIN ... CREATE -----------------

set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;




create table sipro_data_store.si_finecon1
as
select  
  ci.dateindex_ci,
  ci.company_id_ci,
  ci.company,
  ci.ticker_ci,
  ci.exchange,
  ci.sic,
  ci.sp,
  ci.adr,
  ci.ind_2_dig,
  ci.ind_3_dig,
  ci.country,
  ci.employees,
  ci.company_id_unq_ci,
  ci.ticker_unq_ci,
  ci.dateindexeom_ci,
  ci.company_id_unq_orig_ci,
  psd.dateindex_psd,
  psd.company_id_psd,
  psd.split_date,
  psd.split_fact,
  psd.price_date,  -- should have gone with si_returns
  psd.price,
  psd.prchg_04w,
  psd.prchg_13w,
  psd.prchg_26w,
  psd.prchg_52w,
  psd.shr_aq1,
  psd.shr_aq2,
  psd.shr_aq3,
  psd.shr_aq4,
  psd.shr_aq5,
  psd.shr_aq6,
  psd.shr_aq7,
  psd.shr_aq8,
  psd.beta,      -- NEW
  psd.mktcap,
  psd.mktcap_q1,
  psd.mktcap_q2,
  psd.mktcap_q3,
  psd.mktcap_q4,
  psd.mktcap_q5,
  psd.mktcap_q6,
  psd.mktcap_q7,
  psd.mktcap_q8,
  psd.shr_dq1,  -- NEW
  psd.shr_dq2,
  psd.shr_dq3,
  psd.shr_dq4,
  psd.shr_dq5,
  psd.shr_dq6,
  psd.shr_dq7,
  psd.shr_dq8,
  psd.shr_dq9,
  psd.shr_dq10,
  psd.company_id_unq_psd,
  psd.ticker_unq_psd,
  psd.dateindexeom_psd,
  psd.company_id_unq_orig_psd
from 
sipro_data_store.si_ci  ci  full outer join 
sipro_data_store.si_psd psd 
on 
ci.dateindex_ci       = psd.dateindex_psd       and 
ci.company_id_unq_ci  = psd.company_id_unq_psd;
-- 40 SECONDS ( USED 4 INDEXES )



create table sipro_data_store.si_finecon2
as
select finecon1.*,
 isq.dateindex_isq,
 isq.company_id_isq,
 isq.sales_q1,
 isq.sales_q2,
 isq.sales_q3,
 isq.sales_q4,
 isq.sales_q5,
 isq.sales_q6,
 isq.sales_q7,
 isq.sales_q8,
 isq.cgs_q1,
 isq.cgs_q2,
 isq.cgs_q3,
 isq.cgs_q4,
 isq.cgs_q5,
 isq.cgs_q6,
 isq.cgs_q7,
 isq.cgs_q8,
 isq.gross_q1,
 isq.gross_q2,
 isq.gross_q3,
 isq.gross_q4,
 isq.gross_q5,
 isq.gross_q6,
 isq.gross_q7,
 isq.gross_q8,
 isq.dep_q1,
 isq.dep_q2,
 isq.dep_q3,
 isq.dep_q4,
 isq.dep_q5,
 isq.dep_q6,
 isq.dep_q7,
 isq.dep_q8,
 isq.int_q1,
 isq.int_q2,
 isq.int_q3,
 isq.int_q4,
 isq.int_q5,
 isq.int_q6,
 isq.int_q7,
 isq.int_q8,
 isq.intno_q1,
 isq.intno_q2,
 isq.intno_q3,
 isq.intno_q4,
 isq.intno_q5,
 isq.intno_q6,
 isq.intno_q7,
 isq.intno_q8,
 isq.uninc_q1,
 isq.uninc_q2,
 isq.uninc_q3,
 isq.uninc_q4,
 isq.uninc_q5,
 isq.uninc_q6,
 isq.uninc_q7,
 isq.uninc_q8,
 isq.totexp_q1,
 isq.totexp_q2,
 isq.totexp_q3,
 isq.totexp_q4,
 isq.totexp_q5,
 isq.totexp_q6,
 isq.totexp_q7,
 isq.totexp_q8,
 isq.gopinc_q1,
 isq.gopinc_q2,
 isq.gopinc_q3,
 isq.gopinc_q4,
 isq.gopinc_q5,
 isq.gopinc_q6,
 isq.gopinc_q7,
 isq.gopinc_q8,
 isq.othinc_q1,
 isq.othinc_q2,
 isq.othinc_q3,
 isq.othinc_q4,
 isq.othinc_q5,
 isq.othinc_q6,
 isq.othinc_q7,
 isq.othinc_q8,
 isq.pti_q1,
 isq.pti_q2,
 isq.pti_q3,
 isq.pti_q4,
 isq.pti_q5,
 isq.pti_q6,
 isq.pti_q7,
 isq.pti_q8,
 isq.inctax_q1,
 isq.inctax_q2,
 isq.inctax_q3,
 isq.inctax_q4,
 isq.inctax_q5,
 isq.inctax_q6,
 isq.inctax_q7,
 isq.inctax_q8,
 isq.adjust_q1,
 isq.adjust_q2,
 isq.adjust_q3,
 isq.adjust_q4,
 isq.adjust_q5,
 isq.adjust_q6,
 isq.adjust_q7,
 isq.adjust_q8,
 isq.nit_q1,
 isq.nit_q2,
 isq.nit_q3,
 isq.nit_q4,
 isq.nit_q5,
 isq.nit_q6,
 isq.nit_q7,
 isq.nit_q8,
 isq.iac_q1,
 isq.iac_q2,
 isq.iac_q3,
 isq.iac_q4,
 isq.iac_q5,
 isq.iac_q6,
 isq.iac_q7,
 isq.iac_q8,
 isq.xord_q1,
 isq.xord_q2,
 isq.xord_q3,
 isq.xord_q4,
 isq.xord_q5,
 isq.xord_q6,
 isq.xord_q7,
 isq.xord_q8,
 isq.netinc_q1,
 isq.netinc_q2,
 isq.netinc_q3,
 isq.netinc_q4,
 isq.netinc_q5,
 isq.netinc_q6,
 isq.netinc_q7,
 isq.netinc_q8,
 isq.epscon_q1,
 isq.epscon_q2,
 isq.epscon_q3,
 isq.epscon_q4,
 isq.epscon_q5,
 isq.epscon_q6,
 isq.epscon_q7,
 isq.epscon_q8,
 isq.eps_q1,
 isq.eps_q2,
 isq.eps_q3,
 isq.eps_q4,
 isq.eps_q5,
 isq.eps_q6,
 isq.eps_q7,
 isq.eps_q8,
 isq.epsdc_q1,
 isq.epsdc_q2,
 isq.epsdc_q3,
 isq.epsdc_q4,
 isq.epsdc_q5,
 isq.epsdc_q6,
 isq.epsdc_q7,
 isq.epsdc_q8,
 isq.epsd_q1,
 isq.epsd_q2,
 isq.epsd_q3,
 isq.epsd_q4,
 isq.epsd_q5,
 isq.epsd_q6,
 isq.epsd_q7,
 isq.epsd_q8,
 isq.dps_q1,
 isq.dps_q2,
 isq.dps_q3,
 isq.dps_q4,
 isq.dps_q5,
 isq.dps_q6,
 isq.dps_q7,
 isq.dps_q8,
 isq.divnq,
 isq.divnqxdt,
 isq.divnqpdt,
 isq.ebit_q1,
 isq.ebit_q2,
 isq.ebit_q3,
 isq.ebit_q4,
 isq.ebit_q5,
 isq.ebit_q6,
 isq.ebit_q7,
 isq.ebit_q8,
 isq.ebitda_q1,
 isq.ebitda_q2,
 isq.ebitda_q3,
 isq.ebitda_q4,
 isq.ebitda_q5,
 isq.ebitda_q6,
 isq.ebitda_q7,
 isq.ebitda_q8,
 isq.dpst_q1,
 isq.dpst_q2,
 isq.dpst_q3,
 isq.dpst_q4,
 isq.dpst_q5,
 isq.dpst_q6,
 isq.dpst_q7,
 isq.dpst_q8,
 isq.epsnd_q1,
 isq.epsnd_q2,
 isq.epsnd_q3,
 isq.epsnd_q4,
 isq.epsnd_q5,
 isq.epsnd_q6,
 isq.epsnd_q7,
 isq.epsnd_q8,
 isq.epsnd_q9,
 isq.epsnd_q10,
 isq.company_id_unq_isq,
 isq.ticker_unq_isq,
 isq.dateindexeom_isq,
 isq.company_id_unq_orig_isq
from 
sipro_data_store.si_finecon1 finecon1 full outer join 
sipro_data_store.si_isq isq 
on 
finecon1.dateindex_ci      = isq.dateindex_isq      and 
finecon1.company_id_unq_ci = isq.company_id_unq_isq;
--    SECONDS ( USED 2 INDEXES - ONLY ISQ HAS INDEXES )

drop table sipro_data_store.si_finecon1;
--

create table sipro_data_store.si_finecon3
as
select  finecon2.*,
 bsq.dateindex_bsq,
 bsq.company_id_bsq,
 bsq.cash_q1,
 bsq.cash_q2,
 bsq.cash_q3,
 bsq.cash_q4,
 bsq.cash_q5,
 bsq.cash_q6,
 bsq.cash_q7,
 bsq.cash_q8,
 bsq.ar_q1,
 bsq.ar_q2,
 bsq.ar_q3,
 bsq.ar_q4,
 bsq.ar_q5,
 bsq.ar_q6,
 bsq.ar_q7,
 bsq.ar_q8,
 bsq.ca_q1,
 bsq.ca_q2,
 bsq.ca_q3,
 bsq.ca_q4,
 bsq.ca_q5,
 bsq.ca_q6,
 bsq.ca_q7,
 bsq.ca_q8,
 bsq.assets_q1,
 bsq.assets_q2,
 bsq.assets_q3,
 bsq.assets_q4,
 bsq.assets_q5,
 bsq.assets_q6,
 bsq.assets_q7,
 bsq.assets_q8,
 bsq.ap_q1,
 bsq.ap_q2,
 bsq.ap_q3,
 bsq.ap_q4,
 bsq.ap_q5,
 bsq.ap_q6,
 bsq.ap_q7,
 bsq.ap_q8,
 bsq.stdebt_q1,
 bsq.stdebt_q2,
 bsq.stdebt_q3,
 bsq.stdebt_q4,
 bsq.stdebt_q5,
 bsq.stdebt_q6,
 bsq.stdebt_q7,
 bsq.stdebt_q8,
 bsq.ocl_q1,
 bsq.ocl_q2,
 bsq.ocl_q3,
 bsq.ocl_q4,
 bsq.ocl_q5,
 bsq.ocl_q6,
 bsq.ocl_q7,
 bsq.ocl_q8,
 bsq.cl_q1,
 bsq.cl_q2,
 bsq.cl_q3,
 bsq.cl_q4,
 bsq.cl_q5,
 bsq.cl_q6,
 bsq.cl_q7,
 bsq.cl_q8,
 bsq.ltdebt_q1,
 bsq.ltdebt_q2,
 bsq.ltdebt_q3,
 bsq.ltdebt_q4,
 bsq.ltdebt_q5,
 bsq.ltdebt_q6,
 bsq.ltdebt_q7,
 bsq.ltdebt_q8,
 bsq.oltl_q1,
 bsq.oltl_q2,
 bsq.oltl_q3,
 bsq.oltl_q4,
 bsq.oltl_q5,
 bsq.oltl_q6,
 bsq.oltl_q7,
 bsq.oltl_q8,
 bsq.liab_q1,
 bsq.liab_q2,
 bsq.liab_q3,
 bsq.liab_q4,
 bsq.liab_q5,
 bsq.liab_q6,
 bsq.liab_q7,
 bsq.liab_q8,
 bsq.equity_q1,
 bsq.equity_q2,
 bsq.equity_q3,
 bsq.equity_q4,
 bsq.equity_q5,
 bsq.equity_q6,
 bsq.equity_q7,
 bsq.equity_q8,
 bsq.bvps_q1,
 bsq.bvps_q2,
 bsq.bvps_q3,
 bsq.bvps_q4,
 bsq.bvps_q5,
 bsq.bvps_q6,
 bsq.bvps_q7,
 bsq.bvps_q8,
 bsq.minor_q1,
 bsq.minor_q2,
 bsq.minor_q3,
 bsq.minor_q4,
 bsq.minor_q5,
 bsq.minor_q6,
 bsq.minor_q7,
 bsq.minor_q8,
 bsq.work_q1,
 bsq.work_q2,
 bsq.work_q3,
 bsq.work_q4,
 bsq.work_q5,
 bsq.work_q6,
 bsq.work_q7,
 bsq.work_q8,
 bsq.entval_q1,
 bsq.entval_q2,
 bsq.entval_q3,
 bsq.entval_q4,
 bsq.entval_q5,
 bsq.entval_q6,
 bsq.entval_q7,
 bsq.entval_q8,
 bsq.company_id_unq_bsq,
 bsq.ticker_unq_bsq,
 bsq.dateindexeom_bsq,
 bsq.company_id_unq_orig_bsq
from 
sipro_data_store.si_finecon2 finecon2 full outer join 
sipro_data_store.si_bsq bsq 
on 
finecon2.dateindex_ci      = bsq.dateindex_bsq   and 
finecon2.company_id_unq_ci = bsq.company_id_unq_bsq;
-- 134 SECONDS


drop table sipro_data_store.si_finecon2;
--


create table sipro_data_store.si_finecon4
as
select finecon3.*,
 cfq.dateindex_cfq,
 cfq.company_id_cfq,
 cfq.tco_q1,
 cfq.tco_q2,
 cfq.tco_q3,
 cfq.tco_q4,
 cfq.tco_q5,
 cfq.tco_q6,
 cfq.tco_q7,
 cfq.tco_q8,
 cfq.ce_q1,
 cfq.ce_q2,
 cfq.ce_q3,
 cfq.ce_q4,
 cfq.ce_q5,
 cfq.ce_q6,
 cfq.ce_q7,
 cfq.ce_q8,
 cfq.tci_q1,
 cfq.tci_q2,
 cfq.tci_q3,
 cfq.tci_q4,
 cfq.tci_q5,
 cfq.tci_q6,
 cfq.tci_q7,
 cfq.tci_q8,
 cfq.tcf_q1,
 cfq.tcf_q2,
 cfq.tcf_q3,
 cfq.tcf_q4,
 cfq.tcf_q5,
 cfq.tcf_q6,
 cfq.tcf_q7,
 cfq.tcf_q8,
 cfq.ere_q1,
 cfq.ere_q2,
 cfq.ere_q3,
 cfq.ere_q4,
 cfq.ere_q5,
 cfq.ere_q6,
 cfq.ere_q7,
 cfq.ere_q8,
 cfq.ncc_q1,
 cfq.ncc_q2,
 cfq.ncc_q3,
 cfq.ncc_q4,
 cfq.ncc_q5,
 cfq.ncc_q6,
 cfq.ncc_q7,
 cfq.ncc_q8,
 cfq.dep_cf_q1,
 cfq.dep_cf_q2,
 cfq.dep_cf_q3,
 cfq.dep_cf_q4,
 cfq.dep_cf_q5,
 cfq.dep_cf_q6,
 cfq.dep_cf_q7,
 cfq.dep_cf_q8,
 cfq.cfps_q1,
 cfq.cfps_q2,
 cfq.cfps_q3,
 cfq.cfps_q4,
 cfq.cfps_q5,
 cfq.cfps_q6,
 cfq.cfps_q7,
 cfq.cfps_q8,
 cfq.fcfps_q1,
 cfq.fcfps_q2,
 cfq.fcfps_q3,
 cfq.fcfps_q4,
 cfq.fcfps_q5,
 cfq.fcfps_q6,
 cfq.fcfps_q7,
 cfq.fcfps_q8,
 cfq.dcfbs_q1,
 cfq.dcfbs_q2,
 cfq.dcfbs_q3,
 cfq.dcfbs_q4,
 cfq.dcfo_q1,
 cfq.dcfo_q2,
 cfq.dcfo_q3,
 cfq.dcfo_q4,
 cfq.dcf_q1,
 cfq.dcf_q2,
 cfq.dcf_q3,
 cfq.dcf_q4,
 cfq.divpaid_q1,
 cfq.divpaid_q2,
 cfq.divpaid_q3,
 cfq.divpaid_q4,
 cfq.divpaid_q5,
 cfq.divpaid_q6,
 cfq.divpaid_q7,
 cfq.divpaid_q8,
 cfq.company_id_unq_cfq,
 cfq.ticker_unq_cfq,
 cfq.dateindexeom_cfq,
 cfq.company_id_unq_orig_cfq
from 
sipro_data_store.si_finecon3 finecon3 full outer join 
sipro_data_store.si_cfq cfq 
on 
finecon3.dateindex_ci      = cfq.dateindex_cfq       and 
finecon3.company_id_unq_ci = cfq.company_id_unq_cfq;
-- 182/210 SECONDS

drop table sipro_data_store.si_finecon3;
--


create table sipro_data_store.si_finecon5
as
select finecon4.*,
 rat.dateindex_rat,
 rat.company_id_rat,
 rat.roic_y1,
 rat.company_id_unq_rat,
 rat.ticker_unq_rat,
 rat.dateindexeom_rat,
 rat.company_id_unq_orig_rat     
from 
sipro_data_store.si_finecon4 finecon4 full outer join 
sipro_data_store.si_rat rat 
on 
finecon4.dateindex_ci      = rat.dateindex_rat      and 
finecon4.company_id_unq_ci = rat.company_id_unq_rat;
-- 318 SECONDS

drop table sipro_data_store.si_finecon4;
--

create table sipro_data_store.si_finecon6
as
select finecon5.*,
 mlt.dateindex_mlt,
 mlt.company_id_mlt,
 mlt.shy,
 mlt.company_id_unq_mlt,
 mlt.ticker_unq_mlt,
 mlt.dateindexeom_mlt,
 mlt.company_id_unq_orig_mlt
from 
sipro_data_store.si_finecon5 finecon5 full outer join 
sipro_data_store.si_mlt mlt 
on 
finecon5.dateindex_ci      = mlt.dateindex_mlt       and 
finecon5.company_id_unq_ci = mlt.company_id_unq_mlt;
-- 231 SECONDS

drop table sipro_data_store.si_finecon5;
--

create table sipro_data_store.si_finecon7
as
select  finecon6.*,
  date.dateindex_date,
  date.company_id_date,
  date.perend_q1,
  date.perlen_q1,
  date.pertyp_q1,
  date.updtyp_q1,
  date.company_id_unq_date,
  date.ticker_unq_date,
  date.dateindexeom_date,
  date.company_id_unq_orig_date
from 
sipro_data_store.si_finecon6 finecon6 full outer join 
sipro_data_store.si_date date 
on 
finecon6.dateindex_ci      = date.dateindex_date       and 
finecon6.company_id_unq_ci = date.company_id_unq_date;
-- 242 SECONDS


drop table sipro_data_store.si_finecon6;
--

create table sipro_data_store.si_finecon8
as
select  finecon7.*,
  psdc.dateindex_psdc,
  psdc.company_id_psdc,
  psdc.company_id_unq_psdc,
  psdc.ticker_unq_psdc,
  psdc.dateindexeom_psdc,
  psdc.company_id_unq_orig_psdc
from 
sipro_data_store.si_finecon7 finecon7 full outer join 
sipro_data_store.si_psdc psdc 
on 
finecon7.dateindex_ci      = psdc.dateindex_psdc      and 
finecon7.company_id_unq_ci = psdc.company_id_unq_psdc;
-- 262 SECONDS

drop table sipro_data_store.si_finecon7;
--

create table sipro_data_store.si_finecon9
as
select  finecon8.*,
  psdd.dateindex_psdd,
  psdd.company_id_psdd,
  psdd.company_id_unq_psdd,
  psdd.ticker_unq_psdd,
  psdd.dateindexeom_psdd,
  psdd.company_id_unq_orig_psdd
from 
sipro_data_store.si_finecon8 finecon8 full outer join 
sipro_data_store.si_psdd psdd 
on 
finecon8.dateindex_ci      = psdd.dateindex_psdd       and 
finecon8.company_id_unq_ci = psdd.company_id_unq_psdd;
-- 300 SECONDS  -- 
-- 


drop table sipro_data_store.si_finecon8;
--

create table sipro_data_store.si_finecon10
as
select  finecon9.*,
  returns.retdate_dateindex,
  returns.retdate_dateindexeom,
  returns.now_dateindex,
  returns.now_dateindexeom,
  returns.now_company_id_unq,
  returns.now_ticker_unq,
  returns.now_company,
  returns.now_price,
  returns.w52_dateindex,
  returns.w52_dateindexeom,
  returns.w52_company_id_unq,
  returns.w52_ticker_unq,
  returns.w52_company,
  returns.w52_prchg_52w_ann,
  returns.w52_pradchg_52w_ann,
  returns.w52_divaccmf4q,
  returns.w26_dateindex,
  returns.w26_dateindexeom,
  returns.w26_company_id_unq,
  returns.w26_ticker_unq,
  returns.w26_company,
  returns.w26_prchg_26w_ann,
  returns.w26_pradchg_26w_ann,
  returns.w26_divaccmf2q,
  returns.w13_dateindex,
  returns.w13_dateindexeom,
  returns.w13_company_id_unq,
  returns.w13_ticker_unq,
  returns.w13_company,
  returns.w13_prchg_13w_ann,
  returns.w13_pradchg_13w_ann,
  returns.w13_divaccmf1q
from 
sipro_data_store.si_finecon9 finecon9 full outer join 
sipro_data_store.si_returns returns 
on 
finecon9.dateindex_ci      = returns.now_dateindex       and 
finecon9.company_id_unq_ci = returns.now_company_id_unq;
-- 555 SECONDS


drop table sipro_data_store.si_finecon9;
--

create table sipro_data_store.si_finecon11
as
select  finecon10.*,
  exchg.dateindex_exchg,
  exchg.exchg_code,
  exchg.exchg_desc,
  exchg.dateindexeom_exchg
from 
sipro_data_store.si_finecon10 finecon10 full outer join 
sipro_data_store.si_exchg exchg 
on 
finecon10.dateindex_ci      = exchg.dateindex_exchg and
finecon10.exchange          = exchg.exchg_code;
-- 706 SECONDS

drop table sipro_data_store.si_finecon10;
--


create table sipro_data_store.si_finecon12
as
select  finecon11.*,
  mgdsc.dateindex_mgdsc dateindex_mgdsc_sect,
  mgdsc.mg_code mg_code_sect,
  mgdsc.mg_desc mg_desc_sect,
  mgdsc.dateindexeom_mgdsc dateindexeom_mgdsc_sect
from 
sipro_data_store.si_finecon11 finecon11 full outer join 
sipro_data_store.si_mgdsc mgdsc 
on 
finecon11.dateindex_ci      = mgdsc.dateindex_mgdsc and
finecon11.ind_2_dig         = mgdsc.mg_code;
-- 370 SECONDS


drop table sipro_data_store.si_finecon11;
--

create table sipro_data_store.si_finecon13
as
select  finecon12.*,
  mgdsc.dateindex_mgdsc dateindex_mgdsc_ind,
  mgdsc.mg_code mg_code_ind,
  mgdsc.mg_desc mg_desc_ind,
  mgdsc.dateindexeom_mgdsc dateindexeom_mgdsc_ind
from 
sipro_data_store.si_finecon12 finecon12 full outer join 
sipro_data_store.si_mgdsc mgdsc 
on 
finecon12.dateindex_ci      = mgdsc.dateindex_mgdsc and
finecon12.ind_3_dig         = mgdsc.mg_code;
-- 360 SECONDS

drop table sipro_data_store.si_finecon12;
--

alter table sipro_data_store.si_finecon13 rename to si_finecon;

--
-- DONE: 34 MINUTES ( IF RUN AS ONE BIG BATCH )


-- Index: sipro_data_store.si_finecon_dateindex_ci_brin_idx

-- DROP INDEX sipro_data_store.si_finecon_dateindex_ci_brin_idx;

CREATE INDEX si_finecon_dateindex_ci_brin_idx  -- PRE MAKE SURE AN INDEX DOES NOT EXIST OF SAME NAME
  ON sipro_data_store.si_finecon
  USING brin
  (dateindex_ci);



----------- END ... CREATE -------------------
---------------------------------------------- NOTE: "core_relation_size"  2982






