
-- without ticker change ( GOOD )
-- USE FAR BELOW INSTEAD

set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;

explain --analyze
select retdate.dateindex retdate_dateindex, 
  ci.dateindex         now_dateindex,
  ci.company_id_unq    now_company_id_unq,
  ci.ticker            now_ticker,
  ci.company           now_company,
  psd.price            now_price,
  w52.dateindex        w52_dateindex,
  w52.company_id_unq   w52_company_id_unq,
  w52.ticker           w52_ticker,
  w52.company          w52_company,
  w52.pricebck         w52_pricebck,
  w52.prchg_52w        w52_prchg_52w,
  w52.prchg_52w_ann    w52_prchg_52w_ann,
  w52.price            w52_price,
  w52.divaccmf4q       w52_divaccmf4q,
  w52.pradchg_52w      w52_pradchg_52w,
  w52.pradchg_52w_ann  w52_pradchg_52w_ann
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex = ci.dateindex 
              
      left join si_psd psd
              on ci.dateindex      = psd.dateindex
             and ci.company_id_unq = psd.company_id_unq

      left join lateral ( select cif.dateindex, cif.company_id_unq, cif.ticker, cif.company,
      fut.pricebck,
      fut.prchg_52w,
      fut.prchg_52w_ann,
      fut.price,
      fut.divaccmf4q,
      fut.pradchg_52w,
      fut.pradchg_52w_ann
      from
              
                 si_ci cif 

      left join (
         select fut_i.dateindex, fut_i.company_id_unq, 
                fut_i.pricebck, 
                fut_i.prchg_52w, 
                fut_i.prchg_52w_ann,
                fut_i.price,
                fut_i.divaccmf4q,

                fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 pradchg_52w,

                fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 * 1 pradchg_52w_ann

         from (
         select psd.dateindex, psd.company_id_unq,

         psd.price::numeric(15,2)/(nullif(psd.prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
         psd.prchg_52w::numeric(15,2),
         psd.prchg_52w::numeric(15,2) * 1 prchg_52w_ann,

         coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q,

         psd.price::numeric(15,2)

         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut_i
         ) fut on cif.dateindex      = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)

;



-- 52 and 26 -- without ticker change ( WORKS )
-- USE FAR BELOW INSTEAD

set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;

show search_path;

explain --analyze
select retdate.dateindex retdate_dateindex, 
  ci.dateindex         now_dateindex,
  ci.company_id_unq    now_company_id_unq,
  ci.ticker            now_ticker,
  ci.company           now_company,
  psd.price            now_price,
  w52.dateindex        w52_dateindex,
  w52.company_id_unq   w52_company_id_unq,
  w52.ticker           w52_ticker,
  w52.company          w52_company,
  w52.pricebck         w52_pricebck,
  w52.prchg_52w        w52_prchg_52w,
  w52.prchg_52w_ann    w52_prchg_52w_ann,
  w52.price            w52_price,
  w52.divaccmf4q       w52_divaccmf4q,
  w52.pradchg_52w      w52_pradchg_52w,
  w52.pradchg_52w_ann  w52_pradchg_52w_ann,
  w26.dateindex        w26_dateindex,
  w26.company_id_unq   w26_company_id_unq,
  w26.ticker           w26_ticker,
  w26.company          w26_company,
  w26.pricebck         w26_pricebck,
  w26.prchg_26w        w26_prchg_26w,
  w26.prchg_26w_ann    w26_prchg_26w_ann,
  w26.price            w26_price,
  w26.divaccmf2q       w26_divaccmf2q,
  w26.pradchg_26w      w26_pradchg_26w,
  w26.pradchg_26w_ann  w26_pradchg_26w_ann
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex = ci.dateindex 
              
      left join si_psd psd
              on ci.dateindex      = psd.dateindex
             and ci.company_id_unq = psd.company_id_unq

      left join lateral ( select cif.dateindex, cif.company_id_unq, cif.ticker, cif.company,
      fut.pricebck,
      fut.prchg_52w,
      fut.prchg_52w_ann,
      fut.price,
      fut.divaccmf4q,
      fut.pradchg_52w,
      fut.pradchg_52w_ann
      from
              
                 si_ci cif 

      left join (
         select fut_i.dateindex, fut_i.company_id_unq, 
                fut_i.pricebck, 
                fut_i.prchg_52w, 
                fut_i.prchg_52w_ann,
                fut_i.price,
                fut_i.divaccmf4q,

                fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 pradchg_52w,

                fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 * 1 pradchg_52w_ann

         from (
         select psd.dateindex, psd.company_id_unq,

         psd.price::numeric(15,2)/(nullif(psd.prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
         psd.prchg_52w::numeric(15,2),
         psd.prchg_52w::numeric(15,2) * 1 prchg_52w_ann,

         coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q,

         psd.price::numeric(15,2)

         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut_i
         ) fut on cif.dateindex      = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)

      left join lateral ( select cif.dateindex, cif.company_id_unq, cif.ticker, cif.company,
      fut.pricebck,
      fut.prchg_26w,
      fut.prchg_26w_ann,
      fut.price,
      fut.divaccmf2q,
      fut.pradchg_26w,
      fut.pradchg_26w_ann
      from
              
                 si_ci cif 

      left join (
         select fut_i.dateindex, fut_i.company_id_unq, 
                fut_i.pricebck, 
                fut_i.prchg_26w, 
                fut_i.prchg_26w_ann,
                fut_i.price,
                fut_i.divaccmf2q,

                fut_i.prchg_26w + 
                  case 
                  when  fut_i.divaccmf2q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf2q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 pradchg_26w,

                fut_i.prchg_26w + 
                  case 
                  when  fut_i.divaccmf2q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf2q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 * 2 pradchg_26w_ann

         from (
         select psd.dateindex, psd.company_id_unq,

         psd.price::numeric(15,2)/(nullif(psd.prchg_26w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
         psd.prchg_26w::numeric(15,2),
         psd.prchg_26w::numeric(15,2) * 2 prchg_26w_ann,

         coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   divaccmf2q,

         psd.price::numeric(15,2)

         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut_i
         ) fut on cif.dateindex      = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w26 on (true)
         
         
;
-- 5:02



    ---- before lateral queries expected entries ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------

--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.2
-- Dumped by pg_dump version 9.5.2

-- Started on 2016-11-09 18:59:33

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET search_path = sipro_data_store, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 3680 (class 1259 OID 11089484)
-- Name: si_retdate; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE si_retdate (
    dateindex integer,
    dateindexf12meom integer,
    dateindexf12mlwd integer,
    dateindexf09meom integer,
    dateindexf09mlwd integer,
    dateindexf06meom integer,
    dateindexf06mlwd integer,
    dateindexf03meom integer,
    dateindexf03mlwd integer
);


ALTER TABLE si_retdate OWNER TO postgres;

--
-- TOC entry 16801 (class 0 OID 11089484)
-- Dependencies: 3680
-- Data for Name: si_retdate; Type: TABLE DATA; Schema: sipro_data_store; Owner: postgres
--

INSERT INTO si_retdate VALUES (14911, 15278, 15278, 15186, 15184, 15094, 15093, 15005, 15005);
INSERT INTO si_retdate VALUES (14943, 15308, 15308, 15217, 15217, 15125, 15125, 15033, 15033);
INSERT INTO si_retdate VALUES (14974, 15339, 15338, 15247, 15247, 15155, 15155, 15064, 15064);
INSERT INTO si_retdate VALUES (15005, 15370, 15370, 15278, 15278, 15186, 15184, 15094, 15093);
INSERT INTO si_retdate VALUES (15033, 15399, 15399, 15308, 15308, 15217, 15217, 15125, 15125);
INSERT INTO si_retdate VALUES (15064, 15430, 15429, 15339, 15338, 15247, 15247, 15155, 15155);
INSERT INTO si_retdate VALUES (15093, 15460, 15460, 15370, 15370, 15278, 15278, 15186, 15184);
INSERT INTO si_retdate VALUES (15125, 15491, 15491, 15399, 15399, 15308, 15308, 15217, 15217);
INSERT INTO si_retdate VALUES (15155, 15521, 15520, 15430, 15429, 15339, 15338, 15247, 15247);
INSERT INTO si_retdate VALUES (15184, 15552, 15552, 15460, 15460, 15370, 15370, 15278, 15278);
INSERT INTO si_retdate VALUES (15217, 15583, 15583, 15491, 15491, 15399, 15399, 15308, 15308);
INSERT INTO si_retdate VALUES (15247, 15613, 15611, 15521, 15520, 15430, 15429, 15339, 15338);
INSERT INTO si_retdate VALUES (15278, 15644, 15644, 15552, 15552, 15460, 15460, 15370, 15370);
INSERT INTO si_retdate VALUES (15308, 15674, 15674, 15583, 15583, 15491, 15491, 15399, 15399);
INSERT INTO si_retdate VALUES (15338, 15705, 15705, 15613, 15611, 15521, 15520, 15430, 15429);
INSERT INTO si_retdate VALUES (15370, 15736, 15736, 15644, 15644, 15552, 15552, 15460, 15460);
INSERT INTO si_retdate VALUES (15399, 15764, 15764, 15674, 15674, 15583, 15583, 15491, 15491);
INSERT INTO si_retdate VALUES (15429, 15795, 15793, 15705, 15705, 15613, 15611, 15521, 15520);
INSERT INTO si_retdate VALUES (15460, 15825, 15825, 15736, 15736, 15644, 15644, 15552, 15552);
INSERT INTO si_retdate VALUES (15491, 15856, 15856, 15764, 15764, 15674, 15674, 15583, 15583);
INSERT INTO si_retdate VALUES (15520, 15886, 15884, 15795, 15793, 15705, 15705, 15613, 15611);
INSERT INTO si_retdate VALUES (15552, 15917, 15917, 15825, 15825, 15736, 15736, 15644, 15644);
INSERT INTO si_retdate VALUES (15583, 15948, 15947, 15856, 15856, 15764, 15764, 15674, 15674);
INSERT INTO si_retdate VALUES (15611, 15978, 15978, 15886, 15884, 15795, 15793, 15705, 15705);
INSERT INTO si_retdate VALUES (15644, 16009, 16009, 15917, 15917, 15825, 15825, 15736, 15736);
INSERT INTO si_retdate VALUES (15674, 16039, 16038, 15948, 15947, 15856, 15856, 15764, 15764);
INSERT INTO si_retdate VALUES (15705, 16070, 16070, 15978, 15978, 15886, 15884, 15795, 15793);
INSERT INTO si_retdate VALUES (15736, 16101, 16101, 16009, 16009, 15917, 15917, 15825, 15825);
INSERT INTO si_retdate VALUES (15764, 16129, 16129, 16039, 16038, 15948, 15947, 15856, 15856);
INSERT INTO si_retdate VALUES (15793, 16160, 16160, 16070, 16070, 15978, 15978, 15886, 15884);
INSERT INTO si_retdate VALUES (15825, 16190, 16190, 16101, 16101, 16009, 16009, 15917, 15917);
INSERT INTO si_retdate VALUES (15856, 16221, 16220, 16129, 16129, 16039, 16038, 15948, 15947);
INSERT INTO si_retdate VALUES (15884, 16251, 16251, 16160, 16160, 16070, 16070, 15978, 15978);
INSERT INTO si_retdate VALUES (15917, 16282, 16282, 16190, 16190, 16101, 16101, 16009, 16009);
INSERT INTO si_retdate VALUES (15947, 16313, 16311, 16221, 16220, 16129, 16129, 16039, 16038);
INSERT INTO si_retdate VALUES (15978, 16343, 16343, 16251, 16251, 16160, 16160, 16070, 16070);
INSERT INTO si_retdate VALUES (16009, 16374, 16374, 16282, 16282, 16190, 16190, 16101, 16101);
INSERT INTO si_retdate VALUES (16038, 16404, 16402, 16313, 16311, 16221, 16220, 16129, 16129);
INSERT INTO si_retdate VALUES (16070, 16435, 16435, 16343, 16343, 16251, 16251, 16160, 16160);
INSERT INTO si_retdate VALUES (16101, 16466, 16465, 16374, 16374, 16282, 16282, 16190, 16190);
INSERT INTO si_retdate VALUES (16129, 16494, 16493, 16404, 16402, 16313, 16311, 16221, 16220);
INSERT INTO si_retdate VALUES (16160, 16525, 16525, 16435, 16435, 16343, 16343, 16251, 16251);
INSERT INTO si_retdate VALUES (16190, 16555, 16555, 16466, 16465, 16374, 16374, 16282, 16282);
INSERT INTO si_retdate VALUES (16220, 16586, 16584, 16494, 16493, 16404, 16402, 16313, 16311);
INSERT INTO si_retdate VALUES (16251, 16616, 16616, 16525, 16525, 16435, 16435, 16343, 16343);
INSERT INTO si_retdate VALUES (16282, 16647, 16647, 16555, 16555, 16466, 16465, 16374, 16374);
INSERT INTO si_retdate VALUES (16311, 16678, 16678, 16586, 16584, 16494, 16493, 16404, 16402);
INSERT INTO si_retdate VALUES (16343, 16708, 16708, 16616, 16616, 16525, 16525, 16435, 16435);
INSERT INTO si_retdate VALUES (16374, 16739, 16738, 16647, 16647, 16555, 16555, 16466, 16465);
INSERT INTO si_retdate VALUES (16402, 16769, 16769, 16678, 16678, 16586, 16584, 16494, 16493);
INSERT INTO si_retdate VALUES (16435, 16800, 16800, 16708, 16708, 16616, 16616, 16525, 16525);
INSERT INTO si_retdate VALUES (16465, 16831, 16829, 16739, 16738, 16647, 16647, 16555, 16555);
INSERT INTO si_retdate VALUES (16493, 16860, 16860, 16769, 16769, 16678, 16678, 16586, 16584);
INSERT INTO si_retdate VALUES (16525, 16891, 16891, 16800, 16800, 16708, 16708, 16616, 16616);
INSERT INTO si_retdate VALUES (16555, 16921, 16920, 16831, 16829, 16739, 16738, 16647, 16647);
INSERT INTO si_retdate VALUES (16584, 16952, 16952, 16860, 16860, 16769, 16769, 16678, 16678);
INSERT INTO si_retdate VALUES (16616, 16982, 16982, 16891, 16891, 16800, 16800, 16708, 16708);
INSERT INTO si_retdate VALUES (16647, 17013, 17011, 16921, 16920, 16831, 16829, 16739, 16738);
INSERT INTO si_retdate VALUES (16678, 17044, 17044, 16952, 16952, 16860, 16860, 16769, 16769);
INSERT INTO si_retdate VALUES (16708, 17074, 17074, 16982, 16982, 16891, 16891, 16800, 16800);
INSERT INTO si_retdate VALUES (16738, 17105, 17105, 17013, 17011, 16921, 16920, 16831, 16829);
INSERT INTO si_retdate VALUES (16769, 17135, 17135, 17044, 17044, 16952, 16952, 16860, 16860);
INSERT INTO si_retdate VALUES (16800, 17166, 17165, 17074, 17074, 16982, 16982, 16891, 16891);
INSERT INTO si_retdate VALUES (16829, 17197, 17197, 17105, 17105, 17013, 17011, 16921, 16920);
INSERT INTO si_retdate VALUES (16860, 17225, 17225, 17135, 17135, 17044, 17044, 16952, 16952);
INSERT INTO si_retdate VALUES (16891, 17256, 17256, 17166, 17165, 17074, 17074, 16982, 16982);
INSERT INTO si_retdate VALUES (16920, 17286, 17284, 17197, 17197, 17105, 17105, 17013, 17011);
INSERT INTO si_retdate VALUES (16952, 17317, 17317, 17225, 17225, 17135, 17135, 17044, 17044);
INSERT INTO si_retdate VALUES (16982, 17347, 17347, 17256, 17256, 17166, 17165, 17074, 17074);
INSERT INTO si_retdate VALUES (17011, 17378, 17378, 17286, 17284, 17197, 17197, 17105, 17105);
INSERT INTO si_retdate VALUES (17044, 17409, 17409, 17317, 17317, 17225, 17225, 17135, 17135);
INSERT INTO si_retdate VALUES (17074, 17439, 17438, 17347, 17347, 17256, 17256, 17166, 17165);


-- Completed on 2016-11-09 18:59:34

--
-- PostgreSQL database dump complete
--

-----------------------------------------------------------------------
-----------------------------------------------------------------------


-- 52 and 26 and 13 -- without ticker change ( WORKS )
-- HAS CORRECTIONS - ONLY USE THIS ONE

-- for a SINGLE MONTH --
REPLACE
from 
                si_retdate retdate
           join si_ci ci 
WITH
from 
                ( select * from si_retdate where dateindex = 15705 ) retdate
           join ( select * from si_ci where dateindex = 15705 ) ci 
--


set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;

show search_path;

create table sipro_data_store.si_returns as
-- explain --analyze
select  
  retdate.dateindex retdate_dateindex, 
  ( extract( 'epoch' from ( date_trunc('month', to_timestamp(retdate.dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  )) / ( 3600 * 24 ) )::integer retdate_dateindexeom,
  ci.dateindex         now_dateindex,
  ( extract( 'epoch' from ( date_trunc('month', to_timestamp(ci.dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  )) / ( 3600 * 24 ) )::integer now_dateindexeom,
  ci.company_id_unq    now_company_id_unq,
  ci.ticker_unq        now_ticker_unq,
  ci.company           now_company,
  psd.price            now_price,
  w52.dateindex        w52_dateindex,
  ( extract( 'epoch' from ( date_trunc('month', to_timestamp(w52.dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  )) / ( 3600 * 24 ) )::integer w52_dateindexeom,
  w52.company_id_unq   w52_company_id_unq,
  w52.ticker_unq       w52_ticker_unq,
  w52.company          w52_company,
  w52.pricebck         w52_pricebck,
  w52.prchg_52w        w52_prchg_52w,
  w52.prchg_52w_ann    w52_prchg_52w_ann,
  w52.price            w52_price,
  w52.divaccmf4q       w52_divaccmf4q,
  w52.pradchg_52w      w52_pradchg_52w,
  w52.pradchg_52w_ann  w52_pradchg_52w_ann,
  w26.dateindex        w26_dateindex,
  ( extract( 'epoch' from ( date_trunc('month', to_timestamp(w26.dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  )) / ( 3600 * 24 ) )::integer w26_dateindexeom,
  w26.company_id_unq   w26_company_id_unq,
  w26.ticker_unq       w26_ticker_unq,
  w26.company          w26_company,
  w26.pricebck         w26_pricebck,
  w26.prchg_26w        w26_prchg_26w,
  w26.prchg_26w_ann    w26_prchg_26w_ann,
  w26.price            w26_price,
  w26.divaccmf2q       w26_divaccmf2q,
  w26.pradchg_26w      w26_pradchg_26w,
  w26.pradchg_26w_ann  w26_pradchg_26w_ann,
  w13.dateindex        w13_dateindex,
  ( extract( 'epoch' from ( date_trunc('month', to_timestamp(w13.dateindex*3600*24)::date) + interval '1 month' - interval '1 day'  )) / ( 3600 * 24 ) )::integer w13_dateindexeom,
  w13.company_id_unq   w13_company_id_unq,
  w13.ticker_unq       w13_ticker_unq,
  w13.company          w13_company,
  w13.pricebck         w13_pricebck,
  w13.prchg_13w        w13_prchg_13w,
  w13.prchg_13w_ann    w13_prchg_13w_ann,
  w13.price            w13_price,
  w13.divaccmf1q       w13_divaccmf1q,
  w13.pradchg_13w      w13_pradchg_13w,
  w13.pradchg_13w_ann  w13_pradchg_13w_ann
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex = ci.dateindex 
              
      left join si_psd psd
              on ci.dateindex      = psd.dateindex
             and ci.company_id_unq = psd.company_id_unq

      left join lateral ( select cif.dateindex, cif.company_id_unq, cif.ticker_unq, cif.company,
      fut.pricebck,
      fut.prchg_52w,
      fut.prchg_52w_ann,
      fut.price,
      fut.divaccmf4q,
      fut.pradchg_52w,
      fut.pradchg_52w_ann
      from
              
                 si_ci cif 

      left join (
         select fut_i.dateindex, fut_i.company_id_unq, 
                fut_i.pricebck, 
                fut_i.prchg_52w, 
                fut_i.prchg_52w_ann,
                fut_i.price,
                fut_i.divaccmf4q,

                fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 pradchg_52w,

              ( fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 ) * 1 pradchg_52w_ann

         from (
         select psd.dateindex, psd.company_id_unq,

         psd.price::numeric(15,2)/(nullif(psd.prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
         psd.prchg_52w::numeric(15,2),
         psd.prchg_52w::numeric(15,2) * 1 prchg_52w_ann,

         coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q,

         psd.price::numeric(15,2)

         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut_i
         ) fut on cif.dateindex      = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)

      left join lateral ( select cif.dateindex, cif.company_id_unq, cif.ticker_unq, cif.company,
      fut.pricebck,
      fut.prchg_26w,
      fut.prchg_26w_ann,
      fut.price,
      fut.divaccmf2q,
      fut.pradchg_26w,
      fut.pradchg_26w_ann
      from
              
                 si_ci cif 

      left join (
         select fut_i.dateindex, fut_i.company_id_unq, 
                fut_i.pricebck, 
                fut_i.prchg_26w, 
                fut_i.prchg_26w_ann,
                fut_i.price,
                fut_i.divaccmf2q,

                fut_i.prchg_26w + 
                  case 
                  when  fut_i.divaccmf2q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf2q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 pradchg_26w,

              ( fut_i.prchg_26w + 
                  case 
                  when  fut_i.divaccmf2q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf2q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 ) * 2  pradchg_26w_ann

         from (
         select psd.dateindex, psd.company_id_unq,

         psd.price::numeric(15,2)/(nullif(psd.prchg_26w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
         psd.prchg_26w::numeric(15,2),
         psd.prchg_26w::numeric(15,2) * 2 prchg_26w_ann,

         coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   divaccmf2q,

         psd.price::numeric(15,2)

         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut_i
         ) fut on cif.dateindex      = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w26 on (true)
         
      left join lateral ( select cif.dateindex, cif.company_id_unq, cif.ticker_unq, cif.company,
      fut.pricebck,
      fut.prchg_13w,
      fut.prchg_13w_ann,
      fut.price,
      fut.divaccmf1q,
      fut.pradchg_13w,
      fut.pradchg_13w_ann
      from
              
                 si_ci cif 

      left join (
         select fut_i.dateindex, fut_i.company_id_unq, 
                fut_i.pricebck, 
                fut_i.prchg_13w, 
                fut_i.prchg_13w_ann,
                fut_i.price,
                fut_i.divaccmf1q,

                fut_i.prchg_13w + 
                  case 
                  when  fut_i.divaccmf1q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf1q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 pradchg_13w,

              ( fut_i.prchg_13w + 
                  case 
                  when  fut_i.divaccmf1q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf1q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 ) * 4 pradchg_13w_ann

         from (
         select psd.dateindex, psd.company_id_unq,

         psd.price::numeric(15,2)/(nullif(psd.prchg_13w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
         psd.prchg_13w::numeric(15,2),
         psd.prchg_13w::numeric(15,2) * 4 prchg_13w_ann,

         coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   divaccmf1q,

         psd.price::numeric(15,2)

         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut_i
         ) fut on cif.dateindex      = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where ci.company_id_unq       = cif.company_id_unq
         and retdate.dateindexf12mlwd  = cif.dateindex

         ) w13 on (true)
order by 2,1  -- retdate_dateindexoem, retdate_dateindex
;
-- 5:45
-- 10:42 COLD START


-- Index: sipro_data_store.si_returns_now_company_id_unq_idx

-- DROP INDEX sipro_data_store.si_returns_now_company_id_unq_idx;

CREATE INDEX si_returns_now_company_id_unq_idx
  ON sipro_data_store.si_returns
  USING btree
  (now_company_id_unq COLLATE pg_catalog."default");

-- Index: sipro_data_store.si_returns_now_ticker_unq_idx

-- DROP INDEX sipro_data_store.si_returns_now_ticker_unq_idx;

CREATE INDEX si_returns_now_ticker_unq_idx
  ON sipro_data_store.si_returns
  USING btree
  (now_ticker_unq COLLATE pg_catalog."default");

-- Index: sipro_data_store.si_returns_retdate_dateindex_idx

-- DROP INDEX sipro_data_store.si_returns_retdate_dateindex_idx;

CREATE INDEX si_returns_retdate_dateindex_idx
  ON sipro_data_store.si_returns
  USING btree
  (retdate_dateindex);

-- Index: sipro_data_store.si_returns_retdate_dateindexeom_idx

-- DROP INDEX sipro_data_store.si_returns_retdate_dateindexeom_idx;

CREATE INDEX si_returns_retdate_dateindexeom_idx
  ON sipro_data_store.si_returns
  USING btree
  (retdate_dateindexeom);

-- Index: sipro_data_store.si_returns_now_dateindex_idx

-- DROP INDEX sipro_data_store.si_returns_now_dateindex_idx;

CREATE INDEX si_returns_now_dateindex_idx
  ON sipro_data_store.si_returns
  USING btree
  (now_dateindex);

-- Index: sipro_data_store.si_returns_now_dateindexeom_idx

-- DROP INDEX sipro_data_store.si_returns_now_dateindexeom_idx;

CREATE INDEX si_returns_now_dateindexeom_idx
  ON sipro_data_store.si_returns
  USING btree
  (now_dateindexeom);

-- 25 seconds

        -- ON sipro_data_store.si_ci
        --
        -- NEED: partial index company not like '%iShares%';            [ ]
        -- NEED: partial index psd.mktcap::numeric(15,2) >=   25        [ ]
        -- NEED: partial index psd.mktcap::numeric(15,2) >=  200        [ ]
        -- NEED: partial index psd.mktcap::numeric(15,2) >= 1600        [ ]
        -- NEED: partial index psd.mktcap::numeric(15,2)  < 1600 and psd.mktcap::numeric(15,2) >=  200 [ ]
        -- NEED: partial index psd.mktcap::numeric(15,2)  <  200 and psd.mktcap::numeric(15,2) >=   25 [ ]
-- DONE AREA - SEE -- partitions.SCRATCH

-- with ticker change ( IN PROGRESS ) x CAN NOT OPTIMIZE ( FOR OLDER DATA: am using A PATCH )

-- NEED: sipro_data_store, OTHER q**_tables [X]

set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;

explain --analyze
select retdate.dateindex retdate_dateindex, 
  ci.dateindex         now_dateindex,
  ci.company_id_unq    now_company_id_unq,
  ci.ticker            now_ticker,
  ci.company           now_company,
  psd.price            now_price,
  w52.dateindex        w52_dateindex,
  w52.company_id_unq   w52_company_id_unq,
  w52.ticker           w52_ticker,
  w52.company          w52_company,
  w52.pricebck         w52_pricebck,
  w52.prchg_52w        w52_prchg_52w,
  w52.prchg_52w_ann    w52_prchg_52w_ann,
  w52.price            w52_price,
  w52.divaccmf4q       w52_divaccmf4q,
  w52.pradchg_52w      w52_pradchg_52w,
  w52.pradchg_52w_ann  w52_pradchg_52w_ann
from 
                si_retdate retdate
           join si_ci ci 
              on retdate.dateindex = ci.dateindex 
              
      left join si_psd psd
              on ci.dateindex      = psd.dateindex
             and ci.company_id_unq = psd.company_id_unq

      left join lateral ( select cif.dateindex, cif.company_id_unq, cif.ticker, cif.company,
      fut.pricebck,
      fut.prchg_52w,
      fut.prchg_52w_ann,
      fut.price,
      fut.divaccmf4q,
      fut.pradchg_52w,
      fut.pradchg_52w_ann
      from
              
                 si_ci cif 

      left join (
         select fut_i.dateindex, fut_i.company_id_unq, 
                fut_i.pricebck, 
                fut_i.prchg_52w, 
                fut_i.prchg_52w_ann,
                fut_i.price,
                fut_i.divaccmf4q,

                fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 pradchg_52w,

                fut_i.prchg_52w + 
                  case 
                  when  fut_i.divaccmf4q = 0.00::numeric(15,2) then 0.00 
                  else (fut_i.divaccmf4q / nullif(fut_i.pricebck,0.00::numeric(15,2))) 
                  end * 100 * 1 pradchg_52w_ann

         from (
         select psd.dateindex, psd.company_id_unq,

         psd.price::numeric(15,2)/(nullif(psd.prchg_52w::numeric(15,2),-100.00::numeric(15,2))/100.00::numeric(15,2) + 1.00) pricebck,
         psd.prchg_52w::numeric(15,2),
         psd.prchg_52w::numeric(15,2) * 1 prchg_52w_ann,

         coalesce(isq.dps_q1::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q2::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q3::numeric(15,2),0.00::numeric(15,2))   + 
         coalesce(isq.dps_q4::numeric(15,2),0.00::numeric(15,2))   divaccmf4q,

         psd.price::numeric(15,2)

         from
         si_isq isq full outer join si_psd psd 
         on  isq.company_id_unq = psd.company_id_unq
         and isq.dateindex      = psd.dateindex
         ) fut_i
         ) fut on cif.dateindex      = fut.dateindex 
              and cif.company_id_unq = fut.company_id_unq 

         where 


           -- can not optimize
           -- "  Join Filter: CASE WHEN (ci.dateindex > 155155) THEN (ci.company_id_unq = cif.company_id_unq) ELSE (ci.ticker_unq = cif.ticker_unq) END"
           -- ??               -- >  15155 --> explodes
           --case when ci.dateindex > 0
           --then ci.company_id_unq  = cif.company_id_unq 
           --else ci.ticker_unq      = cif.ticker_unq
           --end

           case when ci.dateindex > 0
           then ci.company_id_unq 
           else ci.ticker_unq
           end 
           =
           case when ci.dateindex > 0
           then cif.company_id_unq
           else cif.ticker_unq
           end 

           -- 1:25
           -- ci.company_id_unq = cif.company_id_unq

           -- 2:04
           -- ci.ticker_unq = cif.ticker_unq
           
           and retdate.dateindexf12mlwd  = cif.dateindex

         ) w52 on (true)

;


select ci.dateindex, ci.company_id_unq, ci_f.company_id_unq, ci_f.ticker_unq
from sipro_data_store.si_ci ci_f, sipro_data_store.si_ci ci
where ci.ticker_unq  = ci_f.ticker_unq
and   ci.dateindex   in (
                         15125,
                         15155
                        )
and   ci_f.dateindex = 15184 and ci_f.ticker_unq = 'AAPL';



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



show search_path;


----

alter table sipro_data_store.si_ci add column company_id_unq_orig text;
--DONE [X]

update sipro_data_store.si_ci set company_id_unq_orig = company_id_unq;
--IN PROGRESS - TIME CONSUMING

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

analyze sipro_data_store.si_ci.si_ci(company_id_unq);

----


--- STORE WITH THE OTHERS [x]
---
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

analyze sipro_data_store.si_ci(company_id_unq);
-- 1 second


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

analyze sipro_data_store.si_psd(company_id_unq);


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

analyze sipro_data_store.si_isq(company_id_unq);


----
----  END company_id_unq patch  ---------
-----------------------------------------

----
----

HAVE sipro_data_store

_ci
_isq
_psd

SHOULD GET
https://github.com/AndreMikulec/expressions/blob/master/main-foresight3-999.R

setup     - SKIP FOR NOW 

si_mgdsc - DONE [X]

si_exchg - DONE [X]



si_psdd - DONE [X]



si_psdc - DONE [X]

si_date - DONE [X]

si_bsq - DONE [X]

si_cfq - DONE [X]

si_mlt - DONE [X]

AND
si_rat - DONE [X]

----
----

set search_path to sipro_data_store,sipro_stage;
set time zone 'utc';
set work_mem to '1200MB';
set constraint_exclusion = on;

----

  create table sipro_data_store.si_mgdsc as select * from sipro_stage.si_mgdsc;
  -- 19 seconds

  -- index: sipro_data_store.si_mgdsc_dateindex_idx

  -- drop index sipro_data_store.si_mgdsc_dateindex_idx;

  create index si_mgdsc_dateindex_idx
    on sipro_data_store.si_mgdsc
    using btree
    (dateindex);

  -- index: sipro_data_store.si_mgdsc_dateindexeom_idx

  -- drop index sipro_data_store.si_mgdsc_dateindexeom_idx;

  create index si_mgdsc_dateindexeom_idx
    on sipro_data_store.si_mgdsc
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_mgdsc_mg_code_idx

  -- drop index sipro_data_store.si_mgdsc_mg_code_idx;

  create index si_mgdsc_mg_code_idx
    on sipro_data_store.si_mgdsc
    using btree
    (mg_code);

  -- index: sipro_data_store.si_mgdsc_mg_desc_idx

  -- drop index sipro_data_store.si_mgdsc_mg_desc_idx;

  create index si_mgdsc_mg_desc_idx
    on sipro_data_store.si_mgdsc
    using btree
    (mg_desc);

  -- 1 seconds

----

  create table sipro_data_store.si_exchg as select * from sipro_stage.si_exchg;
  -- 1 seconds

  -- index: sipro_data_store.si_exchg_dateindex_idx

  -- drop index sipro_data_store.si_exchg_dateindex_idx;

  create index si_exchg_dateindex_idx
    on sipro_data_store.si_exchg
    using btree
    (dateindex);

  -- index: sipro_data_store.si_exchg_dateindexeom_idx

  -- drop index sipro_data_store.si_exchg_dateindexeom_idx;

  create index si_exchg_dateindexeom_idx
    on sipro_data_store.si_exchg
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_exchg_exchg_code_idx

  -- drop index sipro_data_store.si_exchg_exchg_code_idx;

  create index si_exchg_exchg_code_idx
    on sipro_data_store.si_exchg
    using btree
    (exchg_code);

  -- index: sipro_data_store.si_exchg_exchg_desc_idx

  -- drop index sipro_data_store.si_exchg_exchg_desc_idx;

  create index si_exchg_exchg_desc_idx
    on sipro_data_store.si_exchg
    using btree
    (exchg_desc);

  -- 1 seconds

----

  create table sipro_data_store.si_psdd as select * from sipro_stage.si_psdd;
  -- 101 seconds

  -- index: sipro_data_store.si_psdd_company_id_idx

  -- drop index sipro_data_store.si_psdd_company_id_idx;

  create index si_psdd_company_id_idx
    on sipro_data_store.si_psdd
    using btree
    (company_id);

  -- index: sipro_data_store.si_psdd_company_id_unq_idx

  -- drop index sipro_data_store.si_psdd_company_id_unq_idx;

  create index si_psdd_company_id_unq_idx
    on sipro_data_store.si_psdd
    using btree
    (company_id_unq);

  -- index: sipro_data_store.si_psdd_dateindex_idx

  -- drop index sipro_data_store.si_psdd_dateindex_idx;

  create index si_psdd_dateindex_idx
    on sipro_data_store.si_psdd
    using btree
    (dateindex);

  -- index: sipro_data_store.si_psdd_dateindexeom_idx

  -- drop index sipro_data_store.si_psdd_dateindexeom_idx;

  create index si_psdd_dateindexeom_idx
    on sipro_data_store.si_psdd
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_psdd_ticker_unq_idx

  -- drop index sipro_data_store.si_psdd_ticker_unq_idx;

  create index si_psdd_ticker_unq_idx
    on sipro_data_store.si_psdd
    using btree
    (ticker_unq);

  --  42 seconds

----


  create table sipro_data_store.si_psdc as select * from sipro_stage.si_psdc;
  -- 120 seconds

  -- index: sipro_data_store.si_psdc_company_id_idx

  -- drop index sipro_data_store.si_psdc_company_id_idx;

  create index si_psdc_company_id_idx
    on sipro_data_store.si_psdc
    using btree
    (company_id);

  -- index: sipro_data_store.si_psdc_company_id_unq_idx

  -- drop index sipro_data_store.si_psdc_company_id_unq_idx;

  create index si_psdc_company_id_unq_idx
    on sipro_data_store.si_psdc
    using btree
    (company_id_unq);

  -- index: sipro_data_store.si_psdc_dateindex_idx

  -- drop index sipro_data_store.si_psdc_dateindex_idx;

  create index si_psdc_dateindex_idx
    on sipro_data_store.si_psdc
    using btree
    (dateindex);

  -- index: sipro_data_store.si_psdc_dateindexeom_idx

  -- drop index sipro_data_store.si_psdc_dateindexeom_idx;

  create index si_psdc_dateindexeom_idx
    on sipro_data_store.si_psdc
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_psdc_ticker_unq_idx

  -- drop index sipro_data_store.si_psdc_ticker_unq_idx;

  create index si_psdc_ticker_unq_idx
    on sipro_data_store.si_psdc
    using btree
    (ticker_unq);

  -- 51 seconds

----

  create table sipro_data_store.si_date as select * from sipro_stage.si_date;
  -- ?? seconds

  -- index: sipro_data_store.si_date_company_id_idx

  -- drop index sipro_data_store.si_date_company_id_idx;

  create index si_date_company_id_idx
    on sipro_data_store.si_date
    using btree
    (company_id);

  -- index: sipro_data_store.si_date_company_id_unq_idx

  -- drop index sipro_data_store.si_date_company_id_unq_idx;

  create index si_date_company_id_unq_idx
    on sipro_data_store.si_date
    using btree
    (company_id_unq);

  -- index: sipro_data_store.si_date_dateindex_idx

  -- drop index sipro_data_store.si_date_dateindex_idx;

  create index si_date_dateindex_idx
    on sipro_data_store.si_date
    using btree
    (dateindex);

  -- index: sipro_data_store.si_date_dateindexeom_idx

  -- drop index sipro_data_store.si_date_dateindexeom_idx;

  create index si_date_dateindexeom_idx
    on sipro_data_store.si_date
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_date_ticker_unq_idx

  -- drop index sipro_data_store.si_date_ticker_unq_idx;

  create index si_date_ticker_unq_idx
    on sipro_data_store.si_date
    using btree
    (ticker_unq);

  -- 44 seconds

----

  create table sipro_data_store.si_bsq as select * from sipro_stage.si_bsq;
  -- 180 seconds

  -- index: sipro_data_store.si_bsq_company_id_idx

  -- drop index sipro_data_store.si_bsq_company_id_idx;

  create index si_bsq_company_id_idx
    on sipro_data_store.si_bsq
    using btree
    (company_id);

  -- index: sipro_data_store.si_bsq_company_id_unq_idx

  -- drop index sipro_data_store.si_bsq_company_id_unq_idx;

  create index si_bsq_company_id_unq_idx
    on sipro_data_store.si_bsq
    using btree
    (company_id_unq);

  -- index: sipro_data_store.si_bsq_dateindex_idx

  -- drop index sipro_data_store.si_bsq_dateindex_idx;

  create index si_bsq_dateindex_idx
    on sipro_data_store.si_bsq
    using btree
    (dateindex);

  -- index: sipro_data_store.si_bsq_dateindexeom_idx

  -- drop index sipro_data_store.si_bsq_dateindexeom_idx;

  create index si_bsq_dateindexeom_idx
    on sipro_data_store.si_bsq
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_bsq_ticker_unq_idx

  -- drop index sipro_data_store.si_bsq_ticker_unq_idx;

  create index si_bsq_ticker_unq_idx
    on sipro_data_store.si_bsq
    using btree
    (ticker_unq);

  -- 60 seconds

----

  create table sipro_data_store.si_cfq as select * from sipro_stage.si_cfq;
  -- 70 seconds

  -- index: sipro_data_store.si_cfq_company_id_idx

  -- drop index sipro_data_store.si_cfq_company_id_idx;

  create index si_cfq_company_id_idx
    on sipro_data_store.si_cfq
    using btree
    (company_id);

  -- index: sipro_data_store.si_cfq_company_id_unq_idx

  -- drop index sipro_data_store.si_cfq_company_id_unq_idx;

  create index si_cfq_company_id_unq_idx
    on sipro_data_store.si_cfq
    using btree
    (company_id_unq);

  -- index: sipro_data_store.si_cfq_dateindex_idx

  -- drop index sipro_data_store.si_cfq_dateindex_idx;

  create index si_cfq_dateindex_idx
    on sipro_data_store.si_cfq
    using btree
    (dateindex);

  -- index: sipro_data_store.si_cfq_dateindexeom_idx

  -- drop index sipro_data_store.si_cfq_dateindexeom_idx;

  create index si_cfq_dateindexeom_idx
    on sipro_data_store.si_cfq
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_cfq_ticker_unq_idx

  -- drop index sipro_data_store.si_cfq_ticker_unq_idx;

  create index si_cfq_ticker_unq_idx
    on sipro_data_store.si_cfq
    using btree
    (ticker_unq);

  -- 30 seconds

----

  create table sipro_data_store.si_mlt as select * from sipro_stage.si_mlt;
  -- 61 seconds

  -- index: sipro_data_store.si_mlt_company_id_idx

  -- drop index sipro_data_store.si_mlt_company_id_idx;

  create index si_mlt_company_id_idx
    on sipro_data_store.si_mlt
    using btree
    (company_id);

  -- index: sipro_data_store.si_mlt_company_id_unq_idx

  -- drop index sipro_data_store.si_mlt_company_id_unq_idx;

  create index si_mlt_company_id_unq_idx
    on sipro_data_store.si_mlt
    using btree
    (company_id_unq);

  -- index: sipro_data_store.si_mlt_dateindex_idx

  -- drop index sipro_data_store.si_mlt_dateindex_idx;

  create index si_mlt_dateindex_idx
    on sipro_data_store.si_mlt
    using btree
    (dateindex);

  -- index: sipro_data_store.si_mlt_dateindexeom_idx

  -- drop index sipro_data_store.si_mlt_dateindexeom_idx;

  create index si_mlt_dateindexeom_idx
    on sipro_data_store.si_mlt
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_mlt_ticker_unq_idx

  -- drop index sipro_data_store.si_mlt_ticker_unq_idx;

  create index si_mlt_ticker_unq_idx
    on sipro_data_store.si_mlt
    using btree
    (ticker_unq);

  -- 32 seconds

----

  create table sipro_data_store.si_rat as select * from sipro_stage.si_rat;
  -- 84 seconds

  -- index: sipro_data_store.si_rat_company_id_idx

  -- drop index sipro_data_store.si_rat_company_id_idx;

  create index si_rat_company_id_idx
    on sipro_data_store.si_rat
    using btree
    (company_id);

  -- index: sipro_data_store.si_rat_company_id_unq_idx

  -- drop index sipro_data_store.si_rat_company_id_unq_idx;

  create index si_rat_company_id_unq_idx
    on sipro_data_store.si_rat
    using btree
    (company_id_unq);

  -- index: sipro_data_store.si_rat_dateindex_idx

  -- drop index sipro_data_store.si_rat_dateindex_idx;

  create index si_rat_dateindex_idx
    on sipro_data_store.si_rat
    using btree
    (dateindex);

  -- index: sipro_data_store.si_rat_dateindexeom_idx

  -- drop index sipro_data_store.si_rat_dateindexeom_idx;

  create index si_rat_dateindexeom_idx
    on sipro_data_store.si_rat
    using btree
    (dateindexeom);

  -- index: sipro_data_store.si_rat_ticker_unq_idx

  -- drop index sipro_data_store.si_rat_ticker_unq_idx;

  create index si_rat_ticker_unq_idx
    on sipro_data_store.si_rat
    using btree
    (ticker_unq);

  -- 39 seconds


---- [x] NEED TO PUT INTO AN 'R' FUNCTION



----
----



  alter table sipro_data_store.si_psdd add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_psdd set company_id_unq_orig = company_id_unq;
  -- 05:33 minutes 

  update sipro_data_store.si_psdd psdd 
  set company_id_unq = psdd_f.company_id_unq
  from sipro_data_store.si_psdd psdd_f
  where psdd.ticker_unq = psdd_f.ticker_unq
  and   psdd.dateindex   in (
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
  and psdd_f.dateindex = 15184;
  -- 28  seconds



  alter table sipro_data_store.si_psdc add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_psdc set company_id_unq_orig = company_id_unq;
  -- 07:54  minutes

  update sipro_data_store.si_psdc psdc 
  set company_id_unq = psdc_f.company_id_unq
  from sipro_data_store.si_psdc psdc_f
  where psdc.ticker_unq = psdc_f.ticker_unq
  and   psdc.dateindex   in (
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
  and psdc_f.dateindex = 15184;
  -- 01:51 minutes



  alter table sipro_data_store.si_date add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_date set company_id_unq_orig = company_id_unq;
  --  19.8 secs

  update sipro_data_store.si_date si_date 
  set company_id_unq = si_date_f.company_id_unq
  from sipro_data_store.si_date si_date_f
  where si_date.ticker_unq = si_date_f.ticker_unq
  and   si_date.dateindex   in (
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
  and si_date_f.dateindex = 15184;
  -- 01:37 minutes



  alter table sipro_data_store.si_bsq add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_bsq set company_id_unq_orig = company_id_unq;
  -- 8:45 minutes

  update sipro_data_store.si_bsq si_bsq 
  set company_id_unq = si_bsq_f.company_id_unq
  from sipro_data_store.si_bsq si_bsq_f
  where si_bsq.ticker_unq = si_bsq_f.ticker_unq
  and   si_bsq.dateindex   in (
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
  and si_bsq_f.dateindex = 15184;
  -- 1:37 minutes



  alter table sipro_data_store.si_cfq add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_cfq set company_id_unq_orig = company_id_unq;
  -- 04:22 minutes

  update sipro_data_store.si_cfq si_cfq 
  set company_id_unq = si_cfq_f.company_id_unq
  from sipro_data_store.si_cfq si_cfq_f
  where si_cfq.ticker_unq = si_cfq_f.ticker_unq
  and   si_cfq.dateindex   in (
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
  and si_cfq_f.dateindex = 15184;
  --  1:55 minutes



  alter table sipro_data_store.si_mlt add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_mlt set company_id_unq_orig = company_id_unq;
  -- 5:18 minutes

  update sipro_data_store.si_mlt si_mlt 
  set company_id_unq = si_mlt_f.company_id_unq
  from sipro_data_store.si_mlt si_mlt_f
  where si_mlt.ticker_unq = si_mlt_f.ticker_unq
  and   si_mlt.dateindex   in (
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
  and si_mlt_f.dateindex = 15184;
  --  1:11 minutes



  alter table sipro_data_store.si_rat add column company_id_unq_orig text;
  -- instantaneously

  update sipro_data_store.si_rat set company_id_unq_orig = company_id_unq;
  -- 4:56 minutes

  update sipro_data_store.si_rat si_rat 
  set company_id_unq = si_rat_f.company_id_unq
  from sipro_data_store.si_rat si_rat_f
  where si_rat.ticker_unq = si_rat_f.ticker_unq
  and   si_rat.dateindex   in (
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
  and si_rat_f.dateindex = 15184;
  -- 1:38 minutes




---- [X] NEED partial index si_ci.sp sp = '500'


  -- index: sipro_data_store.si_ci_sp_idx

  -- drop index sipro_data_store.si_ci_sp_idx;

  create index si_ci_sp_idx
    on sipro_data_store.si_ci
    using btree
    (sp);

  -- 7 seconds

  -- index: sipro_data_store.si_ci_sp_500_idx

  -- drop index sipro_data_store.si_ci_sp_500_idx;

  create index si_ci_sp_500_idx
    on sipro_data_store.si_ci
    using btree
    (sp) 
    where sp = '500';

  -- 1 second

---- [X] NEED TO PUT INTO AN 'R' FUNCTION

-- NEXT TO DO
-- SEE ** BOTTOM OF : 1607.sql.txt
