

------------- begin sipro.sql ----------------

--show search_path;

--set search_path=sipro_stage,"$user", public;

set search_path="$user", public;

--select count(*) from sipro_stage.alsmast
--union
select count(*) from sipro_stage.alsmast_14911;

--select count(*) from alsmast;

-- 

--

show search_path;
--"$user", public

set search_path to sipro_data_store,"$user", public;

-- attributes_profiles --

--drop table attributes_cleans_profiles cascade;
--drop table attributes_profiles_dependents cascade;
--drop table attributes_cleans_profiles cascade;
--drop table attributes_cleans cascade;

-- attributes_profiles --

--drop table attributes_profiles;

create table attributes_profiles
(
  attributes_profiles_id smallint not null,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_expression character varying(253),
  attributes_cleans_profiles_id smallint not null,
  table_nm character varying(63),
  column_nm character varying(63)
)
with (
  oids=false
);
alter table attributes_profiles
  owner to postgres;

--drop index attributes_profiles_pk_idx;

create unique index attributes_profiles_pk_idx
  on attributes_profiles
  using btree
  (attributes_profiles_id);
  
--alter table attributes_profiles drop constraint attributes_profiles_pk;

alter table attributes_profiles
  add constraint attributes_profiles_pk primary key (attributes_profiles_id);

--alter table table attributes_profiles drop constraint attributes_cleans_profiles_fk;

alter table attributes_profiles
  add constraint attributes_cleans_profiles_fk 
    foreign key (attributes_cleans_profiles_id) references attributes_cleans_profiles(attributes_cleans_profiles_id) 
      on delete cascade on update cascade;

--set search_path to "$user", public;

--

-- attributes_profiles_dependents

-- drop table attributes_profiles_dependents;

create table attributes_profiles_dependents
(
  attributes_profiles_dependents_id smallint not null,
  timeends_range character varying(63),
  last_modified_date double precision,
  dependent_id smallint not null,
  superior_id  smallint not null
)
with (
  oids=false
);
alter table attributes_profiles_dependents
  owner to postgres;

--drop index attributes_profiles_dependents_pk_idx;

create unique index attributes_profiles_dependents_pk_idx
  on attributes_profiles_dependents
  using btree
  (attributes_profiles_dependents_id);
  
--alter table attributes_profiles_dependents drop constraint attributes_profiles_dependents_pk;

alter table attributes_profiles_dependents
  add constraint attributes_profiles_dependents_pk primary key (attributes_profiles_dependents_id);

--alter table attributes_profiles_dependents drop constraint superior_id_fk;

alter table  attributes_profiles_dependents
  add constraint superior_id_fk 
    foreign key (superior_id) references attributes_profiles(attributes_profiles_id) 
      on delete cascade on update cascade;

--alter table attributes_profiles_dependents drop constraint dependent_id_fk;

alter table  attributes_profiles_dependents
  add constraint dependent_id_fk 
    foreign key (dependent_id) references attributes_profiles(attributes_profiles_id) 
      on delete cascade on update cascade;

--

-- attributes_cleans_profiles

-- drop table attributes_cleans_profiles;

create table attributes_cleans_profiles
(
  attributes_cleans_profiles_id smallint not null,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_cleans_profiles_nm character varying(63)
)
with (
  oids=false
);
alter table attributes_cleans_profiles
  owner to postgres;

--drop index attributes_cleans_profiles_pk_idx;

create unique index attributes_cleans_profiles_pk_idx
  on attributes_cleans_profiles
  using btree
  (attributes_cleans_profiles_id);
  
--alter table attributes_cleans_profiles drop constraint attributes_cleans_profiles_pk;

alter table attributes_cleans_profiles
  add constraint attributes_cleans_profiles_pk primary key (attributes_cleans_profiles_id);

--

-- attributes_cleans

-- drop table attributes_cleans;

create table attributes_cleans
(
  attributes_cleans_id smallint not null,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_cleans_nm  character varying(63)
)
with (
  oids=false
);
alter table attributes_cleans
  owner to postgres;

--drop index attributes_cleans_pk_idx;

create unique index attributes_cleans_pk_idx
  on attributes_cleans
  using btree
  (attributes_cleans_id);
  
--alter table attributes_cleans drop constraint attributes_cleans_pk;

alter table attributes_cleans
  add constraint attributes_cleans_pk primary key (attributes_cleans_id);

-- attributes_cleans_profiles_details

-- drop table attributes_cleans_profiles_details;

create table attributes_cleans_profiles_details
(
  attributes_cleans_profiles_details_id smallint not null,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_cleans_profiles_id smallint not null,
  order_id smallint not null,
  attributes_cleans_id smallint not null
)
with (
  oids=false
);
alter table attributes_cleans_profiles_details
  owner to postgres;

--drop index attributes_cleans_profiles_details_pk_idx;

create unique index attributes_cleans_profiles_details_pk_idx
  on attributes_cleans_profiles_details
  using btree
  (attributes_cleans_profiles_details_id);
  
--alter table attributes_cleans_profiles_details drop constraint attributes_cleans_profiles_details_pk;

alter table attributes_cleans_profiles_details
  add constraint attributes_cleans_profiles_details_pk primary key (attributes_cleans_profiles_details_id);

--alter table attributes_cleans_profiles_details drop constraint attributes_cleans_profiles_fk2;

alter table attributes_cleans_profiles_details
  add constraint attributes_cleans_profiles_fk2 
    foreign key (attributes_cleans_profiles_id) references attributes_cleans_profiles(attributes_cleans_profiles_id) 
      on delete cascade on update cascade;

--alter table attributes_cleans_profiles_details drop constraint attributes_cleans_id_fk;

alter table attributes_cleans_profiles_details
  add constraint attributes_cleans_id_fk 
    foreign key (attributes_cleans_id) references attributes_cleans(attributes_cleans_id) 
      on delete cascade on update cascade;

--


--drop table attributes_cleans_profiles_actuals;

create table attributes_cleans_profiles_actuals
(
  attributes_cleans_profiles_actuals_id smallint not null,
  timeends character varying(63),
  last_modified_date double precision,
  attributes_cleans_profiles_id smallint not null
)
with (
  oids=false
);
alter table attributes_cleans_profiles_actuals
  owner to postgres;

--drop index attributes_cleans_profiles_actuals_pk_idx;

create unique index attributes_cleans_profiles_actuals_pk_idx
  on attributes_cleans_profiles_actuals
  using btree
  (attributes_cleans_profiles_actuals_id);
  
--alter table attributes_cleans_profiles_actuals drop constraint attributes_cleans_profiles_actuals_pk;

alter table attributes_cleans_profiles_actuals
  add constraint attributes_cleans_profiles_actuals_pk primary key (attributes_cleans_profiles_actuals_id);

--alter table table attributes_cleans_profiles_actuals drop constraint attributes_cleans_profiles_fk;

alter table attributes_cleans_profiles_actuals
  add constraint attributes_cleans_profiles_fk 
    foreign key (attributes_cleans_profiles_id) references attributes_cleans_profiles(attributes_cleans_profiles_id) 
      on delete cascade on update cascade;

--set search_path to "$user", public;

----
---- TABLE CREATION not in ORDER

-- Schema: sipro_data_store

-- DROP SCHEMA sipro_data_store;

CREATE SCHEMA sipro_data_store
  AUTHORIZATION postgres;

COMMENT ON SCHEMA sipro_data_store
  IS 'aaii stockinvestor pro data store';

-- Table: sipro_data_store.attributes_cleans

-- DROP TABLE sipro_data_store.attributes_cleans;

CREATE TABLE sipro_data_store.attributes_cleans
(
  attributes_cleans_id smallint NOT NULL,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_cleans_nm character varying(63),
  CONSTRAINT attributes_cleans_pk PRIMARY KEY (attributes_cleans_id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sipro_data_store.attributes_cleans
  OWNER TO postgres;

-- Index: sipro_data_store.attributes_cleans_pk_idx

-- DROP INDEX sipro_data_store.attributes_cleans_pk_idx;

CREATE UNIQUE INDEX attributes_cleans_pk_idx
  ON sipro_data_store.attributes_cleans
  USING btree
  (attributes_cleans_id);

-- Table: sipro_data_store.attributes_cleans_profiles

-- DROP TABLE sipro_data_store.attributes_cleans_profiles;

CREATE TABLE sipro_data_store.attributes_cleans_profiles
(
  attributes_cleans_profiles_id smallint NOT NULL,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_cleans_profiles_nm character varying(63),
  CONSTRAINT attributes_cleans_profiles_pk PRIMARY KEY (attributes_cleans_profiles_id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sipro_data_store.attributes_cleans_profiles
  OWNER TO postgres;

-- Index: sipro_data_store.attributes_cleans_profiles_pk_idx

-- DROP INDEX sipro_data_store.attributes_cleans_profiles_pk_idx;

CREATE UNIQUE INDEX attributes_cleans_profiles_pk_idx
  ON sipro_data_store.attributes_cleans_profiles
  USING btree
  (attributes_cleans_profiles_id);

-- Table: sipro_data_store.attributes_cleans_profiles_actuals

-- DROP TABLE sipro_data_store.attributes_cleans_profiles_actuals;

CREATE TABLE sipro_data_store.attributes_cleans_profiles_actuals
(
  attributes_cleans_profiles_actuals_id smallint NOT NULL,
  timeends character varying(63),
  last_modified_date double precision,
  attributes_cleans_profiles_id smallint NOT NULL,
  CONSTRAINT attributes_cleans_profiles_actuals_pk PRIMARY KEY (attributes_cleans_profiles_actuals_id),
  CONSTRAINT attributes_cleans_profiles_fk FOREIGN KEY (attributes_cleans_profiles_id)
      REFERENCES sipro_data_store.attributes_cleans_profiles (attributes_cleans_profiles_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sipro_data_store.attributes_cleans_profiles_actuals
  OWNER TO postgres;

-- Index: sipro_data_store.attributes_cleans_profiles_actuals_pk_idx

-- DROP INDEX sipro_data_store.attributes_cleans_profiles_actuals_pk_idx;

CREATE UNIQUE INDEX attributes_cleans_profiles_actuals_pk_idx
  ON sipro_data_store.attributes_cleans_profiles_actuals
  USING btree
  (attributes_cleans_profiles_actuals_id);

-- Table: sipro_data_store.attributes_cleans_profiles_details

-- DROP TABLE sipro_data_store.attributes_cleans_profiles_details;

CREATE TABLE sipro_data_store.attributes_cleans_profiles_details
(
  attributes_cleans_profiles_details_id smallint NOT NULL,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_cleans_profiles_id smallint NOT NULL,
  order_id smallint NOT NULL,
  attributes_cleans_id smallint NOT NULL,
  CONSTRAINT attributes_cleans_profiles_details_pk PRIMARY KEY (attributes_cleans_profiles_details_id),
  CONSTRAINT attributes_cleans_id_fk FOREIGN KEY (attributes_cleans_id)
      REFERENCES sipro_data_store.attributes_cleans (attributes_cleans_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT attributes_cleans_profiles_fk2 FOREIGN KEY (attributes_cleans_profiles_id)
      REFERENCES sipro_data_store.attributes_cleans_profiles (attributes_cleans_profiles_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sipro_data_store.attributes_cleans_profiles_details
  OWNER TO postgres;

-- Index: sipro_data_store.attributes_cleans_profiles_details_pk_idx

-- DROP INDEX sipro_data_store.attributes_cleans_profiles_details_pk_idx;

CREATE UNIQUE INDEX attributes_cleans_profiles_details_pk_idx
  ON sipro_data_store.attributes_cleans_profiles_details
  USING btree
  (attributes_cleans_profiles_details_id);

- Table: sipro_data_store.attributes_profiles

-- DROP TABLE sipro_data_store.attributes_profiles;

CREATE TABLE sipro_data_store.attributes_profiles
(
  attributes_profiles_id smallint NOT NULL,
  timeends_range character varying(63),
  last_modified_date double precision,
  attributes_expression character varying(253),
  attributes_cleans_profiles_id smallint NOT NULL,
  table_nm character varying(63),
  column_nm character varying(63),
  CONSTRAINT attributes_profiles_pk PRIMARY KEY (attributes_profiles_id),
  CONSTRAINT attributes_cleans_profiles_fk FOREIGN KEY (attributes_cleans_profiles_id)
      REFERENCES sipro_data_store.attributes_cleans_profiles (attributes_cleans_profiles_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sipro_data_store.attributes_profiles
  OWNER TO postgres;

-- Index: sipro_data_store.attributes_profiles_pk_idx

-- DROP INDEX sipro_data_store.attributes_profiles_pk_idx;

CREATE UNIQUE INDEX attributes_profiles_pk_idx
  ON sipro_data_store.attributes_profiles
  USING btree
  (attributes_profiles_id);

- Table: sipro_data_store.attributes_profiles_dependents

-- DROP TABLE sipro_data_store.attributes_profiles_dependents;

CREATE TABLE sipro_data_store.attributes_profiles_dependents
(
  attributes_profiles_dependents_id smallint NOT NULL,
  timeends_range character varying(63),
  last_modified_date double precision,
  dependent_id smallint NOT NULL,
  superior_id smallint NOT NULL,
  CONSTRAINT attributes_profiles_dependents_pk PRIMARY KEY (attributes_profiles_dependents_id),
  CONSTRAINT dependent_id_fk FOREIGN KEY (dependent_id)
      REFERENCES sipro_data_store.attributes_profiles (attributes_profiles_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT superior_id_fk FOREIGN KEY (superior_id)
      REFERENCES sipro_data_store.attributes_profiles (attributes_profiles_id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (
  OIDS=FALSE
);
ALTER TABLE sipro_data_store.attributes_profiles_dependents
  OWNER TO postgres;

-- Index: sipro_data_store.attributes_profiles_dependents_pk_idx

-- DROP INDEX sipro_data_store.attributes_profiles_dependents_pk_idx;

CREATE UNIQUE INDEX attributes_profiles_dependents_pk_idx
  ON sipro_data_store.attributes_profiles_dependents
  USING btree
  (attributes_profiles_dependents_id);

------------- end sipro.sql ----------------
