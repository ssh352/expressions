--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.2
-- Dumped by pg_dump version 9.5.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: sipro_data_store; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA sipro_data_store;


ALTER SCHEMA sipro_data_store OWNER TO postgres;

--
-- Name: SCHEMA sipro_data_store; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA sipro_data_store IS 'aaii stockinvestor pro data store';


SET search_path = sipro_data_store, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: atable; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE atable (
    acolumn "char"[] NOT NULL
);


ALTER TABLE atable OWNER TO postgres;

--
-- Name: attributes_cleans; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE attributes_cleans (
    attributes_cleans_id bigint NOT NULL,
    timeend integer NOT NULL,
    last_modified_date integer,
    attributes_cleans_nm text
);


ALTER TABLE attributes_cleans OWNER TO postgres;

--
-- Name: attributes_cleans_attributes_cleans_id_seq; Type: SEQUENCE; Schema: sipro_data_store; Owner: postgres
--

CREATE SEQUENCE attributes_cleans_attributes_cleans_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attributes_cleans_attributes_cleans_id_seq OWNER TO postgres;

--
-- Name: attributes_cleans_attributes_cleans_id_seq; Type: SEQUENCE OWNED BY; Schema: sipro_data_store; Owner: postgres
--

ALTER SEQUENCE attributes_cleans_attributes_cleans_id_seq OWNED BY attributes_cleans.attributes_cleans_id;


--
-- Name: attributes_cleans_profiles; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE attributes_cleans_profiles (
    attributes_cleans_profiles_id bigint NOT NULL,
    timeend integer NOT NULL,
    last_modified_date integer,
    attributes_cleans_profiles_nm text
);


ALTER TABLE attributes_cleans_profiles OWNER TO postgres;

--
-- Name: attributes_cleans_profiles_actuals; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE attributes_cleans_profiles_actuals (
    attributes_cleans_profiles_actuals_id bigint NOT NULL,
    timeend integer NOT NULL,
    last_modified_date integer,
    attributes_cleans_profiles_id bigint NOT NULL
);


ALTER TABLE attributes_cleans_profiles_actuals OWNER TO postgres;

--
-- Name: attributes_cleans_profiles_ac_attributes_cleans_profiles_ac_seq; Type: SEQUENCE; Schema: sipro_data_store; Owner: postgres
--

CREATE SEQUENCE attributes_cleans_profiles_ac_attributes_cleans_profiles_ac_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attributes_cleans_profiles_ac_attributes_cleans_profiles_ac_seq OWNER TO postgres;

--
-- Name: attributes_cleans_profiles_ac_attributes_cleans_profiles_ac_seq; Type: SEQUENCE OWNED BY; Schema: sipro_data_store; Owner: postgres
--

ALTER SEQUENCE attributes_cleans_profiles_ac_attributes_cleans_profiles_ac_seq OWNED BY attributes_cleans_profiles_actuals.attributes_cleans_profiles_actuals_id;


--
-- Name: attributes_cleans_profiles_attributes_cleans_profiles_id_seq; Type: SEQUENCE; Schema: sipro_data_store; Owner: postgres
--

CREATE SEQUENCE attributes_cleans_profiles_attributes_cleans_profiles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attributes_cleans_profiles_attributes_cleans_profiles_id_seq OWNER TO postgres;

--
-- Name: attributes_cleans_profiles_attributes_cleans_profiles_id_seq; Type: SEQUENCE OWNED BY; Schema: sipro_data_store; Owner: postgres
--

ALTER SEQUENCE attributes_cleans_profiles_attributes_cleans_profiles_id_seq OWNED BY attributes_cleans_profiles.attributes_cleans_profiles_id;


--
-- Name: attributes_cleans_profiles_details; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE attributes_cleans_profiles_details (
    attributes_cleans_profiles_details_id bigint NOT NULL,
    timeend integer NOT NULL,
    last_modified_date integer,
    attributes_cleans_profiles_id bigint NOT NULL,
    order_id bigint NOT NULL,
    attributes_cleans_id bigint NOT NULL
);


ALTER TABLE attributes_cleans_profiles_details OWNER TO postgres;

--
-- Name: attributes_cleans_profiles_de_attributes_cleans_profiles_de_seq; Type: SEQUENCE; Schema: sipro_data_store; Owner: postgres
--

CREATE SEQUENCE attributes_cleans_profiles_de_attributes_cleans_profiles_de_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attributes_cleans_profiles_de_attributes_cleans_profiles_de_seq OWNER TO postgres;

--
-- Name: attributes_cleans_profiles_de_attributes_cleans_profiles_de_seq; Type: SEQUENCE OWNED BY; Schema: sipro_data_store; Owner: postgres
--

ALTER SEQUENCE attributes_cleans_profiles_de_attributes_cleans_profiles_de_seq OWNED BY attributes_cleans_profiles_details.attributes_cleans_profiles_details_id;


--
-- Name: attributes_profiles; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE attributes_profiles (
    attributes_profiles_id bigint NOT NULL,
    timeend integer NOT NULL,
    last_modified_date integer,
    attributes_expression text,
    attributes_cleans_profiles_id bigint NOT NULL,
    schema_nm text,
    table_nm text,
    column_nm text
);


ALTER TABLE attributes_profiles OWNER TO postgres;

--
-- Name: attributes_profiles_attributes_profiles_id_seq; Type: SEQUENCE; Schema: sipro_data_store; Owner: postgres
--

CREATE SEQUENCE attributes_profiles_attributes_profiles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attributes_profiles_attributes_profiles_id_seq OWNER TO postgres;

--
-- Name: attributes_profiles_attributes_profiles_id_seq; Type: SEQUENCE OWNED BY; Schema: sipro_data_store; Owner: postgres
--

ALTER SEQUENCE attributes_profiles_attributes_profiles_id_seq OWNED BY attributes_profiles.attributes_profiles_id;


--
-- Name: attributes_profiles_dependents; Type: TABLE; Schema: sipro_data_store; Owner: postgres
--

CREATE TABLE attributes_profiles_dependents (
    attributes_profiles_dependents_id bigint NOT NULL,
    timeend integer NOT NULL,
    last_modified_date integer,
    dependent_id bigint NOT NULL,
    superior_id bigint NOT NULL
);


ALTER TABLE attributes_profiles_dependents OWNER TO postgres;

--
-- Name: attributes_profiles_dependent_attributes_profiles_dependent_seq; Type: SEQUENCE; Schema: sipro_data_store; Owner: postgres
--

CREATE SEQUENCE attributes_profiles_dependent_attributes_profiles_dependent_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE attributes_profiles_dependent_attributes_profiles_dependent_seq OWNER TO postgres;

--
-- Name: attributes_profiles_dependent_attributes_profiles_dependent_seq; Type: SEQUENCE OWNED BY; Schema: sipro_data_store; Owner: postgres
--

ALTER SEQUENCE attributes_profiles_dependent_attributes_profiles_dependent_seq OWNED BY attributes_profiles_dependents.attributes_profiles_dependents_id;


--
-- Name: attributes_cleans_id; Type: DEFAULT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans ALTER COLUMN attributes_cleans_id SET DEFAULT nextval('attributes_cleans_attributes_cleans_id_seq'::regclass);


--
-- Name: attributes_cleans_profiles_id; Type: DEFAULT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles ALTER COLUMN attributes_cleans_profiles_id SET DEFAULT nextval('attributes_cleans_profiles_attributes_cleans_profiles_id_seq'::regclass);


--
-- Name: attributes_cleans_profiles_actuals_id; Type: DEFAULT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles_actuals ALTER COLUMN attributes_cleans_profiles_actuals_id SET DEFAULT nextval('attributes_cleans_profiles_ac_attributes_cleans_profiles_ac_seq'::regclass);


--
-- Name: attributes_cleans_profiles_details_id; Type: DEFAULT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles_details ALTER COLUMN attributes_cleans_profiles_details_id SET DEFAULT nextval('attributes_cleans_profiles_de_attributes_cleans_profiles_de_seq'::regclass);


--
-- Name: attributes_profiles_id; Type: DEFAULT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_profiles ALTER COLUMN attributes_profiles_id SET DEFAULT nextval('attributes_profiles_attributes_profiles_id_seq'::regclass);


--
-- Name: attributes_profiles_dependents_id; Type: DEFAULT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_profiles_dependents ALTER COLUMN attributes_profiles_dependents_id SET DEFAULT nextval('attributes_profiles_dependent_attributes_profiles_dependent_seq'::regclass);


--
-- Name: attributes_cleans_pk; Type: CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans
    ADD CONSTRAINT attributes_cleans_pk PRIMARY KEY (attributes_cleans_id, timeend);


--
-- Name: attributes_cleans_profiles_actuals_pk; Type: CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles_actuals
    ADD CONSTRAINT attributes_cleans_profiles_actuals_pk PRIMARY KEY (attributes_cleans_profiles_actuals_id, timeend);


--
-- Name: attributes_cleans_profiles_details_pk; Type: CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles_details
    ADD CONSTRAINT attributes_cleans_profiles_details_pk PRIMARY KEY (attributes_cleans_profiles_details_id, timeend);


--
-- Name: attributes_cleans_profiles_pk; Type: CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles
    ADD CONSTRAINT attributes_cleans_profiles_pk PRIMARY KEY (attributes_cleans_profiles_id, timeend);


--
-- Name: attributes_profiles_dependents_pk; Type: CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_profiles_dependents
    ADD CONSTRAINT attributes_profiles_dependents_pk PRIMARY KEY (attributes_profiles_dependents_id, timeend);


--
-- Name: attributes_profiles_pk; Type: CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_profiles
    ADD CONSTRAINT attributes_profiles_pk PRIMARY KEY (attributes_profiles_id, timeend);


--
-- Name: attributes_cleans_pk_idx; Type: INDEX; Schema: sipro_data_store; Owner: postgres
--

CREATE UNIQUE INDEX attributes_cleans_pk_idx ON attributes_cleans USING btree (attributes_cleans_id, timeend);


--
-- Name: attributes_cleans_profiles_actuals_pk_idx; Type: INDEX; Schema: sipro_data_store; Owner: postgres
--

CREATE UNIQUE INDEX attributes_cleans_profiles_actuals_pk_idx ON attributes_cleans_profiles_actuals USING btree (attributes_cleans_profiles_actuals_id, timeend);


--
-- Name: attributes_cleans_profiles_details_pk_idx; Type: INDEX; Schema: sipro_data_store; Owner: postgres
--

CREATE UNIQUE INDEX attributes_cleans_profiles_details_pk_idx ON attributes_cleans_profiles_details USING btree (attributes_cleans_profiles_details_id, timeend);


--
-- Name: attributes_cleans_profiles_pk_idx; Type: INDEX; Schema: sipro_data_store; Owner: postgres
--

CREATE UNIQUE INDEX attributes_cleans_profiles_pk_idx ON attributes_cleans_profiles USING btree (attributes_cleans_profiles_id, timeend);


--
-- Name: attributes_profiles_dependents_pk_idx; Type: INDEX; Schema: sipro_data_store; Owner: postgres
--

CREATE UNIQUE INDEX attributes_profiles_dependents_pk_idx ON attributes_profiles_dependents USING btree (attributes_profiles_dependents_id, timeend);


--
-- Name: attributes_profiles_pk_idx; Type: INDEX; Schema: sipro_data_store; Owner: postgres
--

CREATE UNIQUE INDEX attributes_profiles_pk_idx ON attributes_profiles USING btree (attributes_profiles_id, timeend);


--
-- Name: attributes_cleans_id_fk; Type: FK CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles_details
    ADD CONSTRAINT attributes_cleans_id_fk FOREIGN KEY (attributes_cleans_id, timeend) REFERENCES attributes_cleans(attributes_cleans_id, timeend) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: attributes_cleans_profiles_fk; Type: FK CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles_actuals
    ADD CONSTRAINT attributes_cleans_profiles_fk FOREIGN KEY (attributes_cleans_profiles_id, timeend) REFERENCES attributes_cleans_profiles(attributes_cleans_profiles_id, timeend) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: attributes_cleans_profiles_fk; Type: FK CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_profiles
    ADD CONSTRAINT attributes_cleans_profiles_fk FOREIGN KEY (attributes_cleans_profiles_id, timeend) REFERENCES attributes_cleans_profiles(attributes_cleans_profiles_id, timeend) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: attributes_cleans_profiles_fk2; Type: FK CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_cleans_profiles_details
    ADD CONSTRAINT attributes_cleans_profiles_fk2 FOREIGN KEY (attributes_cleans_profiles_id, timeend) REFERENCES attributes_cleans_profiles(attributes_cleans_profiles_id, timeend) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: dependent_id_fk; Type: FK CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_profiles_dependents
    ADD CONSTRAINT dependent_id_fk FOREIGN KEY (dependent_id, timeend) REFERENCES attributes_profiles(attributes_profiles_id, timeend) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: superior_id_fk; Type: FK CONSTRAINT; Schema: sipro_data_store; Owner: postgres
--

ALTER TABLE ONLY attributes_profiles_dependents
    ADD CONSTRAINT superior_id_fk FOREIGN KEY (superior_id, timeend) REFERENCES attributes_profiles(attributes_profiles_id, timeend) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

