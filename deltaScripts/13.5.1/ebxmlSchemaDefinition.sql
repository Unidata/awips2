--
-- PostgreSQL database dump
--

-- Dumped from database version 9.2.3
-- Dumped by pg_dump version 9.2.3
-- Started on 2013-03-22 13:36:06 CDT

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 11 (class 2615 OID 91156)
-- Name: ebxml; Type: SCHEMA; Schema: -; Owner: awips
--

DROP SCHEMA IF EXISTS ebxml CASCADE;
CREATE SCHEMA ebxml;


ALTER SCHEMA ebxml OWNER TO awips;

SET search_path = ebxml, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 346 (class 1259 OID 103385)
-- Name: action; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE action (
    key integer NOT NULL,
    eventtype character varying(255),
    affectedobjectrefs_key integer,
    affectedobjects_key integer
);


ALTER TABLE ebxml.action OWNER TO awips;

--
-- TOC entry 424 (class 1259 OID 104509)
-- Name: action_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE action_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.action_sequence OWNER TO awips;

--
-- TOC entry 347 (class 1259 OID 103390)
-- Name: action_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE action_slot (
    action_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.action_slot OWNER TO awips;

--
-- TOC entry 348 (class 1259 OID 103395)
-- Name: association; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE association (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    sourceobject character varying(255),
    targetobject character varying(255),
    type character varying(255)
);


ALTER TABLE ebxml.association OWNER TO awips;

--
-- TOC entry 349 (class 1259 OID 103403)
-- Name: auditableevent; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE auditableevent (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    requestid character varying(255),
    "timestamp" timestamp without time zone,
    username character varying(255)
);


ALTER TABLE ebxml.auditableevent OWNER TO awips;

--
-- TOC entry 350 (class 1259 OID 103411)
-- Name: auditableevent_action; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE auditableevent_action (
    auditableevent_id character varying(255) NOT NULL,
    action_key integer NOT NULL
);


ALTER TABLE ebxml.auditableevent_action OWNER TO awips;

--
-- TOC entry 351 (class 1259 OID 103416)
-- Name: classification; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE classification (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    classificationnode character varying(255),
    classificationscheme character varying(255),
    classifiedobject character varying(255),
    noderepresentation character varying(255)
);


ALTER TABLE ebxml.classification OWNER TO awips;

--
-- TOC entry 352 (class 1259 OID 103424)
-- Name: classificationnode; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE classificationnode (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    code character varying(255),
    parent character varying(255),
    path character varying(255)
);


ALTER TABLE ebxml.classificationnode OWNER TO awips;

--
-- TOC entry 353 (class 1259 OID 103432)
-- Name: classificationscheme; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE classificationscheme (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    isinternal boolean NOT NULL,
    nodetype character varying(255)
);


ALTER TABLE ebxml.classificationscheme OWNER TO awips;

--
-- TOC entry 354 (class 1259 OID 103440)
-- Name: deliveryinfo; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE deliveryinfo (
    key integer NOT NULL,
    notificationoption character varying(255),
    notifyto text
);


ALTER TABLE ebxml.deliveryinfo OWNER TO awips;

--
-- TOC entry 425 (class 1259 OID 104511)
-- Name: deliveryinfo_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE deliveryinfo_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.deliveryinfo_sequence OWNER TO awips;

--
-- TOC entry 355 (class 1259 OID 103445)
-- Name: deliveryinfo_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE deliveryinfo_slot (
    deliveryinfo_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.deliveryinfo_slot OWNER TO awips;

--
-- TOC entry 356 (class 1259 OID 103450)
-- Name: emailaddress; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE emailaddress (
    key integer NOT NULL,
    address character varying(255),
    type character varying(255)
);


ALTER TABLE ebxml.emailaddress OWNER TO awips;

--
-- TOC entry 426 (class 1259 OID 104513)
-- Name: emailaddress_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE emailaddress_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.emailaddress_sequence OWNER TO awips;

--
-- TOC entry 357 (class 1259 OID 103458)
-- Name: emailaddress_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE emailaddress_slot (
    emailaddress_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.emailaddress_slot OWNER TO awips;

--
-- TOC entry 358 (class 1259 OID 103463)
-- Name: entry; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE entry (
    entryvalue_key integer NOT NULL,
    entrykey_key integer NOT NULL
);


ALTER TABLE ebxml.entry OWNER TO awips;

--
-- TOC entry 359 (class 1259 OID 103472)
-- Name: externalidentifier; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE externalidentifier (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    identificationscheme character varying(255),
    registryobject character varying(255),
    value character varying(255)
);


ALTER TABLE ebxml.externalidentifier OWNER TO awips;

--
-- TOC entry 360 (class 1259 OID 103480)
-- Name: externallink; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE externallink (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    registryobject character varying(255),
    externalref_key integer
);


ALTER TABLE ebxml.externallink OWNER TO awips;

--
-- TOC entry 361 (class 1259 OID 103488)
-- Name: extrinsicobject; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE extrinsicobject (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    contentuserversionname character varying(255),
    contentversionname character varying(255),
    mimetype character varying(255),
    repositoryitem bytea,
    repositoryitemref_key integer
);


ALTER TABLE ebxml.extrinsicobject OWNER TO awips;

--
-- TOC entry 362 (class 1259 OID 103496)
-- Name: federation; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE federation (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer
);


ALTER TABLE ebxml.federation OWNER TO awips;

--
-- TOC entry 363 (class 1259 OID 103504)
-- Name: internationalstring; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE internationalstring (
    key integer NOT NULL
);


ALTER TABLE ebxml.internationalstring OWNER TO awips;

--
-- TOC entry 364 (class 1259 OID 103509)
-- Name: internationalstring_localizedstring; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE internationalstring_localizedstring (
    internationalstring_key integer NOT NULL,
    localizedstring_key integer NOT NULL
);


ALTER TABLE ebxml.internationalstring_localizedstring OWNER TO awips;

--
-- TOC entry 427 (class 1259 OID 104515)
-- Name: internationalstring_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE internationalstring_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.internationalstring_sequence OWNER TO awips;

--
-- TOC entry 365 (class 1259 OID 103514)
-- Name: localizedstring; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE localizedstring (
    key integer NOT NULL,
    lang character varying(255),
    value character varying(1024)
);


ALTER TABLE ebxml.localizedstring OWNER TO awips;

--
-- TOC entry 428 (class 1259 OID 104517)
-- Name: localizedstring_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE localizedstring_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.localizedstring_sequence OWNER TO awips;

--
-- TOC entry 366 (class 1259 OID 103522)
-- Name: map; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE map (
    key integer NOT NULL
);


ALTER TABLE ebxml.map OWNER TO awips;

--
-- TOC entry 367 (class 1259 OID 103527)
-- Name: map_entry; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE map_entry (
    map_key integer NOT NULL,
    entry_entryvalue_key integer NOT NULL,
    entry_entrykey_key integer NOT NULL
);


ALTER TABLE ebxml.map_entry OWNER TO awips;

--
-- TOC entry 429 (class 1259 OID 104519)
-- Name: map_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE map_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.map_sequence OWNER TO awips;

--
-- TOC entry 368 (class 1259 OID 103530)
-- Name: notification; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE notification (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    subscription character varying(255)
);


ALTER TABLE ebxml.notification OWNER TO awips;

--
-- TOC entry 369 (class 1259 OID 103538)
-- Name: notification_auditableevent; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE notification_auditableevent (
    notification_id character varying(255) NOT NULL,
    event_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.notification_auditableevent OWNER TO awips;

--
-- TOC entry 370 (class 1259 OID 103544)
-- Name: objectref; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE objectref (
    id character varying(255) NOT NULL
);


ALTER TABLE ebxml.objectref OWNER TO awips;

--
-- TOC entry 373 (class 1259 OID 103557)
-- Name: objectref_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE objectref_slot (
    objectref_id character varying(255) NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.objectref_slot OWNER TO awips;

--
-- TOC entry 371 (class 1259 OID 103549)
-- Name: objectreflist; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE objectreflist (
    key integer NOT NULL
);


ALTER TABLE ebxml.objectreflist OWNER TO awips;

--
-- TOC entry 372 (class 1259 OID 103554)
-- Name: objectreflist_objectref; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE objectreflist_objectref (
    objectreflist_key integer NOT NULL,
    objectref_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.objectreflist_objectref OWNER TO awips;

--
-- TOC entry 430 (class 1259 OID 104521)
-- Name: objectreflist_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE objectreflist_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.objectreflist_sequence OWNER TO awips;

--
-- TOC entry 374 (class 1259 OID 103562)
-- Name: organization; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE organization (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    primarycontact character varying(255)
);


ALTER TABLE ebxml.organization OWNER TO awips;

--
-- TOC entry 375 (class 1259 OID 103570)
-- Name: organization_emailaddress; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE organization_emailaddress (
    organization_id character varying(255) NOT NULL,
    emailaddress_key integer NOT NULL
);


ALTER TABLE ebxml.organization_emailaddress OWNER TO awips;

--
-- TOC entry 376 (class 1259 OID 103573)
-- Name: organization_organization; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE organization_organization (
    organization_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.organization_organization OWNER TO awips;

--
-- TOC entry 377 (class 1259 OID 103576)
-- Name: organization_postaladdress; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE organization_postaladdress (
    organization_id character varying(255) NOT NULL,
    postaladdress_key integer NOT NULL
);


ALTER TABLE ebxml.organization_postaladdress OWNER TO awips;

--
-- TOC entry 378 (class 1259 OID 103579)
-- Name: organization_telephonenumber; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE organization_telephonenumber (
    organization_id character varying(255) NOT NULL,
    telephonenumber_key integer NOT NULL
);


ALTER TABLE ebxml.organization_telephonenumber OWNER TO awips;

--
-- TOC entry 379 (class 1259 OID 103582)
-- Name: parameter; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE parameter (
    key integer NOT NULL,
    datatype character varying(255),
    defaultvalue character varying(255),
    maxoccurs numeric(19,2),
    minoccurs numeric(19,2),
    parametername character varying(255),
    description_key integer,
    name_key integer
);


ALTER TABLE ebxml.parameter OWNER TO awips;

--
-- TOC entry 431 (class 1259 OID 104523)
-- Name: parameter_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE parameter_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.parameter_sequence OWNER TO awips;

--
-- TOC entry 380 (class 1259 OID 103590)
-- Name: parameter_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE parameter_slot (
    parameter_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.parameter_slot OWNER TO awips;

--
-- TOC entry 381 (class 1259 OID 103595)
-- Name: person; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE person (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    personname_middlename character varying(255),
    personname_lastname character varying(255),
    personname_firstname character varying(255)
);


ALTER TABLE ebxml.person OWNER TO awips;

--
-- TOC entry 384 (class 1259 OID 103619)
-- Name: person_emailaddress; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE person_emailaddress (
    person_id character varying(255) NOT NULL,
    emailaddress_key integer NOT NULL
);


ALTER TABLE ebxml.person_emailaddress OWNER TO awips;

--
-- TOC entry 385 (class 1259 OID 103622)
-- Name: person_postaladdress; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE person_postaladdress (
    person_id character varying(255) NOT NULL,
    postaladdress_key integer NOT NULL
);


ALTER TABLE ebxml.person_postaladdress OWNER TO awips;

--
-- TOC entry 386 (class 1259 OID 103625)
-- Name: person_telephonenumber; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE person_telephonenumber (
    person_id character varying(255) NOT NULL,
    telephonenumber_key integer NOT NULL
);


ALTER TABLE ebxml.person_telephonenumber OWNER TO awips;

--
-- TOC entry 382 (class 1259 OID 103603)
-- Name: personname; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE personname (
    middlename character varying(255) NOT NULL,
    lastname character varying(255) NOT NULL,
    firstname character varying(255) NOT NULL
);


ALTER TABLE ebxml.personname OWNER TO awips;

--
-- TOC entry 383 (class 1259 OID 103611)
-- Name: personname_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE personname_slot (
    personname_middlename character varying(255) NOT NULL,
    personname_lastname character varying(255) NOT NULL,
    personname_firstname character varying(255) NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.personname_slot OWNER TO awips;

--
-- TOC entry 387 (class 1259 OID 103628)
-- Name: postaladdress; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE postaladdress (
    key integer NOT NULL,
    city character varying(255),
    country character varying(255),
    postalcode character varying(255),
    stateorprovince character varying(255),
    street character varying(255),
    streetnumber character varying(255),
    type character varying(255)
);


ALTER TABLE ebxml.postaladdress OWNER TO awips;

--
-- TOC entry 432 (class 1259 OID 104525)
-- Name: postaladdress_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE postaladdress_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.postaladdress_sequence OWNER TO awips;

--
-- TOC entry 388 (class 1259 OID 103636)
-- Name: postaladdress_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE postaladdress_slot (
    postaladdress_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.postaladdress_slot OWNER TO awips;

--
-- TOC entry 389 (class 1259 OID 103641)
-- Name: query; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

--
-- TOC entry 390 (class 1259 OID 257160)
-- Name: query; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE query (
    key integer NOT NULL,
    querydefinition character varying(255)
);


ALTER TABLE ebxml.query OWNER TO awips;

--
-- TOC entry 436 (class 1259 OID 258050)
-- Name: query_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE query_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.query_sequence OWNER TO awips;

--
-- TOC entry 394 (class 1259 OID 103667)
-- Name: query_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE query_slot (
    query_querydefinition character varying(255) NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.query_slot OWNER TO awips;

--
-- TOC entry 390 (class 1259 OID 103646)
-- Name: querydefinition; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE querydefinition (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    queryexpression_key integer
);


ALTER TABLE ebxml.querydefinition OWNER TO awips;

--
-- TOC entry 391 (class 1259 OID 103654)
-- Name: querydefinition_parameter; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE querydefinition_parameter (
    querydefinition_id character varying(255) NOT NULL,
    parameter_key integer NOT NULL
);


ALTER TABLE ebxml.querydefinition_parameter OWNER TO awips;

--
-- TOC entry 392 (class 1259 OID 103657)
-- Name: queryexpression; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE queryexpression (
    key integer NOT NULL,
    querylanguage character varying(255)
);


ALTER TABLE ebxml.queryexpression OWNER TO awips;

--
-- TOC entry 433 (class 1259 OID 104527)
-- Name: queryexpression_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE queryexpression_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.queryexpression_sequence OWNER TO awips;

--
-- TOC entry 393 (class 1259 OID 103662)
-- Name: queryexpression_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE queryexpression_slot (
    queryexpression_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.queryexpression_slot OWNER TO awips;

--
-- TOC entry 395 (class 1259 OID 103672)
-- Name: registry; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registry (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    baseurl character varying(255),
    conformanceprofile character varying(255),
    operator character varying(255),
    specificationversion character varying(255)
);


ALTER TABLE ebxml.registry OWNER TO awips;

--
-- TOC entry 396 (class 1259 OID 103680)
-- Name: registryobject; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registryobject (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer
);


ALTER TABLE ebxml.registryobject OWNER TO awips;

--
-- TOC entry 399 (class 1259 OID 103696)
-- Name: registryobject_classification; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registryobject_classification (
    registryobject_id character varying(255) NOT NULL,
    classification_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.registryobject_classification OWNER TO awips;

--
-- TOC entry 400 (class 1259 OID 103704)
-- Name: registryobject_externalidentifier; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registryobject_externalidentifier (
    registryobject_id character varying(255) NOT NULL,
    externalidentifier_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.registryobject_externalidentifier OWNER TO awips;

--
-- TOC entry 401 (class 1259 OID 103712)
-- Name: registryobject_externallink; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registryobject_externallink (
    registryobject_id character varying(255) NOT NULL,
    externallink_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.registryobject_externallink OWNER TO awips;

--
-- TOC entry 402 (class 1259 OID 103720)
-- Name: registryobject_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registryobject_slot (
    registryobject_id character varying(255) NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.registryobject_slot OWNER TO awips;

--
-- TOC entry 397 (class 1259 OID 103688)
-- Name: registryobjectlist; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registryobjectlist (
    key integer NOT NULL
);


ALTER TABLE ebxml.registryobjectlist OWNER TO awips;

--
-- TOC entry 398 (class 1259 OID 103693)
-- Name: registryobjectlist_registryobject; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registryobjectlist_registryobject (
    registryobjectlist_key integer NOT NULL,
    registryobject_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.registryobjectlist_registryobject OWNER TO awips;

--
-- TOC entry 434 (class 1259 OID 104529)
-- Name: registryobjectlist_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE registryobjectlist_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.registryobjectlist_sequence OWNER TO awips;

--
-- TOC entry 403 (class 1259 OID 103725)
-- Name: registrypackage; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE registrypackage (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    registryobjectlist_key integer
);


ALTER TABLE ebxml.registrypackage OWNER TO awips;

--
-- TOC entry 404 (class 1259 OID 103733)
-- Name: role; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE role (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    type character varying(255)
);


ALTER TABLE ebxml.role OWNER TO awips;

--
-- TOC entry 405 (class 1259 OID 103741)
-- Name: service; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE service (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    serviceinterface character varying(255)
);


ALTER TABLE ebxml.service OWNER TO awips;

--
-- TOC entry 409 (class 1259 OID 103773)
-- Name: service_serviceendpoint; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE service_serviceendpoint (
    service_id character varying(255) NOT NULL,
    serviceendpoint_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.service_serviceendpoint OWNER TO awips;

--
-- TOC entry 406 (class 1259 OID 103749)
-- Name: servicebinding; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE servicebinding (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    serviceinterface character varying(255)
);


ALTER TABLE ebxml.servicebinding OWNER TO awips;

--
-- TOC entry 407 (class 1259 OID 103757)
-- Name: serviceendpoint; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE serviceendpoint (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    address character varying(255),
    servicebinding character varying(255)
);


ALTER TABLE ebxml.serviceendpoint OWNER TO awips;

--
-- TOC entry 408 (class 1259 OID 103765)
-- Name: serviceinterface; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE serviceinterface (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer
);


ALTER TABLE ebxml.serviceinterface OWNER TO awips;

--
-- TOC entry 410 (class 1259 OID 103779)
-- Name: simplelink; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE simplelink (
    key integer NOT NULL,
    arcrole character varying(255),
    href character varying(255),
    role character varying(255),
    title character varying(255)
);


ALTER TABLE ebxml.simplelink OWNER TO awips;

--
-- TOC entry 435 (class 1259 OID 104531)
-- Name: simplelink_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE simplelink_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.simplelink_sequence OWNER TO awips;

--
-- TOC entry 411 (class 1259 OID 103787)
-- Name: slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE slot (
    key integer NOT NULL,
    name character varying(255),
    type character varying(255),
    slotvalue_key integer
);


ALTER TABLE ebxml.slot OWNER TO awips;

--
-- TOC entry 436 (class 1259 OID 104533)
-- Name: slot_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE slot_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.slot_sequence OWNER TO awips;

--
-- TOC entry 412 (class 1259 OID 103795)
-- Name: slot_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE slot_slot (
    slot_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.slot_slot OWNER TO awips;

--
-- TOC entry 413 (class 1259 OID 103800)
-- Name: stringqueryexpression; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE stringqueryexpression (
    key integer NOT NULL,
    querylanguage character varying(255),
    value character varying(255)
);


ALTER TABLE ebxml.stringqueryexpression OWNER TO awips;

--
-- TOC entry 414 (class 1259 OID 103808)
-- Name: subscription; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE subscription (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    endtime timestamp without time zone,
    starttime timestamp without time zone,
    selector_querydefinition character varying(255)
);


ALTER TABLE ebxml.subscription OWNER TO awips;

--
-- TOC entry 415 (class 1259 OID 103816)
-- Name: subscription_deliveryinfo; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE subscription_deliveryinfo (
    subscription_id character varying(255) NOT NULL,
    deliveryinfo_key integer NOT NULL
);


ALTER TABLE ebxml.subscription_deliveryinfo OWNER TO awips;

--
-- TOC entry 416 (class 1259 OID 103819)
-- Name: taxonomyelementtype_classificationnode; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE taxonomyelementtype_classificationnode (
    taxonomyelementtype_id character varying(255) NOT NULL,
    classificationnode_id character varying(255) NOT NULL
);


ALTER TABLE ebxml.taxonomyelementtype_classificationnode OWNER TO awips;

--
-- TOC entry 417 (class 1259 OID 103827)
-- Name: telephonenumber; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE telephonenumber (
    key integer NOT NULL,
    areacode character varying(255),
    countrycode character varying(255),
    extension character varying(255),
    number character varying(255),
    type character varying(255)
);


ALTER TABLE ebxml.telephonenumber OWNER TO awips;

--
-- TOC entry 437 (class 1259 OID 104535)
-- Name: telephonenumber_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE telephonenumber_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.telephonenumber_sequence OWNER TO awips;

--
-- TOC entry 418 (class 1259 OID 103835)
-- Name: telephonenumber_slot; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE telephonenumber_slot (
    telephonenumber_key integer NOT NULL,
    child_slot_key integer NOT NULL
);


ALTER TABLE ebxml.telephonenumber_slot OWNER TO awips;

--
-- TOC entry 419 (class 1259 OID 103840)
-- Name: value; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE value (
    dtype character varying(31) NOT NULL,
    key integer NOT NULL,
    mapvalue text,
    stringvalue text,
    slotvalue text,
    durationvalue bigint,
    anyvalue text,
    floatvalue real,
    vocabularytermvalue text,
    integervalue numeric(19,2),
    datetimevalue timestamp without time zone,
    collectiontype character varying(255),
    booleanvalue boolean,
    internationalstringvalue_key integer
);


ALTER TABLE ebxml.value OWNER TO awips;

--
-- TOC entry 438 (class 1259 OID 104537)
-- Name: value_sequence; Type: SEQUENCE; Schema: ebxml; Owner: awips
--

CREATE SEQUENCE value_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ebxml.value_sequence OWNER TO awips;

--
-- TOC entry 420 (class 1259 OID 103848)
-- Name: value_value; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE value_value (
    value_key integer NOT NULL,
    collectionvalue_key integer NOT NULL
);


ALTER TABLE ebxml.value_value OWNER TO awips;

--
-- TOC entry 421 (class 1259 OID 103851)
-- Name: vocabularyterm; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE vocabularyterm (
    vocabulary character varying(255) NOT NULL,
    term character varying(255) NOT NULL
);


ALTER TABLE ebxml.vocabularyterm OWNER TO awips;

--
-- TOC entry 422 (class 1259 OID 103859)
-- Name: workflowaction; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE workflowaction (
    id character varying(255) NOT NULL,
    lid character varying(255),
    objecttype character varying(255),
    owner character varying(255),
    status character varying(255),
    userversionname character varying(255),
    versionname character varying(255),
    description_key integer,
    name_key integer,
    actiontype character varying(255),
    targetobject character varying(255)
);


ALTER TABLE ebxml.workflowaction OWNER TO awips;

--
-- TOC entry 423 (class 1259 OID 103867)
-- Name: xmlqueryexpression; Type: TABLE; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE TABLE xmlqueryexpression (
    key integer NOT NULL,
    querylanguage character varying(255),
    anyvalue text
);


ALTER TABLE ebxml.xmlqueryexpression OWNER TO awips;

--
-- TOC entry 4599 (class 2606 OID 103389)
-- Name: action_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY action
    ADD CONSTRAINT action_pkey PRIMARY KEY (key);


--
-- TOC entry 4601 (class 2606 OID 103394)
-- Name: action_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY action_slot
    ADD CONSTRAINT action_slot_pkey PRIMARY KEY (action_key, child_slot_key);


--
-- TOC entry 4603 (class 2606 OID 103402)
-- Name: association_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY association
    ADD CONSTRAINT association_pkey PRIMARY KEY (id);


--
-- TOC entry 4612 (class 2606 OID 103415)
-- Name: auditableevent_action_action_key_key; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY auditableevent_action
    ADD CONSTRAINT auditableevent_action_action_key_key UNIQUE (action_key);


--
-- TOC entry 4608 (class 2606 OID 103410)
-- Name: auditableevent_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY auditableevent
    ADD CONSTRAINT auditableevent_pkey PRIMARY KEY (id);


--
-- TOC entry 4614 (class 2606 OID 103423)
-- Name: classification_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY classification
    ADD CONSTRAINT classification_pkey PRIMARY KEY (id);


--
-- TOC entry 4618 (class 2606 OID 103431)
-- Name: classificationnode_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY classificationnode
    ADD CONSTRAINT classificationnode_pkey PRIMARY KEY (id);


--
-- TOC entry 4623 (class 2606 OID 103439)
-- Name: classificationscheme_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY classificationscheme
    ADD CONSTRAINT classificationscheme_pkey PRIMARY KEY (id);


--
-- TOC entry 4627 (class 2606 OID 103444)
-- Name: deliveryinfo_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY deliveryinfo
    ADD CONSTRAINT deliveryinfo_pkey PRIMARY KEY (key);


--
-- TOC entry 4629 (class 2606 OID 103449)
-- Name: deliveryinfo_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY deliveryinfo_slot
    ADD CONSTRAINT deliveryinfo_slot_pkey PRIMARY KEY (deliveryinfo_key, child_slot_key);


--
-- TOC entry 4631 (class 2606 OID 103457)
-- Name: emailaddress_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY emailaddress
    ADD CONSTRAINT emailaddress_pkey PRIMARY KEY (key);


--
-- TOC entry 4633 (class 2606 OID 103462)
-- Name: emailaddress_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY emailaddress_slot
    ADD CONSTRAINT emailaddress_slot_pkey PRIMARY KEY (emailaddress_key, child_slot_key);


--
-- TOC entry 4635 (class 2606 OID 103469)
-- Name: entry_entrykey_key_key; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY entry
    ADD CONSTRAINT entry_entrykey_key_key UNIQUE (entrykey_key);


--
-- TOC entry 4637 (class 2606 OID 103471)
-- Name: entry_entryvalue_key_key; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY entry
    ADD CONSTRAINT entry_entryvalue_key_key UNIQUE (entryvalue_key);


--
-- TOC entry 4639 (class 2606 OID 103467)
-- Name: entry_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY entry
    ADD CONSTRAINT entry_pkey PRIMARY KEY (entryvalue_key, entrykey_key);


--
-- TOC entry 4641 (class 2606 OID 103479)
-- Name: externalidentifier_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY externalidentifier
    ADD CONSTRAINT externalidentifier_pkey PRIMARY KEY (id);


--
-- TOC entry 4645 (class 2606 OID 103487)
-- Name: externallink_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY externallink
    ADD CONSTRAINT externallink_pkey PRIMARY KEY (id);


--
-- TOC entry 4649 (class 2606 OID 103495)
-- Name: extrinsicobject_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT extrinsicobject_pkey PRIMARY KEY (id);


--
-- TOC entry 4653 (class 2606 OID 103503)
-- Name: federation_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY federation
    ADD CONSTRAINT federation_pkey PRIMARY KEY (id);


--
-- TOC entry 4660 (class 2606 OID 103513)
-- Name: internationalstring_localizedstring_localizedstring_key_key; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY internationalstring_localizedstring
    ADD CONSTRAINT internationalstring_localizedstring_localizedstring_key_key UNIQUE (localizedstring_key);


--
-- TOC entry 4657 (class 2606 OID 103508)
-- Name: internationalstring_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY internationalstring
    ADD CONSTRAINT internationalstring_pkey PRIMARY KEY (key);


--
-- TOC entry 4662 (class 2606 OID 103521)
-- Name: localizedstring_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY localizedstring
    ADD CONSTRAINT localizedstring_pkey PRIMARY KEY (key);


--
-- TOC entry 4664 (class 2606 OID 103526)
-- Name: map_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY map
    ADD CONSTRAINT map_pkey PRIMARY KEY (key);


--
-- TOC entry 4666 (class 2606 OID 103537)
-- Name: notification_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY notification
    ADD CONSTRAINT notification_pkey PRIMARY KEY (id);


--
-- TOC entry 4680 (class 2606 OID 257056)
-- Name: notificationinfo_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY notificationinfo
    ADD CONSTRAINT notificationinfo_pkey PRIMARY KEY (key);


--
-- TOC entry 4670 (class 2606 OID 103548)
-- Name: objectref_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY objectref
    ADD CONSTRAINT objectref_pkey PRIMARY KEY (id);


--
-- TOC entry 4674 (class 2606 OID 103561)
-- Name: objectref_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY objectref_slot
    ADD CONSTRAINT objectref_slot_pkey PRIMARY KEY (objectref_id, child_slot_key);


--
-- TOC entry 4672 (class 2606 OID 103553)
-- Name: objectreflist_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY objectreflist
    ADD CONSTRAINT objectreflist_pkey PRIMARY KEY (key);


--
-- TOC entry 4676 (class 2606 OID 103569)
-- Name: organization_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_pkey PRIMARY KEY (id);


--
-- TOC entry 4680 (class 2606 OID 103589)
-- Name: parameter_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY parameter
    ADD CONSTRAINT parameter_pkey PRIMARY KEY (key);


--
-- TOC entry 4682 (class 2606 OID 103594)
-- Name: parameter_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY parameter_slot
    ADD CONSTRAINT parameter_slot_pkey PRIMARY KEY (parameter_key, child_slot_key);


--
-- TOC entry 4684 (class 2606 OID 103602)
-- Name: person_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);


--
-- TOC entry 4688 (class 2606 OID 103610)
-- Name: personname_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY personname
    ADD CONSTRAINT personname_pkey PRIMARY KEY (middlename, lastname, firstname);


--
-- TOC entry 4690 (class 2606 OID 103618)
-- Name: personname_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY personname_slot
    ADD CONSTRAINT personname_slot_pkey PRIMARY KEY (personname_middlename, personname_lastname, personname_firstname, child_slot_key);


--
-- TOC entry 4692 (class 2606 OID 103635)
-- Name: postaladdress_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY postaladdress
    ADD CONSTRAINT postaladdress_pkey PRIMARY KEY (key);


--
-- TOC entry 4694 (class 2606 OID 103640)
-- Name: postaladdress_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY postaladdress_slot
    ADD CONSTRAINT postaladdress_slot_pkey PRIMARY KEY (postaladdress_key, child_slot_key);


--
-- TOC entry 4696 (class 2606 OID 103645)
-- Name: query_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY query
    ADD CONSTRAINT query_pkey PRIMARY KEY (key);


--
-- TOC entry 4706 (class 2606 OID 103671)
-- Name: query_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY query_slot
    ADD CONSTRAINT query_slot_pkey PRIMARY KEY (query_key, child_slot_key);


--
-- TOC entry 4698 (class 2606 OID 103653)
-- Name: querydefinition_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY querydefinition
    ADD CONSTRAINT querydefinition_pkey PRIMARY KEY (id);


--
-- TOC entry 4702 (class 2606 OID 103661)
-- Name: queryexpression_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY queryexpression
    ADD CONSTRAINT queryexpression_pkey PRIMARY KEY (key);


--
-- TOC entry 4704 (class 2606 OID 103666)
-- Name: queryexpression_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY queryexpression_slot
    ADD CONSTRAINT queryexpression_slot_pkey PRIMARY KEY (queryexpression_key, child_slot_key);


--
-- TOC entry 4708 (class 2606 OID 103679)
-- Name: registry_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registry
    ADD CONSTRAINT registry_pkey PRIMARY KEY (id);


--
-- TOC entry 4718 (class 2606 OID 103703)
-- Name: registryobject_classification_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registryobject_classification
    ADD CONSTRAINT registryobject_classification_pkey PRIMARY KEY (registryobject_id, classification_id);


--
-- TOC entry 4720 (class 2606 OID 103711)
-- Name: registryobject_externalidentifier_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registryobject_externalidentifier
    ADD CONSTRAINT registryobject_externalidentifier_pkey PRIMARY KEY (registryobject_id, externalidentifier_id);


--
-- TOC entry 4722 (class 2606 OID 103719)
-- Name: registryobject_externallink_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registryobject_externallink
    ADD CONSTRAINT registryobject_externallink_pkey PRIMARY KEY (registryobject_id, externallink_id);


--
-- TOC entry 4713 (class 2606 OID 103687)
-- Name: registryobject_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registryobject
    ADD CONSTRAINT registryobject_pkey PRIMARY KEY (id);


--
-- TOC entry 4725 (class 2606 OID 103724)
-- Name: registryobject_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registryobject_slot
    ADD CONSTRAINT registryobject_slot_pkey PRIMARY KEY (registryobject_id, child_slot_key);


--
-- TOC entry 4716 (class 2606 OID 103692)
-- Name: registryobjectlist_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registryobjectlist
    ADD CONSTRAINT registryobjectlist_pkey PRIMARY KEY (key);


--
-- TOC entry 4727 (class 2606 OID 103732)
-- Name: registrypackage_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT registrypackage_pkey PRIMARY KEY (id);


--
-- TOC entry 4731 (class 2606 OID 103740)
-- Name: role_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY role
    ADD CONSTRAINT role_pkey PRIMARY KEY (id);


--
-- TOC entry 4735 (class 2606 OID 103748)
-- Name: service_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY service
    ADD CONSTRAINT service_pkey PRIMARY KEY (id);


--
-- TOC entry 4739 (class 2606 OID 103756)
-- Name: servicebinding_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY servicebinding
    ADD CONSTRAINT servicebinding_pkey PRIMARY KEY (id);


--
-- TOC entry 4743 (class 2606 OID 103764)
-- Name: serviceendpoint_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY serviceendpoint
    ADD CONSTRAINT serviceendpoint_pkey PRIMARY KEY (id);


--
-- TOC entry 4747 (class 2606 OID 103772)
-- Name: serviceinterface_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY serviceinterface
    ADD CONSTRAINT serviceinterface_pkey PRIMARY KEY (id);


--
-- TOC entry 4751 (class 2606 OID 103786)
-- Name: simplelink_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY simplelink
    ADD CONSTRAINT simplelink_pkey PRIMARY KEY (key);


--
-- TOC entry 4754 (class 2606 OID 103794)
-- Name: slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY slot
    ADD CONSTRAINT slot_pkey PRIMARY KEY (key);


--
-- TOC entry 4756 (class 2606 OID 103799)
-- Name: slot_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY slot_slot
    ADD CONSTRAINT slot_slot_pkey PRIMARY KEY (slot_key, child_slot_key);


--
-- TOC entry 4758 (class 2606 OID 103807)
-- Name: stringqueryexpression_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY stringqueryexpression
    ADD CONSTRAINT stringqueryexpression_pkey PRIMARY KEY (key);


--
-- TOC entry 4760 (class 2606 OID 103815)
-- Name: subscription_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT subscription_pkey PRIMARY KEY (id);


--
-- TOC entry 4764 (class 2606 OID 103826)
-- Name: taxonomyelementtype_classificationnode_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY taxonomyelementtype_classificationnode
    ADD CONSTRAINT taxonomyelementtype_classificationnode_pkey PRIMARY KEY (taxonomyelementtype_id, classificationnode_id);


--
-- TOC entry 4766 (class 2606 OID 103834)
-- Name: telephonenumber_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY telephonenumber
    ADD CONSTRAINT telephonenumber_pkey PRIMARY KEY (key);


--
-- TOC entry 4768 (class 2606 OID 103839)
-- Name: telephonenumber_slot_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY telephonenumber_slot
    ADD CONSTRAINT telephonenumber_slot_pkey PRIMARY KEY (telephonenumber_key, child_slot_key);


--
-- TOC entry 4770 (class 2606 OID 103847)
-- Name: value_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY value
    ADD CONSTRAINT value_pkey PRIMARY KEY (key);


--
-- TOC entry 4773 (class 2606 OID 103858)
-- Name: vocabularyterm_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vocabularyterm
    ADD CONSTRAINT vocabularyterm_pkey PRIMARY KEY (vocabulary, term);


--
-- TOC entry 4775 (class 2606 OID 103866)
-- Name: workflowaction_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY workflowaction
    ADD CONSTRAINT workflowaction_pkey PRIMARY KEY (id);


--
-- TOC entry 4779 (class 2606 OID 103874)
-- Name: xmlqueryexpression_pkey; Type: CONSTRAINT; Schema: ebxml; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY xmlqueryexpression
    ADD CONSTRAINT xmlqueryexpression_pkey PRIMARY KEY (key);


--
-- TOC entry 4604 (class 1259 OID 103896)
-- Name: associationlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX associationlid_index ON association USING btree (lid);


--
-- TOC entry 4605 (class 1259 OID 103895)
-- Name: associationregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX associationregistryobjecttype_objecttype_idx ON association USING btree (objecttype);


--
-- TOC entry 4606 (class 1259 OID 103897)
-- Name: assoociation_type_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX assoociation_type_idx ON association USING btree (type);


--
-- TOC entry 4609 (class 1259 OID 103909)
-- Name: auditableeventlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX auditableeventlid_index ON auditableevent USING btree (lid);


--
-- TOC entry 4610 (class 1259 OID 103908)
-- Name: auditableeventregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX auditableeventregistryobjecttype_objecttype_idx ON auditableevent USING btree (objecttype);


--
-- TOC entry 4615 (class 1259 OID 103931)
-- Name: classificationlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX classificationlid_index ON classification USING btree (lid);


--
-- TOC entry 4619 (class 1259 OID 103944)
-- Name: classificationnodepath_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX classificationnodepath_idx ON classificationnode USING btree (path);


--
-- TOC entry 4620 (class 1259 OID 103943)
-- Name: classificationnodetaxonomyelementtypelid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX classificationnodetaxonomyelementtypelid_index ON classificationnode USING btree (lid);


--
-- TOC entry 4621 (class 1259 OID 103942)
-- Name: classificationnodetaxonomyelementtyperegistryobjecttype_objectt; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX classificationnodetaxonomyelementtyperegistryobjecttype_objectt ON classificationnode USING btree (objecttype);


--
-- TOC entry 4616 (class 1259 OID 103930)
-- Name: classificationregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX classificationregistryobjecttype_objecttype_idx ON classification USING btree (objecttype);


--
-- TOC entry 4624 (class 1259 OID 103956)
-- Name: classificationschemetaxonomyelementtypelid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX classificationschemetaxonomyelementtypelid_index ON classificationscheme USING btree (lid);


--
-- TOC entry 4625 (class 1259 OID 103955)
-- Name: classificationschemetaxonomyelementtyperegistryobjecttype_objec; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX classificationschemetaxonomyelementtyperegistryobjecttype_objec ON classificationscheme USING btree (objecttype);


--
-- TOC entry 4642 (class 1259 OID 103998)
-- Name: externalidentifierlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX externalidentifierlid_index ON externalidentifier USING btree (lid);


--
-- TOC entry 4643 (class 1259 OID 103997)
-- Name: externalidentifierregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX externalidentifierregistryobjecttype_objecttype_idx ON externalidentifier USING btree (objecttype);


--
-- TOC entry 4646 (class 1259 OID 104010)
-- Name: externallinklid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX externallinklid_index ON externallink USING btree (lid);


--
-- TOC entry 4647 (class 1259 OID 104009)
-- Name: externallinkregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX externallinkregistryobjecttype_objecttype_idx ON externallink USING btree (objecttype);


--
-- TOC entry 4650 (class 1259 OID 104027)
-- Name: extrinsicobjectlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX extrinsicobjectlid_index ON extrinsicobject USING btree (lid);


--
-- TOC entry 4651 (class 1259 OID 104026)
-- Name: extrinsicobjectregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX extrinsicobjectregistryobjecttype_objecttype_idx ON extrinsicobject USING btree (objecttype);


--
-- TOC entry 4654 (class 1259 OID 104044)
-- Name: federationlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX federationlid_index ON federation USING btree (lid);


--
-- TOC entry 4655 (class 1259 OID 104043)
-- Name: federationregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX federationregistryobjecttype_objecttype_idx ON federation USING btree (objecttype);


--
-- TOC entry 4658 (class 1259 OID 104539)
-- Name: internationalstring_localizedString_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX "internationalstring_localizedString_idx" ON internationalstring_localizedstring USING btree (internationalstring_key);


--
-- TOC entry 4711 (class 1259 OID 104301)
-- Name: lid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX lid_index ON registryobject USING btree (lid);


--
-- TOC entry 4667 (class 1259 OID 104076)
-- Name: notificationlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX notificationlid_index ON notification USING btree (lid);


--
-- TOC entry 4668 (class 1259 OID 104075)
-- Name: notificationregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX notificationregistryobjecttype_objecttype_idx ON notification USING btree (objecttype);


--
-- TOC entry 4677 (class 1259 OID 104118)
-- Name: organizationlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX organizationlid_index ON organization USING btree (lid);


--
-- TOC entry 4678 (class 1259 OID 104117)
-- Name: organizationregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX organizationregistryobjecttype_objecttype_idx ON organization USING btree (objecttype);


--
-- TOC entry 4685 (class 1259 OID 104185)
-- Name: personlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX personlid_index ON person USING btree (lid);


--
-- TOC entry 4686 (class 1259 OID 104184)
-- Name: personregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX personregistryobjecttype_objecttype_idx ON person USING btree (objecttype);


--
-- TOC entry 4699 (class 1259 OID 104252)
-- Name: querydefinitionlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX querydefinitionlid_index ON querydefinition USING btree (lid);


--
-- TOC entry 4700 (class 1259 OID 104251)
-- Name: querydefinitionregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX querydefinitionregistryobjecttype_objecttype_idx ON querydefinition USING btree (objecttype);


--
-- TOC entry 4709 (class 1259 OID 104289)
-- Name: registrylid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX registrylid_index ON registry USING btree (lid);


--
-- TOC entry 4723 (class 1259 OID 104540)
-- Name: registryobject_slot_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX registryobject_slot_idx ON registryobject_slot USING btree (registryobject_id);


--
-- TOC entry 4714 (class 1259 OID 104300)
-- Name: registryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX registryobjecttype_objecttype_idx ON registryobject USING btree (objecttype);


--
-- TOC entry 4728 (class 1259 OID 104338)
-- Name: registrypackagelid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX registrypackagelid_index ON registrypackage USING btree (lid);


--
-- TOC entry 4729 (class 1259 OID 104337)
-- Name: registrypackageregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX registrypackageregistryobjecttype_objecttype_idx ON registrypackage USING btree (objecttype);


--
-- TOC entry 4710 (class 1259 OID 104288)
-- Name: registryregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX registryregistryobjecttype_objecttype_idx ON registry USING btree (objecttype);


--
-- TOC entry 4732 (class 1259 OID 104355)
-- Name: rolelid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX rolelid_index ON role USING btree (lid);


--
-- TOC entry 4733 (class 1259 OID 104354)
-- Name: roleregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX roleregistryobjecttype_objecttype_idx ON role USING btree (objecttype);


--
-- TOC entry 4740 (class 1259 OID 104379)
-- Name: servicebindinglid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX servicebindinglid_index ON servicebinding USING btree (lid);


--
-- TOC entry 4741 (class 1259 OID 104378)
-- Name: servicebindingregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX servicebindingregistryobjecttype_objecttype_idx ON servicebinding USING btree (objecttype);


--
-- TOC entry 4744 (class 1259 OID 104391)
-- Name: serviceendpointlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX serviceendpointlid_index ON serviceendpoint USING btree (lid);


--
-- TOC entry 4745 (class 1259 OID 104390)
-- Name: serviceendpointregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX serviceendpointregistryobjecttype_objecttype_idx ON serviceendpoint USING btree (objecttype);


--
-- TOC entry 4748 (class 1259 OID 104403)
-- Name: serviceinterfacelid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX serviceinterfacelid_index ON serviceinterface USING btree (lid);


--
-- TOC entry 4749 (class 1259 OID 104402)
-- Name: serviceinterfaceregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX serviceinterfaceregistryobjecttype_objecttype_idx ON serviceinterface USING btree (objecttype);


--
-- TOC entry 4736 (class 1259 OID 104367)
-- Name: servicelid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX servicelid_index ON service USING btree (lid);


--
-- TOC entry 4737 (class 1259 OID 104366)
-- Name: serviceregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX serviceregistryobjecttype_objecttype_idx ON service USING btree (objecttype);


--
-- TOC entry 4752 (class 1259 OID 104424)
-- Name: slot_name_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX slot_name_idx ON slot USING btree (name);


--
-- TOC entry 4761 (class 1259 OID 104441)
-- Name: subscriptionlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX subscriptionlid_index ON subscription USING btree (lid);


--
-- TOC entry 4762 (class 1259 OID 104440)
-- Name: subscriptionregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX subscriptionregistryobjecttype_objecttype_idx ON subscription USING btree (objecttype);


--
-- TOC entry 4771 (class 1259 OID 104541)
-- Name: value_value_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX value_value_idx ON value_value USING btree (value_key);


--
-- TOC entry 4776 (class 1259 OID 104498)
-- Name: workflowactionlid_index; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX workflowactionlid_index ON workflowaction USING btree (lid);


--
-- TOC entry 4777 (class 1259 OID 104497)
-- Name: workflowactionregistryobjecttype_objecttype_idx; Type: INDEX; Schema: ebxml; Owner: awips; Tablespace: 
--

CREATE INDEX workflowactionregistryobjecttype_objecttype_idx ON workflowaction USING btree (objecttype);


--
-- TOC entry 4845 (class 2606 OID 104221)
-- Name: fk1a9f253f1cc2d914; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person_postaladdress
    ADD CONSTRAINT fk1a9f253f1cc2d914 FOREIGN KEY (postaladdress_key) REFERENCES postaladdress(key);


--
-- TOC entry 4844 (class 2606 OID 104226)
-- Name: fk1a9f253ff14343cc; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person_postaladdress
    ADD CONSTRAINT fk1a9f253ff14343cc FOREIGN KEY (person_id) REFERENCES person(id);


--
-- TOC entry 4890 (class 2606 OID 104477)
-- Name: fk1aba1ed0791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY telephonenumber_slot
    ADD CONSTRAINT fk1aba1ed0791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4891 (class 2606 OID 104472)
-- Name: fk1aba1ed0f3468e5c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY telephonenumber_slot
    ADD CONSTRAINT fk1aba1ed0f3468e5c FOREIGN KEY (telephonenumber_key) REFERENCES telephonenumber(key);


--
-- TOC entry 4884 (class 2606 OID 104452)
-- Name: fk1e21ad3deb1f4f85; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT fk1e21ad3deb1f4f85 FOREIGN KEY (selector_querydefinition) REFERENCES query(querydefinition);


--
-- TOC entry 4846 (class 2606 OID 104236)
-- Name: fk1edf7383f14343cc; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person_telephonenumber
    ADD CONSTRAINT fk1edf7383f14343cc FOREIGN KEY (person_id) REFERENCES person(id);


--
-- TOC entry 4847 (class 2606 OID 104231)
-- Name: fk1edf7383f3468e5c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person_telephonenumber
    ADD CONSTRAINT fk1edf7383f3468e5c FOREIGN KEY (telephonenumber_key) REFERENCES telephonenumber(key);


--
-- TOC entry 4894 (class 2606 OID 104487)
-- Name: fk214ba4e31cb1cda6; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY value_value
    ADD CONSTRAINT fk214ba4e31cb1cda6 FOREIGN KEY (collectionvalue_key) REFERENCES value(key);


--
-- TOC entry 4893 (class 2606 OID 104492)
-- Name: fk214ba4e3ae207b86; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY value_value
    ADD CONSTRAINT fk214ba4e3ae207b86 FOREIGN KEY (value_key) REFERENCES value(key);


--
-- TOC entry 4827 (class 2606 OID 104129)
-- Name: fk219f5c4459e50e0c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization_emailaddress
    ADD CONSTRAINT fk219f5c4459e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);


--
-- TOC entry 4826 (class 2606 OID 104134)
-- Name: fk219f5c4487a0de2; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization_emailaddress
    ADD CONSTRAINT fk219f5c4487a0de2 FOREIGN KEY (emailaddress_key) REFERENCES emailaddress(key);


--
-- TOC entry 4862 (class 2606 OID 104317)
-- Name: fk220c0ca97146a5ac; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registryobject_classification
    ADD CONSTRAINT fk220c0ca97146a5ac FOREIGN KEY (classification_id) REFERENCES classification(id);


--
-- TOC entry 4841 (class 2606 OID 104201)
-- Name: fk23dea75d305d8b4d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY personname_slot
    ADD CONSTRAINT fk23dea75d305d8b4d FOREIGN KEY (personname_middlename, personname_lastname, personname_firstname) REFERENCES personname(middlename, lastname, firstname);


--
-- TOC entry 4840 (class 2606 OID 104206)
-- Name: fk23dea75d791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY personname_slot
    ADD CONSTRAINT fk23dea75d791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4881 (class 2606 OID 104425)
-- Name: fk275e1e3d4e5e46; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY slot
    ADD CONSTRAINT fk275e1e3d4e5e46 FOREIGN KEY (slotvalue_key) REFERENCES value(key);


--
-- TOC entry 4832 (class 2606 OID 104154)
-- Name: fk2763b16159e50e0c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization_telephonenumber
    ADD CONSTRAINT fk2763b16159e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);


--
-- TOC entry 4831 (class 2606 OID 104159)
-- Name: fk2763b161f3468e5c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization_telephonenumber
    ADD CONSTRAINT fk2763b161f3468e5c FOREIGN KEY (telephonenumber_key) REFERENCES telephonenumber(key);


--
-- TOC entry 4829 (class 2606 OID 104149)
-- Name: fk3035429d1cc2d914; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization_postaladdress
    ADD CONSTRAINT fk3035429d1cc2d914 FOREIGN KEY (postaladdress_key) REFERENCES postaladdress(key);


--
-- TOC entry 4830 (class 2606 OID 104144)
-- Name: fk3035429d59e50e0c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization_postaladdress
    ADD CONSTRAINT fk3035429d59e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);


--
-- TOC entry 4799 (class 2606 OID 103977)
-- Name: fk31c32925791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY emailaddress_slot
    ADD CONSTRAINT fk31c32925791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4798 (class 2606 OID 103982)
-- Name: fk31c3292587a0de2; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY emailaddress_slot
    ADD CONSTRAINT fk31c3292587a0de2 FOREIGN KEY (emailaddress_key) REFERENCES emailaddress(key);


--
-- TOC entry 4800 (class 2606 OID 103992)
-- Name: fk40018526aa2c3c0; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY entry
    ADD CONSTRAINT fk40018526aa2c3c0 FOREIGN KEY (entrykey_key) REFERENCES value(key);


--
-- TOC entry 4801 (class 2606 OID 103987)
-- Name: fk400185272ea4252; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY entry
    ADD CONSTRAINT fk400185272ea4252 FOREIGN KEY (entryvalue_key) REFERENCES value(key);


--
-- TOC entry 4892 (class 2606 OID 104482)
-- Name: fk4e9a151f53db13; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY value
    ADD CONSTRAINT fk4e9a151f53db13 FOREIGN KEY (internationalstringvalue_key) REFERENCES internationalstring(key);


--
-- TOC entry 4860 (class 2606 OID 104302)
-- Name: fk5880a9fc94b6895d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registryobject
    ADD CONSTRAINT fk5880a9fc94b6895d FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4787 (class 2606 OID 103910)
-- Name: fk5880a9fc94b6895d134a8c05; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY auditableevent
    ADD CONSTRAINT fk5880a9fc94b6895d134a8c05 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4886 (class 2606 OID 104442)
-- Name: fk5880a9fc94b6895d1e21ad3d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT fk5880a9fc94b6895d1e21ad3d FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4870 (class 2606 OID 104356)
-- Name: fk5880a9fc94b6895d26f496; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY role
    ADD CONSTRAINT fk5880a9fc94b6895d26f496 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4817 (class 2606 OID 104077)
-- Name: fk5880a9fc94b6895d2d45dd0b; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY notification
    ADD CONSTRAINT fk5880a9fc94b6895d2d45dd0b FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4874 (class 2606 OID 104380)
-- Name: fk5880a9fc94b6895d44524630; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY servicebinding
    ADD CONSTRAINT fk5880a9fc94b6895d44524630 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4825 (class 2606 OID 104119)
-- Name: fk5880a9fc94b6895d50104153; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT fk5880a9fc94b6895d50104153 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4803 (class 2606 OID 103999)
-- Name: fk5880a9fc94b6895d56d1a6f4; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY externalidentifier
    ADD CONSTRAINT fk5880a9fc94b6895d56d1a6f4 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4785 (class 2606 OID 103898)
-- Name: fk5880a9fc94b6895d5e328461; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY association
    ADD CONSTRAINT fk5880a9fc94b6895d5e328461 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4876 (class 2606 OID 104392)
-- Name: fk5880a9fc94b6895d75b47b4a; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY serviceendpoint
    ADD CONSTRAINT fk5880a9fc94b6895d75b47b4a FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4811 (class 2606 OID 104045)
-- Name: fk5880a9fc94b6895d85f55363; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY federation
    ADD CONSTRAINT fk5880a9fc94b6895d85f55363 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4839 (class 2606 OID 104186)
-- Name: fk5880a9fc94b6895d8e488775; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person
    ADD CONSTRAINT fk5880a9fc94b6895d8e488775 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4806 (class 2606 OID 104011)
-- Name: fk5880a9fc94b6895db72bfe85; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY externallink
    ADD CONSTRAINT fk5880a9fc94b6895db72bfe85 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4851 (class 2606 OID 104253)
-- Name: fk5880a9fc94b6895dca0a881b; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY querydefinition
    ADD CONSTRAINT fk5880a9fc94b6895dca0a881b FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4878 (class 2606 OID 104404)
-- Name: fk5880a9fc94b6895dcdb3c624; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY serviceinterface
    ADD CONSTRAINT fk5880a9fc94b6895dcdb3c624 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4896 (class 2606 OID 104499)
-- Name: fk5880a9fc94b6895dce4002f5; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY workflowaction
    ADD CONSTRAINT fk5880a9fc94b6895dce4002f5 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4809 (class 2606 OID 104028)
-- Name: fk5880a9fc94b6895dd97867b6; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT fk5880a9fc94b6895dd97867b6 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4872 (class 2606 OID 104368)
-- Name: fk5880a9fc94b6895dd97c5e95; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY service
    ADD CONSTRAINT fk5880a9fc94b6895dd97c5e95 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4858 (class 2606 OID 104290)
-- Name: fk5880a9fc94b6895ddab6945d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registry
    ADD CONSTRAINT fk5880a9fc94b6895ddab6945d FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4791 (class 2606 OID 103932)
-- Name: fk5880a9fc94b6895ddb110006; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY classification
    ADD CONSTRAINT fk5880a9fc94b6895ddb110006 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4795 (class 2606 OID 103957)
-- Name: fk5880a9fc94b6895ddcd5f4255afc854b; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY classificationscheme
    ADD CONSTRAINT fk5880a9fc94b6895ddcd5f4255afc854b FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4793 (class 2606 OID 103945)
-- Name: fk5880a9fc94b6895ddcd5f425eb0aaf28; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY classificationnode
    ADD CONSTRAINT fk5880a9fc94b6895ddcd5f425eb0aaf28 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4868 (class 2606 OID 104339)
-- Name: fk5880a9fc94b6895dea660529; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT fk5880a9fc94b6895dea660529 FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4859 (class 2606 OID 104307)
-- Name: fk5880a9fcc7a8a06c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registryobject
    ADD CONSTRAINT fk5880a9fcc7a8a06c FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4786 (class 2606 OID 103915)
-- Name: fk5880a9fcc7a8a06c134a8c05; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY auditableevent
    ADD CONSTRAINT fk5880a9fcc7a8a06c134a8c05 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4885 (class 2606 OID 104447)
-- Name: fk5880a9fcc7a8a06c1e21ad3d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY subscription
    ADD CONSTRAINT fk5880a9fcc7a8a06c1e21ad3d FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4869 (class 2606 OID 104361)
-- Name: fk5880a9fcc7a8a06c26f496; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY role
    ADD CONSTRAINT fk5880a9fcc7a8a06c26f496 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4816 (class 2606 OID 104082)
-- Name: fk5880a9fcc7a8a06c2d45dd0b; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY notification
    ADD CONSTRAINT fk5880a9fcc7a8a06c2d45dd0b FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4873 (class 2606 OID 104385)
-- Name: fk5880a9fcc7a8a06c44524630; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY servicebinding
    ADD CONSTRAINT fk5880a9fcc7a8a06c44524630 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4824 (class 2606 OID 104124)
-- Name: fk5880a9fcc7a8a06c50104153; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization
    ADD CONSTRAINT fk5880a9fcc7a8a06c50104153 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4802 (class 2606 OID 104004)
-- Name: fk5880a9fcc7a8a06c56d1a6f4; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY externalidentifier
    ADD CONSTRAINT fk5880a9fcc7a8a06c56d1a6f4 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4784 (class 2606 OID 103903)
-- Name: fk5880a9fcc7a8a06c5e328461; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY association
    ADD CONSTRAINT fk5880a9fcc7a8a06c5e328461 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4875 (class 2606 OID 104397)
-- Name: fk5880a9fcc7a8a06c75b47b4a; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY serviceendpoint
    ADD CONSTRAINT fk5880a9fcc7a8a06c75b47b4a FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4810 (class 2606 OID 104050)
-- Name: fk5880a9fcc7a8a06c85f55363; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY federation
    ADD CONSTRAINT fk5880a9fcc7a8a06c85f55363 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4838 (class 2606 OID 104191)
-- Name: fk5880a9fcc7a8a06c8e488775; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person
    ADD CONSTRAINT fk5880a9fcc7a8a06c8e488775 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4804 (class 2606 OID 104021)
-- Name: fk5880a9fcc7a8a06cb72bfe85; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY externallink
    ADD CONSTRAINT fk5880a9fcc7a8a06cb72bfe85 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4850 (class 2606 OID 104258)
-- Name: fk5880a9fcc7a8a06cca0a881b; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY querydefinition
    ADD CONSTRAINT fk5880a9fcc7a8a06cca0a881b FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4877 (class 2606 OID 104409)
-- Name: fk5880a9fcc7a8a06ccdb3c624; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY serviceinterface
    ADD CONSTRAINT fk5880a9fcc7a8a06ccdb3c624 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4895 (class 2606 OID 104504)
-- Name: fk5880a9fcc7a8a06cce4002f5; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY workflowaction
    ADD CONSTRAINT fk5880a9fcc7a8a06cce4002f5 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4808 (class 2606 OID 104033)
-- Name: fk5880a9fcc7a8a06cd97867b6; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT fk5880a9fcc7a8a06cd97867b6 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4871 (class 2606 OID 104373)
-- Name: fk5880a9fcc7a8a06cd97c5e95; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY service
    ADD CONSTRAINT fk5880a9fcc7a8a06cd97c5e95 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4857 (class 2606 OID 104295)
-- Name: fk5880a9fcc7a8a06cdab6945d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registry
    ADD CONSTRAINT fk5880a9fcc7a8a06cdab6945d FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4790 (class 2606 OID 103937)
-- Name: fk5880a9fcc7a8a06cdb110006; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY classification
    ADD CONSTRAINT fk5880a9fcc7a8a06cdb110006 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4794 (class 2606 OID 103962)
-- Name: fk5880a9fcc7a8a06cdcd5f4255afc854b; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY classificationscheme
    ADD CONSTRAINT fk5880a9fcc7a8a06cdcd5f4255afc854b FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4792 (class 2606 OID 103950)
-- Name: fk5880a9fcc7a8a06cdcd5f425eb0aaf28; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY classificationnode
    ADD CONSTRAINT fk5880a9fcc7a8a06cdcd5f425eb0aaf28 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4866 (class 2606 OID 104349)
-- Name: fk5880a9fcc7a8a06cea660529; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT fk5880a9fcc7a8a06cea660529 FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4863 (class 2606 OID 104322)
-- Name: fk5a5faa17239c540c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registryobject_externalidentifier
    ADD CONSTRAINT fk5a5faa17239c540c FOREIGN KEY (externalidentifier_id) REFERENCES externalidentifier(id);


--
-- TOC entry 4842 (class 2606 OID 104216)
-- Name: fk62fd9d6287a0de2; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person_emailaddress
    ADD CONSTRAINT fk62fd9d6287a0de2 FOREIGN KEY (emailaddress_key) REFERENCES emailaddress(key);


--
-- TOC entry 4843 (class 2606 OID 104211)
-- Name: fk62fd9d62f14343cc; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person_emailaddress
    ADD CONSTRAINT fk62fd9d62f14343cc FOREIGN KEY (person_id) REFERENCES person(id);


--
-- TOC entry 4856 (class 2606 OID 104278)
-- Name: fk6cbd175565b826e; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY query_slot
    ADD CONSTRAINT fk6cbd175565b826e FOREIGN KEY (query_querydefinition) REFERENCES query(querydefinition);


--
-- TOC entry 4855 (class 2606 OID 104283)
-- Name: fk6cbd1755791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY query_slot
    ADD CONSTRAINT fk6cbd1755791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4823 (class 2606 OID 104107)
-- Name: fk6f31e6c9791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY objectref_slot
    ADD CONSTRAINT fk6f31e6c9791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4822 (class 2606 OID 104112)
-- Name: fk6f31e6c9e422821c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY objectref_slot
    ADD CONSTRAINT fk6f31e6c9e422821c FOREIGN KEY (objectref_id) REFERENCES objectref(id);


--
-- TOC entry 4864 (class 2606 OID 104327)
-- Name: fk71b7d7e8976827ec; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registryobject_externallink
    ADD CONSTRAINT fk71b7d7e8976827ec FOREIGN KEY (externallink_id) REFERENCES externallink(id);


--
-- TOC entry 4780 (class 2606 OID 103880)
-- Name: fk74946a5618cc8927; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY action
    ADD CONSTRAINT fk74946a5618cc8927 FOREIGN KEY (affectedobjectrefs_key) REFERENCES objectreflist(key);


--
-- TOC entry 4781 (class 2606 OID 103875)
-- Name: fk74946a566900fa0c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY action
    ADD CONSTRAINT fk74946a566900fa0c FOREIGN KEY (affectedobjects_key) REFERENCES registryobjectlist(key);


--
-- TOC entry 4796 (class 2606 OID 103972)
-- Name: fk7709e55b3af51bb6; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY deliveryinfo_slot
    ADD CONSTRAINT fk7709e55b3af51bb6 FOREIGN KEY (deliveryinfo_key) REFERENCES deliveryinfo(key);


--
-- TOC entry 4797 (class 2606 OID 103967)
-- Name: fk7709e55b791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY deliveryinfo_slot
    ADD CONSTRAINT fk7709e55b791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4888 (class 2606 OID 104457)
-- Name: fk7a416a43af51bb6; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY subscription_deliveryinfo
    ADD CONSTRAINT fk7a416a43af51bb6 FOREIGN KEY (deliveryinfo_key) REFERENCES deliveryinfo(key);


--
-- TOC entry 4887 (class 2606 OID 104462)
-- Name: fk7a416a458788cc; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY subscription_deliveryinfo
    ADD CONSTRAINT fk7a416a458788cc FOREIGN KEY (subscription_id) REFERENCES subscription(id);


--
-- TOC entry 4835 (class 2606 OID 104179)
-- Name: fk7c511145e6862f4; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY parameter_slot
    ADD CONSTRAINT fk7c511145e6862f4 FOREIGN KEY (parameter_key) REFERENCES parameter(key);


--
-- TOC entry 4836 (class 2606 OID 104174)
-- Name: fk7c51114791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY parameter_slot
    ADD CONSTRAINT fk7c51114791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4861 (class 2606 OID 104312)
-- Name: fk8312dc217486c946; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registryobjectlist_registryobject
    ADD CONSTRAINT fk8312dc217486c946 FOREIGN KEY (registryobjectlist_key) REFERENCES registryobjectlist(key);


--
-- TOC entry 4837 (class 2606 OID 104196)
-- Name: fk8e488775305d8b4d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY person
    ADD CONSTRAINT fk8e488775305d8b4d FOREIGN KEY (personname_middlename, personname_lastname, personname_firstname) REFERENCES personname(middlename, lastname, firstname);


--
-- TOC entry 4819 (class 2606 OID 104087)
-- Name: fk9a005b9886a370c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY notification_auditableevent
    ADD CONSTRAINT fk9a005b9886a370c FOREIGN KEY (notification_id) REFERENCES notification(id);


--
-- TOC entry 4818 (class 2606 OID 104092)
-- Name: fk9a005b995d0eef7; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY notification_auditableevent
    ADD CONSTRAINT fk9a005b995d0eef7 FOREIGN KEY (event_id) REFERENCES auditableevent(id);


--
-- TOC entry 4883 (class 2606 OID 104430)
-- Name: fka056db9f791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY slot_slot
    ADD CONSTRAINT fka056db9f791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4882 (class 2606 OID 104435)
-- Name: fka056db9fd0b27a4e; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY slot_slot
    ADD CONSTRAINT fka056db9fd0b27a4e FOREIGN KEY (slot_key) REFERENCES slot(key);


--
-- TOC entry 4854 (class 2606 OID 104273)
-- Name: fka0fd8d9d791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY queryexpression_slot
    ADD CONSTRAINT fka0fd8d9d791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4828 (class 2606 OID 104139)
-- Name: fka7b968bf59e50e0c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY organization_organization
    ADD CONSTRAINT fka7b968bf59e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);


--
-- TOC entry 4805 (class 2606 OID 104016)
-- Name: fkb72bfe856d351206; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY externallink
    ADD CONSTRAINT fkb72bfe856d351206 FOREIGN KEY (externalref_key) REFERENCES simplelink(key);


--
-- TOC entry 4812 (class 2606 OID 104060)
-- Name: fkba1f172c246c6800; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY internationalstring_localizedstring
    ADD CONSTRAINT fkba1f172c246c6800 FOREIGN KEY (internationalstring_key) REFERENCES internationalstring(key);


--
-- TOC entry 4813 (class 2606 OID 104055)
-- Name: fkba1f172cee9b1d5a; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY internationalstring_localizedstring
    ADD CONSTRAINT fkba1f172cee9b1d5a FOREIGN KEY (localizedstring_key) REFERENCES localizedstring(key);


--
-- TOC entry 4853 (class 2606 OID 104263)
-- Name: fkbd1306518f9477c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY querydefinition_parameter
    ADD CONSTRAINT fkbd1306518f9477c FOREIGN KEY (querydefinition_id) REFERENCES querydefinition(id);


--
-- TOC entry 4852 (class 2606 OID 104268)
-- Name: fkbd130655e6862f4; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY querydefinition_parameter
    ADD CONSTRAINT fkbd130655e6862f4 FOREIGN KEY (parameter_key) REFERENCES parameter(key);


--
-- TOC entry 4814 (class 2606 OID 104070)
-- Name: fkbfbdcb8f92e9c6ba; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY map_entry
    ADD CONSTRAINT fkbfbdcb8f92e9c6ba FOREIGN KEY (entry_entryvalue_key, entry_entrykey_key) REFERENCES entry(entryvalue_key, entrykey_key);


--
-- TOC entry 4815 (class 2606 OID 104065)
-- Name: fkbfbdcb8ffb10015a; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY map_entry
    ADD CONSTRAINT fkbfbdcb8ffb10015a FOREIGN KEY (map_key) REFERENCES map(key);


--
-- TOC entry 4849 (class 2606 OID 104241)
-- Name: fkc0fe08141cc2d914; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY postaladdress_slot
    ADD CONSTRAINT fkc0fe08141cc2d914 FOREIGN KEY (postaladdress_key) REFERENCES postaladdress(key);


--
-- TOC entry 4848 (class 2606 OID 104246)
-- Name: fkc0fe0814791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY postaladdress_slot
    ADD CONSTRAINT fkc0fe0814791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4783 (class 2606 OID 103885)
-- Name: fkc1c0b26772bc74be; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY action_slot
    ADD CONSTRAINT fkc1c0b26772bc74be FOREIGN KEY (action_key) REFERENCES action(key);


--
-- TOC entry 4782 (class 2606 OID 103890)
-- Name: fkc1c0b267791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY action_slot
    ADD CONSTRAINT fkc1c0b267791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4789 (class 2606 OID 103920)
-- Name: fkc76a3bd072bc74be; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY auditableevent_action
    ADD CONSTRAINT fkc76a3bd072bc74be FOREIGN KEY (action_key) REFERENCES action(key);


--
-- TOC entry 4788 (class 2606 OID 103925)
-- Name: fkc76a3bd086bc35ac; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY auditableevent_action
    ADD CONSTRAINT fkc76a3bd086bc35ac FOREIGN KEY (auditableevent_id) REFERENCES auditableevent(id);


--
-- TOC entry 4821 (class 2606 OID 104097)
-- Name: fkcffc8027caf899c6; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY objectreflist_objectref
    ADD CONSTRAINT fkcffc8027caf899c6 FOREIGN KEY (objectreflist_key) REFERENCES objectreflist(key);


--
-- TOC entry 4820 (class 2606 OID 104102)
-- Name: fkcffc8027e422821c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY objectreflist_objectref
    ADD CONSTRAINT fkcffc8027e422821c FOREIGN KEY (objectref_id) REFERENCES objectref(id);


--
-- TOC entry 4807 (class 2606 OID 104038)
-- Name: fkd97867b6a9a5cc34; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT fkd97867b6a9a5cc34 FOREIGN KEY (repositoryitemref_key) REFERENCES simplelink(key);


--
-- TOC entry 4865 (class 2606 OID 104332)
-- Name: fkde84a81791fb611; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registryobject_slot
    ADD CONSTRAINT fkde84a81791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);


--
-- TOC entry 4879 (class 2606 OID 104419)
-- Name: fkdeb465a04ee4881c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY service_serviceendpoint
    ADD CONSTRAINT fkdeb465a04ee4881c FOREIGN KEY (service_id) REFERENCES service(id);


--
-- TOC entry 4880 (class 2606 OID 104414)
-- Name: fkdeb465a0ad801d9c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY service_serviceendpoint
    ADD CONSTRAINT fkdeb465a0ad801d9c FOREIGN KEY (serviceendpoint_id) REFERENCES serviceendpoint(id);


--
-- TOC entry 4867 (class 2606 OID 104344)
-- Name: fkea6605297486c946; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT fkea6605297486c946 FOREIGN KEY (registryobjectlist_key) REFERENCES registryobjectlist(key);


--
-- TOC entry 4834 (class 2606 OID 104164)
-- Name: fkebf0d38994b6895d; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY parameter
    ADD CONSTRAINT fkebf0d38994b6895d FOREIGN KEY (description_key) REFERENCES internationalstring(key);


--
-- TOC entry 4833 (class 2606 OID 104169)
-- Name: fkebf0d389c7a8a06c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY parameter
    ADD CONSTRAINT fkebf0d389c7a8a06c FOREIGN KEY (name_key) REFERENCES internationalstring(key);


--
-- TOC entry 4889 (class 2606 OID 104467)
-- Name: fkf9067d82c36558c; Type: FK CONSTRAINT; Schema: ebxml; Owner: awips
--

ALTER TABLE ONLY taxonomyelementtype_classificationnode
    ADD CONSTRAINT fkf9067d82c36558c FOREIGN KEY (classificationnode_id) REFERENCES classificationnode(id);


-- Completed on 2013-03-22 13:36:06 CDT

--
-- PostgreSQL database dump complete
--

