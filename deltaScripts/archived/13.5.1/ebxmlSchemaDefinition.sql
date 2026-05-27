SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
DROP SCHEMA IF EXISTS ebxml CASCADE;
CREATE SCHEMA ebxml;
ALTER SCHEMA ebxml OWNER TO awips;
SET search_path = ebxml, pg_catalog;
SET default_tablespace = '';
SET default_with_oids = false;
CREATE TABLE action (
    key integer NOT NULL,
    eventtype character varying(255),
    affectedobjectrefs_key integer,
    affectedobjects_key integer
);
ALTER TABLE ebxml.action OWNER TO awips;
CREATE SEQUENCE action_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.action_sequence OWNER TO awips;
CREATE TABLE action_slot (
    action_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.action_slot OWNER TO awips;
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
CREATE TABLE auditableevent_action (
    auditableevent_id character varying(255) NOT NULL,
    action_key integer NOT NULL
);
ALTER TABLE ebxml.auditableevent_action OWNER TO awips;
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
CREATE TABLE deliveryinfo (
    key integer NOT NULL,
    notificationoption character varying(255),
    notifyto text
);
ALTER TABLE ebxml.deliveryinfo OWNER TO awips;
CREATE SEQUENCE deliveryinfo_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.deliveryinfo_sequence OWNER TO awips;
CREATE TABLE deliveryinfo_slot (
    deliveryinfo_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.deliveryinfo_slot OWNER TO awips;
CREATE TABLE emailaddress (
    key integer NOT NULL,
    address character varying(255),
    type character varying(255)
);
ALTER TABLE ebxml.emailaddress OWNER TO awips;
CREATE SEQUENCE emailaddress_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.emailaddress_sequence OWNER TO awips;
CREATE TABLE emailaddress_slot (
    emailaddress_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.emailaddress_slot OWNER TO awips;
CREATE TABLE entry (
    entryvalue_key integer NOT NULL,
    entrykey_key integer NOT NULL
);
ALTER TABLE ebxml.entry OWNER TO awips;
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
CREATE TABLE internationalstring (
    key integer NOT NULL
);
ALTER TABLE ebxml.internationalstring OWNER TO awips;
CREATE TABLE internationalstring_localizedstring (
    internationalstring_key integer NOT NULL,
    localizedstring_key integer NOT NULL
);
ALTER TABLE ebxml.internationalstring_localizedstring OWNER TO awips;
CREATE SEQUENCE internationalstring_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.internationalstring_sequence OWNER TO awips;
CREATE TABLE localizedstring (
    key integer NOT NULL,
    lang character varying(255),
    value character varying(1024)
);
ALTER TABLE ebxml.localizedstring OWNER TO awips;
CREATE SEQUENCE localizedstring_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.localizedstring_sequence OWNER TO awips;
CREATE TABLE map (
    key integer NOT NULL
);
ALTER TABLE ebxml.map OWNER TO awips;
CREATE TABLE map_entry (
    map_key integer NOT NULL,
    entry_entryvalue_key integer NOT NULL,
    entry_entrykey_key integer NOT NULL
);
ALTER TABLE ebxml.map_entry OWNER TO awips;
CREATE SEQUENCE map_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.map_sequence OWNER TO awips;
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
CREATE TABLE notification_auditableevent (
    notification_id character varying(255) NOT NULL,
    event_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.notification_auditableevent OWNER TO awips;
CREATE TABLE objectref (
    id character varying(255) NOT NULL
);
ALTER TABLE ebxml.objectref OWNER TO awips;
CREATE TABLE objectref_slot (
    objectref_id character varying(255) NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.objectref_slot OWNER TO awips;
CREATE TABLE objectreflist (
    key integer NOT NULL
);
ALTER TABLE ebxml.objectreflist OWNER TO awips;
CREATE TABLE objectreflist_objectref (
    objectreflist_key integer NOT NULL,
    objectref_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.objectreflist_objectref OWNER TO awips;
CREATE SEQUENCE objectreflist_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.objectreflist_sequence OWNER TO awips;
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
CREATE TABLE organization_emailaddress (
    organization_id character varying(255) NOT NULL,
    emailaddress_key integer NOT NULL
);
ALTER TABLE ebxml.organization_emailaddress OWNER TO awips;
CREATE TABLE organization_organization (
    organization_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.organization_organization OWNER TO awips;
CREATE TABLE organization_postaladdress (
    organization_id character varying(255) NOT NULL,
    postaladdress_key integer NOT NULL
);
ALTER TABLE ebxml.organization_postaladdress OWNER TO awips;
CREATE TABLE organization_telephonenumber (
    organization_id character varying(255) NOT NULL,
    telephonenumber_key integer NOT NULL
);
ALTER TABLE ebxml.organization_telephonenumber OWNER TO awips;
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
CREATE SEQUENCE parameter_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.parameter_sequence OWNER TO awips;
CREATE TABLE parameter_slot (
    parameter_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.parameter_slot OWNER TO awips;
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
CREATE TABLE person_emailaddress (
    person_id character varying(255) NOT NULL,
    emailaddress_key integer NOT NULL
);
ALTER TABLE ebxml.person_emailaddress OWNER TO awips;
CREATE TABLE person_postaladdress (
    person_id character varying(255) NOT NULL,
    postaladdress_key integer NOT NULL
);
ALTER TABLE ebxml.person_postaladdress OWNER TO awips;
CREATE TABLE person_telephonenumber (
    person_id character varying(255) NOT NULL,
    telephonenumber_key integer NOT NULL
);
ALTER TABLE ebxml.person_telephonenumber OWNER TO awips;
CREATE TABLE personname (
    middlename character varying(255) NOT NULL,
    lastname character varying(255) NOT NULL,
    firstname character varying(255) NOT NULL
);
ALTER TABLE ebxml.personname OWNER TO awips;
CREATE TABLE personname_slot (
    personname_middlename character varying(255) NOT NULL,
    personname_lastname character varying(255) NOT NULL,
    personname_firstname character varying(255) NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.personname_slot OWNER TO awips;
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
CREATE SEQUENCE postaladdress_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.postaladdress_sequence OWNER TO awips;
CREATE TABLE postaladdress_slot (
    postaladdress_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.postaladdress_slot OWNER TO awips;
CREATE TABLE query (
    key integer NOT NULL,
    querydefinition character varying(255)
);
ALTER TABLE ebxml.query OWNER TO awips;
CREATE SEQUENCE query_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.query_sequence OWNER TO awips;
CREATE TABLE query_slot (
    query_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.query_slot OWNER TO awips;
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
CREATE TABLE querydefinition_parameter (
    querydefinition_id character varying(255) NOT NULL,
    parameter_key integer NOT NULL
);
ALTER TABLE ebxml.querydefinition_parameter OWNER TO awips;
CREATE TABLE queryexpression (
    key integer NOT NULL,
    querylanguage character varying(255)
);
ALTER TABLE ebxml.queryexpression OWNER TO awips;
CREATE SEQUENCE queryexpression_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.queryexpression_sequence OWNER TO awips;
CREATE TABLE queryexpression_slot (
    queryexpression_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.queryexpression_slot OWNER TO awips;
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
CREATE TABLE registryobject_classification (
    registryobject_id character varying(255) NOT NULL,
    classification_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.registryobject_classification OWNER TO awips;
CREATE TABLE registryobject_externalidentifier (
    registryobject_id character varying(255) NOT NULL,
    externalidentifier_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.registryobject_externalidentifier OWNER TO awips;
CREATE TABLE registryobject_externallink (
    registryobject_id character varying(255) NOT NULL,
    externallink_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.registryobject_externallink OWNER TO awips;
CREATE TABLE registryobject_slot (
    registryobject_id character varying(255) NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.registryobject_slot OWNER TO awips;
CREATE TABLE registryobjectlist (
    key integer NOT NULL
);
ALTER TABLE ebxml.registryobjectlist OWNER TO awips;
CREATE TABLE registryobjectlist_registryobject (
    registryobjectlist_key integer NOT NULL,
    registryobject_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.registryobjectlist_registryobject OWNER TO awips;
CREATE SEQUENCE registryobjectlist_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.registryobjectlist_sequence OWNER TO awips;
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
CREATE TABLE service_serviceendpoint (
    service_id character varying(255) NOT NULL,
    serviceendpoint_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.service_serviceendpoint OWNER TO awips;
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
CREATE TABLE simplelink (
    key integer NOT NULL,
    arcrole character varying(255),
    href character varying(255),
    role character varying(255),
    title character varying(255)
);
ALTER TABLE ebxml.simplelink OWNER TO awips;
CREATE SEQUENCE simplelink_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.simplelink_sequence OWNER TO awips;
CREATE TABLE slot (
    key integer NOT NULL,
    name character varying(255),
    type character varying(255),
    slotvalue_key integer
);
ALTER TABLE ebxml.slot OWNER TO awips;
CREATE SEQUENCE slot_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.slot_sequence OWNER TO awips;
CREATE TABLE slot_slot (
    slot_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.slot_slot OWNER TO awips;
CREATE TABLE stringqueryexpression (
    key integer NOT NULL,
    querylanguage character varying(255),
    value character varying(255)
);
ALTER TABLE ebxml.stringqueryexpression OWNER TO awips;
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
    notificationinterval bigint,
    starttime timestamp without time zone,
    selector_key integer
);
ALTER TABLE ebxml.subscription OWNER TO awips;
CREATE TABLE subscription_deliveryinfo (
    subscription_id character varying(255) NOT NULL,
    deliveryinfo_key integer NOT NULL
);
ALTER TABLE ebxml.subscription_deliveryinfo OWNER TO awips;
CREATE TABLE taxonomyelementtype_classificationnode (
    taxonomyelementtype_id character varying(255) NOT NULL,
    classificationnode_id character varying(255) NOT NULL
);
ALTER TABLE ebxml.taxonomyelementtype_classificationnode OWNER TO awips;
CREATE TABLE telephonenumber (
    key integer NOT NULL,
    areacode character varying(255),
    countrycode character varying(255),
    extension character varying(255),
    number character varying(255),
    type character varying(255)
);
ALTER TABLE ebxml.telephonenumber OWNER TO awips;
CREATE SEQUENCE telephonenumber_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.telephonenumber_sequence OWNER TO awips;
CREATE TABLE telephonenumber_slot (
    telephonenumber_key integer NOT NULL,
    child_slot_key integer NOT NULL
);
ALTER TABLE ebxml.telephonenumber_slot OWNER TO awips;
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
CREATE SEQUENCE value_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE ebxml.value_sequence OWNER TO awips;
CREATE TABLE value_value (
    value_key integer NOT NULL,
    collectionvalue_key integer NOT NULL
);
ALTER TABLE ebxml.value_value OWNER TO awips;
CREATE TABLE vocabularyterm (
    vocabulary character varying(255) NOT NULL,
    term character varying(255) NOT NULL
);
ALTER TABLE ebxml.vocabularyterm OWNER TO awips;
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
CREATE TABLE xmlqueryexpression (
    key integer NOT NULL,
    querylanguage character varying(255),
    anyvalue text
);
ALTER TABLE ebxml.xmlqueryexpression OWNER TO awips;
ALTER TABLE ONLY action
    ADD CONSTRAINT action_pkey PRIMARY KEY (key);
ALTER TABLE ONLY action_slot
    ADD CONSTRAINT action_slot_pkey PRIMARY KEY (action_key, child_slot_key);
ALTER TABLE ONLY association
    ADD CONSTRAINT association_pkey PRIMARY KEY (id);
ALTER TABLE ONLY auditableevent_action
    ADD CONSTRAINT auditableevent_action_action_key_key UNIQUE (action_key);
ALTER TABLE ONLY auditableevent
    ADD CONSTRAINT auditableevent_pkey PRIMARY KEY (id);
ALTER TABLE ONLY classification
    ADD CONSTRAINT classification_pkey PRIMARY KEY (id);
ALTER TABLE ONLY classificationnode
    ADD CONSTRAINT classificationnode_pkey PRIMARY KEY (id);
ALTER TABLE ONLY classificationscheme
    ADD CONSTRAINT classificationscheme_pkey PRIMARY KEY (id);
ALTER TABLE ONLY deliveryinfo
    ADD CONSTRAINT deliveryinfo_pkey PRIMARY KEY (key);
ALTER TABLE ONLY deliveryinfo_slot
    ADD CONSTRAINT deliveryinfo_slot_pkey PRIMARY KEY (deliveryinfo_key, child_slot_key);
ALTER TABLE ONLY emailaddress
    ADD CONSTRAINT emailaddress_pkey PRIMARY KEY (key);
ALTER TABLE ONLY emailaddress_slot
    ADD CONSTRAINT emailaddress_slot_pkey PRIMARY KEY (emailaddress_key, child_slot_key);
ALTER TABLE ONLY entry
    ADD CONSTRAINT entry_entrykey_key_key UNIQUE (entrykey_key);
ALTER TABLE ONLY entry
    ADD CONSTRAINT entry_entryvalue_key_key UNIQUE (entryvalue_key);
ALTER TABLE ONLY entry
    ADD CONSTRAINT entry_pkey PRIMARY KEY (entryvalue_key, entrykey_key);
ALTER TABLE ONLY externalidentifier
    ADD CONSTRAINT externalidentifier_pkey PRIMARY KEY (id);
ALTER TABLE ONLY externallink
    ADD CONSTRAINT externallink_pkey PRIMARY KEY (id);
ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT extrinsicobject_pkey PRIMARY KEY (id);
ALTER TABLE ONLY federation
    ADD CONSTRAINT federation_pkey PRIMARY KEY (id);
ALTER TABLE ONLY internationalstring_localizedstring
    ADD CONSTRAINT internationalstring_localizedstring_localizedstring_key_key UNIQUE (localizedstring_key);
ALTER TABLE ONLY internationalstring
    ADD CONSTRAINT internationalstring_pkey PRIMARY KEY (key);
ALTER TABLE ONLY localizedstring
    ADD CONSTRAINT localizedstring_pkey PRIMARY KEY (key);
ALTER TABLE ONLY map
    ADD CONSTRAINT map_pkey PRIMARY KEY (key);
ALTER TABLE ONLY notification
    ADD CONSTRAINT notification_pkey PRIMARY KEY (id);
ALTER TABLE ONLY objectref
    ADD CONSTRAINT objectref_pkey PRIMARY KEY (id);
ALTER TABLE ONLY objectref_slot
    ADD CONSTRAINT objectref_slot_pkey PRIMARY KEY (objectref_id, child_slot_key);
ALTER TABLE ONLY objectreflist
    ADD CONSTRAINT objectreflist_pkey PRIMARY KEY (key);
ALTER TABLE ONLY organization
    ADD CONSTRAINT organization_pkey PRIMARY KEY (id);
ALTER TABLE ONLY parameter
    ADD CONSTRAINT parameter_pkey PRIMARY KEY (key);
ALTER TABLE ONLY parameter_slot
    ADD CONSTRAINT parameter_slot_pkey PRIMARY KEY (parameter_key, child_slot_key);
ALTER TABLE ONLY person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);
ALTER TABLE ONLY personname
    ADD CONSTRAINT personname_pkey PRIMARY KEY (middlename, lastname, firstname);
ALTER TABLE ONLY personname_slot
    ADD CONSTRAINT personname_slot_pkey PRIMARY KEY (personname_middlename, personname_lastname, personname_firstname, child_slot_key);
ALTER TABLE ONLY postaladdress
    ADD CONSTRAINT postaladdress_pkey PRIMARY KEY (key);
ALTER TABLE ONLY postaladdress_slot
    ADD CONSTRAINT postaladdress_slot_pkey PRIMARY KEY (postaladdress_key, child_slot_key);
ALTER TABLE ONLY query
    ADD CONSTRAINT query_pkey PRIMARY KEY (key);
ALTER TABLE ONLY query_slot
    ADD CONSTRAINT query_slot_pkey PRIMARY KEY (query_key, child_slot_key);
ALTER TABLE ONLY querydefinition
    ADD CONSTRAINT querydefinition_pkey PRIMARY KEY (id);
ALTER TABLE ONLY queryexpression
    ADD CONSTRAINT queryexpression_pkey PRIMARY KEY (key);
ALTER TABLE ONLY queryexpression_slot
    ADD CONSTRAINT queryexpression_slot_pkey PRIMARY KEY (queryexpression_key, child_slot_key);
ALTER TABLE ONLY registry
    ADD CONSTRAINT registry_pkey PRIMARY KEY (id);
ALTER TABLE ONLY registryobject_classification
    ADD CONSTRAINT registryobject_classification_pkey PRIMARY KEY (registryobject_id, classification_id);
ALTER TABLE ONLY registryobject_externalidentifier
    ADD CONSTRAINT registryobject_externalidentifier_pkey PRIMARY KEY (registryobject_id, externalidentifier_id);
ALTER TABLE ONLY registryobject_externallink
    ADD CONSTRAINT registryobject_externallink_pkey PRIMARY KEY (registryobject_id, externallink_id);
ALTER TABLE ONLY registryobject
    ADD CONSTRAINT registryobject_pkey PRIMARY KEY (id);
ALTER TABLE ONLY registryobject_slot
    ADD CONSTRAINT registryobject_slot_pkey PRIMARY KEY (registryobject_id, child_slot_key);
ALTER TABLE ONLY registryobjectlist
    ADD CONSTRAINT registryobjectlist_pkey PRIMARY KEY (key);
ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT registrypackage_pkey PRIMARY KEY (id);
ALTER TABLE ONLY role
    ADD CONSTRAINT role_pkey PRIMARY KEY (id);
ALTER TABLE ONLY service
    ADD CONSTRAINT service_pkey PRIMARY KEY (id);
ALTER TABLE ONLY servicebinding
    ADD CONSTRAINT servicebinding_pkey PRIMARY KEY (id);
ALTER TABLE ONLY serviceendpoint
    ADD CONSTRAINT serviceendpoint_pkey PRIMARY KEY (id);
ALTER TABLE ONLY serviceinterface
    ADD CONSTRAINT serviceinterface_pkey PRIMARY KEY (id);
ALTER TABLE ONLY simplelink
    ADD CONSTRAINT simplelink_pkey PRIMARY KEY (key);
ALTER TABLE ONLY slot
    ADD CONSTRAINT slot_pkey PRIMARY KEY (key);
ALTER TABLE ONLY slot_slot
    ADD CONSTRAINT slot_slot_pkey PRIMARY KEY (slot_key, child_slot_key);
ALTER TABLE ONLY stringqueryexpression
    ADD CONSTRAINT stringqueryexpression_pkey PRIMARY KEY (key);
ALTER TABLE ONLY subscription_deliveryinfo
    ADD CONSTRAINT subscription_deliveryinfo_deliveryinfo_key_key UNIQUE (deliveryinfo_key);
ALTER TABLE ONLY subscription
    ADD CONSTRAINT subscription_pkey PRIMARY KEY (id);
ALTER TABLE ONLY taxonomyelementtype_classificationnode
    ADD CONSTRAINT taxonomyelementtype_classificationnode_pkey PRIMARY KEY (taxonomyelementtype_id, classificationnode_id);
ALTER TABLE ONLY telephonenumber
    ADD CONSTRAINT telephonenumber_pkey PRIMARY KEY (key);
ALTER TABLE ONLY telephonenumber_slot
    ADD CONSTRAINT telephonenumber_slot_pkey PRIMARY KEY (telephonenumber_key, child_slot_key);
ALTER TABLE ONLY value
    ADD CONSTRAINT value_pkey PRIMARY KEY (key);
ALTER TABLE ONLY vocabularyterm
    ADD CONSTRAINT vocabularyterm_pkey PRIMARY KEY (vocabulary, term);
ALTER TABLE ONLY workflowaction
    ADD CONSTRAINT workflowaction_pkey PRIMARY KEY (id);
ALTER TABLE ONLY xmlqueryexpression
    ADD CONSTRAINT xmlqueryexpression_pkey PRIMARY KEY (key);
CREATE INDEX associationlid_index ON association USING btree (lid);
CREATE INDEX associationregistryobjecttype_objecttype_idx ON association USING btree (objecttype);
CREATE INDEX assoociation_type_idx ON association USING btree (type);
CREATE INDEX auditableeventlid_index ON auditableevent USING btree (lid);
CREATE INDEX auditableeventregistryobjecttype_objecttype_idx ON auditableevent USING btree (objecttype);
CREATE INDEX classificationlid_index ON classification USING btree (lid);
CREATE INDEX classificationnodepath_idx ON classificationnode USING btree (path);
CREATE INDEX classificationnodetaxonomyelementtypelid_index ON classificationnode USING btree (lid);
CREATE INDEX classificationnodetaxonomyelementtyperegistryobjecttype_objectt ON classificationnode USING btree (objecttype);
CREATE INDEX classificationregistryobjecttype_objecttype_idx ON classification USING btree (objecttype);
CREATE INDEX classificationschemetaxonomyelementtypelid_index ON classificationscheme USING btree (lid);
CREATE INDEX classificationschemetaxonomyelementtyperegistryobjecttype_objec ON classificationscheme USING btree (objecttype);
CREATE INDEX externalidentifierlid_index ON externalidentifier USING btree (lid);
CREATE INDEX externalidentifierregistryobjecttype_objecttype_idx ON externalidentifier USING btree (objecttype);
CREATE INDEX externallinklid_index ON externallink USING btree (lid);
CREATE INDEX externallinkregistryobjecttype_objecttype_idx ON externallink USING btree (objecttype);
CREATE INDEX extrinsicobjectlid_index ON extrinsicobject USING btree (lid);
CREATE INDEX extrinsicobjectregistryobjecttype_objecttype_idx ON extrinsicobject USING btree (objecttype);
CREATE INDEX federationlid_index ON federation USING btree (lid);
CREATE INDEX federationregistryobjecttype_objecttype_idx ON federation USING btree (objecttype);
CREATE INDEX "internationalstring_localizedString_idx" ON internationalstring_localizedstring USING btree (internationalstring_key);
CREATE INDEX lid_index ON registryobject USING btree (lid);
CREATE INDEX notificationlid_index ON notification USING btree (lid);
CREATE INDEX notificationregistryobjecttype_objecttype_idx ON notification USING btree (objecttype);
CREATE INDEX organizationlid_index ON organization USING btree (lid);
CREATE INDEX organizationregistryobjecttype_objecttype_idx ON organization USING btree (objecttype);
CREATE INDEX personlid_index ON person USING btree (lid);
CREATE INDEX personregistryobjecttype_objecttype_idx ON person USING btree (objecttype);
CREATE INDEX querydefinitionlid_index ON querydefinition USING btree (lid);
CREATE INDEX querydefinitionregistryobjecttype_objecttype_idx ON querydefinition USING btree (objecttype);
CREATE INDEX registrylid_index ON registry USING btree (lid);
CREATE INDEX registryobject_slot_idx ON registryobject_slot USING btree (registryobject_id);
CREATE INDEX registryobjecttype_objecttype_idx ON registryobject USING btree (objecttype);
CREATE INDEX registrypackagelid_index ON registrypackage USING btree (lid);
CREATE INDEX registrypackageregistryobjecttype_objecttype_idx ON registrypackage USING btree (objecttype);
CREATE INDEX registryregistryobjecttype_objecttype_idx ON registry USING btree (objecttype);
CREATE INDEX rolelid_index ON role USING btree (lid);
CREATE INDEX roleregistryobjecttype_objecttype_idx ON role USING btree (objecttype);
CREATE INDEX servicebindinglid_index ON servicebinding USING btree (lid);
CREATE INDEX servicebindingregistryobjecttype_objecttype_idx ON servicebinding USING btree (objecttype);
CREATE INDEX serviceendpointlid_index ON serviceendpoint USING btree (lid);
CREATE INDEX serviceendpointregistryobjecttype_objecttype_idx ON serviceendpoint USING btree (objecttype);
CREATE INDEX serviceinterfacelid_index ON serviceinterface USING btree (lid);
CREATE INDEX serviceinterfaceregistryobjecttype_objecttype_idx ON serviceinterface USING btree (objecttype);
CREATE INDEX servicelid_index ON service USING btree (lid);
CREATE INDEX serviceregistryobjecttype_objecttype_idx ON service USING btree (objecttype);
CREATE INDEX slot_name_idx ON slot USING btree (name);
CREATE INDEX subscriptionlid_index ON subscription USING btree (lid);
CREATE INDEX subscriptionregistryobjecttype_objecttype_idx ON subscription USING btree (objecttype);
CREATE INDEX value_value_idx ON value_value USING btree (value_key);
CREATE INDEX workflowactionlid_index ON workflowaction USING btree (lid);
CREATE INDEX workflowactionregistryobjecttype_objecttype_idx ON workflowaction USING btree (objecttype);
ALTER TABLE ONLY person_postaladdress
    ADD CONSTRAINT fk1a9f253f1cc2d914 FOREIGN KEY (postaladdress_key) REFERENCES postaladdress(key);
ALTER TABLE ONLY person_postaladdress
    ADD CONSTRAINT fk1a9f253ff14343cc FOREIGN KEY (person_id) REFERENCES person(id);
ALTER TABLE ONLY telephonenumber_slot
    ADD CONSTRAINT fk1aba1ed0791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY telephonenumber_slot
    ADD CONSTRAINT fk1aba1ed0f3468e5c FOREIGN KEY (telephonenumber_key) REFERENCES telephonenumber(key);
ALTER TABLE ONLY subscription
    ADD CONSTRAINT fk1e21ad3d31e769c9 FOREIGN KEY (selector_key) REFERENCES query(key);
ALTER TABLE ONLY person_telephonenumber
    ADD CONSTRAINT fk1edf7383f14343cc FOREIGN KEY (person_id) REFERENCES person(id);
ALTER TABLE ONLY person_telephonenumber
    ADD CONSTRAINT fk1edf7383f3468e5c FOREIGN KEY (telephonenumber_key) REFERENCES telephonenumber(key);
ALTER TABLE ONLY value_value
    ADD CONSTRAINT fk214ba4e31cb1cda6 FOREIGN KEY (collectionvalue_key) REFERENCES value(key);
ALTER TABLE ONLY value_value
    ADD CONSTRAINT fk214ba4e3ae207b86 FOREIGN KEY (value_key) REFERENCES value(key);
ALTER TABLE ONLY organization_emailaddress
    ADD CONSTRAINT fk219f5c4459e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);
ALTER TABLE ONLY organization_emailaddress
    ADD CONSTRAINT fk219f5c4487a0de2 FOREIGN KEY (emailaddress_key) REFERENCES emailaddress(key);
ALTER TABLE ONLY registryobject_classification
    ADD CONSTRAINT fk220c0ca97146a5ac FOREIGN KEY (classification_id) REFERENCES classification(id);
ALTER TABLE ONLY personname_slot
    ADD CONSTRAINT fk23dea75d305d8b4d FOREIGN KEY (personname_middlename, personname_lastname, personname_firstname) REFERENCES personname(middlename, lastname, firstname);
ALTER TABLE ONLY personname_slot
    ADD CONSTRAINT fk23dea75d791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY slot
    ADD CONSTRAINT fk275e1e3d4e5e46 FOREIGN KEY (slotvalue_key) REFERENCES value(key);
ALTER TABLE ONLY organization_telephonenumber
    ADD CONSTRAINT fk2763b16159e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);
ALTER TABLE ONLY organization_telephonenumber
    ADD CONSTRAINT fk2763b161f3468e5c FOREIGN KEY (telephonenumber_key) REFERENCES telephonenumber(key);
ALTER TABLE ONLY organization_postaladdress
    ADD CONSTRAINT fk3035429d1cc2d914 FOREIGN KEY (postaladdress_key) REFERENCES postaladdress(key);
ALTER TABLE ONLY organization_postaladdress
    ADD CONSTRAINT fk3035429d59e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);
ALTER TABLE ONLY emailaddress_slot
    ADD CONSTRAINT fk31c32925791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY emailaddress_slot
    ADD CONSTRAINT fk31c3292587a0de2 FOREIGN KEY (emailaddress_key) REFERENCES emailaddress(key);
ALTER TABLE ONLY entry
    ADD CONSTRAINT fk40018526aa2c3c0 FOREIGN KEY (entrykey_key) REFERENCES value(key);
ALTER TABLE ONLY entry
    ADD CONSTRAINT fk400185272ea4252 FOREIGN KEY (entryvalue_key) REFERENCES value(key);
ALTER TABLE ONLY value
    ADD CONSTRAINT fk4e9a151f53db13 FOREIGN KEY (internationalstringvalue_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY registryobject
    ADD CONSTRAINT fk5880a9fc94b6895d FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY auditableevent
    ADD CONSTRAINT fk5880a9fc94b6895d134a8c05 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY subscription
    ADD CONSTRAINT fk5880a9fc94b6895d1e21ad3d FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY role
    ADD CONSTRAINT fk5880a9fc94b6895d26f496 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY notification
    ADD CONSTRAINT fk5880a9fc94b6895d2d45dd0b FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY servicebinding
    ADD CONSTRAINT fk5880a9fc94b6895d44524630 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY organization
    ADD CONSTRAINT fk5880a9fc94b6895d50104153 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY externalidentifier
    ADD CONSTRAINT fk5880a9fc94b6895d56d1a6f4 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY association
    ADD CONSTRAINT fk5880a9fc94b6895d5e328461 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY serviceendpoint
    ADD CONSTRAINT fk5880a9fc94b6895d75b47b4a FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY federation
    ADD CONSTRAINT fk5880a9fc94b6895d85f55363 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY person
    ADD CONSTRAINT fk5880a9fc94b6895d8e488775 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY externallink
    ADD CONSTRAINT fk5880a9fc94b6895db72bfe85 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY querydefinition
    ADD CONSTRAINT fk5880a9fc94b6895dca0a881b FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY serviceinterface
    ADD CONSTRAINT fk5880a9fc94b6895dcdb3c624 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY workflowaction
    ADD CONSTRAINT fk5880a9fc94b6895dce4002f5 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT fk5880a9fc94b6895dd97867b6 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY service
    ADD CONSTRAINT fk5880a9fc94b6895dd97c5e95 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY registry
    ADD CONSTRAINT fk5880a9fc94b6895ddab6945d FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY classification
    ADD CONSTRAINT fk5880a9fc94b6895ddb110006 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY classificationscheme
    ADD CONSTRAINT fk5880a9fc94b6895ddcd5f4255afc854b FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY classificationnode
    ADD CONSTRAINT fk5880a9fc94b6895ddcd5f425eb0aaf28 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT fk5880a9fc94b6895dea660529 FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY registryobject
    ADD CONSTRAINT fk5880a9fcc7a8a06c FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY auditableevent
    ADD CONSTRAINT fk5880a9fcc7a8a06c134a8c05 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY subscription
    ADD CONSTRAINT fk5880a9fcc7a8a06c1e21ad3d FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY role
    ADD CONSTRAINT fk5880a9fcc7a8a06c26f496 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY notification
    ADD CONSTRAINT fk5880a9fcc7a8a06c2d45dd0b FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY servicebinding
    ADD CONSTRAINT fk5880a9fcc7a8a06c44524630 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY organization
    ADD CONSTRAINT fk5880a9fcc7a8a06c50104153 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY externalidentifier
    ADD CONSTRAINT fk5880a9fcc7a8a06c56d1a6f4 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY association
    ADD CONSTRAINT fk5880a9fcc7a8a06c5e328461 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY serviceendpoint
    ADD CONSTRAINT fk5880a9fcc7a8a06c75b47b4a FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY federation
    ADD CONSTRAINT fk5880a9fcc7a8a06c85f55363 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY person
    ADD CONSTRAINT fk5880a9fcc7a8a06c8e488775 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY externallink
    ADD CONSTRAINT fk5880a9fcc7a8a06cb72bfe85 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY querydefinition
    ADD CONSTRAINT fk5880a9fcc7a8a06cca0a881b FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY serviceinterface
    ADD CONSTRAINT fk5880a9fcc7a8a06ccdb3c624 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY workflowaction
    ADD CONSTRAINT fk5880a9fcc7a8a06cce4002f5 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT fk5880a9fcc7a8a06cd97867b6 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY service
    ADD CONSTRAINT fk5880a9fcc7a8a06cd97c5e95 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY registry
    ADD CONSTRAINT fk5880a9fcc7a8a06cdab6945d FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY classification
    ADD CONSTRAINT fk5880a9fcc7a8a06cdb110006 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY classificationscheme
    ADD CONSTRAINT fk5880a9fcc7a8a06cdcd5f4255afc854b FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY classificationnode
    ADD CONSTRAINT fk5880a9fcc7a8a06cdcd5f425eb0aaf28 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT fk5880a9fcc7a8a06cea660529 FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY registryobject_externalidentifier
    ADD CONSTRAINT fk5a5faa17239c540c FOREIGN KEY (externalidentifier_id) REFERENCES externalidentifier(id);
ALTER TABLE ONLY person_emailaddress
    ADD CONSTRAINT fk62fd9d6287a0de2 FOREIGN KEY (emailaddress_key) REFERENCES emailaddress(key);
ALTER TABLE ONLY person_emailaddress
    ADD CONSTRAINT fk62fd9d62f14343cc FOREIGN KEY (person_id) REFERENCES person(id);
ALTER TABLE ONLY query_slot
    ADD CONSTRAINT fk6cbd17551cf1f232 FOREIGN KEY (query_key) REFERENCES query(key);
ALTER TABLE ONLY query_slot
    ADD CONSTRAINT fk6cbd1755791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY objectref_slot
    ADD CONSTRAINT fk6f31e6c9791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY objectref_slot
    ADD CONSTRAINT fk6f31e6c9e422821c FOREIGN KEY (objectref_id) REFERENCES objectref(id);
ALTER TABLE ONLY registryobject_externallink
    ADD CONSTRAINT fk71b7d7e8976827ec FOREIGN KEY (externallink_id) REFERENCES externallink(id);
ALTER TABLE ONLY action
    ADD CONSTRAINT fk74946a5618cc8927 FOREIGN KEY (affectedobjectrefs_key) REFERENCES objectreflist(key);
ALTER TABLE ONLY action
    ADD CONSTRAINT fk74946a566900fa0c FOREIGN KEY (affectedobjects_key) REFERENCES registryobjectlist(key);
ALTER TABLE ONLY deliveryinfo_slot
    ADD CONSTRAINT fk7709e55b3af51bb6 FOREIGN KEY (deliveryinfo_key) REFERENCES deliveryinfo(key);
ALTER TABLE ONLY deliveryinfo_slot
    ADD CONSTRAINT fk7709e55b791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY subscription_deliveryinfo
    ADD CONSTRAINT fk7a416a43af51bb6 FOREIGN KEY (deliveryinfo_key) REFERENCES deliveryinfo(key);
ALTER TABLE ONLY subscription_deliveryinfo
    ADD CONSTRAINT fk7a416a458788cc FOREIGN KEY (subscription_id) REFERENCES subscription(id);
ALTER TABLE ONLY parameter_slot
    ADD CONSTRAINT fk7c511145e6862f4 FOREIGN KEY (parameter_key) REFERENCES parameter(key);
ALTER TABLE ONLY parameter_slot
    ADD CONSTRAINT fk7c51114791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY registryobjectlist_registryobject
    ADD CONSTRAINT fk8312dc217486c946 FOREIGN KEY (registryobjectlist_key) REFERENCES registryobjectlist(key);
ALTER TABLE ONLY person
    ADD CONSTRAINT fk8e488775305d8b4d FOREIGN KEY (personname_middlename, personname_lastname, personname_firstname) REFERENCES personname(middlename, lastname, firstname);
ALTER TABLE ONLY notification_auditableevent
    ADD CONSTRAINT fk9a005b9886a370c FOREIGN KEY (notification_id) REFERENCES notification(id);
ALTER TABLE ONLY notification_auditableevent
    ADD CONSTRAINT fk9a005b995d0eef7 FOREIGN KEY (event_id) REFERENCES auditableevent(id);
ALTER TABLE ONLY slot_slot
    ADD CONSTRAINT fka056db9f791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY slot_slot
    ADD CONSTRAINT fka056db9fd0b27a4e FOREIGN KEY (slot_key) REFERENCES slot(key);
ALTER TABLE ONLY queryexpression_slot
    ADD CONSTRAINT fka0fd8d9d791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY organization_organization
    ADD CONSTRAINT fka7b968bf59e50e0c FOREIGN KEY (organization_id) REFERENCES organization(id);
ALTER TABLE ONLY externallink
    ADD CONSTRAINT fkb72bfe856d351206 FOREIGN KEY (externalref_key) REFERENCES simplelink(key);
ALTER TABLE ONLY internationalstring_localizedstring
    ADD CONSTRAINT fkba1f172c246c6800 FOREIGN KEY (internationalstring_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY internationalstring_localizedstring
    ADD CONSTRAINT fkba1f172cee9b1d5a FOREIGN KEY (localizedstring_key) REFERENCES localizedstring(key);
ALTER TABLE ONLY querydefinition_parameter
    ADD CONSTRAINT fkbd1306518f9477c FOREIGN KEY (querydefinition_id) REFERENCES querydefinition(id);
ALTER TABLE ONLY querydefinition_parameter
    ADD CONSTRAINT fkbd130655e6862f4 FOREIGN KEY (parameter_key) REFERENCES parameter(key);
ALTER TABLE ONLY map_entry
    ADD CONSTRAINT fkbfbdcb8f92e9c6ba FOREIGN KEY (entry_entryvalue_key, entry_entrykey_key) REFERENCES entry(entryvalue_key, entrykey_key);
ALTER TABLE ONLY map_entry
    ADD CONSTRAINT fkbfbdcb8ffb10015a FOREIGN KEY (map_key) REFERENCES map(key);
ALTER TABLE ONLY postaladdress_slot
    ADD CONSTRAINT fkc0fe08141cc2d914 FOREIGN KEY (postaladdress_key) REFERENCES postaladdress(key);
ALTER TABLE ONLY postaladdress_slot
    ADD CONSTRAINT fkc0fe0814791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY action_slot
    ADD CONSTRAINT fkc1c0b26772bc74be FOREIGN KEY (action_key) REFERENCES action(key);
ALTER TABLE ONLY action_slot
    ADD CONSTRAINT fkc1c0b267791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY auditableevent_action
    ADD CONSTRAINT fkc76a3bd072bc74be FOREIGN KEY (action_key) REFERENCES action(key);
ALTER TABLE ONLY auditableevent_action
    ADD CONSTRAINT fkc76a3bd086bc35ac FOREIGN KEY (auditableevent_id) REFERENCES auditableevent(id);
ALTER TABLE ONLY objectreflist_objectref
    ADD CONSTRAINT fkcffc8027caf899c6 FOREIGN KEY (objectreflist_key) REFERENCES objectreflist(key);
ALTER TABLE ONLY objectreflist_objectref
    ADD CONSTRAINT fkcffc8027e422821c FOREIGN KEY (objectref_id) REFERENCES objectref(id);
ALTER TABLE ONLY extrinsicobject
    ADD CONSTRAINT fkd97867b6a9a5cc34 FOREIGN KEY (repositoryitemref_key) REFERENCES simplelink(key);
ALTER TABLE ONLY registryobject_slot
    ADD CONSTRAINT fkde84a81791fb611 FOREIGN KEY (child_slot_key) REFERENCES slot(key);
ALTER TABLE ONLY service_serviceendpoint
    ADD CONSTRAINT fkdeb465a04ee4881c FOREIGN KEY (service_id) REFERENCES service(id);
ALTER TABLE ONLY service_serviceendpoint
    ADD CONSTRAINT fkdeb465a0ad801d9c FOREIGN KEY (serviceendpoint_id) REFERENCES serviceendpoint(id);
ALTER TABLE ONLY registrypackage
    ADD CONSTRAINT fkea6605297486c946 FOREIGN KEY (registryobjectlist_key) REFERENCES registryobjectlist(key);
ALTER TABLE ONLY parameter
    ADD CONSTRAINT fkebf0d38994b6895d FOREIGN KEY (description_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY parameter
    ADD CONSTRAINT fkebf0d389c7a8a06c FOREIGN KEY (name_key) REFERENCES internationalstring(key);
ALTER TABLE ONLY taxonomyelementtype_classificationnode
    ADD CONSTRAINT fkf9067d82c36558c FOREIGN KEY (classificationnode_id) REFERENCES classificationnode(id);
