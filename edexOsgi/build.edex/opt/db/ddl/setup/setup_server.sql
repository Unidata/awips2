/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
alter user $USER_NAME with password 'postgres';
create role awips with password 'awips' login nosuperuser;
create tablespace metadata owner awips location '$database_files_home/metadata';


create database metadata owner awips tablespace metadata;

\c metadata;

create schema awips authorization awips;
create table awips.static();
alter table awips.static owner to awips;
create or replace rule rule_static as on insert to awips.static do also delete from awips.static;
CREATE TABLE awips.plugin_version (name varchar(20),table_name varchar(40),version varchar(10),"key" int4 NOT NULL,hib_class varchar(40),CONSTRAINT plugin_version_pkey PRIMARY KEY ("key")) WITH OIDS;
ALTER TABLE awips.plugin_version OWNER TO awips;
create sequence awips.hibernate_sequence start 1 increment 1;
alter table awips.hibernate_sequence owner to awips;
