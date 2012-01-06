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
-- Performs the initial setup for creating the VTEC tables.
-- Note: this script assumes that the postgres and awips
--       users have been created and that the metadata
--       tablespace has been created.
--
-- Database: metadata
-- Schema:   vtec
--
-- File History:
-- 14Aug2007     #393    MW Fegan     Initial Creation.
--

--Create the VTEC schema
create schema vtec authorization awips;

--Create the "static" table (required for rules)
create table vtec.static();
alter table vtec.static owner to awips;
create or replace rule rule_static as on insert to vtec.static do also delete from vtec.static;
