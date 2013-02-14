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
\set ON_ERROR_STOP 1
DROP DATABASE IF EXISTS ebxml;
DROP TABLESPACE IF EXISTS ebxml_data;
CREATE TABLESPACE ebxml_data OWNER awips LOCATION '%{database_files_home}%/ebxml';
COMMENT ON TABLESPACE ebxml_data IS 'EBXML Registry Database tablespace';
CREATE DATABASE ebxml OWNER awips TABLESPACE ebxml_data;
