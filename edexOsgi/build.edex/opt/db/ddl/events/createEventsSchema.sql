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
BEGIN TRANSACTION;
CREATE SCHEMA events AUTHORIZATION awipsadmin;

-- Grant privileges to awips
GRANT USAGE ON SCHEMA events to awips; -- Don't grant create
ALTER DEFAULT PRIVILEGES IN SCHEMA events GRANT SELECT, INSERT, UPDATE, DELETE, TRIGGER ON TABLES TO awips; -- Don't grant references
ALTER DEFAULT PRIVILEGES IN SCHEMA events GRANT ALL ON SEQUENCES TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA events GRANT ALL ON FUNCTIONS TO awips;
ALTER DEFAULT PRIVILEGES IN SCHEMA events GRANT ALL ON TYPES TO awips;
COMMIT TRANSACTION;
