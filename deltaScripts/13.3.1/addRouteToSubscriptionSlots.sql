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
\connect ebxml;

-- Create a function that will load plpgsql 
CREATE OR REPLACE FUNCTION make_plpgsql() 
    RETURNS VOID
    LANGUAGE SQL
    AS $$
    CREATE LANGUAGE plpgsql;
$$;
 
-- Load plpgsql if it is not already loaded
SELECT
    CASE
    WHEN EXISTS(
        SELECT 1
        FROM pg_catalog.pg_language
        WHERE lanname='plpgsql'
    )
    THEN NULL
    ELSE make_plpgsql() END;

-- The function to add a route slot to subscriptions
CREATE OR REPLACE FUNCTION addRouteSlot() RETURNS void AS $$
    DECLARE
       registryobject_record RECORD;
       value_key INTEGER;
       slot_key INTEGER;
       registryobjectslot_key INTEGER;

    BEGIN
        -- Find all subscription registry objects
        FOR registryobject_record IN SELECT id from registryobject where objecttype like '%Subscription' LOOP
           -- Create the value for the slot
           SELECT INTO value_key nextval('hibernate_sequence');
           INSERT INTO value (dtype, key, stringvalue) VALUES ('StringValueType', value_key, 'OPSNET');
           -- Create the slot entry itself
           SELECT INTO slot_key nextval('hibernate_sequence');
           INSERT INTO slot (key, name, slotvalue_key) VALUES (slot_key, 'route', value_key);
           -- Create the registryobject_slot entry
           SELECT INTO registryobjectslot_key nextval('hibernate_sequence');
           INSERT INTO registryobject_slot (registryobject_id, child_slot_key) VALUES (registryobject_record.id, slot_key);
        END LOOP;

    END;
$$ LANGUAGE plpgsql;

-- Add the route slots to subscriptions
SELECT addRouteSlot();

-- Drop functions
DROP FUNCTION make_plpgsql();
DROP FUNCTION addRouteSlot();