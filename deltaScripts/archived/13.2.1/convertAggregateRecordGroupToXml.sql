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
\connect metadata;

-- Start a transaction
BEGIN;

-- Temporarily replace dashes in pluginName rows with @@@
update events.aggregate set grouping = regexp_replace(grouping, 'pluginName:(.*?)-(.*)', E'pluginName:\\1@@@\\2', 'g');

-- Convert to XML format
update events.aggregate set grouping = regexp_replace(grouping, ':', '" value="', 'g');
update events.aggregate set grouping = regexp_replace(grouping, '-', '"/><group name="', 'g');
update events.aggregate set grouping = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><stat><group name="' || grouping || '"/></stat>';

-- Restore dashes from @@@
update events.aggregate set grouping = regexp_replace(grouping, '<group name="(.*?)" value="(.*?)@@@(.*?)"', E'<group name="\\1" value="\\2-\\3"', 'g');

-- Commit the transaction
END;
