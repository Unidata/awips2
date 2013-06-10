package com.raytheon.uf.edex.datadelivery.harvester.purge;

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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.purgesrv.PurgeManager;

/* 
* <pre>
* 
* SOFTWARE HISTORY
* 
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* Apr 11, 2013 #1959      dhladky     Our own OGC purger
* 
* </pre>
* 
* @author dhladky
* @version 1.0
*/

public class OGCPurgeManager extends PurgeManager {

    /**
     * Creates a new PurgeManager
     */
    protected OGCPurgeManager() {
        
    }

    /**
     * Executes the purge only on available plugins that are registered with
     * camel.  This works better for the DPA instances that will be running it.
     * They aren't required to purge data from plugins that aren't registerd to 
     * their JVM which would be highly wasteful in this instance.
     */
    public void executePurge() {
        // check for any new plugins or database being purged and needing
        // entries recreated
        List<String> availablePlugins = new ArrayList<String>(PluginRegistry
                .getInstance().getRegisteredObjects());

        purgeRunner(availablePlugins);
    }
}
