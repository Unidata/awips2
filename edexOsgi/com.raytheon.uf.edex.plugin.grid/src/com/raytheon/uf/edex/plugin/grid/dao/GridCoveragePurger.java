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
package com.raytheon.uf.edex.plugin.grid.dao;

import java.util.Collection;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;

/**
 * Java Bean called from spring framework to purge cached grid coverage 
 * objects in the JVM
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 20, 2020  21070    dhaines    Initial Creation
 * </pre>
 *
 * @author dhaines
 * @version 1
 */
public class GridCoveragePurger {

    private static GridCoveragePurger instance = new GridCoveragePurger();

    public static GridCoveragePurger getInstance() {
        return instance;
    }
    
    public void purgeCaches(Collection<Integer> idsToPurge) {
        /**
         * ingestGrib has its own gridcoverage purging method in GribSpatialCache 
         * triggered by the same topic
         */
        if (!System.getProperty("edex.run.mode").equals("ingestGrib")) {
            GridCoverageLookup.getInstance().purgeCaches(idsToPurge);
        }
    }
}

