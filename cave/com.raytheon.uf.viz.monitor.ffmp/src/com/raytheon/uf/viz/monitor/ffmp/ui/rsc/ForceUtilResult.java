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
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.List;

/**
 * Holds the results of calculating forcings from the FFFGForceUtil.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2013 2085       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ForceUtilResult {

    protected boolean forced;

    protected List<Long> pfafList;

    protected List<Long> forcedPfafList;

    /**
     * Constructor
     * 
     * @param forced
     * @param pfafList
     * @param forcedPfafList
     */
    protected ForceUtilResult(boolean forced, List<Long> pfafList,
            List<Long> forcedPfafList) {
        this.forced = forced;
        this.pfafList = pfafList;
        this.forcedPfafList = forcedPfafList;
    }

    /**
     * @return the forced
     */
    public boolean isForced() {
        return forced;
    }

    /**
     * @return the forcedPfafList
     */
    public List<Long> getForcedPfafList() {
        return forcedPfafList;
    }

    /**
     * @return the pfafList
     */
    public List<Long> getPfafList() {
        return pfafList;
    }

}
