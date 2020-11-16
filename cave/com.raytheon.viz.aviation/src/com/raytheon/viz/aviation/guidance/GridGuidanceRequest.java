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
package com.raytheon.viz.aviation.guidance;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Base request class used by the tab viewers and to be notified of alerts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2009            njensen     Initial creation
 * Apr 28, 2011 8065       rferrel     Implement data caching
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GridGuidanceRequest extends GuidanceRequest {
    protected List<String> siteObjs;

    protected boolean routine;

    public void setSiteObjs(List<String> siteObjs) {
        this.siteObjs = siteObjs;
    }

    /**
     * Place in the map arguments need for grid.
     */
    @Override
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("siteObjs", siteObjs);
        map.put("format", format);
        map.put("routine", routine);
        return map;
    }

    /**
     * @param routine
     *            the routine to set
     */
    public void setRoutine(boolean routine) {
        this.routine = routine;
    }

    /**
     * @return the routine
     */
    public boolean isRoutine() {
        return routine;
    }
}
