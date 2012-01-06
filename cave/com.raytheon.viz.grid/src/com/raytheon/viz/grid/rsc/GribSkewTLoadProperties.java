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
package com.raytheon.viz.grid.rsc;

import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009            chammack     Initial creation
 * 10-21-09     #1711      bsteffen    Updated Cordiante rather than POINT
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GribSkewTLoadProperties extends LoadProperties {

    private String point;

    /**
     * @return the point
     */
    public String getPoint() {
        return point;
    }

    /**
     * @param point
     *            the point to set
     */
    public void setPoint(String point) {
        this.point = point;
    }

}
