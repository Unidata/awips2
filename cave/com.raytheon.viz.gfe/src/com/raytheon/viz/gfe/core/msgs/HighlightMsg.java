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
package com.raytheon.viz.gfe.core.msgs;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * A message used to highlight grids in the Grid Manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2009       #2719 randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class HighlightMsg extends Message {
    private final Parm parm;

    private final TimeRange[] timeRanges;

    private final boolean on;

    private final String color;

    /**
     * @param parm
     * @param timeRanges
     * @param on
     * @param color
     */
    public HighlightMsg(final Parm parm, final TimeRange[] timeRanges,
            boolean on, final String color) {
        this.parm = parm;
        this.timeRanges = timeRanges;
        this.on = on;
        this.color = color;
    }

    /**
     * @return the parm
     */
    public Parm getParm() {
        return parm;
    }

    /**
     * @return the timeRanges
     */
    public TimeRange[] getTimeRanges() {
        return timeRanges;
    }

    /**
     * @return the on
     */
    public boolean isOn() {
        return on;
    }

    /**
     * @return the color
     */
    public String getColor() {
        return color;
    }

}
