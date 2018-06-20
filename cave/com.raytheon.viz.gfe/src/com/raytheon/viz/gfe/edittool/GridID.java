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
package com.raytheon.viz.gfe.edittool;

import java.util.Date;

import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Provides an non-binding of parm and time, for which grid data may or may not
 * exist.
 * 
 * Convenient retrieval/caching of the actual grid data is provided.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/10/2008              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GridID {

    private Parm parm;

    private Date date;

    private IGridData gridData;

    public GridID(Parm parm, Date date) {
        super();
        this.parm = parm;
        this.date = date;
    }

    /**
     * @return the parm
     */
    public Parm getParm() {
        return parm;
    }

    /**
     * @param parm
     *            the parm to set
     */
    public void setParm(Parm parm) {
        this.parm = parm;
    }

    /**
     * @return the date
     */
    public Date getDate() {
        return date;
    }

    /**
     * @param date
     *            the date to set
     */
    public void setDate(Date date) {
        this.date = date;
    }

    public IGridData grid() {
        if (parm == null) {
            return null;
        }

        if (gridData == null) {
            this.gridData = parm.overlappingGrid(date);
        }

        return this.gridData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof GridID)) {
            return false;
        }

        GridID rhs = (GridID) obj;
        IGridData g = grid();

        if (g != null && g == rhs.grid()) {
            return true;
        } else {

            return (this.parm.equals(rhs.parm) && this.date.equals(rhs.date));
        }
    }

    @Override
    public String toString() {
        return parm.getParmID().toString() + " " + date.toGMTString();
    }
}
