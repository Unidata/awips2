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
package com.raytheon.viz.ghg.monitor.event;

import java.util.EventObject;

/**
 * GHG Monitor data change event.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2010            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GhgMonitorFilterChangeEvent extends EventObject {

    private static final long serialVersionUID = -4455895822255108354L;
    
    private boolean filterChanged = false;

    /**
     * @param source
     */
    public GhgMonitorFilterChangeEvent(Object source) {
        super(source);
    }

    /**
     * @return the filterChanged
     */
    public boolean isFilterChanged() {
        return filterChanged;
    }

    /**
     * @param filterChanged the filterChanged to set
     */
    public void setFilterChanged(boolean filterChanged) {
        this.filterChanged = filterChanged;
    }    
}
