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
package com.raytheon.viz.gfe.gridmanager;

import java.util.Date;

/**
 * GFE Grid Manager Interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009             randerso    Initial creation
 * May 28, 2009 #2159      rjpeter     Added support for temporal editor.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public interface IGridManager {
    public enum GridManagerState {
        GridManager, TemporalEditor
    };

    /**
     * Set the selected time
     * 
     * @param selectedTime
     */
    public void setSelectedTime(Date selectedTime);

    /**
     * Expands the time scale of the GridManager
     */
    public void expandTimeScale();

    /**
     * Contacts the time scale of the GridManager
     */
    public void contractTimeScale();

    /**
     * Paints the TimeScale and all of the GridBars.
     */
    public void redraw();

    /**
     * Toggles the temporal editor and the grid manager
     */
    public void toggleTemporalEditor();

    /**
     * Returns the grid manager state.
     * 
     * @return
     */
    public GridManagerState getState();

    /**
     * @return true if selection time range is locked to time step
     */
    public boolean isLockSelectionTRtoTimeStep();

    /**
     * Set lockSelectionTRtoTimeStep
     * 
     * @param lockSelectionTRtoTimeStep
     */
    public void setLockSelectionTRtoTimeStep(boolean lockSelectionTRtoTimeStep);
}
