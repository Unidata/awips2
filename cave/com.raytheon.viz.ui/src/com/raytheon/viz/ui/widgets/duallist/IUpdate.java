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
package com.raytheon.viz.ui.widgets.duallist;

/**
 * Interface for an update.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2012            mpduff     Initial creation.
 * Aug 08, 2012    863     jpiatt     Added selectedChange method for clean & dirty checks.
 * Nov 02, 2012 1302       djohnson   Add javadoc.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface IUpdate {
    /**
     * Method called when a control has items or when the control becomes empty.
     * 
     * @param entries
     *            Entries flag. True if there are entries, false if there are no
     *            entries.
     */
    void hasEntries(boolean entries);
    
    /**
     * Method called when a change in selection occurs.
     */
    void selectionChanged();
}
