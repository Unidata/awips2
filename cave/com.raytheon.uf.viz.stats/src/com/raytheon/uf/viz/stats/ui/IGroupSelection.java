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
package com.raytheon.uf.viz.stats.ui;

import java.util.List;
import java.util.Map;

/**
 * Interface for Group Selections.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2012    728     mpduff      Initial creation.
 * Jan 17, 2013   1357     mpduff      Added setItemsOff and getStates.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface IGroupSelection {
    /**
     * Set the selections.
     * 
     * @param selectionMap
     */
    void setSelections(Map<String, Map<String, Boolean>> selectionMap);

    /**
     * Turn off the provided items.
     * 
     * @param keys
     *            keys of the items to not draw
     */
    void setItemsOff(List<String> keys);

    /**
     * Get the state of each item.
     * 
     * @return Map of item -> checked or not checked
     */
    Map<String, Boolean> getStates();
}
