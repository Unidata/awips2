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
package com.raytheon.viz.ghg.monitor;

/**
 * Interface defining functionality to be implemented in a Menu Selection client class.
 * Classes that implement this interface may be registered to react to menu selections.
 * The class is registered with an instance of {@link IMenuSelectionListener}.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 30May2008    1157       MW Fegan    Initial Creation
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0 
 */

public interface IMenuSelectionClient {
    /**
     * Call-back method that is called by the {@link IMenuSelectionListener} to
     * propagate the menu selection to the client. The client is responsible for
     * interpreting the {@code event} and {@code data} objects. The client may
     * optionally ignore events.
     * 
     * @param menu identifies the menu containing the selection
     * @param event identifies the event - usually the menu selection
     * @param data additional data for use by the client
     */
    public void notifyMenuSelection(String menu, Object event, Object data);
}
