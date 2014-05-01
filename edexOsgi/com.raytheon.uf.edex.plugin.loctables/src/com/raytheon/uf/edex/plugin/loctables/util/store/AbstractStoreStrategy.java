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
package com.raytheon.uf.edex.plugin.loctables.util.store;

import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2011            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public abstract class AbstractStoreStrategy implements RowStoreStrategy {

    private TableHandler parentHandler = null;
    
    /**
     * Post status information to the parent handler if it exists.
     * @param status The current status, informative status messages
     * must be positive value greater than zero (0). Error status must
     * be a negative value.
     * @param statusMsg A String message describing the status.
     */
    public void postStatus(int status, String statusMsg) {
        if(parentHandler != null) {
            parentHandler.setStatus(status);
            parentHandler.setStatusMsg(statusMsg);
        }
    }
    
    /**
     * 
     * @param handler The handler containing (using) this store strategy.
     * @see com.raytheon.uf.edex.plugin.loctables.util.store.RowStoreStrategy#setParent(com.raytheon.uf.edex.plugin.loctables.util.TableHandler)
     */
    @Override
    public void setParent(TableHandler handler) {
        parentHandler = handler;
    }

}
