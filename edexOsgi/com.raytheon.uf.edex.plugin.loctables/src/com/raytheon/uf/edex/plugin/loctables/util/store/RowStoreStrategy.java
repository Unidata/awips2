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

import java.io.Closeable;

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
 * Apr 16, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface RowStoreStrategy extends Closeable {

    /**
     * 
     * @param row
     * @return
     */
    boolean store(ObStationRow row);

    /**
     * 
     * @param handler
     */
    void setParent(TableHandler handler);
    
    /**
     * Post status information to the parent handler if it exists.
     * @param status The current status, informative status messages
     * must be positive value greater than zero (0). Error status must
     * be a negative value.
     * @param statusMsg A String message describing the status.
     */
    void postStatus(int status, String statusMsg);

}
