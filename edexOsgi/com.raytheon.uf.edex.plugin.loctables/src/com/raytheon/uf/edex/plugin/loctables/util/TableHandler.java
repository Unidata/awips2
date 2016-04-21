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
package com.raytheon.uf.edex.plugin.loctables.util;

import java.io.File;

import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface TableHandler {

    /**
     * 
     * @param file
     */
    void processFile(File file);
    
    /**
     * 
     * @param data
     * @return
     */
    ObStationRow parseLine(String data);

    /**
     * 
     * @param row
     * @return
     */
    boolean processObStationRow(ObStationRow row);
    
    /**
     * 
     * @param data
     * @return
     */
    String findDirective(String data);
    
    /**
     * 
     * @param data
     * @return
     */
    void handleDirective(String data);
    
    /**
     * Set a status to this handler.
     * @param status Current status.
     */
    void setStatus(Integer status);
    
    /**
     * Set the position of the last error encountered.
     * @param pos Position of the last error.
     */
    void setErrorPos(Integer pos);

    /**
     * Set a status message for this handler.
     * @param errorMsg The status message to be displayed.
     */
    void setStatusMsg(String statusMsg);
}
