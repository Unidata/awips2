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
package com.raytheon.viz.aviation.utility;

import java.util.List;
import java.util.Map;

/**
 * Interface for backing up or restarting the TAF Monitor GUI.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2009 1982       grichard    Initial creation.
 * Oct 06, 2010 6009       rferrel     backupTafMonitor now takes product.
 * Mar 14, 2011 8588       rferrel     backupTafMonitor uses a product list.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public interface IBackupRestart {

    /**
     * Method to restart the Taf Monitor.
     */
    void restartTafMonitor();

    /**
     * Method to backup the TAF Monitor.
     * 
     * @param productDisplayList
     *            - The list of products to backup (display)
     * @param stationMap
     *            - A station Map that contains the product information
     */
    void backupTafMonitor(List<String> productDispalyList,
            Map<String, List<String>> stationMap);

}
