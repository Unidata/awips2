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
package com.raytheon.uf.viz.alertviz;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

/**
 * Callback to tie into user interface handling of newly arrived messages
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2008  #1433      chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public interface IAlertArrivedCallback {

    /**
     * Call back with alert message and applicable metadata for handling the
     * message
     * 
     * @param statusMessage
     * @param alertMetadata
     * @param category
     * @param globalConfiguration
     */
    public void alertArrived(StatusMessage statusMessage,
            AlertMetadata alertMetadata, Category category,
            TrayConfiguration globalConfiguration);
}
