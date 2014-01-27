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
package com.raytheon.uf.edex.ingest.notification.router;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Notification router for a specific destination and format.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2013 2170       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public interface INotificationRouter {
    /**
     * The destination for this router.
     * 
     * @return
     */
    public String getRoute();

    /**
     * Process the pdo into notification format.
     * 
     * @param pdo
     */
    public void process(PluginDataObject pdo);

    /**
     * Send data that should be sent immediately to route. This is mainly used
     * for PDO data that we don't want to build up and send as a bulk message.
     * Future enhancement could allow for priority data such as warnings and
     * radar data to always be notified immediately instead of a timer driver
     * basis.
     * 
     * @throws EdexException
     */
    public void sendImmediateData() throws EdexException;

    /**
     * Send any queued data to route. Generally used for data uri notifications
     * being sent over JMS to allow for better bundling. The data is queued in
     * memory and sent out as a bundled message. The interval is defined by the
     * notification timer in persist-ingest.xml.
     * 
     * @throws EdexException
     */
    public void sendQueuedData() throws EdexException;
}
