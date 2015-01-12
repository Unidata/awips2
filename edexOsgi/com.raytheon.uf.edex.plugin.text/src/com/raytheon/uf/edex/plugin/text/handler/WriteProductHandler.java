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
package com.raytheon.uf.edex.plugin.text.handler;

import java.util.Date;
import java.util.logging.Logger;

import com.raytheon.uf.common.dataplugin.text.request.WriteProductRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.plugin.text.db.TextDB;
import com.raytheon.uf.edex.plugin.text.dbsrv.impl.AlarmAlertUtil;

/**
 * Request handler for WriteProductRequests. Forwards request to textdb.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2010            njensen     Initial creation
 * 01Jun2010               cjeanbap    Added operational mode functionality.
 * Jul 02, 2010 4687       cjeanbap    Added watch warn queue.
 * May 23, 2012 14952      rferrel     Alarm Alerts date now set to the
 *                                      products reference/create time.
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class WriteProductHandler implements
        IRequestHandler<WriteProductRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WriteProductHandler.class);

    private static final String WATCH_WARN_QUEUE = "ldadWatchWarnDirect";

    private static Logger logger = Logger.getLogger(WriteProductHandler.class
            .toString());

    @Override
    public Object handleRequest(WriteProductRequest request) throws Exception {
        TextDB textdb = new TextDB();
        long result = textdb.writeProduct(request.getProductId(),
                request.getReportData(), request.getOperationalMode(), null);

        if (result != Long.MIN_VALUE) {
            if (request.getOperationalMode()) {
                sendTextToQueue(request.getProductId(), WATCH_WARN_QUEUE);
            }

            if (request.isNotifyAlarmAlert()) {
                Date d = new Date();
                d.setTime(result);

                AlarmAlertUtil.sendProductAlarmAlert(request.getProductId(),
                        String.valueOf(d.getTime()),
                        request.getOperationalMode());
            }
        }

        return result;
    }

    /**
     * 
     * Sends an asynchronous message to the specified queue. This is basically a
     * wrapper of the utility method that handles/logs any errors.
     * 
     * @param message
     *            the message to send
     * @param queue
     *            the queue to receive the message
     */
    private void sendTextToQueue(String message, String queue) {
        try {
            logger.info("Sending Product Id [" + message + "] to " + queue
                    + "Queue.");
            EDEXUtil.getMessageProducer().sendAsync(queue, message);
            logger.info("\"Submitted\" Product Id [" + message + "] to "
                    + queue);
        } catch (EdexException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to send product '"
                    + message + "' to queue '" + queue + "'", e);
        }
    }
}
