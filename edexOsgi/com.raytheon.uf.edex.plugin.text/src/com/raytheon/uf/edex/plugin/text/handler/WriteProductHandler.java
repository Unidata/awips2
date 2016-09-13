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

import com.raytheon.uf.common.dataplugin.text.request.WriteProductRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.plugin.text.AlarmAlertUtil;
import com.raytheon.uf.edex.plugin.text.TextDecoder;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * Request handler for WriteProductRequests. Forwards request to textdb.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 02, 2010            njensen     Initial creation
 * Jun 01, 2010            cjeanbap    Added operational mode functionality.
 * Jul 02, 2010 4687       cjeanbap    Added watch warn queue.
 * May 23, 2012 14952      rferrel     Alarm Alerts date now set to the
 *                                      products reference/create time.
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * Dec 17, 2015 5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class WriteProductHandler implements
        IRequestHandler<WriteProductRequest> {

    @Override
    public Object handleRequest(WriteProductRequest request) throws Exception {
        TextDB textdb = new TextDB();
        long result = textdb.writeProduct(request.getProductId(),
                request.getReportData(), request.getOperationalMode(), null);

        if (result != Long.MIN_VALUE) {
            if (request.getOperationalMode()) {
                TextDecoder.sendTextToQueue(request.getProductId());
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
}
