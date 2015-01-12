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

import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.StdTextProductServerRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.plugin.text.db.TextDB;
import com.raytheon.uf.edex.plugin.text.dbsrv.impl.AlarmAlertUtil;

/**
 * Handles StdTextProductServerRequest by placing them onto the LDAD queue and
 * sending product alarm alerts as specified by the request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 19May2010               cjeanbap    Initial creation
 * 07Jul2010    2187       cjeanbap    Submit correct Afos Id.
 * 07Jul2010    2187       cjeanbap    Submit AfosId to AlarmAlertUtil.
 * 02Aug2010    2187       cjeanbap    Move AlarmAlertUtil.sendProductAlarmAlert() 
 *                                     outside of if-statement.
 * May 12, 2014 2536       bclement    removed unused import
 * 
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */

public class StdTextProductRequestHandler implements
        IRequestHandler<StdTextProductServerRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(StdTextProductRequestHandler.class);

    private static final String WATCH_WARN_QUEUE = "ldadWatchWarnDirect";

    private TextDB dao;

    public StdTextProductRequestHandler() {
        dao = new TextDB();
    }

    @Override
    public Object handleRequest(StdTextProductServerRequest request)
            throws Exception {
        Date d = new Date();
        String wmoid = request.getWmoid();
        String site = request.getSite();
        String cccid = request.getCccid();
        String nnnid = request.getNnnid();
        String xxxid = request.getXxxid();
        String hdrtime = request.getHdrtime();
        String bbbid = request.getBbbid();
        Long createtime = request.getCreatetime();
        String product = request.getProduct();
        boolean operationalFlag = request.isOpertionalFlag();

        StdTextProduct text = (operationalFlag ? new OperationalStdTextProduct()
                : new PracticeStdTextProduct());
        text.setWmoid(wmoid);
        text.setSite(site);
        text.setCccid(cccid);
        text.setNnnid(nnnid);
        text.setXxxid(xxxid);
        text.setHdrtime(hdrtime);
        text.setBbbid(bbbid);
        if (createtime == null) {
            createtime = new Date().getTime();
        }
        text.setRefTime(createtime);
        text.setProduct(product);
        boolean success = dao.writeProduct(text);

        if (success) {
            String afosId = text.getCccid() + text.getNnnid() + text.getXxxid();
            if (operationalFlag) {
                sendTextToQueue(afosId, WATCH_WARN_QUEUE);
            }

            AlarmAlertUtil.sendProductAlarmAlert(afosId, new Date(createtime),
                    operationalFlag);
        }
        return d;
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
            EDEXUtil.getMessageProducer().sendAsync(queue, message);
        } catch (EdexException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to send product '"
                            + message + "' to queue '" + queue + "'", e);
        }
    }
}
