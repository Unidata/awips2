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
package com.raytheon.edex.plugin.tafqueue;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.tafqueue.TafQueueRecord;
import com.raytheon.uf.common.tafqueue.TafQueueRecord.TafQueueState;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * This class manages taf_queue table sending PENDING tafs out at the desired
 * time and purging the table of old records no longer needed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2012 14715      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class TafQueueManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafQueueManager.class);

    /**
     * Attempt to send all PENDING jobs with a xmitTime on or before the current
     * time. Update each records statusMessage and state based on the outcome of
     * the attempt.
     */
    public void prossessSendJobs() {
        TafQueueDao dao = new TafQueueDao();
        List<TafQueueRecord> records;
        List<TafQueueRecord> sentList = new ArrayList<TafQueueRecord>();
        List<TafQueueRecord> errorList = new ArrayList<TafQueueRecord>();
        Calendar cal = null;
        try {
            records = dao.getRecordsToSend();
            for (TafQueueRecord record : records) {
                cal = Calendar.getInstance();
                cal.setTimeZone(TimeZone.getTimeZone("GMT"));
                System.out.println("TafQueueManager sending TAF @ "
                        + cal.getTime() + ": " + record.getInfo());
                if (sendTAF(record)) {
                    sentList.add(record);
                } else {
                    errorList.add(record);
                }
            }
            if (sentList.size() > 0) {
                dao.updateState(sentList, TafQueueState.SENT);
            }
            if (errorList.size() > 0) {
                dao.updateState(errorList, TafQueueState.BAD);
            }
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
    }

    /**
     * The purge method used to remove no longer needed records.
     */
    public void purgeExpiredData() {
        TafQueueDao dao = new TafQueueDao();
        int numExpired = -999;
        try {
            numExpired = dao.purgeExpiredData();
        } catch (PluginException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        System.out.println("TafQueueManager expired " + numExpired
                + " records @ " + cal.getTime());
    }

    /**
     * Distribute the TAF and set the record's status message.
     * 
     * @param record
     *            - TAF to send
     * @return recordSent - true when record sent otherwise false
     */
    private boolean sendTAF(TafQueueRecord record) {
        OfficialUserProduct oup = new OfficialUserProduct();
        String name = record.getInfo();
        String[] words = name.split("-");
        String awips = words[1];
        String tstamp = words[4].substring(4);
        String bbb = words[5].substring(words[5].length() - 3);
        oup.setFilename(name);
        oup.setProductText(record.getTafText());
        oup.setAwipsWanPil(awips);
        oup.setUserDateTimeStamp(tstamp);
        oup.setSource("AvnFPS");
        if (!bbb.equals("___")) {
            oup.setWmoType(bbb);
        }

        OUPRequest req = new OUPRequest();
        req.setProduct(oup);
        OUPResponse resp;
        boolean success = false;
        String additionalInfo = "";
        try {
            resp = (OUPResponse) RequestRouter.route(req);
            success = resp.isSendLocalSuccess();
            if (resp.hasFailure()) {
                // check which kind of failure
                Priority p = Priority.EVENTA;
                if (!resp.isAttempted()) {
                    // if was never attempted to send or store even locally
                    p = Priority.CRITICAL;
                    additionalInfo = "ERROR local store never attempted";
                } else if (!resp.isSendLocalSuccess()) {
                    // if send/store locally failed
                    p = Priority.CRITICAL;
                    additionalInfo = "ERROR store locally failed";
                } else if (!resp.isSendWANSuccess()) {
                    // if send to WAN failed
                    if (resp.getNeedAcknowledgment()) {
                        // if ack was needed, if it never sent then no ack
                        // was received
                        p = Priority.CRITICAL;
                        additionalInfo = "ERROR send to WAN failed and no acknowledgment received";
                    } else {
                        // if no ack was needed
                        p = Priority.EVENTA;
                        additionalInfo = "WARNING send to WAN failed";
                    }
                } else if (resp.getNeedAcknowledgment()
                        && !resp.isAcknowledged()) {
                    // if sent but not acknowledged when acknowledgment is
                    // needed
                    p = Priority.CRITICAL;
                    additionalInfo = "ERROR no acknowledgment received";
                }
                statusHandler.handle(p, resp.getMessage());
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        record.setStatusMessage(additionalInfo);
        return success;
    }
}
