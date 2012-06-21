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
package com.raytheon.uf.edex.tafqueue;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
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

public class TafQueueManager implements Runnable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafQueueManager.class);

    private static TafQueueManager instance = new TafQueueManager();

    private String destinationUri;

    public static TafQueueManager getInstance() {
        return instance;
    }

    private TafQueueManager() {
        super();
    }

    /**
     * Used to inform run thread that there is a change in the pending queue.
     */
    public void queueChanged() {
        synchronized (this) {
            this.notify();
        }
    }

    /**
     * Start up the runtime thread.
     */
    public void startManager() {
        Thread thread = new Thread(this);
        thread.start();
        thread.setName("TafQueueManager");
        statusHandler.handle(Priority.INFO, "Tafqueue manager started.");
    }

    /**
     * Attempt to transmit all TAFs in records. Update each record's
     * statusMessage and state based on the outcome of the attempt.
     * 
     * @param records
     *            - The TAFs to transmit
     * @param dao
     *            - Used to update state and status of records
     */
    private void prossessSendJobs(final List<TafQueueRecord> records,
            TafQueueDao dao) {
        List<TafQueueRecord> sentList = new ArrayList<TafQueueRecord>();
        List<TafQueueRecord> badList = new ArrayList<TafQueueRecord>();
        Calendar cal = null;
        for (TafQueueRecord record : records) {
            cal = Calendar.getInstance();
            cal.setTimeZone(TimeZone.getTimeZone("GMT"));
            if (sendTAF(record)) {
                sentList.add(record);
            } else {
                badList.add(record);
            }
        }

        if (sentList.size() > 0) {
            dao.updateState(sentList, TafQueueState.SENT);
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                StringBuilder sb = new StringBuilder("TAFs sent: ");
                for (TafQueueRecord record : sentList) {
                    sb.append(record.getInfo()).append("\n");
                }
                statusHandler.handle(Priority.INFO, sb.toString());
            }
        }

        if (badList.size() > 0) {
            dao.updateState(badList, TafQueueState.BAD);
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                StringBuilder sb = new StringBuilder("TAFs failed to send: ");
                for (TafQueueRecord record : sentList) {
                    sb.append(record.getInfo()).append("\n");
                }
                statusHandler.handle(Priority.PROBLEM, sb.toString());
            }
        }
    }

    /**
     * The purge method used to remove no longer needed records.
     * 
     * @param dao
     */
    private void purgeExpiredData(TafQueueDao dao) {
        try {
            int numExpired = dao.purgeExpiredData();
            statusHandler.handle(Priority.INFO, "Expired " + numExpired
                    + " records");
        } catch (PluginException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Purging taf_queue table failed.", e);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Purging taf_queue table failed.", e);
        }
    }

    /**
     * Distribute the TAF and set the record's status message. - Error loading
     * template [marinestatement] for site [PQR], skipping geometry generation
     * for site/template.
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

    @Override
    public void run() {
        TafQueueDao dao = new TafQueueDao();
        List<TafQueueRecord> processList = null;
        Date nextProccessTime = null;
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        Date nextPurgeTime = cal.getTime();

        while (true) {
            try {
                processList = dao.getRecordsToSend();

                if (processList.size() > 0) {
                    prossessSendJobs(processList, dao);
                    // more PENDING may have been added while sending
                    continue;
                }

                if (nextPurgeTime.compareTo(Calendar.getInstance().getTime()) <= 0) {
                    purgeExpiredData(dao);
                    cal = Calendar.getInstance();
                    cal.setTime(nextPurgeTime);
                    cal.add(Calendar.DAY_OF_MONTH, 1);
                    nextPurgeTime = cal.getTime();
                    // more PENDING may have been added while purging
                    continue;
                }

                nextProccessTime = dao.nextXmitTime();
                if (nextProccessTime == null) {
                    nextProccessTime = nextPurgeTime;
                } else if (nextProccessTime.compareTo(nextPurgeTime) > 0) {
                    nextProccessTime = nextPurgeTime;
                } else if (nextProccessTime.compareTo(Calendar.getInstance()
                        .getTime()) <= 0) {
                    // immediate transmit placed on queue while processing.
                    continue;
                }

                synchronized (this) {
                    long timeOut = nextProccessTime.getTime()
                            - Calendar.getInstance().getTime().getTime();
                    // save memory while waiting.
                    processList = null;
                    cal = null;
                    try {
                        wait(timeOut);
                    } catch (InterruptedException e) {
                        statusHandler.handle(Priority.DEBUG,
                                e.getLocalizedMessage(), e);
                    }
                }
            } catch (DataAccessLayerException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    public String getDestinationUri() {
        return destinationUri;
    }

    public void setDestinationUri(String destinationUri) {
        this.destinationUri = destinationUri;
    }
}
