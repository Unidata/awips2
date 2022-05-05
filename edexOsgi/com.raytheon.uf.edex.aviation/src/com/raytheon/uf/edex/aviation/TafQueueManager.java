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
package com.raytheon.uf.edex.aviation;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.auth.user.User;
import com.raytheon.uf.common.aviation.avnconfig.ITafSiteConfig;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteConfigFactory;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteData;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.tafqueue.TafQueueRecord;
import com.raytheon.uf.common.tafqueue.TafQueueRecord.TafQueueState;
import com.raytheon.uf.edex.database.DataAccessLayerException;

import jep.JepConfig;

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
 * Mar 21, 2013 15375      zhao        Modified to also handle AvnFPS VFT product
 * Jun 07, 2013  1981      mpduff      Add user to OUPRequest.
 * Jun 18, 2013  2110      rferrel     Modified to handle the new class RequestRouter.route
 *                                      returns (SuccessfulExecution).
 * Feb 11, 2016 16939      zhao        Modified sendTaf() to ensure bbb is null for routine TAF
 * May 15, 2019 20693   mgamazaychikov Added sendIWXXMMessages
 * Aug 27, 2019 21545   mgamazaychikov Added handling of forecasterId in sendIWXXMMessages
 * Sep 30, 2019 21615   mgamazaychikov Fixed bug to use startsWith("TAF")) in sendIWXXMMessages
 * Jan 29, 2020 21611   mgamazaychikov Send IWXXM XMLs directly to msg_send bypassing handleOUP
 * Jan 14, 2022  8736      jsebahar    Added additional logging to verify process is running
 * </pre>
 *
 * @author rferrel
 */

public class TafQueueManager implements Runnable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafQueueManager.class);

    private static final String ZERO_MINUTES = "00";

    private static final String MSG_SEND = "msg_send";

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
    private void processSendJobs(final List<TafQueueRecord> records,
            TafQueueDao dao) {
        List<TafQueueRecord> sentList = new ArrayList<>();
        List<TafQueueRecord> badList = new ArrayList<>();
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

        if (!sentList.isEmpty()) {
            dao.updateState(sentList, TafQueueState.SENT);
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                StringBuilder sb = new StringBuilder("TAFs sent: ");
                for (TafQueueRecord record : sentList) {
                    sb.append(record.getInfo()).append("\n");
                }
                statusHandler.handle(Priority.INFO, sb.toString());
            }
            /*
             * Successfully sent TAFs are realized in IWXXM form and sent also
             */
            generateIWXXMMessages(sentList);
        }

        if (!badList.isEmpty()) {
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
     * IWXXM messages follow after successful transmission of the TAF(s) in text
     * form.
     */
    @SuppressWarnings("unchecked")
    private void generateIWXXMMessages(List<TafQueueRecord> sentList) {
        /*
         * Each successfully sent alphanumeric TAF product will now be
         * represented in IWXXM form and be sent also. textToXml is the python
         * interpreter that will do the work.
         */
        ITafSiteConfig siteConfig;
        try {
            siteConfig = TafSiteConfigFactory.getInstance();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading DEFAULT configuration: ", e);
            return;
        }
        IPathManager pm = PathManagerFactory.getPathManager();
        File runner = pm.getStaticFile("aviation/python/XmlTafEncoder.py");
        LocalizationContext baseCtx = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        // need getFile to pull whole python dir from localization server
        pm.getLocalizationFile(baseCtx, "aviation/python").getFile();
        String filePath = runner.getPath();
        String loggingHandlerDir = pm.getStaticFile("python/UFStatusHandler.py")
                .getParentFile().getPath();
        String pythonDir = pm.getFile(baseCtx, "python").getPath();
        String includePath = PyUtil.buildJepIncludePath(
                runner.getParentFile().getPath(), loggingHandlerDir, pythonDir);
        JepConfig config = new JepConfig().setIncludePath(includePath)
                .setClassLoader(TafQueueManager.class.getClassLoader());
        try (PythonScript python = new PythonScript(config, filePath)) {
            python.instantiatePythonClass("textToXml", "XmlTafEncoder", null);

            for (TafQueueRecord record : sentList) {
                if (record.getStatusMessage().contains("WAN failed")) {
                    continue;
                }
                String siteLocation = null;
                String siteId = record.getSiteId();
                String tafText = record.getTafText();
                String lines[] = tafText.split("\\r?\\n");
                if (lines[0].startsWith("TAF")) {
                    siteId = lines[1].trim().substring(0, 4);
                }
                TafSiteData siteData = null;
                try {
                    siteData = siteConfig.getSite(siteId);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error reading site configuration for " + siteId
                                    + ": ",
                            e);
                    return;
                }
                siteLocation = String.format("%s %s %s", siteData.latitude,
                        siteData.longitude, siteData.elevation);
                //
                // Build the WMO AHL line from the current TAF record and use
                // that as a dictionary key
                // TTAAii - change first letter from 'F' to 'L' to indicate that
                // this is a XML Aviation Product
                StringBuilder ttaaiiSB = new StringBuilder(record.getWmoId());
                ttaaiiSB.setCharAt(0, 'L');
                String ttaaii = ttaaiiSB.toString();
                //
                // CCCC - remains unchanged
                String cccc = record.getStationId();
                //
                // YYGG00
                String[] temp = record.getHeaderTime().toString().split(" ");
                String yygg = temp[0].split("-")[2] + temp[1].split(":")[0]
                        + ZERO_MINUTES;
                //
                // BBB
                String bbb = record.getBBB();
                //
                // Construct the new AHL line to be used as a dictionary key for
                // this XML TAF
                String ahl = String.format("%s %s %s %s", ttaaii, cccc, yygg,
                        bbb);
                Map<String, Object> args = new HashMap<>();
                args.put("tac", record.getTafText());
                args.put("ahl", ahl);
                args.put("geolocation", siteLocation);
                // encode does not return anything back
                python.execute("encode", "textToXml", args);
            }
            // Write out IWXXM documents, grouped by AHL line
            boolean done = false;
            while (!done) {
                // write method generates the flat file containing the XML
                // document for msg_send in the /awips2/edex/data/outgoing/
                // and returns the command line arguments used to send that file
                Object lst = python.execute("write", "textToXml", null);
                List<String> argsList = (List<String>) lst;
                if (argsList == null || argsList.isEmpty()) {
                    done = true;
                    continue;
                } else {
                    LinkedList<String> command = new LinkedList<>();
                    // first string is the name of msg_send
                    command.add(MSG_SEND);
                    for (String str : argsList) {
                        command.add(str);
                    }
                    sendIWXXMMessages(command);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Python error occurred while executing XmlTafEncoder - see message details.",
                    e);

        }
        return;
    }

    private void sendIWXXMMessages(LinkedList<String> command) {
        Process proc = null;
        String messageId = null;
        ProcessBuilder procDesc = new ProcessBuilder(command);
        procDesc.redirectErrorStream(true);
        try {
            proc = procDesc.start();
            InputStream stdout = proc.getInputStream();
            InputStreamReader isr = new InputStreamReader(stdout);
            BufferedReader br = new BufferedReader(isr);
            String outp;
            while ((outp = br.readLine()) != null) {
                if (outp.length() > 0) {
                    messageId = outp;
                }
            }
            int exitVal = proc.waitFor();
            if (exitVal != 0) {
                statusHandler.handle(Priority.PROBLEM,
                        "Abnormal exit code from msg_send command: " + exitVal);
            } else {
                statusHandler.handle(Priority.INFO,
                        "Successfully executed msg_send command: " + command
                                + "; message ID=" + messageId);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception encountered during msg_send(): "
                            + e.getMessage(),
                    e);
        } finally {
            if (proc != null) {
                proc.destroy();
            }
        }
        return;
    }

    /**
     * The purge method used to remove no longer needed records.
     *
     * @param dao
     */
    private void purgeExpiredData(TafQueueDao dao) {
        try {
            int numExpired = dao.purgeExpiredData();
            statusHandler.handle(Priority.INFO,
                    "Expired " + numExpired + " records");
        } catch (PluginException | DataAccessLayerException e) {
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
        if (!"___".equals(bbb)) {
            if (bbb.trim().length() == 3) {
                oup.setWmoType(bbb);
            }
        }
        OUPRequest req = new OUPRequest();
        req.setUser(new User(OUPRequest.EDEX_ORIGINATION));
        req.setProduct(oup);
        OUPResponse resp = null;
        boolean success = false;
        String additionalInfo = "";
        try {
            Object object = RequestRouter.route(req);
            if (!(object instanceof SuccessfulExecution)) {
                statusHandler.handle(Priority.ERROR,
                        "Error transferring Official User Product. Unexpected response class:"
                                + object.getClass().getName());
            } else {
                resp = (OUPResponse) ((SuccessfulExecution) object)
                        .getResponse();
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

        /**
         * (for DR15375)
         */
        TafQueueVFTMgr vftMgr = TafQueueVFTMgr.getInstance();
        Date nextVftTime = null;

        while (true) {
            try {

                processList = dao.getRecordsToSend();

                statusHandler.handle(Priority.INFO,
                        "Processing " + processList.size() + " TAFS");

                if (!processList.isEmpty()) {
                    processSendJobs(processList, dao);
                    // more PENDING may have been added while sending
                    continue;
                }

                /**
                 * (for DR15375)
                 */
                nextVftTime = vftMgr.getNextVftTime();
                if (nextVftTime
                        .compareTo(Calendar.getInstance().getTime()) <= 0) {
                    vftMgr.makeVftProduct();
                    // transmit immediately
                    continue;
                }

                if (nextPurgeTime
                        .compareTo(Calendar.getInstance().getTime()) <= 0) {
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
                } else if (nextProccessTime
                        .compareTo(Calendar.getInstance().getTime()) <= 0) {
                    // immediate transmit placed on queue while processing.
                    continue;
                }

                /**
                 * (DR15375)
                 */
                nextVftTime = vftMgr.getNextVftTime();
                if (nextVftTime.compareTo(nextProccessTime) < 0) {
                    nextProccessTime = nextVftTime;
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
