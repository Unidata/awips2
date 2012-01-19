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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;

/**
 * The TransmissionQueue class manages the transmission queue for AvnFPS. It
 * stores a list of TAFs that are pending and sent as well as a list of TAFs
 * that failed to transmit.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2009            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class TransmissionQueue {
    private static TransmissionQueue xmitQueueInstance = null;

    private HashMap<TafMessageData, TafTransmissionJob> pendingList;

    private ArrayList<TafMessageData> sentList;

    private ArrayList<TafMessageData> errorList;

    /**
     * private constructor
     */
    private TransmissionQueue() {
        pendingList = new HashMap<TafMessageData, TafTransmissionJob>();
        sentList = new ArrayList<TafMessageData>();
        errorList = new ArrayList<TafMessageData>();
    }

    /**
     * @return a reference to the TransmissionQueue instance
     */
    public static synchronized TransmissionQueue getInstance() {
        if (xmitQueueInstance == null) {
            xmitQueueInstance = new TransmissionQueue();
        }

        return xmitQueueInstance;
    }

    /**
     * Add a TafMessageData object to the pending queue.
     * 
     * @param data
     *            TafMessageData object
     */
    public void enqueue(TafMessageData data) {
        long now = System.currentTimeMillis();
        long xmitTime = data.getXmitTime();
        long delay = xmitTime - now;

        if (delay < 0) {
            delay = 0;
        }

        TafTransmissionJob job = new TafTransmissionJob(
                "AvnFPS TAF transmission job", data);
        job.setSystem(true);
        job.schedule(delay);
        pendingList.put(data, job);
    }

    /**
     * Prohibit cloning
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }

    /**
     * Retrieve a list of the pending TAFs.
     * 
     * @return an ArrayList containing the info strings of the pending TAFs.
     */
    public ArrayList<String> getPending() {
        ArrayList<String> out = new ArrayList<String>();
        Set<TafMessageData> keys = pendingList.keySet();

        for (TafMessageData data : keys) {
            out.add(data.getInfo());
        }

        return out;
    }

    public TafMessageData getPendingBySiteId(String siteId) {
        Set<TafMessageData> keys = pendingList.keySet();

        TafMessageData found = null;
        for (TafMessageData data : keys) {
            if (data.getSiteId().equals(siteId)) {
                found = data;
                break;
            }
        }
        return found;
    }

    /**
     * Retrieve a list of the TAFs that have been successfuly sent.
     * 
     * @return an ArrayLsit containing the info strings of the TAFs that have
     *         been successfuly sent.
     */
    public ArrayList<String> getSent() {
        ArrayList<String> out = new ArrayList<String>();

        for (TafMessageData data : sentList) {
            out.add(data.getInfo());
        }

        return out;
    }

    /**
     * Retrieve a list of the TAFs that failed to send.
     * 
     * @return an ArrayList containing the info string of the TAFs that failed
     *         to send.
     */
    public ArrayList<String> getErrors() {
        ArrayList<String> out = new ArrayList<String>();

        for (TafMessageData data : errorList) {
            out.add(data.getInfo());
        }

        return out;
    }

    /**
     * Retrieve the text of a specified TAF in the pending list
     * 
     * @param tafInfo
     *            TAF info string for the desired TAF
     * @return The text of the TAF
     */
    public String getPendingText(String tafInfo) {
        Set<TafMessageData> keys = pendingList.keySet();
        String text = null;

        for (TafMessageData data : keys) {
            if (tafInfo.equals(data.getInfo())) {
                text = data.getTafText();
                break;
            }
        }

        return text;
    }

    /**
     * Retrieve the text of a specified TAF in the sent list
     * 
     * @param tafInfo
     *            TAF info string for the desired TAF
     * @return The text of the TAF
     */
    public String getSentText(String tafInfo) {
        String text = null;

        for (TafMessageData data : sentList) {
            if (tafInfo.equals(data.getInfo())) {
                text = data.getTafText();
                break;
            }
        }

        return text;
    }

    /**
     * Retrieve the text of a specified TAF in the error list
     * 
     * @param tafInfo
     *            TAF info string for the desired TAF
     * @return The text of the TAF
     */
    public String getErrorText(String tafInfo) {
        String text = null;

        for (TafMessageData data : errorList) {
            if (tafInfo.equals(data.getInfo())) {
                text = data.getTafText();
                break;
            }
        }

        return text;
    }

    /**
     * Retrive a log of all transmission attempts for the current day.
     * 
     * @return an ArrayList containing the status of all transmission attempts
     *         for the current day.
     */
    public ArrayList<String> getLog() {
        ArrayList<String> logList = new ArrayList<String>();

        try {
            String tempTafPath = "aviation/transmissionLogs/";
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
            String fname = "AviationTransmissionLog.txt";
            LocalizationFile lFile = pm.getLocalizationFile(context,
                    tempTafPath + fname);
            File file = lFile.getFile();
            FileReader reader = new FileReader(file);
            BufferedReader input = new BufferedReader(reader);
            String line = null;

            while ((line = input.readLine()) != null) {
                logList.add(line);
            }
        } catch (IOException e) {
            logList.add("Error, log file not found.");
        }

        return logList;
    }

    /**
     * Remove a TAF from the list of pending TAFs.
     * 
     * @param tafInfo
     *            the TafMessageData info string for the TAF to be removed from
     *            the pending queue.
     */
    public void remove(String tafInfo) {
        Set<TafMessageData> keys = pendingList.keySet();

        for (TafMessageData data : keys) {
            if (tafInfo.equals(data.getInfo())) {
                TafTransmissionJob job = pendingList.remove(data);
                job.cancel();
                break;
            }
        }
    }

    /**
     * Attempt to retransmit a TAF.
     * 
     * @param tafInfo
     *            the TafMessageData info string for the TAF to retransmit.
     * @param bad
     *            flag that determines which list to look in for the TAF to
     *            retransmit. True indicates the TAF should come from the error
     *            list, false indicates the sent list.
     */
    public void retransmit(String tafInfo, boolean bad) {
        TafMessageData retrans = null;
        ArrayList<TafMessageData> list = null;

        // If bad flag is set, check the bad list for the taf to retransmit,
        // otherwise check the sent list.
        if (bad) {
            list = errorList;
        } else {
            list = sentList;
        }

        for (TafMessageData data : list) {
            if (tafInfo.equals(data.getInfo())) {
                list.remove(data);
                retrans = data;
                break;
            }
        }

        retrans.updateTime();

        enqueue(retrans);
    }

    /**
     * Update the status of a pending TAF by moving it either to the sent or the
     * error list. This method should only be called by the TafTransmissionJob's
     * run method.
     * 
     * @param taf
     *            the TafMessageData object to update
     * @param success
     *            boolean
     */
    public void updateMessageStatus(TafMessageData taf, boolean success) {
        String tafInfo = taf.getInfo();
        Set<TafMessageData> keys = pendingList.keySet();

        for (TafMessageData data : keys) {
            if (tafInfo.equals(data.getInfo())) {
                pendingList.remove(data);
                break;
            }
        }

        if (success) {
            sentList.add(taf);
        } else {
            errorList.add(taf);
        }
    }
}
