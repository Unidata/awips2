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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Calendar;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * This job handles the actual transmission of the TAF.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2009            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class TafTransmissionJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafTransmissionJob.class);

    /**
     * The TafMessageData to be sent out by this job.
     */
    private TafMessageData data;

    /**
     * @param name
     */
    public TafTransmissionJob(String name, TafMessageData data) {
        super(name);
        this.data = data;
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        OfficialUserProduct oup = new OfficialUserProduct();
        String name = data.getInfo();
        String[] words = name.split("-");
        String awips = words[1];
        String tstamp = words[4].substring(4);
        String bbb = words[5].substring(words[5].length() - 3);
        oup.setFilename(name);
        oup.setProductText(data.getTafText());
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
            resp = (OUPResponse) ThriftClient.sendRequest(req);
            success = resp.isSendLocalSuccess();
            if (resp.hasFailure()) {
                // check which kind of failure
                Priority p = Priority.EVENTA;
                if (!resp.isAttempted()) {
                    // if was never attempted to send or store even locally
                    p = Priority.CRITICAL;
                    additionalInfo = " ERROR local store never attempted";
                } else if (!resp.isSendLocalSuccess()) {
                    // if send/store locally failed
                    p = Priority.CRITICAL;
                    additionalInfo = " ERROR store locally failed";
                } else if (!resp.isSendWANSuccess()) {
                    // if send to WAN failed
                    if (resp.getNeedAcknowledgment()) {
                        // if ack was needed, if it never sent then no ack
                        // was received
                        p = Priority.CRITICAL;
                        additionalInfo = " ERROR send to WAN failed and no acknowledgment received";
                    } else {
                        // if no ack was needed
                        p = Priority.EVENTA;
                        additionalInfo = " WARNING send to WAN failed";
                    }
                } else if (resp.getNeedAcknowledgment()
                        && !resp.isAcknowledged()) {
                    // if sent but not acknowledged when acknowledgment is
                    // needed
                    p = Priority.CRITICAL;
                    additionalInfo = " ERROR no acknowledgment received";
                }
                statusHandler.handle(p, resp.getMessage());
            }
        } catch (VizException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }

        TransmissionQueue queue = TransmissionQueue.getInstance();
        queue.updateMessageStatus(data, success);

        String logMessage;
        if (success) {
            logMessage = "SUCCESS " + data.getInfo() + additionalInfo;
        } else {
            logMessage = "FAILURE " + data.getInfo() + additionalInfo;
        }

        String tempTafPath = "aviation/transmissionLogs/";
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        String path = pm.getFile(context, tempTafPath).getAbsolutePath();

        File tmp = new File(path);
        if (!(tmp.exists())) {
            tmp.mkdirs();
        }

        String fname = "AviationTransmissionLog.txt";
        LocalizationFile lFile = pm.getLocalizationFile(context, tempTafPath
                + fname);
        File file = lFile.getFile();
        try {
            FileWriter writer = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(writer);
            Calendar c = Calendar.getInstance();
            String year = Integer.toString(c.get(Calendar.YEAR));
            String month = Integer.toString(c.get(Calendar.MONTH) + 1);
            String day = Integer.toString(c.get(Calendar.DAY_OF_MONTH));

            output.write(year + month + day + "," + logMessage + "\n");
            output.close();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return Status.OK_STATUS;
    }
}
