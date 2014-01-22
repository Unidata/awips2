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
package com.raytheon.uf.edex.esb.camel;

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;

import org.apache.camel.Exchange;
import org.apache.camel.Header;
import org.apache.camel.Headers;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.stats.ProcessEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Provides logging and deletion services for camel
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2008             chammack    Initial creation
 * Feb 05, 2013 1580       mpduff      EventBus refactor.
 * Feb 12, 2013 1615       bgonzale    Changed ProcessEvent pluginName to dataType.
 * Jan 21, 2014 2627       njensen     Added logFailedData() and logFailureAsInfo()
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class ProcessUtil {

    protected static final IUFStatusHandler handler = UFStatus
            .getNamedHandler("Ingest");

    protected transient final static ThreadLocal<DecimalFormat> FORMAT = new ThreadLocal<DecimalFormat>() {

        @Override
        protected DecimalFormat initialValue() {
            DecimalFormat rval = new DecimalFormat();
            rval.setMaximumFractionDigits(4);
            rval.setMinimumFractionDigits(4);
            return rval;
        }

    };

    private static final String FAILED_DIR;

    protected static final boolean RETAIN_FAILED_DATA;

    static {
        // this will probably only ever be true on a testbed
        RETAIN_FAILED_DATA = Boolean.getBoolean("retain.failed.data");
        if (RETAIN_FAILED_DATA) {
            FAILED_DIR = PropertiesFactory.getInstance().getEnvProperties()
                    .getEnvValue("DEFAULTDATADIR")
                    + File.separator + "failed";
            File file = new File(FAILED_DIR);
            if (!file.exists()) {
                file.mkdir();
            }
        } else {
            FAILED_DIR = null;
        }
    }

    /**
     * Get the value of a specified property if it exists.
     * 
     * @param <T>
     *            Type of the receiving variable.
     * @param headers
     *            Map of header properties.
     * @param propertyName
     *            Name of the property to get.
     * @return Value of the requested property. Null if the property does not
     *         exist.
     */
    @SuppressWarnings("unchecked")
    private static <T> T getHeaderProperty(Map<?, ?> headers,
            Object propertyName) {

        Object o = headers.get(propertyName);

        T result = null;
        if (o != null) {
            result = (T) o;
        }
        return result;
    }

    /**
     * Return an incoming array as an Iterator to the elements.
     * 
     * @param objects
     * @return
     */
    public static Iterator<?> iterate(PluginDataObject[] objects) {
        return Arrays.asList(objects).iterator();
    }

    public void delete(@Header(value = "ingestFileName") String path) {
        File f = new File(path);
        if (f.exists()) {
            f.delete();
        }
    }

    public void deleteFile(File f) {
        if (f.exists()) {
            f.delete();
        }
    }

    /**
     * Logs the processing and latency time of a file
     * 
     * @param headers
     */
    public void log(@Headers Map<?, ?> headers) {
        logInternal(headers, true);
    }

    /**
     * Logs the processing and latency time of a file
     * 
     * @param headers
     *            the headers associated with the ingest routes
     * @param successful
     *            whether or not the file was successfully ingested
     */
    protected void logInternal(Map<?, ?> headers, boolean successful) {
        long curTime = System.currentTimeMillis();

        StringBuilder sb = new StringBuilder(128);

        ProcessEvent processEvent = new ProcessEvent();
        String dataType = getHeaderProperty(headers, "dataType");
        String pluginName = getHeaderProperty(headers, "pluginName");
        if (dataType != null) {
            sb.append(dataType);
            processEvent.setDataType(dataType);
        } else if (pluginName != null) {
            sb.append(pluginName);
            processEvent.setDataType(pluginName);
        }

        String fileName = getHeaderProperty(headers, "ingestFileName");
        if (fileName != null) {
            sb.append(":: ");
            sb.append(fileName);
            processEvent.setFileName(fileName);
        }

        Long dequeueTime = getHeaderProperty(headers, "dequeueTime");
        DecimalFormat df = FORMAT.get();
        if (dequeueTime != null) {
            long elapsedMilliseconds = curTime - dequeueTime;
            double elapsed = elapsedMilliseconds / 1000.0;
            sb.append(" processed in: ");
            sb.append(df.format(elapsed));
            sb.append(" (sec)");
            processEvent.setProcessingTime(elapsedMilliseconds);
        }

        Long enqueueTime = getHeaderProperty(headers, "enqueueTime");
        if (enqueueTime != null) {
            long latencyMilliseconds = curTime - enqueueTime;
            double latency = latencyMilliseconds / 1000.0;
            sb.append(" Latency: ");
            sb.append(df.format(latency));
            sb.append(" (sec)");
            processEvent.setProcessingLatency(latencyMilliseconds);
        }

        // processing in less than 0 millis isn't trackable, usually due to an
        // error occurred and statement logged incorrectly
        if (successful && (processEvent.getProcessingLatency() > 0)
                && (processEvent.getProcessingTime() > 0)) {
            EventBus.publish(processEvent);
        }

        // Make sure we have something to log.
        if (sb.length() > 0) {
            handler.handle(Priority.INFO, sb.toString());
        } else {
            handler.handle(Priority.INFO, "No logging information available");
        }

    }

    /**
     * Logs a failure to ingest a file. Potentially saves off the data that
     * failed to ingest, based on a system property.
     * 
     * @param ex
     *            the exchange that failed to ingest
     */
    public void logFailedData(Exchange ex) {
        Exception e = ex.getException();
        if (e == null) {
            e = ex.getProperty(Exchange.EXCEPTION_CAUGHT, Exception.class);
        }
        Map<?, ?> headers = ex.getIn().getHeaders();
        String fullpath = getHeaderProperty(headers, ("ingestFileName"));
        handler.error("Failed to ingest " + fullpath, e);

        if (RETAIN_FAILED_DATA) {
            File badfile = new File(fullpath);
            if (badfile.exists()) {
                String filename = badfile.getName();
                File keepfile = new File(FAILED_DIR + File.separator + filename);
                try {
                    FileUtil.copyFile(badfile, keepfile);
                    handler.info("Copied failed data to " + keepfile.getPath());
                } catch (IOException e1) {
                    handler.error(
                            "Unable to copy failed data for later analysis", e);
                }
            }
        }

        logInternal(headers, false);
    }

    /**
     * Logs a failure to ingest data as an info message. This should only be
     * used when the failure can be considered as expected given the input, such
     * as submitting invalid/bad data to a decoder.
     * 
     * @param ex
     *            the exchange that failed to ingest
     */
    public void logFailureAsInfo(Exchange ex) {
        Exception e = ex.getException();
        if (e == null) {
            e = ex.getProperty(Exchange.EXCEPTION_CAUGHT, Exception.class);
        }
        Map<?, ?> headers = ex.getIn().getHeaders();
        String fullpath = getHeaderProperty(headers, ("ingestFileName"));
        String msg = "Discarding " + fullpath;
        if (e != null) {
            msg += " due to " + e.getClass().getSimpleName() + ": "
                    + e.getLocalizedMessage();
        }
        handler.info(msg);

        logInternal(headers, false);
    }

}
