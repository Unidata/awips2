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
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;

import org.apache.camel.Header;
import org.apache.camel.Headers;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Provides logging and deletion services for camel
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2008            chammack     Initial creation
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
     * 
     * @param headers
     */
    public void log(@Headers Map<?, ?> headers) {

        long curTime = System.currentTimeMillis();

        StringBuilder sb = new StringBuilder(128);

        String pluginName = getHeaderProperty(headers, "pluginName");
        if (pluginName != null) {
            sb.append(pluginName);
        }

        String fileName = getHeaderProperty(headers, "ingestFileName");
        if (fileName != null) {
            sb.append(":: ");
            sb.append(fileName);
        }

        Long dequeueTime = getHeaderProperty(headers, "dequeueTime");
        DecimalFormat df = FORMAT.get();
        if (dequeueTime != null) {
            double elapsed = (curTime - dequeueTime) / 1000.0;
            sb.append(" processed in: ");
            sb.append(df.format(elapsed));
            sb.append(" (sec)");
        }

        Long enqueueTime = getHeaderProperty(headers, "enqueueTime");
        if (enqueueTime != null) {
            double latency = (curTime - enqueueTime) / 1000.0;
            sb.append(" Latency: ");
            sb.append(df.format(latency));
            sb.append(" (sec)");
        }
        // Make sure we have something to log.
        if (sb.length() > 0) {
            handler.handle(Priority.INFO, sb.toString());
        } else {
            handler.handle(Priority.INFO, "No logging information available");
        }
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
}
