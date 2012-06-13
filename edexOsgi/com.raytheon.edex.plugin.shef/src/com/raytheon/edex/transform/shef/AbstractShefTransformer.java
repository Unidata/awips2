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
package com.raytheon.edex.transform.shef;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import javax.xml.transform.TransformerException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.transform.shef.obs.ObsToSHEFOptions;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Base class for observation data to SHEF conversions.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2008       1659 jkorman     Initial creation
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class AbstractShefTransformer<T extends PluginDataObject>
        implements ShefTransformerInterface {

    protected Log logger = LogFactory.getLog(getClass());

    public static final String METAR_2_SHEF_NNN = "MTR";
    
    public static final String METAR_2_SHEF_OPT = "metar2shef_options";

    public static final String SHEF_A_RECORD = ".A";

    public static final String SHEF_REVISION = "R";

    public static final String SHEF_SEPARATOR = "/";

    public static final String SHEF_OBS_DATEY2K_FMT = "%1$tY%1$tm%1$td Z";

    public static final String SHEF_OBS_DATE_FMT = "%1$ty%1$tm%1$td Z";

    public static final String SHEF_OBS_TIME_FMT = " DH%1$tH%1$tM";

    public static final String SHEF_OBS_BASISTIMEY2K_FMT = "/DC%1$tY%1$tm%1$td%1$tH%1$tM";

    public static final String SHEF_OBS_BASISTIME_FMT = "/DC%1$ty%1$tm%1$td%1$tH%1$tM";

    // ***********************
    private static Integer instanceId = 0;

    private Boolean jmxModeOn = null;

    // ***********************
    // Exposed properties
    private String serviceName = null;

    private int messageCount = 0;

    private String lastMessage;

    private String commandLineOptions = null;

    private String metar2ShefOptions = null;

    ObsToSHEFOptions options = null;

    // ************************************************************

    protected static final String SOH = String.valueOf((char) 1);

    protected static final String CRCRLF = "\r\r\n";

    protected static final String ETX = String.valueOf((char) 3);

    private final String WMO_HEADER_FMT;

    public AbstractShefTransformer(String cmdLine, String headerFmt) {

        commandLineOptions = cmdLine;
        getAppsDefaults();

        instanceId = instanceId + 1;

        WMO_HEADER_FMT = headerFmt;

        logger.debug("Creating " + getClass().getName() + " instance "
                + instanceId);
    }

    /**
     * 
     * @param report
     * @return
     * @throws TransformerException
     */
    public final byte[] transform(T report, Headers headers)
            throws TransformerException {
        String cmdLine = AppsDefaults.getInstance().getToken(METAR_2_SHEF_OPT,
                null);
        if ((options != null) && (cmdLine != null)) {
            if (!cmdLine.equals(metar2ShefOptions)) {
                metar2ShefOptions = cmdLine;
                options.updateCommandLine(cmdLine);
            }
            options.updateOptions();
        }
        return transformReport(report, headers);
    }

    /**
     * 
     * @param objects
     * @return
     */
    public static Iterator<?> iterate(PluginDataObject[] objects) {
        Iterator<PluginDataObject> it = null;
        if (objects != null) {
            List<PluginDataObject> obj = Arrays.asList(objects);
            if (obj != null) {
                it = obj.iterator();
            }
        }
        // Ensure that we always pass back a not null Iterator.
        if (it == null) {
            it = new Iterator<PluginDataObject>() {
                @Override
                public boolean hasNext() {
                    return false;
                }

                @Override
                public PluginDataObject next() {
                    return null;
                }

                @Override
                public void remove() {
                }
            };
        }
        return it;
    }

    /**
     * 
     * @param report
     * @return
     * @throws TransformerException
     */
    protected abstract byte[] transformReport(T report, Headers headers)
            throws TransformerException;

    /**
     * 
     * @param buffer
     * @param report
     * @return
     */
    protected StringBuilder openWMOMessage(int sequenceId, int initialSize) {

        sequenceId = (sequenceId % 1000) + 1;
        StringBuilder buffer = new StringBuilder(initialSize);
        buffer.append(String.format("%s" + CRCRLF + "%03d", SOH, sequenceId));
        return buffer;
    }

    /**
     * 
     * @param buffer
     * @param headers
     * @param report
     * @return
     */
    protected StringBuilder makeWMOHeader(StringBuilder buffer,
            String stationId, Headers headers, WMOHeader hdr) {

        Calendar c = null;
        
        if((hdr != null)&&(headers != null)) {
            c = TimeTools.findDataTime(hdr.getYYGGgg(), headers);
        } else {
            c = TimeTools.getSystemCalendar();
        }
        buffer.append(String.format(WMO_HEADER_FMT, stationId, c));
        
        return buffer;
    }

    /**
     * 
     * @param buffer
     * @param report
     * @return
     */
    protected static StringBuilder closeWMOMessage(StringBuilder buffer) {
        return startMessageLine(buffer).append(ETX);
    }

    /**
     * 
     * @param buffer
     * @return
     */
    protected static StringBuilder startMessageLine(StringBuilder buffer) {
        buffer.append(CRCRLF);
        return buffer;
    }

    /**
     * Create the body of a SHEF message that encodes various elements contained
     * in the PluginDataObject being transformed. Each line within the message
     * must start with a CRCRLF segment.
     * 
     * @param buffer
     *            StringBuilder to receive the encoded data.
     * @param report
     *            The PluginDataObject being transformed.
     * @return The StringBuilder instance.
     */
    protected abstract StringBuilder encodeShef(StringBuilder buffer, T report,
            Headers headers);

    /**
     * Should this reports data be encoded?
     * 
     * @param report
     *            Data to be used to determine encoding status.
     * @return Should this reports data be encoded?
     */

    protected abstract boolean encodeThisStation(T report);

    /**
     * Clear the message count to zero.
     */
    @Override
    public void clearMessageCount() {
        messageCount = 0;
    }

    /**
     * Get a count of messages processed since startup or the last reset.
     * 
     * @return Message count.
     */
    @Override
    public int getMessageCount() {
        return messageCount;
    }

    protected void incrementMessageCount() {
        messageCount++;
    }

    /**
     * @return the lastMessage
     */
    public String getLastMessage() {
        return lastMessage;
    }

    /**
     * @param lastMessage
     *            the lastMessage to set
     */
    public void setLastMessage(String lastMessage) {
        this.lastMessage = lastMessage;
    }

    /**
     * Get the name of this service.
     * 
     * @return The service name.
     */
    @Override
    public String getServiceName() {
        return serviceName;
    }

    /**
     * Set the name of this service.
     * 
     * @param serviceName
     *            The service name.
     */
    public void setServiceName(String serviceName) {
        logger.debug("Setting serviceName:" + serviceName + " on instance:"
                + instanceId);
        this.serviceName = serviceName;
    }

    /**
     * @return the jmxModeOn
     */
    public boolean isJmxModeOn() {
        return jmxModeOn;
    }

    /**
     * 
     */
    private void getAppsDefaults() {
        String appsDefaults = AppsDefaults.getInstance().getToken(
                METAR_2_SHEF_OPT, null);
        if (appsDefaults != null) {
            metar2ShefOptions = appsDefaults;
        } else {
            metar2ShefOptions = commandLineOptions;
        }
        options = new ObsToSHEFOptions(metar2ShefOptions, true);
    }

    /**
     * Shortens a dot separated name to the specified number of elements.
     * 
     * @param name
     *            the name to shorten.
     * @param elements
     *            the maximum number of elements in the final.
     * @return the shortened name.
     */
    protected static String rightShortenName(String name, int elements) {
        StringBuffer rtn = new StringBuffer();
        String[] parts = name.split("\\.");
        int length = parts.length;
        int start = length - elements;
        /*
         * return the input name of the requested elements contains the entire
         * name.
         */
        if (length <= elements) {
            rtn.append(name);
        } else {
            for (int i = start; i < length; i++) {
                rtn.append((i > start ? "." : "") + parts[i]);
            }
        }
        return rtn.toString();
    }

}