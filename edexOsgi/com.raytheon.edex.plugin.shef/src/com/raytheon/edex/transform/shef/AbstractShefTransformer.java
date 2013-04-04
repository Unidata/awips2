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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import javax.xml.transform.TransformerException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.transform.shef.obs.ObsToSHEFOptions;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.ohd.AppsDefaults;
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
 * ======================================
 * AWIPS2 DR Work
 * 20120918           1185 jkorman     Added save to archive capability.     
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class AbstractShefTransformer<T extends PluginDataObject>
        implements ShefTransformerInterface {

    protected static final String SOH = String.valueOf((char) 1);

    protected static final String CRCRLF = "\r\r\n";

    protected static final String ETX = String.valueOf((char) 3);

    private final String WMO_HEADER_FMT;

    private static final String ARCHIVE_FORMAT = "%s_%04d";
    
    // Note that the nuber of digits must agree with ARCHIVE_FORMAT
    private static final int MAX_ARCHIVE_SEQUENCE = 10000;
    
    private static final String WMO_MSG_SEQ_FMT = "%03d";
    
    // WMO Sequence number range from 001..999
    private static final int MAX_WMO_MSG_SEQ = 1000;
    
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

    public static final String OPT_ARC_ENABLE = "archive_enable";
    
    public static final String OPT_SHEF_ARC_DIR = "archive_shefdata_dir";

    // ***********************
    private static Integer instanceId = 0;

    private Boolean jmxModeOn = null;

    // ***********************
    // Exposed properties

    protected Log logger = LogFactory.getLog(getClass());

    private String serviceName = null;

    private int messageCount = 0;

    private String lastMessage;

    private String commandLineOptions = null;

    private String metar2ShefOptions = null;

    private boolean archiveEnabled = false;
    
    private String shefArchiveDir = null;

    private File shefArchiveFileDir = null;
    
    protected ObsToSHEFOptions options = null;

    private static AtomicInteger sequenceNumber = new AtomicInteger();

    protected static AtomicInteger msgSequence = new AtomicInteger();
    // ************************************************************

    /**
     * Create the common transformer.
     * @param cmdLine Command line options that may be used if these
     * options are not present in the Apps_defaults.
     * @param headerFmt The specific WMO header format string to be used
     * when constructing a WMO header for a particular subclass.
     */
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
        if(options != null) {
            if(cmdLine != null) {
                if (!cmdLine.equals(metar2ShefOptions)) {
                    metar2ShefOptions = cmdLine;
                    options.updateCommandLine(cmdLine);
                }
            }
            options.updateOptions();
        }
        configureArchiveDir();
        
        return transformReport(report, headers);
    }

    /**
     * 
     * @param objects
     * @return
     */
    public static Iterator<?> iterate(PluginDataObject[] objects) {
        Iterator<PluginDataObject> it = null;
        if ((objects != null)&&(objects.length > 0)) {
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
     * Transform the input report to a SHEF encoded report.
     * @param report A report to transform.
     * @return The encoded SHEF report.
     * @throws TransformerException An error occurred during proccessing.
     */
    protected abstract byte[] transformReport(T report, Headers headers)
            throws TransformerException;

    /**
     * Create a new buffer containing the opening stanza of a WMO bulletin.
     * @param sequenceId Abuffer 
     * @param report
     * @return
     */
    protected StringBuilder openWMOMessage(int initialSize) {
        StringBuilder buffer = new StringBuilder(initialSize);
        startMessageLine(buffer);
        buffer.append(String.format(WMO_MSG_SEQ_FMT, getMsgSequenceNumber()));
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
     * Get the state of the archive enabled flag. This value follows
     * AppsDefaults:archive_enable. 
     * @return The archive enabled state.
     */
    private boolean isArchiveEnabled() {
        return archiveEnabled;
    }

    /**
     * Get the shef archive file directory if it exists.
     * @return 
     */
    private File getShefArchiveFileDir() {
        return shefArchiveFileDir;
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
        configureArchiveDir();
    }

    /**
     * Write an encoded SHEF observation to a specified archive directory.
     * @param shefObs The SHEF encoded data to archive.
     * @param fileName The base filename.
     */
    protected void archiveSHEFObs(String shefObs, String fileName) {
        if(isArchiveEnabled()) {
            File arcFile = getShefArchiveFileDir();
            if(arcFile != null) {
                String fName = String.format(ARCHIVE_FORMAT, fileName, getSequenceNumber());
                File outFile = new File(arcFile, fName);
                FileOutputStream fos = null;
                try {
                    fos = new FileOutputStream(outFile);
                    fos.write(shefObs.getBytes());
                    fos.flush();
                } catch(IOException ioe) {
                    logger.error("Could not archive data " + fName);
                } finally {
                    if(fos != null) {
                        try {
                            fos.close();
                        } catch(IOException ioe) {
                            logger.error("Could not close archive file " + fName);
                        }
                    }
                }
            } else {
                logger.error("Could not archive data for " + fileName);
            }
        }
    }
    
    /**
     * Get the next sequence number.
     * @return The sequence number.
     */
    private synchronized int getSequenceNumber() {
        int seq = sequenceNumber.addAndGet(1);
        sequenceNumber.compareAndSet(MAX_ARCHIVE_SEQUENCE, 1);
        return seq;
    }

    /**
     * Get the next message sequence number.
     * @return The message sequence number.
     */
    private synchronized int getMsgSequenceNumber() {
        int seq = msgSequence.addAndGet(1);
        msgSequence.compareAndSet(MAX_WMO_MSG_SEQ, 1);
        return seq;
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

    /**
     * Create, or recreate and validate the shef archive directory.
     */
    protected void configureArchiveDir() {
        archiveEnabled = AppsDefaults.getInstance().getBoolean(OPT_ARC_ENABLE,
                false);
        String arcDir = AppsDefaults.getInstance().getToken(OPT_SHEF_ARC_DIR);
        if (arcDir != null) {
            if (archiveEnabled) {
                boolean update = false;
                if (shefArchiveDir == null) {
                    update = true;
                } else {
                    update = !shefArchiveDir.equals(arcDir);
                }
                if (update) {
                    File f = null;
                    try {
                        f = new File(arcDir);
                        if (!f.exists()) {
                            if (!f.mkdirs()) {
                                f = null;
                                logger.error(String
                                        .format("Could not create SHEF archive directory [%s] - Are permissions set correctly?",
                                                arcDir));
                            }
                        } else {
                            // Ok, the arcDir exists, ensure that it is a
                            // directory!
                            if (!f.isDirectory()) {
                                logger.error(String
                                        .format("Path [%s] is not a directory, cannot create directory",
                                                f.getAbsolutePath()));
                                f = null;
                            }
                        }
                    } catch (Exception e) {
                        f = null;
                        logger.error(
                                String.format(
                                        "Could not create SHEF archive directory [%s] - Are permissions set correctly?",
                                        arcDir), e);
                    }
                    if (f != null) {
                        shefArchiveFileDir = f;
                        shefArchiveDir = arcDir;
                    }
                }
            }
        } else {
            logger.error(String.format(
                    "Apps_defaults token [%s] is not defined!",
                    OPT_SHEF_ARC_DIR));
        }
    }
}