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
package com.raytheon.edex.plugin.text;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.text.dao.AfosToAwipsDao;
import com.raytheon.edex.plugin.text.impl.TextSeparatorFactory;
import com.raytheon.edex.plugin.text.impl.separator.WMOMessageSeparator;
import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.edex.textdb.dbapi.impl.WMOReportData;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.AFOSProductId;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Decoder implementation for text products
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 11, 2008             jkorman     Initial creation
 * Jul 10, 2009 2191        rjpeter     Finished implementation.
 * Apr 14, 2010 4734        mhuang      Corrected StdTextProduct import
 *                                      dependency
 * May 28, 2010 2187        cjeanbap    Added StdTextProductFactory
 *                                      functionality.
 * Aug 26, 2010 2187        cjeanbap    Renamed operationalMode for
 *                                      consistency.
 * Dec 13, 2010 5805        cjeanbap    Parse Report to get AFOS Product Id
 * Jul 16, 2013 16323       D. Friedman Use concurrent map
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Mar 14, 2014 2652        skorolev    Changed logging for skipped headers.
 * </pre>
 * 
 * @author
 * @version 1
 */

public class TextDecoder extends AbstractDecoder {
    private static final long MILLIS_PER_HOUR = 60 * 60 * 1000;

    private static final long NOT_FOUND_LOG_PERIOD = 24 * MILLIS_PER_HOUR;

    private static final long MSG_HDR_LOG_PERIOD = 10 * 60 * 1000;

    private static boolean moveBadTxt = Boolean.parseBoolean(PropertiesFactory
            .getInstance().getEnvProperties()
            .getEnvValue("FXADEBUGSAVEBADTEXTFILES"));

    private static String badTxtDir = PropertiesFactory.getInstance()
            .getEnvProperties().getEnvValue("DEFAULTDATADIR")
            + "badTxt";

    private final Log logger = LogFactory.getLog(getClass());

    private String pluginName;

    private String decoderMode;

    private String fileName;

    private final TextDB textdb = new TextDB();

    // keeps track of the headers that have been logged as not mapped and the
    // time it was logged, so that each one is only logged once a day
    private final Map<String, Long> notMappedHeaders = new ConcurrentHashMap<String, Long>();

    private long msgHdrLogTime = 0;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public TextDecoder() throws DecoderException {
    }

    /**
     * 
     * 
     * @see com.raytheon.edex.plugin.IMessageDecoder#decode()
     */
    public PluginDataObject[] writeTextProduct(StdTextProduct textProduct)
            throws DecoderException {
        PluginDataObject[] returnObjects = null;
        TextRecord pdo = null;
        String traceId = null;

        if (textProduct != null) {
            traceId = UUID.randomUUID().toString();

            try {
                boolean success = textdb.writeProduct(textProduct);

                if (success) {
                    pdo = new TextRecord();
                    pdo.setProductId(textProduct.getCccid()
                            + textProduct.getNnnid() + textProduct.getXxxid());
                    DataTime dt = new DataTime(
                            TimeTools.newCalendar(textProduct.getRefTime()));
                    pdo.setDataTime(dt);
                } else {
                    // throw new Exception("product already exists");
                }
            } catch (Exception e) {
                logger.error("Error writing product", e);
                // save off or move to backup queue
            } // catch
        }

        if (pdo != null) {
            pdo.setTraceId(traceId);
            try {
                pdo.constructDataURI();
            } catch (PluginException e) {
                logger.error("Unable to construct dataURI", e);
            }
            logger.debug(traceId + " - " + pdo);

            returnObjects = new TextRecord[1];
            returnObjects[0] = pdo;
        } else {
            returnObjects = new TextRecord[0];
        }

        return returnObjects;
    }

    /**
     * 
     * 
     * @see com.raytheon.edex.plugin.IMessageDecoder#decode()
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        TextRecord[] returnObjects = null;

        boolean saveFile = false;

        if ((data != null) && (data.length > 0)) {

            String traceId = null;
            if (fileName != null) {
                traceId = fileName;
            } else {
                traceId = UUID.randomUUID().toString();
            }

            WMOMessageSeparator separator = TextSeparatorFactory.getSeparator(
                    data, traceId, textdb, headers);

            if (separator != null) {
                List<TextRecord> reports = new ArrayList<TextRecord>();
                WMOHeader msgHdr = separator.getWmoHeader();
                AfosToAwipsDao dao = new AfosToAwipsDao();

                int stored = 0;

                while (separator.hasNext()) {

                    TextRecord pdo = null;

                    WMOReportData rptData = separator.next();
                    boolean operationalMode = true;

                    if (rptData != null) {
                        if (WMOMessageSeparator.NOAFOSPIL.equals(rptData
                                .getAfosProdId())) {
                            // sanity check afos2awips
                            WMOHeader rptHdr = rptData.getWmoHeader();
                            boolean notMapped = true;

                            try {
                                AfosWmoIdDataContainer afosIdContainer = dao
                                        .lookupAfosId(rptHdr.getTtaaii(),
                                                rptHdr.getCccc());

                                if (afosIdContainer != null) {
                                    List<AfosToAwips> afosIdList = afosIdContainer
                                            .getIdList();
                                    if ((afosIdList != null)
                                            && (afosIdList.size() > 0)) {
                                        StringTokenizer st = new StringTokenizer(
                                                rptData.getReportData(), "\n");
                                        String potentialId = st.nextToken();
                                        String patternStr = "\\s+";
                                        String replaceStr = "";
                                        Pattern pattern = Pattern
                                                .compile(patternStr);
                                        Matcher matcher = pattern
                                                .matcher(potentialId);

                                        potentialId = matcher
                                                .replaceAll(replaceStr);

                                        for (AfosToAwips afosId : afosIdList) {
                                            if (afosId != null) {
                                                AFOSProductId id = new AFOSProductId(
                                                        afosId.getAfosid());

                                                if (id.toString().endsWith(
                                                        potentialId)) {
                                                    rptData.setAfosProdId(id);
                                                    notMapped = false;
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            } catch (DataAccessLayerException e) {

                            }

                            if (notMapped) {
                                String key = rptHdr.getTtaaii()
                                        + rptHdr.getCccc();
                                Long time = notMappedHeaders.get(key);
                                long curTime = System.currentTimeMillis();
                                if ((time == null)
                                        || ((curTime - time) > NOT_FOUND_LOG_PERIOD)) {
                                    StringBuilder msg = new StringBuilder(200);
                                    msg.append("Could not determine AFOS id for wmo header: [");
                                    msg.append(rptHdr.toString());
                                    msg.append("]");

                                    if ((msgHdr != null)
                                            && !msgHdr.equals(rptHdr)) {
                                        msg.append(" incoming message header [");
                                        msg.append(msgHdr.toString());
                                        msg.append("]");
                                    }
                                    if ((curTime - msgHdrLogTime) > MSG_HDR_LOG_PERIOD) {
                                        msg.append("\nMsg for a given header will only be logged once in a "
                                                + (NOT_FOUND_LOG_PERIOD / MILLIS_PER_HOUR)
                                                + " hour period");
                                        msgHdrLogTime = curTime;
                                    }

                                    logger.info(msg.toString());
                                    notMappedHeaders.put(key, curTime);
                                }
                            }
                            // saveFile = moveBadTxt;
                        }

                        try {
                            long writeTime = textdb.writeProduct(rptData,
                                    operationalMode, headers);

                            if (writeTime != Long.MIN_VALUE) {
                                pdo = new TextRecord();
                                pdo.setProductId(rptData.getAfosProdId()
                                        .toString());
                                DataTime dt = new DataTime(
                                        TimeTools.newCalendar(writeTime));
                                pdo.setDataTime(dt);
                                stored++;
                            } else {
                                // throw new
                                // Exception("product already exists");
                            }
                        } catch (Exception e) {
                            logger.error("Error writing product", e);
                            // save off or move to backup queue
                        } // catch
                    } else {
                        logger.debug(traceId + " - " + "Contained no reports");
                    }

                    if (pdo != null) {
                        pdo.setTraceId(traceId);
                        try {
                            pdo.constructDataURI();
                        } catch (PluginException e) {
                            logger.error("Unable to construct dataURI", e);
                            continue;
                        }
                        logger.debug(traceId + " - " + pdo);
                        reports.add(pdo);
                    }
                }

                if (separator.getMessagesSkipped() > 0) {
                    StringBuilder msg = new StringBuilder();
                    msg.append("Message failed parsing for WMO message header: [");
                    msg.append(msgHdr.toString());
                    msg.append("], products stored [");
                    msg.append(stored);
                    msg.append("/");
                    msg.append(stored + separator.getMessagesSkipped());
                    msg.append("]");
                    Map<WMOHeader, String> skippedSubHeaders = separator
                            .getSubHeadersSkipped();

                    if (skippedSubHeaders.size() > 0) {
                        msg.append("\nSkipped WMO Sub Headers:");
                        for (WMOHeader header : skippedSubHeaders.keySet()) {
                            if (header.isValid()) {
                                msg.append("\n[");
                                msg.append(header.getTtaaii());
                                msg.append(" ");
                                msg.append(header.getCccc());
                                msg.append("] - ");
                                msg.append(skippedSubHeaders.get(header));
                            } else {
                                msg.append("\n[");
                                if (header.getOriginalMessage().length() > 11) {
                                    msg.append(header.getOriginalMessage()
                                            .substring(0, 11));
                                } else {
                                    msg.append(header.getOriginalMessage());
                                }
                                msg.append("]");
                            }
                        }

                        logger.info(msg.toString());
                        saveFile = moveBadTxt;
                    }
                }

                if (saveFile) {
                    OutputStream os = null;

                    try {
                        File dir = new File(badTxtDir);
                        if (!dir.exists()) {
                            dir.mkdirs();
                        }

                        File destFile = null;
                        if (fileName != null) {
                            destFile = new File(dir, fileName);
                        } else {
                            destFile = new File(badTxtDir,
                                    msgHdr.getWmoHeader());
                        }

                        os = new FileOutputStream(destFile);
                        os.write(data);
                    } catch (IOException e) {
                        logger.error("Failed to save msg to bad txt", e);
                    } finally {
                        if (os != null) {
                            try {
                                os.close();
                            } catch (IOException e) {
                                logger.info("Failed to close FileOutputStream",
                                        e);
                            }
                        }
                    }
                }

                returnObjects = new TextRecord[reports.size()];
                reports.toArray(returnObjects);
            } else {
                returnObjects = new TextRecord[0];
            }
        } else {
            returnObjects = new TextRecord[0];
        }
        return returnObjects;
    }

    /**
     * 
     * @param data
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decodeFile(File inputFile, Headers headers)
            throws DecoderException {
        byte[] fileData = null;
        InputStream is = null;
        try {
            try {
                is = new FileInputStream(inputFile);

                fileData = new byte[(int) inputFile.length()];
                int bytesRead = is.read(fileData);
                // If we didn't or couldn't read all the data, signal the
                // fact by setting the data to null;
                if (bytesRead != fileData.length) {
                    fileData = null;
                }
                fileName = inputFile.getName();
            } catch (IOException ioe) {
                logger.error("Error reading input file " + inputFile.getName(),
                        ioe);
                fileData = null;
            }
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                    logger.error("Could not close input file "
                            + inputFile.getName());
                }
            }
        }
        return decode(fileData, headers);
    }

    /**
     * 
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * 
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * @return the decoderMode
     */
    public String getDecoderMode() {
        return decoderMode;
    }

    /**
     * @param decoderMode
     *            the decoderMode to set
     */
    public void setDecoderMode(String decoderMode) {
        this.decoderMode = decoderMode;
    }

    public String[] transformToProductIds(PluginDataObject[] pdos) {
        TextRecord[] trs = (TextRecord[]) pdos;
        String[] rval = new String[pdos.length];
        for (int i = 0; i < trs.length; i++) {
            rval[i] = trs[i].getProductId();
        }
        return rval;
    }

    public String transformToSimpleString(PluginDataObject pdo) {
        TextRecord tr = (TextRecord) pdo;
        return tr.getProductId() + "_"
                + tr.getDataTime().getRefTime().getTime();
    }

    public Iterator<?> separator(PluginDataObject[] pdo) {
        if (pdo == null) {
            pdo = new PluginDataObject[0];
        }
        return Arrays.asList(pdo).iterator();
    }
}
