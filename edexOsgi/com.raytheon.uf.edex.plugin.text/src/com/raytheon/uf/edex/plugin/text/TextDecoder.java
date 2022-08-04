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
package com.raytheon.uf.edex.plugin.text;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.AFOSProductId;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.plugin.text.db.TextDB;
import com.raytheon.uf.edex.plugin.text.impl.TextSeparatorFactory;
import com.raytheon.uf.edex.plugin.text.impl.WMOReportData;
import com.raytheon.uf.edex.plugin.text.impl.separator.WMOMessageSeparator;

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
 * May 12, 2014 2536        bclement    added createTextRecord(), removed deprecated code
 * Jul 10, 2014 2914        garmendariz Remove EnvProperties
 * Dec 09, 2015 5166        kbisanz     Update logging to use SLF4J.
 * Mar  4, 2016 4716        rferrel     Add AWIPS products to the TextRecords.
 *                                      {@link #decodeFile(File, Headers)} now uses try with resources.
 * Aug  9, 2016 5801        tgurney     Use AfosToAwipsLookup
 * Sep 23, 2021 8608        mapeters    Handle PDO.traceId changes
 * </pre>
 *
 * @author jkorman
 */
public class TextDecoder {

    private static final String textToStageNotificationRoute = "jms-durable:queue:textToStageNotification";

    private static final long MILLIS_PER_HOUR = 60 * 60 * 1000;

    private static final long NOT_FOUND_LOG_PERIOD = 24 * MILLIS_PER_HOUR;

    private static final long MSG_HDR_LOG_PERIOD = 10 * 60 * 1000;

    private static boolean moveBadTxt = Boolean
            .parseBoolean(System.getProperty("fxadebugsavedbadtextfiles"));

    private static String badTxtDir = EDEXUtil.getEdexData() + File.separator
            + "badTxt";

    private static final Logger logger = LoggerFactory
            .getLogger(TextDecoder.class);

    private String pluginName;

    private String decoderMode;

    private String fileName;

    private final TextDB textdb = new TextDB();

    // keeps track of the headers that have been logged as not mapped and the
    // time it was logged, so that each one is only logged once a day
    private final Map<String, Long> notMappedHeaders = new ConcurrentHashMap<>();

    private long msgHdrLogTime = 0;

    /**
     * Constructor
     *
     * @throws DecoderException
     */
    public TextDecoder() throws DecoderException {
    }

    /**
     * @see com.raytheon.edex.uf.plugin.IMessageDecoder#decode()
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
                    pdo = createTextRecord(textProduct);
                } else {
                    // throw new Exception("product already exists");
                }
            } catch (Exception e) {
                logger.error("Error writing product", e);
                // save off or move to backup queue
            } // catch
        }

        if (pdo != null) {
            pdo.setSourceTraceId(traceId);
            logger.debug(traceId + " - " + pdo);

            returnObjects = new TextRecord[1];
            returnObjects[0] = pdo;
        } else {
            returnObjects = new TextRecord[0];
        }

        return returnObjects;
    }

    /**
     * Construct a new text record from the Standard Text Product.
     *
     * @param textProduct
     * @return pdo
     */
    private static TextRecord createTextRecord(StdTextProduct textProduct) {

        /*
         * This assumes textProduct's site is 4 characters; cccid, nnnid and
         * xxxid are 3 and the refTime is not null.
         */
        TextRecord pdo = new TextRecord();
        StringBuilder sb = new StringBuilder(10);
        sb.append(textProduct.getCccid());
        sb.append(textProduct.getNnnid());
        sb.append(textProduct.getXxxid());
        pdo.setProductId(sb.toString());
        sb.replace(0, 3, textProduct.getSite());
        pdo.setAwipsProductId(sb.toString());
        DataTime dt = new DataTime(new Date(textProduct.getRefTime()));
        pdo.setDataTime(dt);
        return pdo;
    }

    /**
     * This generates a text record with Awips and Afos product id set to the
     * values in the WMO Report Data.
     *
     * @param rpdData
     * @param refTime
     * @return pdo
     */
    private static TextRecord createTextRecord(WMOReportData rpdData,
            long refTime) {
        TextRecord pdo = new TextRecord();
        pdo.setProductId(rpdData.getAfosProdId().toString());
        pdo.setAwipsProductId(rpdData.getAwipsProdId());
        pdo.setDataTime(new DataTime(new Date(refTime)));
        return pdo;
    }

    /**
     * @see com.raytheon.edex.plugin.IMessageDecoder#decode()
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        TextRecord[] returnObjects = null;

        boolean saveFile = false;

        if (data != null && data.length > 0) {

            String traceId = null;
            if (fileName != null) {
                traceId = fileName;
            } else {
                traceId = UUID.randomUUID().toString();
            }

            WMOMessageSeparator separator = TextSeparatorFactory
                    .getSeparator(data, traceId, textdb, headers);

            if (separator != null) {
                List<TextRecord> reports = new ArrayList<>();
                WMOHeader msgHdr = separator.getWmoHeader();

                int stored = 0;

                while (separator.hasNext()) {

                    TextRecord pdo = null;

                    WMOReportData rptData = separator.next();
                    boolean operationalMode = true;

                    if (rptData != null) {
                        if (WMOMessageSeparator.NOAFOSPIL
                                .equals(rptData.getAfosProdId())) {
                            // sanity check afos2awips
                            WMOHeader rptHdr = rptData.getWmoHeader();
                            boolean notMapped = true;

                            AfosWmoIdDataContainer afosIdContainer = AfosToAwipsLookup
                                    .lookupAfosId(rptHdr.getTtaaii(),
                                            rptHdr.getCccc());

                            if (afosIdContainer != null) {
                                List<AfosToAwips> afosIdList = afosIdContainer
                                        .getIdList();
                                if (afosIdList != null
                                        && !afosIdList.isEmpty()) {
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

                                            if (id.toString()
                                                    .endsWith(potentialId)) {
                                                rptData.setAfosProdId(id);
                                                notMapped = false;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }

                            if (notMapped) {
                                String key = rptHdr.getTtaaii()
                                        + rptHdr.getCccc();
                                Long time = notMappedHeaders.get(key);
                                long curTime = System.currentTimeMillis();
                                if (time == null || curTime
                                        - time > NOT_FOUND_LOG_PERIOD) {
                                    StringBuilder msg = new StringBuilder(200);
                                    msg.append(
                                            "Could not determine AFOS id for wmo header: [");
                                    msg.append(rptHdr.toString());
                                    msg.append("]");

                                    if (msgHdr != null
                                            && !msgHdr.equals(rptHdr)) {
                                        msg.append(
                                                " incoming message header [");
                                        msg.append(msgHdr.toString());
                                        msg.append("]");
                                    }
                                    if (curTime
                                            - msgHdrLogTime > MSG_HDR_LOG_PERIOD) {
                                        msg.append(
                                                "\nMsg for a given header will only be logged once in a "
                                                        + NOT_FOUND_LOG_PERIOD
                                                                / MILLIS_PER_HOUR
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
                                pdo = createTextRecord(rptData, writeTime);
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
                        pdo.setSourceTraceId(traceId);
                        logger.debug(traceId + " - " + pdo);
                        reports.add(pdo);
                    }
                }

                if (separator.getMessagesSkipped() > 0) {
                    StringBuilder msg = new StringBuilder();
                    msg.append(
                            "Message failed parsing for WMO message header: [");
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

                        try (OutputStream os = new FileOutputStream(destFile)) {
                            os.write(data);
                        }
                    } catch (IOException e) {
                        logger.error("Failed to save msg to bad txt", e);
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
        Path inPath = Paths.get(inputFile.getAbsolutePath());
        byte[] fileData = null;

        try {
            fileData = Files.readAllBytes(inPath);
        } catch (IOException ioe) {
            logger.error("Error reading input file " + inputFile.getName(),
                    ioe);
            fileData = null;
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
        String[] rval = new String[0];
        List<String> rvalList = new ArrayList<>(pdos.length * 2);

        try {
            for (PluginDataObject pdo : pdos) {
                TextRecord tr = (TextRecord) pdo;
                rvalList.add(tr.getProductId());
                rvalList.add(tr.getAwipsProductId());
            }
            rval = rvalList.toArray(rval);
        } catch (Exception e) {
            logger.error("Error transforming PDOs to Product IDs: ", e);
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

    /**
     * Wraps a string containing an id in a TextRecord.
     *
     * @param id
     *            the id to put in a TextRecord
     * @return PluginDataObject array (TextRecord) containing the id.
     */
    public PluginDataObject[] transformStringToTextRecord(String id) {

        TextRecord tr = new TextRecord();
        tr.setProductId(id);

        PluginDataObject[] pdos = new TextRecord[1];
        pdos[0] = tr;
        return pdos;
    }

    /**
     *
     * Sends an asynchronous message to the textToStageNotification queue. This
     * is basically a wrapper of the utility method that handles/logs any
     * errors.
     *
     * @param message
     *            the message to send
     */
    public static void sendTextToQueue(String message) {
        try {
            EDEXUtil.getMessageProducer()
                    .sendAsyncUri(textToStageNotificationRoute, message);
        } catch (EdexException e) {
            logger.warn("Unable to send product '" + message + "' to queue '"
                    + textToStageNotificationRoute + "'", e);
        }
    }

}
