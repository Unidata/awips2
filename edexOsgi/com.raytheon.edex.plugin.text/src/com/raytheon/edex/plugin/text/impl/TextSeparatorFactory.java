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
package com.raytheon.edex.plugin.text.impl;

import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.text.impl.separator.StdCollectiveSeparator;
import com.raytheon.edex.plugin.text.impl.separator.StdTextSeparator;
import com.raytheon.edex.plugin.text.impl.separator.TextDecoderMode;
import com.raytheon.edex.plugin.text.impl.separator.UACollectiveSeparator;
import com.raytheon.edex.plugin.text.impl.separator.WMOMessageSeparator;
import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.edex.textdb.dbapi.impl.TextDBStaticData;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2008            jkorman     Initial creation
 * Jul 10, 2009 2191       rjpeter     Finished implementation.
 * Jun 29, 2012 15154      D. Friedman Fix detection of TAF collectives.
 * Jul 25, 2012 959        jkorman     Modified order of entry for determining
 *                                     the data type (standard or collective)
 *                                     for input data.
 * Apr 01, 2014 2915       dgilling    Support re-factored TextDBStaticData.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TextSeparatorFactory {

    private static final Log logger = LogFactory
            .getLog(TextSeparatorFactory.class);

    private static final String siteId = PropertiesFactory.getInstance()
            .getEnvProperties().getEnvValue("SITENAME");

    private static Pattern TAF_PTRN = Pattern.compile("^TAF\\s*$");

    /**
     * 
     * @param messageData
     * @param traceId
     * @return
     */
    public static WMOMessageSeparator getSeparator(byte[] rawMessage,
            String traceId, TextDB textDB, Headers headers) {
        WMOMessageSeparator separator = null;
        WMOHeader wmoHeader = null;

        try {
            if (rawMessage != null) {
                wmoHeader = new WMOHeader(rawMessage, headers);
                if (wmoHeader.isValid()) {
                    int endPos = findDataEnd(rawMessage);

                    int startPos = wmoHeader.getMessageDataStart();
                    if (endPos <= startPos) {
                        endPos = rawMessage.length - 1;
                    }
                    // Only create a separator if there is data in the message,
                    // otherwise just leave the separator null.
                    if (endPos > startPos) {
                        switch (determineType(rawMessage, startPos, wmoHeader)) {
                        case UA_COLLECTIVE: {
                            separator = new UACollectiveSeparator(traceId,
                                    siteId, wmoHeader);
                            break;
                        }
                        case STD_COLLECTIVE: {
                            separator = new StdCollectiveSeparator(traceId,
                                    siteId, wmoHeader);
                            break;
                        }
                        case STD_TEXT: {
                            separator = new StdTextSeparator(traceId, siteId,
                                    wmoHeader, TextDecoderMode.STD);
                            break;
                        }
                        case WARN: {
                            separator = new StdTextSeparator(traceId, siteId,
                                    wmoHeader, TextDecoderMode.WARN);
                            break;
                        }
                        // These message types fall through.
                        case MSG_UNKNOWN:
                        case MSG_DISCARD:
                        default: {
                            // do nothing
                        }
                        }
                        if (separator != null) {
                            separator.setData(rawMessage, headers);
                        }
                    }
                }
            }
            if (separator != null) {
                logger.debug(separator.getClass().getName() + "  "
                        + separator.getProductId());
            } else {
                logger.info(traceId + "- Discarding empty message: ");
            }
        } catch (Exception e) {
            logger.error(traceId + "- Text Separation failed", e);
        }
        return separator;
    }

    /**
     * 
     * @param rawMessage
     * @return
     */
    public static int findDataEnd(byte[] rawMessage) {
        int endPos = -1;
        if (rawMessage != null) {

            // first find the ETX marker
            for (endPos = rawMessage.length - 1; endPos > 0; endPos--) {
                if (rawMessage[endPos] == 3) {
                    endPos--;
                    break;
                }
            }
            // If we get to the start of the message without finding
            // an ETX then set the position back to the message length.
            if (endPos == 0) {
                endPos = rawMessage.length - 1;
            }
            // now back up over any trailing carriage control
            for (; endPos > 0; endPos--) {
                byte b = rawMessage[endPos];
                if ((b == 10) || (b == 13)) {
                    continue;
                } else {
                    endPos++;
                    break;
                }
            }
        }
        return endPos;
    }

    private static WMOMessageType determineType(byte[] rawData, int startIndex,
            WMOHeader wmoHeader) {
        WMOMessageType msgType = null;
        String hdr = wmoHeader.getWmoHeader();
        String dataDes = WMOMessageSeparator.createDataDes(wmoHeader);
        String ispanId = WMOMessageSeparator.createIspanId(wmoHeader);

        msgType = null; // WMOMessageType.STD_TEXT;
        String startOfMessage = new String(rawData, startIndex,
                (rawData.length < startIndex + 50 ? rawData.length - startIndex
                        : 50));
        if (startOfMessage.length() > 10) {
            // check for binary data types
            if (startOfMessage.indexOf("GRIB") >= 0) {
                msgType = WMOMessageType.MSG_DISCARD;
            }

            if (msgType == null) {
                String stdAfosId = null;

                String firstLine = WMOMessageSeparator.getLine(rawData,
                        startIndex);
                int firstLineLen = firstLine.length();

                // Maintain this order of entry so that Standard Text products
                // are checked before collectives.
                if ((stdAfosId = TextDBStaticData.getProductId(ispanId)) != null) {
                    msgType = WMOMessageType.STD_TEXT;
                } else if (TextDBStaticData.matchStdCollective(dataDes) != null) {
                    msgType = WMOMessageType.STD_COLLECTIVE;
                } else if (TextDBStaticData.matchUACollective(dataDes) != null) {
                    msgType = WMOMessageType.UA_COLLECTIVE;
                }

                // dataDes/ispanId were not mapped, check hard coded
                if (msgType == null) {
                    if (!hdr.startsWith("SXUS70")
                            && !hdr.startsWith("FRUS45")
                            && (firstLineLen > 6 || TAF_PTRN.matcher(firstLine)
                                    .matches())
                            && (hdr.startsWith("SA") || hdr.startsWith("SP")
                                    || hdr.startsWith("FR") || hdr
                                        .startsWith("FT"))) {
                        msgType = WMOMessageType.STD_COLLECTIVE;
                    } else if (!hdr.startsWith("UAXX")
                            && firstLineLen > 6
                            && (hdr.startsWith("UA") || hdr.startsWith("UF")
                                    || hdr.startsWith("UI")
                                    || hdr.startsWith("UM")
                                    || hdr.startsWith("UP") || hdr
                                        .startsWith("US"))) {
                        msgType = WMOMessageType.UA_COLLECTIVE;
                    } else if (ispanId.startsWith("W")
                            || ("NOUS71KNCF".equals(ispanId)
                                    || "NTUS96KNCF".equals(ispanId) || "NTUS98KNCF"
                                        .equals(ispanId))) {
                        msgType = WMOMessageType.WARN;
                    } else {
                        msgType = WMOMessageType.STD_TEXT;
                    }
                }

                if (WMOMessageType.STD_TEXT.equals(msgType)
                        && stdAfosId != null) {
                    if (TextDBStaticData.isExcluded(stdAfosId)) {
                        logger.debug("NCF_ENTRY " + ispanId + " is skipped");
                    }
                }
            }
        }

        if (msgType == null) {
            msgType = WMOMessageType.MSG_UNKNOWN;
        }

        return msgType;
    }
}
