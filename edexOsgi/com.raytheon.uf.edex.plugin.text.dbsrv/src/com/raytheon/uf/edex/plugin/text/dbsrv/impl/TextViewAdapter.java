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
package com.raytheon.uf.edex.plugin.text.dbsrv.impl;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.dbsrv.ICommandExecutor;
import com.raytheon.uf.common.dataplugin.text.dbsrv.TextDBSrvCommandTags;
import com.raytheon.uf.common.dataplugin.text.dbsrv.TextViewGetTags;
import com.raytheon.uf.common.dataplugin.text.dbsrv.TextViewTags;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.MarshalOptions;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.text.AlarmAlertUtil;
import com.raytheon.uf.edex.plugin.text.TextDecoder;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * Handles database interactions with the Text DB's standard text products
 * table.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2008 1538       jkorman     Initial creation
 * 30Sep2009    3076       MW Fegan    Notify script runner on inserts.
 * 15Feb2010    4426       MW Fegan    correct data parsing problem.
 * Apr 14, 2010 4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * 21May2010    2187       cjeanbap    Add operational mode functionality
 * 07Jul2010    2187       cjeanbap    Check operational mode for null.
 * 02Aug2010    2187       cjeanbap    Move AlarmAlertUtil.sendProductAlarmAlert() 
 *                                     outside of if-statement.
 * 28Sep2010    6338       cjeanbap    Added retrieval of current node by site.
 * --------------------------------
 * 27Apr2012     564       jkorman     Added sort to ALL times retrieval.
 * May 15, 2014 2536       bclement    moved from uf.edex.textdbsrv, added marshalToStream()
 * Jul 15, 2014 3373       bclement    jaxb manager api changes
 * Aug 22, 2014 2926       bclement    compatibility changes with new textdb service
 * Dec 09, 2015 5166       kbisanz     Update logging to use SLF4J.
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.dbsrv
 * Feb 12, 2016 4716       rferrel     {@link #processGetRequest} modified to handle AWIPS commands.
 * Aug 28, 2016 5839       rferrel     Added past version.
 * 
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TextViewAdapter implements ICommandExecutor {

    /**
     * This is the default textdb formatted time.
     * 
     * <pre>
     * Oct 31 08 10:13:15 GMT
     * </pre>
     */
    private static final String DEFAULT_TIME_FORMAT = "%1$tb %1$td %1$ty %1$tT GMT";

    private static final String FALSE = "FALSE";

    private static final String UNKNOWN_SITE = "Unable to locate site from National Category list!";

    private Logger logger = LoggerFactory.getLogger(getClass());

    private TextDB textDB;

    /**
     * 
     */
    public TextViewAdapter() {
        textDB = new TextDB();
    }

    /**
     * 
     * @return
     */
    public static final String getViewTag() {
        return "text";
    }

    /**
     * 
     */
    public void dispose() {

    }

    /**
     * 
     */
    @Override
    public Message execute(Message cmdMessage) {

        Header sHeader = cmdMessage.getHeader();

        // Get the operation code
        String op = sHeader.getProperty(TextViewTags.OP.name());

        TextDBSrvCommandTags opTag = TextDBSrvCommandTags.valueOf(op);

        if (opTag != null) {
            switch (opTag) {

            case PUT: {
                sHeader = processPutRequest(sHeader);
                break;
            }

            case GET: {
                sHeader = processGetRequest(sHeader);
                break;
            }
            case DELETE: {
                Property[] props = new Property[] { new Property(
                        CommandExecutor.STDERR, "ERROR:Command tag = ["
                                + opTag.name() + "] not implemented"), };
                sHeader.setProperties(props);
                break;
            }
            default: {
                Property[] props = new Property[] { new Property(
                        CommandExecutor.STDERR, "ERROR:Invalid command tag = ["
                                + op + "]"), };
                sHeader.setProperties(props);
                break;
            }

            }

        }

        cmdMessage.setHeader(sHeader);
        return cmdMessage;
    }

    /**
     * Processes the {@code PUT} request based on the PIL and product contained
     * in the Message Header. As a side effect, the PIL is sent to the
     * watch/warn queue. The results of the insert are returned in an updated
     * Message Header.
     * 
     * @param msgHeader
     *            the message header to process
     * 
     * @return the updated message header
     */
    private Header processPutRequest(Header msgHeader) {
        String prodId = msgHeader.getProperty(TextViewTags.PRODID.name());
        String product = msgHeader.getProperty(TextViewTags.PRODUCT.name());
        String strMode = msgHeader.getProperty(TextViewTags.OPERATIONAL.name());

        boolean operationalMode = isOperationalMode(strMode);
        long insertTime = textDB.writeProduct(prodId, product, operationalMode,
                null);
        if (operationalMode && insertTime != Long.MIN_VALUE) {
            TextDecoder.sendTextToQueue(prodId);
        }

        if (insertTime != Long.MIN_VALUE) {
            Date d = new Date();
            d.setTime(insertTime);
            AlarmAlertUtil.sendProductAlarmAlert(prodId, d, operationalMode);
            Property[] props = new Property[] { new Property(
                    CommandExecutor.STDERR, "NORMAL:Saved " + prodId), };
            msgHeader.setProperties(props);
        } else {
            Property[] props = new Property[] { new Property(
                    CommandExecutor.STDERR,
                    "NORMAL:Not Saved; duplicate product " + prodId), };
            msgHeader.setProperties(props);
        }

        return msgHeader;
    }

    /**
     * 
     * @param msgHeader
     * @return
     */
    private Header processGetRequest(Header msgHeader) {

        String op = msgHeader.getProperty(TextViewTags.SUBOP.name());

        TextViewGetTags subOp = TextViewGetTags.valueOf(op);
        boolean operationalMode = isOperationalMode(msgHeader
                .getProperty(TextViewTags.OPERATIONAL.name()));

        if (subOp != null) {
            if (TextViewGetTags.LATEST.equals(subOp)) {

                List<Long> times = new ArrayList<>();

                String fmtType = msgHeader.getProperty(TextViewTags.FORMAT
                        .name());
                String timeFmt = msgHeader.getProperty(TextViewTags.CLIENTFMT
                        .name());

                // get the latest time for one or more products.
                Property[] msgProps = msgHeader.getProperties();
                for (Property p : msgProps) {

                    if (TextViewTags.PRODID.name().equals(p.getName())) {
                        times.add(textDB.getLatestTime(p.getValue(),
                                operationalMode));
                    }
                } // for
                msgProps = new Property[times.size()];
                int pIndex = 0;
                for (Long t : times) {
                    String s = formatTime(t, fmtType, timeFmt);
                    msgProps[pIndex++] = new Property(CommandExecutor.STDOUT, s);
                }
                msgHeader.setProperties(msgProps);
            } else if (TextViewGetTags.ALL.equals(subOp)) {
                // get all times for a product
                String productId = msgHeader.getProperty(TextViewTags.PRODID
                        .name());
                String fmtType = msgHeader.getProperty(TextViewTags.FORMAT
                        .name());
                String timeFmt = msgHeader.getProperty(TextViewTags.CLIENTFMT
                        .name());

                List<Long> times = textDB.getAllTimes(productId,
                        operationalMode);
                // sort the list first...
                Collections.sort(times);

                Property[] msgProps = new Property[times.size()];
                int pIndex = 0;
                for (Long t : times) {
                    String s = formatTime(t, fmtType, timeFmt);
                    msgProps[pIndex++] = new Property(CommandExecutor.STDOUT, s);
                }
                msgHeader.setProperties(msgProps);
            } else if (TextViewGetTags.INFO.equals(subOp)
                    || TextViewGetTags.PROD.equals(subOp)
                    || TextViewGetTags.PRODXML.equals(subOp)) {
                String siteCCCNNNXX = msgHeader.getProperty(TextViewTags.SITE
                        .name());
                if (siteCCCNNNXX == null) {
                    boolean infoFlag = TextViewGetTags.INFO.equals(subOp);
                    boolean xmlFlag = TextViewGetTags.PRODXML.equals(subOp);

                    List<StdTextProduct> prods = null;
                    String afosCmd = msgHeader.getProperty(TextViewTags.AFOSCMD
                            .name());

                    if (afosCmd != null) {
                        logger.info("AFOS Command = " + afosCmd);

                        prods = textDB.executeAFOSCommand(afosCmd, null,
                                operationalMode);
                    } else {
                        String awipsCmd = msgHeader
                                .getProperty(TextViewTags.AWIPSCMD.name());
                        if (awipsCmd != null) {
                            logger.info("AWIPS Command = " + awipsCmd);
                        }
                        prods = textDB.executeAWIPSCommand(awipsCmd, null,
                                operationalMode);
                    }

                    List<Property> prodList = new ArrayList<>(prods.size());

                    if (infoFlag) {
                        String ss = "********** Product Count = "
                                + prods.size();
                        prodList.add(new Property(CommandExecutor.STDOUT, ss));
                    }

                    try {
                        for (StdTextProduct prod : prods) {
                            if (xmlFlag) {
                                ByteArrayOutputStream strm = new ByteArrayOutputStream();
                                marshalToStream(prod, strm);
                                prodList.add(new Property(
                                        CommandExecutor.STDOUT, strm.toString()));
                            } else {
                                String s = prod.getProduct();
                                if (s != null) {
                                    if (infoFlag) {
                                        String ss = "********** Product Size = "
                                                + s.length();
                                        prodList.add(new Property(
                                                CommandExecutor.STDOUT, ss));
                                    }

                                    prodList.add(new Property(
                                            CommandExecutor.STDOUT, s));
                                }
                            }
                        }
                    } catch (SerializationException e) {
                        logger.error(
                                "JAXB Exception marshaling StdTextProducts", e);
                    }

                    msgHeader.setProperties(prodList
                            .toArray(new Property[prodList.size()]));
                } else {
                    String cccNNNXXX = SiteMap.getInstance().getCCCFromXXXCode(
                            siteCCCNNNXX.toUpperCase());

                    Property[] props = new Property[1];
                    if (cccNNNXXX != null) {
                        props[0] = new Property(CommandExecutor.STDOUT,
                                cccNNNXXX);
                    } else {
                        props[0] = new Property(CommandExecutor.STDERR,
                                UNKNOWN_SITE);
                    }

                    msgHeader.setProperties(props);
                }
            } else if (TextViewGetTags.JOIN.equals(subOp)
                    || TextViewGetTags.JOINXML.equals(subOp)) {
                boolean xmlFlag = TextViewGetTags.JOINXML.equals(subOp);

                // get all times for a product
                String wmoId = msgHeader.getProperty(TextViewTags.WMOID.name());
                String site = msgHeader.getProperty(TextViewTags.SITE.name());
                String abbrId = msgHeader.getProperty(TextViewTags.NNNXXX
                        .name());
                String lastHrs = msgHeader
                        .getProperty(TextViewTags.HOUR.name());
                String hdrTime = msgHeader.getProperty(TextViewTags.HDRTIME
                        .name());
                String bbbId = msgHeader.getProperty(TextViewTags.BBB.name());
                String fullDataReadProp = msgHeader
                        .getProperty(TextViewTags.FULLREAD.name());
                boolean fullDataRead = false;

                if (fullDataReadProp != null && fullDataReadProp.length() > 0) {
                    fullDataRead = Boolean.parseBoolean(fullDataReadProp);
                }

                int intlProd = 0;
                List<StdTextProduct> prods = textDB.readAwips(wmoId, site,
                        intlProd, abbrId, lastHrs, hdrTime, null, bbbId,
                        fullDataRead, operationalMode);

                List<Property> prodList = new ArrayList<>(prods.size());

                // if not xml or last hours request, add the number of returned
                // items
                if (!xmlFlag && (lastHrs == null || lastHrs.length() == 0)) {
                    prodList.add(new Property(CommandExecutor.STDOUT, ""
                            + prods.size()));
                }

                StringBuilder header = new StringBuilder();

                try {

                    for (StdTextProduct prod : prods) {
                        if (xmlFlag) {
                            ByteArrayOutputStream strm = new ByteArrayOutputStream();
                            marshalToStream(prod, strm);
                            prodList.add(new Property(CommandExecutor.STDOUT,
                                    strm.toString()));
                        } else {
                            String cccId = prod.getCccid();
                            String nnnId = prod.getNnnid();
                            String xxxId = prod.getXxxid();
                            String b = prod.getBbbid();
                            if (b == null || b.length() == 0) {
                                b = "-";
                            }

                            header.setLength(0);
                            header.append(prod.getWmoid());
                            header.append(' ');
                            header.append(prod.getSite());
                            header.append(' ');
                            header.append(prod.getHdrtime());
                            header.append(' ');
                            header.append(b);
                            header.append(' ');
                            header.append(nnnId);
                            header.append(xxxId);
                            header.append(' ');
                            header.append(cccId);
                            header.append(nnnId);
                            header.append(xxxId);
                            prodList.add(new Property(CommandExecutor.STDOUT,
                                    header.toString()));
                        }
                    }
                } catch (SerializationException e) {
                    logger.error("JAXB Exception marshaling StdTextProducts", e);
                }

                msgHeader.setProperties(prodList.toArray(new Property[prodList
                        .size()]));
            }
        }

        return msgHeader;
    }

    /**
     * Marshals product to XML in the provided stream. Does not format the
     * output.
     * 
     * @param prod
     * @param stream
     * @throws SerializationException
     */
    private void marshalToStream(StdTextProduct prod, OutputStream stream)
            throws SerializationException {
        JAXBManager jaxbManager;
        try {
            jaxbManager = SerializationUtil.getJaxbManager();
        } catch (JAXBException e) {
            throw new SerializationException("Unable to create JAXB manager", e);
        }
        jaxbManager.marshalToStream(prod, stream, MarshalOptions.UNFORMATTED);
    }

    /**
     * Convert the database product time into the client requested format.
     * 
     * @param prodTime
     *            The product time as a Long.
     * @param timeFormat
     *            Requested format.
     * @return
     */
    private String formatTime(Long prodTime, String formatType,
            String timeFormat) {
        String formattedTime = null;
        TextViewGetTags fmt = TextViewGetTags.valueOf(formatType);

        switch (fmt) {

        case UNIX: {
            formattedTime = "" + prodTime / 1000;
            break;
        }
        case DEFAULT: {
            Calendar t = TimeUtil.newGmtCalendar(new Date(prodTime));
            formattedTime = String.format(DEFAULT_TIME_FORMAT, t);
            break;
        }
        case CLIENT: {
            Calendar t = TimeUtil.newGmtCalendar(new Date(prodTime));
            if (timeFormat != null) {
                formattedTime = String.format(timeFormat, t);
            } else {
                formattedTime = "0";
                logger.error("Client time format missing");
            }

            break;
        }
        case RAW: {
            formattedTime = "" + prodTime;
            break;
        }
        default: {
            logger.error("Invalid time format");
            break;
        }

        }

        return formattedTime;
    }

    private boolean isOperationalMode(String operationalName) {
        boolean result = true;

        if (operationalName == null || "".equals(operationalName)) {
            result = true;
        }

        if (FALSE.equalsIgnoreCase(operationalName)) {
            result = false;
        }

        return result;
    }
}
