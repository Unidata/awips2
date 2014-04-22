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
package com.raytheon.edex.plugin.redbook.decoder;

import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.redbook.RedbookRecord;
import com.raytheon.uf.common.dataplugin.redbook.blocks.ProductIdBlock;
import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlock;
import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlockBuilder;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * The Redbook parser accepts a potential Redbook record and attempts to decode
 * certain identifying data to determine the the data is indeed valid. If the
 * data is determined valid, the date time and product identification
 * information is extracted and used to populate a new RedbookRecord.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2008 1131       jkorman     Initial implementation.
 * May 29, 2008 1131       jkorman     Added traceId, implemented in logger.
 * Oct 22, 2010 6424       kshrestha   Added fcsttime
 * May 16, 2011 8296       mhuang      fixed fcsttime problem
 * Apr 29, 2013 1958       bgonzale    Refactored to improve performance.
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class RedbookParser {

    private static final RedbookBlockBuilder blockBuilder = new RedbookBlockBuilder();

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(RedbookParser.class);

    // private Calendar issueDate = null;

    private List<RedbookBlock> redbookDocument;

    private RedbookRecord rRecord;

    private RedbookFcstMap redbookFcstMap = RedbookFcstMap.getInstance();

    /**
     * 
     * @param traceId
     * @param data
     * @param hdr
     */
    public RedbookParser(String traceId, byte[] data, WMOHeader hdr) {
        rRecord = internalParse(traceId, data, hdr);
        if (rRecord != null) {

            rRecord.setWmoCCCCdt(hdr.getWmoHeader().substring(6));
            rRecord.setWmoTTAAii(hdr.getWmoHeader().substring(0, 6));

            int day = hdr.getDay();
            int hour = hdr.getHour();
            int min = hdr.getMinute();

            int fcstTime = rRecord.getFcstHours() * 3600;

            Calendar wmoTime = TimeTools.copy(rRecord.getTimeObs());

            if (day - wmoTime.get(Calendar.DAY_OF_MONTH) < 0) {
                wmoTime.add(Calendar.MONTH, 1);
            }
            wmoTime.set(Calendar.DAY_OF_MONTH, day);
            wmoTime.set(Calendar.HOUR_OF_DAY, hour);
            wmoTime.set(Calendar.MINUTE, min);

            long binnedTime = getBinnedTime(traceId, hdr,
                    wmoTime.getTimeInMillis());

            DataTime dt = null;

            if (fcstTime > 0)
                dt = new DataTime(new Date(binnedTime), fcstTime);
            else
                dt = new DataTime(new Date(binnedTime));

            rRecord.setDataTime(dt);

            String cor = hdr.getBBBIndicator();
            if ((cor != null) && (cor.indexOf("CC") >= 0)) {
                rRecord.setCorIndicator(cor);
            }

        }
    }

    /**
     * Return the last decoded Redbook record from this parser.
     * 
     * @return A RedbookRecord.
     */
    public RedbookRecord getDecodedRecord() {
        return rRecord;
    }

    /**
     * 
     * @param hdr
     * @param separator
     */
    private RedbookRecord internalParse(String traceId, byte[] redbookMsg,
            WMOHeader hdr) {

        RedbookRecord record = null;

        ByteBuffer dataBuf = ByteBuffer.wrap(redbookMsg);

        ProductIdBlock productId = null;

        redbookDocument = new ArrayList<RedbookBlock>();

        while (dataBuf.hasRemaining()) {

            RedbookBlock currBlock = null;

            try {
                currBlock = blockBuilder.getBlock(dataBuf);

                redbookDocument.add(currBlock);

                if (currBlock.isEndBlock()) {
                    int endPos = dataBuf.position();
                    record = new RedbookRecord();
                    byte[] redBookData = new byte[endPos];
                    System.arraycopy(redbookMsg, 0, redBookData, 0, endPos);
                    record.setRedBookData(redBookData);
                    break;
                } else if (currBlock.isUpperAirPlot()) {
                    /*
                     * Upper air plots are malformed and require special
                     * handling to extract the data. If we get this far, it is
                     * enough.
                     */
                    if (hdr.getTtaaii().substring(0, 4).equals("PYMA")) {
                        record = new RedbookRecord();
                        record.setRedBookData(redbookMsg);
                        break;
                    }
                }

                if (currBlock.isProductId()) {
                    productId = (ProductIdBlock) currBlock;
                }

            } catch (BufferUnderflowException bue) {
                logger.error(traceId + "- Out of data", bue);
                return record;
            } catch (Exception e) {
                logger.error(traceId + "- Error in parser", e);
                return record;
            }
        }

        if (record != null) {
            if (productId != null) {
                record.setTimeObs(productId.getProductFileTime());
                record.setRetentionHours(productId.getRetentionHours());

                record.setFileId(productId.getFileIndicator());
                record.setProductId(productId.getProductId());
                record.setOriginatorId(productId.getOriginatorId());

                /* record.setFcstHours(id.getFcstHours()); */
                record.setFcstHours(getForecastTime(traceId, hdr));

                record.setTraceId(traceId);
            }

        } else {
            logger.info(traceId + "- No EndOfProductBlock found");
        }

        return record;
    }

    public int getForecastTime(String traceId, WMOHeader hdr) {
        RedbookFcstMap.MapFcstHr xmlInfo = redbookFcstMap.get(hdr.getTtaaii());

        if (xmlInfo != null && xmlInfo.fcstHR != null
                && !xmlInfo.fcstHR.isEmpty()) {
            return (Integer.parseInt(xmlInfo.fcstHR));
        }
        return 0;
    }

    public long getBinnedTime(String traceId, WMOHeader hdr, long timeMillis) {
        try {
            long period = 43200 * 1000; // default period is 12 hours
            long offset = 0;
            RedbookFcstMap.MapFcstHr xmlInfo = redbookFcstMap.get(hdr
                    .getTtaaii());

            if (xmlInfo != null) {
                /*
                 * Does not support AWIPS 1 semantics of "period < 0 means apply
                 * offset first".
                 */
                if (xmlInfo.binPeriod != null && xmlInfo.binPeriod > 0)
                    period = (long) xmlInfo.binPeriod * 1000;
                if (xmlInfo.binOffset != null)
                    offset = (long) xmlInfo.binOffset * 1000;
            }

            timeMillis = (timeMillis / period) * period + offset;
        } catch (Exception e) {
            logger.error(traceId + " - Error in parser - mappingFCST: ", e);
        }
        return timeMillis;
    }

}