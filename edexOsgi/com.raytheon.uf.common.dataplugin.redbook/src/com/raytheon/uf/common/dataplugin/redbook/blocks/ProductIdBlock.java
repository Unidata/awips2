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
package com.raytheon.uf.common.dataplugin.redbook.blocks;

import java.nio.ByteBuffer;
import java.util.Calendar;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2008 1131       jkorman     Initial implementation.
 * Apr 29, 2013 1958       bgonzale    Added class RedbookBlockHeader, and
 *                                     nested Factory class.
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ProductIdBlock extends RedbookBlock {

    private static final String TM_FMT = "%04d%02d%02d%02d%02d";

    private static final int BLOCK_LEN = 34;

    private static final int ORIG_ID_SIZE = 4;

    private static final int PRODM_ID_SIZE = 9;

    private static final int PRODC_ID_SIZE = 6;

    private String originatorId;

    private String productId;

    private Integer retentionHours;

    private Calendar productFileTime;

    private String prodFileTime;

    private Integer fileIndicator;

    private Integer fcstHours;

    public static class Factory implements RedbookBlockFactory {
        @Override
        public RedbookBlock createBlock(RedbookBlockHeader header,
                ByteBuffer data) {
            return new ProductIdBlock(header, data);
        }
    }

    /**
     * 
     * @param separator
     */
    public ProductIdBlock(RedbookBlockHeader header, ByteBuffer data) {
        super(header, data);

        int blockLen = (hasChkSum()) ? BLOCK_LEN : BLOCK_LEN - 2;
        if (data.remaining() >= blockLen) {
            char[] pId = new char[ORIG_ID_SIZE];
            for (int i = 0; i < pId.length; i++) {
                char c = (char) (data.get() & 0xFF);
                if (c > 0) {
                    pId[i] = c;
                } else {
                    pId[i] = '.';
                }
            }
            // originatorId = new String(pId);
            // Until I figure out what's actually being sent here.
            originatorId = null;

            // classification -- empty read for now!
            data.get();
            retentionHours = (data.get() & 0xFF);
            fileIndicator = (data.get() & 0xFF);

            StringBuilder sb = new StringBuilder();
            pId = new char[PRODM_ID_SIZE];
            for (int i = 0; i < pId.length; i++) {
                char c = (char) (data.get() & 0xFF);
                if (c > 0) {
                    pId[i] = c;
                } else {
                    pId[i] = '.';
                }
            }
            sb.append(pId);

            int year = (data.getShort() & 0xFFFF);
            int month = (data.get() & 0xFF);
            int day = (data.get() & 0xFF);
            int hour = (data.get() & 0xFF);
            int minute = (data.get() & 0xFF);

            prodFileTime = String
                    .format(TM_FMT, year, month, day, hour, minute);

            Calendar t = TimeUtil.newGmtCalendar();
            t.set(Calendar.YEAR, year);
            t.set(Calendar.MONTH, month);
            t.set(Calendar.DAY_OF_MONTH, day);
            t.set(Calendar.HOUR_OF_DAY, hour);
            t.set(Calendar.MINUTE, minute);
            t.set(Calendar.SECOND, 0);
            t.set(Calendar.MILLISECOND, 0);

            productFileTime = t;

            pId = new char[PRODC_ID_SIZE];
            for (int i = 0; i < pId.length; i++) {
                char c = (char) (data.get() & 0xFF);
                if (c > 0) {
                    pId[i] = c;
                } else {
                    pId[i] = '.';
                }
            }
            sb.append(pId);
            productId = sb.toString();
            parseFcstHours();

            // Just read over the checksum for now.
            if (hasChkSum()) {
                data.getShort();
            }
        }
    }

    private void parseFcstHours() {
        if ((productId != null) && (productId.length() > 7)) {

            Integer fcstHour = null;
            try {
                fcstHour = new Integer(productId.substring(4, 7));
            } catch (NumberFormatException nfe) {
                // nothing
            }
            if (fcstHour != null) {
                if (fcstHour < 300) {
                    fcstHours = fcstHour;
                } else if (fcstHour < 400) {

                } else if (fcstHour < 500) {

                } else if (fcstHour == 999) {
                    fcstHours = null;
                }
            }
        }
    }

    /**
     * 
     */
    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb = super.toString(sb);
        sb.append(":");
        sb.append(productId);
        sb.append(":");
        sb.append(prodFileTime);
        return sb;
    }

    /**
     * @return the originatorId
     */
    public String getOriginatorId() {
        return originatorId;
    }

    /**
     * @param originatorId
     *            the originatorId to set
     */
    public void setOriginatorId(String originatorId) {
        this.originatorId = originatorId;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * @return the retentionHours
     */
    public Integer getRetentionHours() {
        return retentionHours;
    }

    /**
     * @param retentionHours
     *            the retentionHours to set
     */
    public void setRetentionHours(Integer retentionHours) {
        this.retentionHours = retentionHours;
    }

    /**
     * @return the productFileTime
     */
    public Calendar getProductFileTime() {
        return productFileTime;
    }

    /**
     * @param productFileTime
     *            the productFileTime to set
     */
    public void setProductFileTime(Calendar productFileTime) {
        this.productFileTime = productFileTime;
    }

    /**
     * @return the fileIndicator
     */
    public Integer getFileIndicator() {
        return fileIndicator;
    }

    /**
     * @param fileIndicator
     *            the fileIndicator to set
     */
    public void setFileIndicator(Integer fileIndicator) {
        this.fileIndicator = fileIndicator;
    }

    /**
     * @return the fcstHours
     */
    public Integer getFcstHours() {
        return fcstHours;
    }

    /**
     * @param fcstHours
     *            the fcstHours to set
     */
    public void setFcstHours(Integer fcstHours) {
        this.fcstHours = fcstHours;
    }

}
