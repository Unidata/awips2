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

public class Block_004_016 extends RedbookBlock {

    private int piSet;

    private int coordinateFlag;

    private int scaleFactor;

    private int areaCode;

    private int labelCode;

    private int refM1coord;

    private int refN1coord;

    private int refM2coord;

    private int refN2coord;

    private int refM3coord;

    private int refN3coord;

    private int vtMonth;

    private int vtDay;

    private int vtHour;

    private int vtMinute;

    private int evtMonth;

    private int evtDay;

    private int evtHour;

    private int evtMinute;

    public static class Factory implements RedbookBlockFactory {
        @Override
        public RedbookBlock createBlock(RedbookBlockHeader header,
                ByteBuffer data) {
            return new Block_004_016(header, data);
        }
    }

    /**
     * 
     * @param header
     * @param separator
     */
    public Block_004_016(RedbookBlockHeader header, ByteBuffer data) {
        super(header, data);
        populate(data);
        if (hasChkSum()) {
            data.getShort();
        }
    }

    private void populate(ByteBuffer data) {

        piSet = (data.get() & 0xFF);
        coordinateFlag = (data.get() & 0xFF);
        scaleFactor = (data.getShort() & 0xFFFF);
        areaCode = (data.get() & 0xFF);
        labelCode = (data.get() & 0xFF);

        refM1coord = (data.getShort() & 0xFFFF);
        refN1coord = (data.getShort() & 0xFFFF);

        refM2coord = (data.getShort() & 0xFFFF);
        refN2coord = (data.getShort() & 0xFFFF);

        refM3coord = (data.getShort() & 0xFFFF);
        refN3coord = (data.getShort() & 0xFFFF);

        vtMonth = (data.get() & 0xFF);
        vtDay = (data.get() & 0xFF);
        vtHour = (data.get() & 0xFF);
        vtMinute = (data.get() & 0xFF);

        evtMonth = (data.get() & 0xFF);
        evtDay = (data.get() & 0xFF);
        evtHour = (data.get() & 0xFF);
        evtMinute = (data.get() & 0xFF);
    }

    /**
     * 
     */
    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb = super.toString(sb);
        sb.append(" piSet=");
        sb.append(piSet);
        sb.append(":CF_");
        if (coordinateFlag == 0) {
            sb.append("LL");
        } else if (coordinateFlag == 1) {
            sb.append("IJ");
        } else if (coordinateFlag == 2) {
            sb.append("XY");
        } else {
            sb.append("??");
        }
        sb.append(":TM_");
        sb.append(String.format("%02d%02d%02d%02d", vtMonth, vtDay, vtHour,
                vtMinute));
        sb.append(":");
        sb.append(String.format("%02d%02d%02d%02d", evtMonth, evtDay, evtHour,
                evtMinute));

        return sb;
    }

    /**
     * @return the piSet
     */
    public int getPiSet() {
        return piSet;
    }

    /**
     * @return the coordinateFlag
     */
    public int getCoordinateFlag() {
        return coordinateFlag;
    }

    /**
     * @return the scaleFactor
     */
    public int getScaleFactor() {
        return scaleFactor;
    }

    /**
     * @return the areaCode
     */
    public int getAreaCode() {
        return areaCode;
    }

    /**
     * @return the labelCode
     */
    public int getLabelCode() {
        return labelCode;
    }

    /**
     * @return the refM1coord
     */
    public int getRefM1coord() {
        return refM1coord;
    }

    /**
     * @return the refN1coord
     */
    public int getRefN1coord() {
        return refN1coord;
    }

    /**
     * @return the refM2coord
     */
    public int getRefM2coord() {
        return refM2coord;
    }

    /**
     * @return the refN2coord
     */
    public int getRefN2coord() {
        return refN2coord;
    }

    /**
     * @return the refM3coord
     */
    public int getRefM3coord() {
        return refM3coord;
    }

    /**
     * @return the refN3coord
     */
    public int getRefN3coord() {
        return refN3coord;
    }

    /**
     * @return the vtMonth
     */
    public int getVtMonth() {
        return vtMonth;
    }

    /**
     * @return the vtDay
     */
    public int getVtDay() {
        return vtDay;
    }

    /**
     * @return the vtHour
     */
    public int getVtHour() {
        return vtHour;
    }

    /**
     * @return the vtMinute
     */
    public int getVtMinute() {
        return vtMinute;
    }

    /**
     * @return the evtMonth
     */
    public int getEvtMonth() {
        return evtMonth;
    }

    /**
     * @return the evtDay
     */
    public int getEvtDay() {
        return evtDay;
    }

    /**
     * @return the evtHour
     */
    public int getEvtHour() {
        return evtHour;
    }

    /**
     * @return the evtMinute
     */
    public int getEvtMinute() {
        return evtMinute;
    }

}
