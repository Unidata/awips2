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

public class Block_004_017 extends RedbookBlock {

    private float ulLat;

    private float ulLon;

    private float urLat;

    private float urLon;

    private float lrLat;

    private float lrLon;

    private float llLat;

    private float llLon;

    private float lon1;

    private float lat1;

    private float lat2;

    private String projName;

    public static class Factory implements RedbookBlockFactory {
        @Override
        public RedbookBlock createBlock(RedbookBlockHeader header,
                ByteBuffer data) {
            return new Block_004_017(header, data);
        }
    }

    /**
     * 
     * @param header
     * @param separator
     */
    public Block_004_017(RedbookBlockHeader header, ByteBuffer data) {
        super(header, data);
        populateProj(data);
        if (hasChkSum()) {
            data.getShort();
        }
    }

    private void populateProj(ByteBuffer data) {

        data.get();

        int sel = (data.get() & 0xFF);
        if (sel >= 1) {
            ulLat = getFloat2(data);
            ulLon = getFloat2(data);
        }
        if (sel >= 2) {
            urLat = getFloat2(data);
            urLon = getFloat2(data);
        }
        if (sel >= 3) {
            lrLat = getFloat2(data);
            lrLon = getFloat2(data);
        }
        if (sel >= 4) {
            llLat = getFloat2(data);
            llLon = getFloat2(data);
        }

        lon1 = getFloat2(data);
        lat1 = getFloat2(data);
        lat2 = getFloat2(data);

        char[] c = new char[6];
        for (int i = 0; i < c.length; i++) {
            c[i] = (char) (data.get() & 0xFF);
        }
        projName = new String(c);

        // empty read!
        data.getShort();
    }

    /**
     * 
     */
    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb = super.toString(sb);
        sb.append(String.format(" ul[%2.2f:%2.2f]", ulLat, ulLon));
        sb.append(String.format(",ur[%2.2f:%2.2f]", urLat, urLon));
        sb.append(String.format(",ll[%2.2f:%2.2f]", llLat, llLon));
        sb.append(String.format(",ur[%2.2f:%2.2f]", urLat, urLon));

        sb.append(projName);
        return sb;
    }

    /**
     * @return the ulLat
     */
    public float getUlLat() {
        return ulLat;
    }

    /**
     * @return the ulLon
     */
    public float getUlLon() {
        return ulLon;
    }

    /**
     * @return the urLat
     */
    public float getUrLat() {
        return urLat;
    }

    /**
     * @return the urLon
     */
    public float getUrLon() {
        return urLon;
    }

    /**
     * @return the lrLat
     */
    public float getLrLat() {
        return lrLat;
    }

    /**
     * @return the lrLon
     */
    public float getLrLon() {
        return lrLon;
    }

    /**
     * @return the llLat
     */
    public float getLlLat() {
        return llLat;
    }

    /**
     * @return the llLon
     */
    public float getLlLon() {
        return llLon;
    }

    /**
     * @return the lon1
     */
    public float getLon1() {
        return lon1;
    }

    /**
     * @return the lat1
     */
    public float getLat1() {
        return lat1;
    }

    /**
     * @return the lat2
     */
    public float getLat2() {
        return lat2;
    }

    /**
     * @return the projName
     */
    public String getProjName() {
        return projName;
    }

}
