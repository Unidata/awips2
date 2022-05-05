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
package com.raytheon.uf.common.dataplugin.radar.level3;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Decodes the general status message portion of the product
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 06, 2016  DCS18795 jdynina     Initial creation
 * Apr 18, 2018  DCS20681 jdynina     Added MRLE
 *
 * </pre>
 *
 * @author jdynina
 * @version 1.0
 */

@DynamicSerialize
public class CPMBlock extends AbstractBlock {

    private static final int BLOCK_ID = 12;
    private static final int SAILS = 1;
    private static final int MRLE = 2;

    public CPMBlock(DataInputStream in) throws IOException {
        super(in);
    }

    public CPMBlock() {

    }

    public static int getBlockId() {
        return BLOCK_ID;
    }

    @DynamicSerialize
    public static class CPMMessage {
        @DynamicSerializeElement
        int[] clearAirVcps;

        @DynamicSerializeElement
        int[] precipVcps;

        @DynamicSerializeElement
        public int maxSailsCuts;

        @DynamicSerializeElement
        public int maxMrleCuts;

        @DynamicSerializeElement
        public int[] allowedSailsCuts = null;

        @DynamicSerializeElement
        public int[] allowedMrleCuts = null;

        /**
         * @return the clearAirVcps
         */
        public int[] getClearAirVcps() {
            return clearAirVcps;
        }

        /**
         * @param clearAirVcps
         *            the clearAirVcps to set
         */
        public void setClearAirVcps(int[] clearAirVcps) {
            this.clearAirVcps = clearAirVcps;
        }

        /**
         * @return the precipVcps
         */
        public int[] getPrecipVcps() {
            return precipVcps;
        }

        /**
         * @param precipVcps
         *            the precipVcps to set
         */
        public void setPrecipVcps(int[] precipVcps) {
            this.precipVcps = precipVcps;
        }

        /**
         * @return the maxSailsCuts
         */
        public int getMaxSailsCuts() {
            return maxSailsCuts;
        }

        /**
         * @param maxSailsCuts
         *            the maxSailsCuts to set
         */
        public void setMaxSailsCuts(int maxSailsCuts) {
            this.maxSailsCuts = maxSailsCuts;
        }

        /**
         * @return the allowedSailsCuts
         */
        public int[] getAllowedSailsCuts() {
            return allowedSailsCuts;
        }

        /**
         * @param allowedSailsCuts
         */
        public void setAllowedSailsCuts(int[] allowedSailsCuts) {
            this.allowedSailsCuts = allowedSailsCuts;
        }

        /**
         * @return the maxMrleCuts
         */
        public int getMaxMrleCuts() {
            return maxMrleCuts;
        }

        /**
         * @param maxMrleCuts
         *            the maxMrleCuts to set
         */
        public void setMaxMrleCuts(int maxMrleCuts) {
            this.maxMrleCuts = maxMrleCuts;
        }

        /**
         * @return the allowedMrleCuts
         */
        public int[] getAllowedMrleCuts() {
            return allowedMrleCuts;
        }

        /**
         * @param allowedMrleCuts
         */
        public void setAllowedMrleCuts(int[] allowedMrleCuts) {
            this.allowedMrleCuts = allowedMrleCuts;
        }

        public String formatCpmBits(int[] bits, int type) {
            StringBuilder result = new StringBuilder();

            result.append(" { ");

            for (int i = 0; i < bits.length; i++) {
                int vcp = bits[i] & 0x0FFF; // bits 11-0
                int allowedCuts = bits[i] >>> 12; // bits 15-12

                result.append("vcp=" + Integer.toString(vcp));
                if (type == 1) {
                    result.append(":allowedSailsCuts=" + Integer.toString(allowedCuts) + " ");
                } else {
                    result.append(":allowedMrleCuts=" + Integer.toString(allowedCuts) + " ");
                }
            }

            result.append("}");
            return result.toString();
        }

        @Override
        public String toString() {
            StringBuilder o = new StringBuilder();

            o.append(" CPMPacket:");
            o.append(" numClearAirVcps=" + clearAirVcps.length);
            o.append(" clearAirVcps=" + Arrays.toString(clearAirVcps));
            o.append(" numPrecipVcps=" + precipVcps.length);
            o.append(" precipVcps=" + Arrays.toString(precipVcps));
            o.append(" maxSailsCuts=" + maxSailsCuts);
            if ((allowedSailsCuts != null) &&
                    (allowedMrleCuts != null)) {
                o.append(formatCpmBits(allowedSailsCuts, SAILS));
                o.append(" maxMrleCuts=" + maxMrleCuts);
                o.append(formatCpmBits(allowedMrleCuts, MRLE));
            }

            return o.toString();
        }
    }

    @DynamicSerializeElement
    CPMMessage message;

    /**
     * @return the points
     */
    public CPMMessage getMessage() {
        return message;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setMessage(CPMMessage message) {
        this.message = message;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        int size = in.available();

        message = new CPMMessage();
        int numClearAirVcps = in.readShort();
        message.clearAirVcps = new int[numClearAirVcps];

        for (int i = 0; i < numClearAirVcps; ++i) {
            message.clearAirVcps[i] = in.readShort();
        }

        int numPrecipVcps = in.readShort();
        message.precipVcps = new int[numPrecipVcps];

        for (int i = 0; i < numPrecipVcps; ++i) {
            message.precipVcps[i] = in.readShort();
        }

        in.skipBytes(46 - (6 + (2 * (numClearAirVcps + numPrecipVcps))));
        message.maxSailsCuts = in.readShort();

        if (size > 100) {
            message.allowedSailsCuts = new int[numClearAirVcps + numPrecipVcps];
            for (int i = 0; i < numClearAirVcps + numPrecipVcps; ++i) {
                message.allowedSailsCuts[i] = in.readShort();
            }

            // skip any blank entries after cuts/sails vcps
            if (numClearAirVcps + numPrecipVcps < 20) {
                in.skipBytes(2 * (20 - (numClearAirVcps + numPrecipVcps)));
            }

            message.maxMrleCuts = in.readShort();

            message.allowedMrleCuts = new int[numClearAirVcps + numPrecipVcps];
            for (int i = 0; i < numClearAirVcps + numPrecipVcps; ++i) {
                message.allowedMrleCuts[i] = in.readShort();
            }
        }
    }
}
