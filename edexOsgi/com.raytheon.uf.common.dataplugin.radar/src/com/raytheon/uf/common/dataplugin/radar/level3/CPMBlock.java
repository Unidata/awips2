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
 * Feb 09, 2021  DCS22417 jdynina     Added VMI
 * Jun 17, 2024  2037570  jdynina     Added MPDA
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

        @DynamicSerializeElement
        public int vmi1;

        @DynamicSerializeElement
        public int vmi2;

        @DynamicSerializeElement
        public int maxMpdaElevs;

        @DynamicSerializeElement
        public int[] numMpdaElevs;

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

        /**
         * @return the vmi1
         */
        public int getVmi1() {
            return vmi1;
        }

        /**
         * @param vmi1
         *            the vmi1 value to set
         */
        public void setVmi1(int vmi1) {
            this.vmi1 = vmi1;
        }

        /**
         * @return the vmi2
         */
        public int getVmi2() {
            return vmi2;
        }

        /**
         * @param vmi2
         *            the vmi2 value to set
         */
        public void setVmi2(int vmi2) {
            this.vmi2 = vmi2;
        }

        /**
         * @param maxMpdaElevs
         *            the maxMPDAElevs to set
         */
        public int getMaxMPDAElevs() {
            return maxMpdaElevs;
        }

        /**
         * @param maxMpdaElevs
         *            the maxMPDAElevs to set
         */
        public void setMaxMPDAElevs(int maxMpdaElevs) {
            this.maxMpdaElevs = maxMpdaElevs;
        }

        /**
         * @return the numMpdaElevs
         */
        public int[] getNumMpdaElevs() {
            return numMpdaElevs;
        }

        /**
         * @param numMpdaElevs
         */
        public void setNumMPDAElevs(int[] numMpdaElevs) {
            this.numMpdaElevs = numMpdaElevs;
        }

        public String formatCpmBits(int[] bits, int type) {
            StringBuilder result = new StringBuilder();

            result.append(" { ");

            for (int bit : bits) {
                int vcp = bit & 0x0FFF; // bits 11-0
                int allowedCuts = bit >>> 12; // bits 15-12

                result.append("vcp=" + Integer.toString(vcp));
                if (type == 1) {
                    result.append(":allowedSailsCuts="
                            + Integer.toString(allowedCuts) + " ");
                } else {
                    result.append(":allowedMrleCuts="
                            + Integer.toString(allowedCuts) + " ");
                }
            }

            result.append("}");
            return result.toString();
        }

        private String formatMPDA(int[] numMpdaElevs, int[] clearAirVcps,
                int[] precipVcps) {
            StringBuilder result = new StringBuilder();

            result.append(" { ");

            for (int i = 0; i < clearAirVcps.length; ++i) {
                result.append(":vcp" + Integer.toString(clearAirVcps[i]) + "="
                        + Integer.toString(numMpdaElevs[i]) + " ");
            }

            for (int i = clearAirVcps.length; i < clearAirVcps.length
                    + precipVcps.length; ++i) {
                result.append(":vcp"
                        + Integer.toString(precipVcps[i - clearAirVcps.length])
                        + "=" + Integer.toString(numMpdaElevs[i]) + " ");
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
            o.append(formatCpmBits(allowedSailsCuts, SAILS));
            o.append(" maxMrleCuts=" + maxMrleCuts);
            o.append(formatCpmBits(allowedMrleCuts, MRLE));

            if ((vmi1 > 0) && (vmi2 > 0)) {
                o.append(" 0.5 m/s VMI request value=" + vmi1);
                o.append(" 1.0 m/s VMI request value=" + vmi2);
            }

            if (numMpdaElevs != null) {
                o.append(" maxMPDAElevs=" + maxMpdaElevs);
                o.append(formatMPDA(numMpdaElevs, clearAirVcps, precipVcps));
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
     * @see
     * com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket#init(java
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

        if (size > 130) {
            // skip any blank entries after cuts/mrle vcps
            if (numClearAirVcps + numPrecipVcps < 20) {
                in.skipBytes(2 * (20 - (numClearAirVcps + numPrecipVcps)));
            }

            message.vmi1 = in.readShort();
            message.vmi2 = in.readShort();
        }

        if (size > 134) {
            message.maxMpdaElevs = in.readShort();

            message.numMpdaElevs = new int[numClearAirVcps + numPrecipVcps];
            for (int i = 0; i < numClearAirVcps + numPrecipVcps; ++i) {
                int maxNumMpda = in.readShort();
                message.numMpdaElevs[i] = (maxNumMpda & 0xf000) >> 12;
            }
        }
    }
}
