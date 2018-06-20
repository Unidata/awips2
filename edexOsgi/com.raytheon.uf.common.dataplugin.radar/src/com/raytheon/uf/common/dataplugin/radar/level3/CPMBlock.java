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
 *
 * </pre>
 *
 * @author jdynina
 * @version 1.0
 */

@DynamicSerialize
public class CPMBlock extends AbstractBlock {

    private static final int BLOCK_ID = 12;

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

        @Override
        public String toString() {
            StringBuilder o = new StringBuilder();

            o.append(" CPMPacket:");
            o.append(" numClearAirVcps=" + clearAirVcps.length);
            o.append(" clearAirVcps=" + Arrays.toString(clearAirVcps));
            o.append(" numPrecipVcps=" + precipVcps.length);
            o.append(" precipVcps=" + Arrays.toString(precipVcps));
            o.append(" maxSailsCuts=" + maxSailsCuts);

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

        in.skipBytes(46 -(6 + (2 * (numClearAirVcps + numPrecipVcps))));
        message.maxSailsCuts = in.readShort();

    }
}
