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
import java.util.ArrayList;
import java.util.HashMap;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Decodes the vector arrow packet as described by packet code 5
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2009            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class CellTrendDataPacket extends SymbologyPacket implements
        ISerializableObject {
    private static final int CELL_TREND_DATA_PACKET21 = 21;

    @DynamicSerializeElement
    private String cellID;

    @DynamicSerializeElement
    private int i;

    @DynamicSerializeElement
    private int j;

    @DynamicSerializeElement
    private ArrayList<CellTrendData> volumeScans;

    @DynamicSerializeElement
    private HashMap<Integer, Integer> latestScans;

    @DynamicSerializeElement
    private ArrayList<Integer> volumeScan;

    @DynamicSerialize
    public static class CellTrendData implements SymbologyPoint,
            ISerializableObject {
        @DynamicSerializeElement
        private int trendCode;

        @DynamicSerializeElement
        private ArrayList<Integer> data;

        public CellTrendData() {
        }

        public CellTrendData(int trendCode) {
            this.trendCode = trendCode;
        }

        public void addData(int newData) {
            if (data == null) {
                data = new ArrayList<Integer>();
            }

            data.add(newData);
        }

        /**
         * @return the trendCode
         */
        public int getTrendCode() {
            return trendCode;
        }

        /**
         * @param trendCode
         *            the trendCode to set
         */
        public void setTrendCode(int trendCode) {
            this.trendCode = trendCode;
        }

        /**
         * @return the data
         */
        public ArrayList<Integer> getData() {
            return data;
        }

        /**
         * @param data
         *            the data to set
         */
        public void setData(ArrayList<Integer> data) {
            this.data = data;
        }
    }

    static {
        PacketFactory.registerPacketType(CellTrendDataPacket.class,
                CELL_TREND_DATA_PACKET21);
    }

    public CellTrendDataPacket() {

    }

    public CellTrendDataPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        volumeScans = new ArrayList<CellTrendData>();
        latestScans = new HashMap<Integer, Integer>();

        // Block Length
        int blockLength = in.readUnsignedShort();

        // Cell ID
        char c0 = (char) in.readByte();
        char c1 = (char) in.readByte();
        setCellID("" + c0 + c1);

        // Position
        setI(in.readShort());
        setJ(in.readShort());

        int trendCode = 0;
        int numScans = 0;
        int latestScan = 0;
        int bytesRead = 8;
        CellTrendData trendData;
        ArrayList<Integer> data;
        while (bytesRead < blockLength) {
            // Read current Trend Code
            trendCode = in.readShort();
            bytesRead += 2;

            // Read the number of scans for the current Trend Code
            numScans = in.readByte();
            bytesRead += 1;

            // Read the latest scan for the current Trend Code
            latestScan = in.readByte();
            bytesRead += 1;

            latestScans.put(trendCode, latestScan);

            // Read and store each of the volumes for the current Trend Code
            int currData = 0;
            for (int k = 0; k < numScans; k++) {
                if (getCellTrendData(trendCode) == null) {
                    volumeScans.add(new CellTrendData(trendCode));
                }

                trendData = getCellTrendData(trendCode);

                currData = in.readShort();
                bytesRead += 2;

                trendData.addData(currData);
            }
        }
    }

    /**
     * @return the cellID
     */
    public String getCellID() {
        return cellID;
    }

    /**
     * @param cellID
     *            the cellID to set
     */
    public void setCellID(String cellID) {
        this.cellID = cellID;
    }

    /**
     * @return the i
     */
    public int getI() {
        return i;
    }

    /**
     * @param i
     *            the i to set
     */
    public void setI(int i) {
        this.i = i;
    }

    /**
     * @return the j
     */
    public int getJ() {
        return j;
    }

    /**
     * @param j
     *            the j to set
     */
    public void setJ(int j) {
        this.j = j;
    }

    /**
     * @return the volumeScans
     */
    public ArrayList<CellTrendData> getVolumeScans() {
        return volumeScans;
    }

    /**
     * @param volumeScans
     *            the volumeScans to set
     */
    public void setVolumeScans(ArrayList<CellTrendData> volumeScans) {
        this.volumeScans = volumeScans;
    }

    /**
     * @return the latestScans
     */
    public HashMap<Integer, Integer> getLatestScans() {
        return latestScans;
    }

    /**
     * @param latestScans
     *            the latestScans to set
     */
    public void setLatestScans(HashMap<Integer, Integer> latestScans) {
        this.latestScans = latestScans;
    }

    /**
     * @return the volumeScan
     */
    public ArrayList<Integer> getVolumeScan() {
        return volumeScan;
    }

    /**
     * @param volumeScan
     *            the volumeScan to set
     */
    public void setVolumeScan(ArrayList<Integer> volumeScan) {
        this.volumeScan = volumeScan;
    }

    public CellTrendData getCellTrendData(int trendCode) {
        CellTrendData rval = null;

        for (CellTrendData data : volumeScans) {
            if (data.getTrendCode() == trendCode) {
                rval = data;
                break;
            }
        }

        return rval;
    }

    @Override
    public String toString() {
        String s = super.toString() + " CellTrendDataPacket";
        s += "\n\tCell ID: " + cellID;
        s += "\n\ti: " + i;
        s += "\n\tj: " + j;
        for (Integer currTrend : latestScans.keySet()) {
            s += "\n\t Trend Code: " + currTrend;
            s += "\n\t   Latest Vol: " + latestScans.get(currTrend);
            int k = 1;
            for (Integer vol : getCellTrendData(currTrend).getData()) {
                s += "\n\t   Vol #" + k++ + ": " + vol;
            }
        }

        return s;
    }
}
