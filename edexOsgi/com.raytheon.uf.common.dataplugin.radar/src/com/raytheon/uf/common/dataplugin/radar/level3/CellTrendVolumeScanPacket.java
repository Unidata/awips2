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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2009            mnash     Initial creation
 * 07/29/2013   2148       mnash     Refactor registering of packets to Spring
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@DynamicSerialize
public class CellTrendVolumeScanPacket extends SymbologyPacket implements
        ISerializableObject {

    @DynamicSerializeElement
    private int numberOfVolumes;

    @DynamicSerializeElement
    private int latestVolume;

    @DynamicSerializeElement
    private ArrayList<Integer> volumeTimes;

    public CellTrendVolumeScanPacket() {
    }

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public CellTrendVolumeScanPacket(int packetId, DataInputStream in)
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
        volumeTimes = new ArrayList<Integer>();

        // Read number of volumes
        numberOfVolumes = in.readByte();

        // Read Latest Volume
        latestVolume = in.readByte();

        // Read in the times of the volumes
        for (int j = 0; j < numberOfVolumes; j++) {
            volumeTimes.add((int) in.readShort());
        }
    }

    /**
     * @return the numberOfVolumes
     */
    public int getNumberOfVolumes() {
        return numberOfVolumes;
    }

    /**
     * @param numberOfVolumes
     *            the numberOfVolumes to set
     */
    public void setNumberOfVolumes(int numberOfVolumes) {
        this.numberOfVolumes = numberOfVolumes;
    }

    /**
     * @return the lastestVolume
     */
    public int getLatestVolume() {
        return latestVolume;
    }

    /**
     * @param lastestVolume
     *            the lastestVolume to set
     */
    public void setLatestVolume(int latestVolume) {
        this.latestVolume = latestVolume;
    }

    /**
     * @return the volumeTimes
     */
    public ArrayList<Integer> getVolumeTimes() {
        return volumeTimes;
    }

    /**
     * @param volumeTimes
     *            the volumeTimes to set
     */
    public void setVolumeTimes(ArrayList<Integer> volumeTimes) {
        this.volumeTimes = volumeTimes;
    }

    @Override
    public String toString() {
        String s = super.toString() + " CellTrendVolume";
        s += "\n\tNumber of Volumes: " + numberOfVolumes;
        s += "\n\tLatest Volume: " + latestVolume;
        int i = 1;
        for (Integer time : volumeTimes) {
            s += "\n\t Volume #" + i++ + " Time: " + time;
        }
        return s;
    }
}
