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
package com.raytheon.viz.lightning.cache;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;

/**
 * Lightning location data for a single time frame. Data is separated out by
 * flash/pulse category.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 9, 2014  3333      bclement     moved from LightningResource
 *                                      added merge() and getDataSize()
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class LightningFrame {

    private final LightningFrameMetadata metadata;

    private final DataTime frameTime;

    private final List<double[]> posLatLonList = new ArrayList<double[]>();

    private final List<double[]> negLatLonList = new ArrayList<double[]>();

    private final List<double[]> cloudLatLonList = new ArrayList<double[]>();

    private final List<double[]> pulseLatLonList = new ArrayList<double[]>();

    private static final int BYTES_PER_DOUBLE = Double.SIZE / Byte.SIZE;

    /**
     * @param frameTime
     * @param metadata
     */
    public LightningFrame(DataTime frameTime, LightningFrameMetadata metadata) {
        this.frameTime = frameTime;
        this.metadata = metadata;
    }

    /**
     * Add all locations from other to this frame
     * 
     * @param other
     */
    public void merge(LightningFrame other) {
        this.posLatLonList.addAll(other.posLatLonList);
        this.negLatLonList.addAll(other.negLatLonList);
        this.cloudLatLonList.addAll(other.cloudLatLonList);
        this.pulseLatLonList.addAll(other.pulseLatLonList);
    }

    /**
     * @return total size of location data in bytes
     */
    public int getDataSize() {
        int doubleCount = 0;
        doubleCount += getSize(getPosLatLonList());
        doubleCount += getSize(getNegLatLonList());
        doubleCount += getSize(getCloudLatLonList());
        doubleCount += getSize(getPulseLatLonList());
        return doubleCount * BYTES_PER_DOUBLE;
    }

    /**
     * @param lonLatList
     * @return total number of doubles in list
     */
    private static int getSize(List<double[]> lonLatList) {
        int rval = 0;
        for (double[] arr : lonLatList) {
            rval += arr.length;
        }
        return rval;
    }

    /**
     * @return the metadata
     */
    public LightningFrameMetadata getMetadata() {
        return metadata;
    }

    /**
     * @return the frameTime
     */
    public DataTime getFrameTime() {
        return frameTime;
    }

    /**
     * @return the posLatLonList
     */
    public List<double[]> getPosLatLonList() {
        return posLatLonList;
    }

    /**
     * @return the negLatLonList
     */
    public List<double[]> getNegLatLonList() {
        return negLatLonList;
    }

    /**
     * @return the cloudLatLonList
     */
    public List<double[]> getCloudLatLonList() {
        return cloudLatLonList;
    }

    /**
     * @return the pulseLatLonList
     */
    public List<double[]> getPulseLatLonList() {
        return pulseLatLonList;
    }

}
