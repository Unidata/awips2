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
package com.raytheon.viz.grid.gridcache;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataCacheCube implements Comparable<DataCacheCube> {
    /**
     * Data cube metadata.
     */
    private DataCubeMetadata metadata = new DataCubeMetadata();

    /**
     * FloatDataRecord list in stack order.
     */
    private ArrayList<FloatDataRecord> fdrStackData = new ArrayList<FloatDataRecord>();

    /**
     * GribRecord list in stack order.
     */
    private ArrayList<GribRecord> gribRecordStack = new ArrayList<GribRecord>();

    /**
     * The creation time of this data cube.
     */
    private long creationTime;

    /**
     * Constructor
     */
    public DataCacheCube() {

    }

    /**
     * Concatenate the layer arrays into one and return it.
     * 
     * @return float[] of data
     */
    public float[] getRawData() {
        // Concatenate all the arrays into one
        int size = 0;
        for (FloatDataRecord fdr : fdrStackData) {
            size += fdr.getFloatData().length;
        }

        float[] floatArray = new float[size];
        int offset = 0;
        for (FloatDataRecord fdr : fdrStackData) {
            float[] tmp = fdr.getFloatData();
            System.arraycopy(tmp, 0, floatArray, offset, tmp.length);
            offset += tmp.length;
        }

        return floatArray;
    }

    public float[] getLevelData() {
        float[] levels = new float[gribRecordStack.size()];
        for (int i = 0; i < gribRecordStack.size(); i++) {
            levels[i] = gribRecordStack.get(i).getModelInfo()
                    .getLevelOneValue().floatValue();
        }
        return levels;
    }

    /**
     * Get the stack of FloatDataRecord objects.
     * 
     * @return the fdrStackData
     */
    public ArrayList<FloatDataRecord> getFdrStackData() {
        return fdrStackData;
    }

    /**
     * Set the stack of FloatDataRecord objects.
     * 
     * @param fdrStackData
     *            the fdrStackData to set
     */
    public void setFdrStackData(ArrayList<FloatDataRecord> fdrStackData) {
        this.fdrStackData = fdrStackData;
    }

    /**
     * Get the stack of GribRecord Objects.
     * 
     * @return the gribRecordStack
     */
    public ArrayList<GribRecord> getGribRecordStack() {
        return gribRecordStack;
    }

    /**
     * Set the stack of GribRecord Objects.
     * 
     * @param gribRecordStack
     *            the gribRecordStack to set
     */
    public void setGribRecordStack(ArrayList<GribRecord> gribRecordStack) {
        this.gribRecordStack = gribRecordStack;
    }

    /**
     * @return the metadata
     */
    public DataCubeMetadata getMetadata() {
        return metadata;
    }

    /**
     * @param metadata
     *            the metadata to set
     */
    public void setMetadata(DataCubeMetadata metadata) {
        this.metadata = metadata;
    }

    /**
     * @return the creationTime
     */
    public long getCreationTime() {
        return creationTime;
    }

    /**
     * @param creationTime
     *            the creationTime to set
     */
    public void setCreationTime(long creationTime) {
        this.creationTime = creationTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if ((obj == null) || (obj.getClass() != this.getClass())) {
            return false;
        }

        DataCacheCube cube = (DataCacheCube) obj;
        if (!metadata.equals(cube.getMetadata())) {
            return false;
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return metadata.getFcstHr() + metadata.getModelName().hashCode()
                + metadata.getParameter().hashCode()
                + metadata.getLevelType().hashCode();
    }

    /**
     * Compare this DataCacheCube to another DataCacheCube. This method compares
     * the Forecast hour attribute.
     */
    @Override
    public int compareTo(DataCacheCube o) {
        DataCacheCube cube = o;
        if (metadata.getFcstHr() > cube.getMetadata().getFcstHr()) {
            return 1;
        } else if (metadata.getFcstHr() < cube.getMetadata().getFcstHr()) {
            return -1;
        }

        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Model Name:  " + metadata.getModelName() + "\n");
        sb.append("Parameter:   " + metadata.getParameter() + "\n");
        sb.append("Level Type:  " + metadata.getLevelType() + "\n");
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTimeInMillis(metadata.getRefTime());
        sb.append("Ref Time:    " + c.get(Calendar.YEAR) + "/"
                + c.get(Calendar.MONTH) + "/" + c.get(Calendar.DAY_OF_MONTH));
        sb.append(" " + c.get(Calendar.HOUR_OF_DAY) + ":"
                + c.get(Calendar.MINUTE) + "\n");
        sb.append("Fcst Hour:   " + metadata.getFcstHr() / 60 / 60 + "\n");
        sb.append("Grid Size: " + metadata.getNx() + " x " + metadata.getNy());

        return sb.toString();
    }
}
