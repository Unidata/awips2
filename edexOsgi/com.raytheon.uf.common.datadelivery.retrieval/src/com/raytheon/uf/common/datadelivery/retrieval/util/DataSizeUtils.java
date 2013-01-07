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
package com.raytheon.uf.common.datadelivery.retrieval.util;

import java.util.Map;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.gridcoverage.GridCoverage;

/**
 * Data Structure for calculating Data Set Size.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012    1002     mpduff     Initial creation
 * Aug 12, 2012  1022      djohnson   Stop coordinates on GriddedCoverage from being corrupted.
 * Oct 31, 2012  1278      mpduff     Clarified a Javadoc comment.
 * Dec 10, 2012  1259      bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 *
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataSizeUtils {

    /**
     * Get data size
     * 
     * @param ra
     * @param st
     * @return
     */

    public static long calculateSize(RetrievalAttribute ra, ServiceType st) {

        if (ra.getCoverage() instanceof GriddedCoverage) {
            GriddedCoverage griddedCov = (GriddedCoverage) ra.getCoverage();
            int nx = griddedCov.getGridCoverage().getNx();
            int ny = griddedCov.getGridCoverage().getNy();
        
            long l = st.getRequestBytesPerParameterPerLevel(nx * ny);

            l = l / bytesPerKilobyte;
            return l;

        } else {
            throw new IllegalStateException(
                    "Couldn't calculate the retrieval size for a retrieval of type "
                            + st.name() + "!");
        }
    }

    /** Bytes per Kilobyte */
    private static final int bytesPerKilobyte = 1024;

    /** Data Set Object */
    private DataSet dataSet = null;

    /** Data Set Size */
    private long size = 0;

    /** Full Data Set Size */
    private long fullSize = -999;

    /** Number of parameters */
    private int numParameters = 0;

    /** Number of levels */
    private int numLevels = 0;

    /** Number of forecast hours */
    private int numFcstHours = 0;

    /** Envelope */
    private ReferencedEnvelope envelope = null;

    /** Number of grid cells */
    private int numberOfGridCells = 0;

    /** Default Constructor */
    public DataSizeUtils() {

    }

    /**
     * Constructor.
     * 
     * @param dataSetName
     *            The data set name
     */
    public DataSizeUtils(DataSet dataSet) {
        this.dataSet = dataSet;
    }

    /**
     * Add to the number of levels.
     * 
     * @param levels
     *            Number of levels to add
     */
    public void addToLevelCount(int levels) {
        numLevels += levels;
    }

    private void calculateGridCells() {
        if (dataSet != null) {
            Coverage cov = dataSet.getCoverage();
            if (cov instanceof GriddedCoverage) {
                GriddedCoverage griddedCov = (GriddedCoverage) cov;

                GridCoverage subgridCov = griddedCov
                        .getRequestGridCoverage(envelope);
                if (subgridCov == null) {
                    subgridCov = griddedCov.getGridCoverage();
                }
                int nx = subgridCov.getNx();
                int ny = subgridCov.getNy();

                numberOfGridCells = nx * ny;

            }
        }
    }

    /**
     * @return the dataSet
     */
    public DataSet getDataSet() {
        return dataSet;
    }

    /**
     * Returns the estimated data set size in KB.
     * 
     * @return
     */
    public long getDataSetSize() {
        // Handle the case where the level is surface only, must artificially
        // set numLevels to 1
        int numberOfLevels = numLevels;
        if (numberOfLevels == 0 && numParameters > 0) {
            numberOfLevels = 1;
        }
        long l = numParameters
                * numberOfLevels
                * numFcstHours
                * dataSet.getServiceType().getRequestBytesPerParameterPerLevel(
                        numberOfGridCells);

        return l / bytesPerKilobyte;
    }

    /**
     * REturns the estimated full dataset size.
     * 
     * @return
     */
    public long getFullSize() {
        if (dataSet != null) {
            if (fullSize == -999) {
                Coverage cov = dataSet.getCoverage();
                if (cov instanceof GriddedCoverage) {
                    GriddedCoverage griddedCov = (GriddedCoverage) cov;
                    long numCells = griddedCov.getGridCoverage().getNx()
                            * griddedCov.getGridCoverage().getNy();
                    // Default to 1 forecast hour if not a gridded data set
                    long fcstHrs = dataSet instanceof GriddedDataSet ? ((GriddedDataSet) dataSet)
                            .getForecastHours().size() : 1;
                    Map<String, Parameter> paramMap = dataSet.getParameters();
                    long numParams = paramMap.size();
                    long numLevels = 0;

                    for (Parameter p : paramMap.values()) {
                        numLevels += p.getLevels().getLevel().size();
                    }

                    fullSize = fcstHrs
                            * numParams
                            * numLevels
                            * dataSet.getServiceType()
                                    .getRequestBytesPerParameterPerLevel(
                                            numCells);

                    fullSize /= bytesPerKilobyte;
                }
            }
        }

        return fullSize;
    }

    public int getNumberOfGridCells() {
        return numberOfGridCells;
    }

    /**
     * @return the numFcstHours
     */
    public int getNumFcstHours() {
        return numFcstHours;
    }

    /**
     * @return the numLevels
     */
    public int getNumLevels() {
        return numLevels;
    }

    /**
     * @return the numParameters
     */
    public int getNumParameters() {
        return numParameters;
    }

    /**
     * @return the size
     */
    public long getSize() {
        return size;
    }

    /**
     * Increment the number of levels by one.
     */
    public void incrementLevelCount() {
        numLevels++;
    }

    /**
     * Increment the number of parameters by one.
     */
    public void incrementParamCount() {
        numParameters++;
    }

    /**
     * Reset the state of this object
     */
    public void reset() {
        numLevels = 0;
        numParameters = 0;
        size = 0;
        dataSet = null;
    }

    public void setEnvelope(ReferencedEnvelope envelope) {
        this.envelope = envelope;
        if (envelope != null) {
            calculateGridCells();
        }
    }

    /**
     * @param dataSet
     *            the dataSet to set
     */
    public void setDataSet(DataSet dataSet) {
        this.dataSet = dataSet;
    }

    /**
     * @param numFcstHours
     *            the numFcstHours to set
     */
    public void setNumFcstHours(int numFcstHours) {
        this.numFcstHours = numFcstHours;
    }

    /**
     * @param numLevels
     *            the numLevels to set
     */
    public void setNumLevels(int numLevels) {
        this.numLevels = numLevels;
    }

    /**
     * @param numParameters
     *            the numParameters to set
     */
    public void setNumParameters(int numParameters) {
        this.numParameters = numParameters;
    }

    /**
     * @param size
     *            the size to set
     */
    public void setSize(long size) {
        this.size = size;
    }
}
