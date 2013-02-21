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

import java.util.List;
import java.util.Map;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.Ensemble;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.util.CollectionUtil;

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

    /** Full Data Set Size in bytes */
    private long fullSize = -999;

    /** Number of requested grids */
    private int numRequestedGrids = 0;

    /** Number of forecast hours */
    private int numFcstHours = 0;

    private int numEnsembleMembers = 0;

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

    public long getDataSetSizeInBytes() {
        long l = numRequestedGrids
                * numFcstHours
                * numEnsembleMembers
                * dataSet.getServiceType().getRequestBytesPerParameterPerLevel(
                        numberOfGridCells);
        return l;
    }

    /**
     * Returns the estimated data set size in KB.
     * 
     * @return
     */
    public long getDataSetSize() {
        return getDataSetSizeInBytes() / bytesPerKilobyte;
    }

    /**
     * Returns the estimated full dataset size in bytes.
     * 
     * @return
     */
    public long getFullSizeInBytes() {
        if (dataSet != null) {
            if (fullSize == -999) {
                Coverage cov = dataSet.getCoverage();
                if (cov instanceof GriddedCoverage) {
                    GriddedCoverage griddedCov = (GriddedCoverage) cov;
                    long numCells = griddedCov.getGridCoverage().getNx()
                            * griddedCov.getGridCoverage().getNy();
                    // Default to 1 forecast hour if not a gridded data set
                    long numEns = 1;
                    long fcstHrs = 1;
                    if (dataSet instanceof GriddedDataSet) {
                        GriddedDataSet gDataSet = (GriddedDataSet) dataSet;
                        fcstHrs = gDataSet.getForecastHours().size();
                        if (gDataSet.getEnsemble() != null) {
                            numEns = gDataSet.getEnsemble().getMemberCount();
                        }
                    }
                    Map<String, Parameter> paramMap = dataSet.getParameters();

                    // get the number of grids available
                    long numGridsAvailable = 0;

                    for (Parameter p : paramMap.values()) {
                        int numLevels = p.getLevels().getLevel().size();

                        // parameter is always at least on one level, level just
                        // may not be named/enumerated
                        numGridsAvailable += (numLevels > 0 ? numLevels : 1);
                    }

                    fullSize = numEns
                            * fcstHrs
                            * numGridsAvailable
                            * dataSet.getServiceType()
                                    .getRequestBytesPerParameterPerLevel(
                                            numCells);
                }
            }
        }

        return fullSize;
    }

    /**
     * Returns the estimated full dataset size in KB.
     * 
     * @return
     */
    public long getFullSize() {
        return getFullSizeInBytes() / bytesPerKilobyte;
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
     * @return the size
     */
    public long getSize() {
        return size;
    }

    /**
     * Reset the state of this object
     */
    public void reset() {
        numRequestedGrids = 0;
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
     * @param numRequestedGrids
     *            the numRequestedGrids to set
     */
    public void determineNumberRequestedGrids(List<Parameter> parameterList) {
        int numGrids = 0;

        // Get the number of requested grids
        if (!CollectionUtil.isNullOrEmpty(parameterList)) {
            for (Parameter par : parameterList) {
                Levels parLevels = par.getLevels();
                int numSelectedLevels = parLevels.getSelectedLevelIndices()
                        .size();
                if (numSelectedLevels < 1) {
                    // if parameter is not available on more than level, then by
                    // default the single level is selected
                    if (parLevels.size() <= 1) {
                        numSelectedLevels = 1;
                    }
                    // else user did not select any levels for this parameter
                }

                numGrids += numSelectedLevels;
            }
        }

        this.numRequestedGrids = numGrids;
    }

    public void setNumEnsembleMembers(Ensemble ensemble) {
        if (ensemble == null) {
            this.numEnsembleMembers = 1;
        } else {
            this.numEnsembleMembers = ensemble.getSelectedMemberCount();
        }

    }
}
