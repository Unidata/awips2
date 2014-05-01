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

import com.raytheon.uf.common.datadelivery.registry.Ensemble;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedTime;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Gridded implementation of DataSizeUtils
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2013    2108    mpduff      Initial creation.
 * Sept 25, 2013  1797     dhladky     separated time from gridded time
 * Nov 20, 2013   2554     dhladky     Generics
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GriddedDataSizeUtils extends DataSizeUtils<GriddedDataSet> {

    /**
     * Gridded constructor.
     * 
     * @param dataSet
     *            the data set
     */
    public GriddedDataSizeUtils(GriddedDataSet dataSet) {
        this.dataSet = dataSet;
    }

    /**
     * Calculate the number of grid cells for the envelope.
     * 
     * @param envelope
     *            The areal envelope
     * @return number of grid cells
     */
    private int calculateGridCells(ReferencedEnvelope envelope) {
        if (dataSet != null) {

            GriddedCoverage griddedCov = dataSet.getCoverage();

            GridCoverage subgridCov = griddedCov
                    .getRequestGridCoverage(envelope);
            if (subgridCov == null) {
                subgridCov = griddedCov.getGridCoverage();
            }
            int nx = subgridCov.getNx();
            int ny = subgridCov.getNy();

            return nx * ny;

        }

        return 0;
    }

    /**
     * Get the number of grids for the request.
     * 
     * @param parameterList
     *            the list of parameters
     * 
     * @return number of grids
     */
    private int getNumberRequestedGrids(List<Parameter> parameterList) {
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

        return numGrids;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getFullSizeInBytes() {
        if (dataSet != null) {
            if (fullSize == -999) {
                GriddedCoverage griddedCov = dataSet
                        .getCoverage();
                long numCells = griddedCov.getGridCoverage().getNx()
                        * griddedCov.getGridCoverage().getNy();
                long numEns = 1;
                long fcstHrs = 1;

                fcstHrs = dataSet.getForecastHours().size();
                if (dataSet.getEnsemble() != null) {
                    numEns = dataSet.getEnsemble().getMemberCount();
                }

                Map<String, Parameter> paramMap = dataSet.getParameters();

                // get the number of grids available
                long numGridsAvailable = 0;

                for (Parameter p : paramMap.values()) {
                    int numLevels = p.getLevels().getLevel().size();

                    // parameter is always at least on one level, level
                    // just may not be named/enumerated
                    numGridsAvailable += (numLevels > 0 ? numLevels : 1);
                }

                fullSize = numEns
                        * fcstHrs
                        * numGridsAvailable
                        * dataSet.getServiceType()
                                .getRequestBytesPerParameterPerLevel(numCells);
            }
        }

        return fullSize;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getDataSetSizeInBytes(Subscription<?, ?> subscription) {
        final Ensemble ensemble = subscription.getEnsemble();
        int numEnsemble = (ensemble == null) ? 1 : ensemble.getMemberCount();

        return getDataSetSizeInBytes(subscription.getParameter(),
                ((GriddedTime) subscription.getTime()).getSelectedTimeIndices()
                        .size(), numEnsemble, subscription.getCoverage()
                        .getRequestEnvelope());
    }

    /**
     * Get the data set size in bytes.
     * 
     * @param params
     *            List of parameters
     * @param numFcstHrs
     *            Number of forecast hours
     * @param numEnsembleMembers
     *            Number of ensemble members
     * @param envelope
     *            ReferencedEnvelope
     * @return Number of bytes
     */
    public long getDataSetSizeInBytes(List<Parameter> params, int numFcstHrs,
            int numEnsembleMembers, ReferencedEnvelope envelope) {
        int numberOfGridCells = calculateGridCells(envelope);

        int numRequestedGrids = getNumberRequestedGrids(params);

        long l = numRequestedGrids
                * numFcstHrs
                * numEnsembleMembers
                * dataSet.getServiceType().getRequestBytesPerParameterPerLevel(
                        numberOfGridCells);
        return l;
    }
}
