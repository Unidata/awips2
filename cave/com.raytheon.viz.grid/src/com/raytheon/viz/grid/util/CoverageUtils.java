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
package com.raytheon.viz.grid.util;

import java.awt.RenderingHints;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.media.jai.BorderExtender;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.ParameterBlockJAI;
import javax.media.jai.PlanarImage;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.ViewType;
import org.geotools.coverage.processing.Operations;
import org.opengis.geometry.Envelope;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Cache for coverages as well as several utility methods for reprojecting data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2010            rjpeter     Initial creation
 * Jul 25, 2013 2112       bsteffen    Fix volume browser sounding errors.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class CoverageUtils implements IAlertObserver {
    private static CoverageUtils instance;

    private final Map<String, Set<UniqueIdGridCoverageWrapper>> coverageCache = new HashMap<String, Set<UniqueIdGridCoverageWrapper>>();

    private boolean hasPerformedBulkQuery = false;

    private CoverageUtils() {
    }

    public static synchronized CoverageUtils getInstance() {
        if (instance == null) {
            instance = new CoverageUtils();
            ProductAlertObserver.addObserver(GridConstants.GRID, instance);
        }

        return instance;
    }

    /**
     * Return an unordered collection of all GridCoverages that are used for a
     * given datasetId.
     * 
     * @param datasetId
     * @return
     * @throws VizException
     */
    public Collection<GridCoverage> getCoverages(String datasetId)
            throws VizException {
        Collection<UniqueIdGridCoverageWrapper> rval = coverageCache
                .get(datasetId);

        if (rval == null) {
            DbQueryRequest query = new DbQueryRequest();
            query.setEntityClass(GridInfoRecord.class.getName());
            query.setDistinct(true);
            query.addRequestField(GridInfoConstants.DATASET_ID);
            query.addRequestField(GridInfoConstants.LOCATION_ID);
            if (hasPerformedBulkQuery) {
                // first time through just request everything.
                // if the cache is empty request data for all models.
                query.addConstraint(GridInfoConstants.DATASET_ID,
                        new RequestConstraint(datasetId));
            }
            hasPerformedBulkQuery = true;
            DbQueryResponse resp = (DbQueryResponse) ThriftClient
                    .sendRequest(query);
            // do a bulk request to GridCoverageLookup as it enables more
            // possible optimizations.
            List<Integer> locationsToRequest = new ArrayList<Integer>(resp
                    .getResults().size());
            for (Map<String, Object> map : resp.getResults()) {
                Integer locationId = (Integer) map
                        .get(GridInfoConstants.LOCATION_ID);
                locationsToRequest.add(locationId);
            }
            Map<Integer, GridCoverage> requestedLocations = GridCoverageLookup
                    .getInstance().getCoverages(locationsToRequest);
            for (Map<String, Object> map : resp.getResults()) {
                Integer locationId = (Integer) map
                        .get(GridInfoConstants.LOCATION_ID);
                String resultId = (String) map
                        .get(GridInfoConstants.DATASET_ID);
                GridCoverage coverage = requestedLocations.get(locationId);
                Set<UniqueIdGridCoverageWrapper> set = coverageCache
                        .get(resultId);
                if (set == null) {
                    set = new HashSet<UniqueIdGridCoverageWrapper>();
                    coverageCache.put(resultId, set);
                }
                set.add(new UniqueIdGridCoverageWrapper(coverage));
            }
            rval = coverageCache.get(datasetId);
            if (rval == null) {
                HashSet<UniqueIdGridCoverageWrapper> set = new HashSet<UniqueIdGridCoverageWrapper>(
                        0);
                coverageCache.put(datasetId, set);
                rval = set;

            }
        }
        List<GridCoverage> finalSet = new ArrayList<GridCoverage>(rval.size());
        for (UniqueIdGridCoverageWrapper wrapper : rval) {
            finalSet.add(wrapper.getGridCoverage());
        }
        return finalSet;
    }

    /**
     * Update the cache for the given model with the supplied coverage. Should
     * only be used for non-grid sources being imported as grid data.
     * 
     * @param modelName
     * @param coverage
     */
    public void setCoverage(String modelName, GridCoverage coverage) {
        if (modelName != null && coverage != null) {
            Set<UniqueIdGridCoverageWrapper> set = coverageCache.get(modelName);
            if (set == null) {
                set = new HashSet<UniqueIdGridCoverageWrapper>();
                coverageCache.put(modelName, set);
            }
            set.add(new UniqueIdGridCoverageWrapper(coverage));
        }
    }

    /**
     * Resamples the grid to a new resolution using scale values
     * 
     * @param img
     *            The image to scale
     * @param xScale
     *            The new horizontal resolution
     * @param yScale
     *            The new vertical resolution
     * @return The resampled image
     */
    public GridCoverage2D cropGrid(GridCoverage2D inputCoverage,
            GridGeometry2D outputGeometry, Interpolation interpolation) {
        RenderingHints hint = new RenderingHints(JAI.KEY_BORDER_EXTENDER,
                BorderExtender.createInstance(BorderExtender.BORDER_COPY));
        Operations oper = new Operations(hint);
        return (GridCoverage2D) oper.resample(
                inputCoverage.view(ViewType.GEOPHYSICS),
                outputGeometry.getCoordinateReferenceSystem(),
                outputGeometry,
                interpolation == null ? Interpolation
                        .getInstance(Interpolation.INTERP_BICUBIC)
                        : interpolation);
    }

    /**
     * Resamples the grid to a new resolution using scale values
     * 
     * @param img
     *            The image to scale
     * @param xScale
     *            The new horizontal resolution
     * @param yScale
     *            The new vertical resolution
     * @return The resampled image
     */
    private PlanarImage scaleGrid(RenderedImage img, float xScale, float yScale) {

        PlanarImage scaledImg;

        ParameterBlockJAI param = new ParameterBlockJAI("Scale");
        param.addSource(img);
        param.setParameter("xScale", xScale);
        param.setParameter("yScale", yScale);
        Interpolation interpol = Interpolation
                .getInstance(Interpolation.INTERP_BICUBIC_2);
        RenderingHints hint = new RenderingHints(JAI.KEY_BORDER_EXTENDER,
                BorderExtender.createInstance(BorderExtender.BORDER_COPY));

        param.setParameter("interpolation", interpol);

        scaledImg = JAI.create("Scale", param, hint).getRendering();

        return scaledImg;
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        for (AlertMessage alertMessage : alertMessages) {
            String datasetId = (String) alertMessage.decodedAlert
                    .get(GridConstants.DATASET_ID);
            GridCoverage coverage = (GridCoverage) alertMessage.decodedAlert
                    .get(GridConstants.LOCATION);
            Set<UniqueIdGridCoverageWrapper> set = coverageCache.get(datasetId);
            if (set != null && coverage != null) {
                set.add(new UniqueIdGridCoverageWrapper(coverage));
            }
        }
    }

    // This class exists so that two coverages that are otherwise equal can be
    // stored together in a set if they have different IDs.
    private static class UniqueIdGridCoverageWrapper {
        private final GridCoverage gridCoverage;

        public UniqueIdGridCoverageWrapper(GridCoverage gridCoverage) {
            super();
            this.gridCoverage = gridCoverage;
        }

        public GridCoverage getGridCoverage() {
            return gridCoverage;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((gridCoverage == null) ? 0 : gridCoverage.hashCode());
            result = prime
                    * result
                    + ((gridCoverage.getId() == null) ? 0 : gridCoverage
                            .getId().hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            UniqueIdGridCoverageWrapper other = (UniqueIdGridCoverageWrapper) obj;
            if (gridCoverage == null) {
                if (other.gridCoverage != null)
                    return false;
            } else if (!gridCoverage.equals(other.gridCoverage)) {
                return false;
            } else if (!gridCoverage.getId().equals(
                    other.getGridCoverage().getId())) {
                return false;
            }
            return true;
        }

    }

}
