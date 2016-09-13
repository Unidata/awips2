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
package com.raytheon.edex.plugin.grib.util;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXB;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * Lookup a region based off the grid name.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2015 5182       tjensen     Initial creation
 * 
 * </pre>
 * 
 * @author tjensen
 * @version 1.0
 */
public class GridRegionLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridRegionLookup.class);

    /** The logger */
    protected transient Logger logger = LoggerFactory.getLogger(getClass());

    private final Map<String, Polygon> regionMaps;

    /** The singleton instance of GribModelLookup **/
    private static GridRegionLookup instance;

    public static synchronized GridRegionLookup getInstance() {
        if (instance == null) {
            instance = new GridRegionLookup();
        }
        return instance;
    }

    private GridRegionLookup() {
        regionMaps = new HashMap<String, Polygon>();
        initRegionList();
    }

    /**
     * Initializes mappings of regions and grids based on information in
     * gribModel files.
     * 
     * Note: This is an extrapolation from existing gribModel information and
     * should not be considered a definitive list of which grids are considered
     * which region.
     */
    private void initRegionList() {
        logger.info("Initializing grib regions");
        long startTime = System.currentTimeMillis();
        LocalizationContext edexStaticBase = PathManagerFactory
                .getPathManager().getContext(
                        LocalizationContext.LocalizationType.EDEX_STATIC,
                        LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext edexStaticSite = PathManagerFactory
                .getPathManager().getContext(
                        LocalizationContext.LocalizationType.EDEX_STATIC,
                        LocalizationContext.LocalizationLevel.SITE);

        LocalizationFile[] regionFiles = PathManagerFactory.getPathManager()
                .listFiles(
                        new LocalizationContext[] { edexStaticBase,
                                edexStaticSite },
                        "grib" + IPathManager.SEPARATOR + "modelRegions", // Win32
                        new String[] { ".xml" }, true, true);

        for (LocalizationFile regionFile : regionFiles) {
            try (InputStream is = regionFile.openInputStream()) {
                GridRegionSet fileSet = JAXB.unmarshal(is, GridRegionSet.class);
                for (GridRegion fileRegion : fileSet.getRegions()) {
                    regionMaps.put(fileRegion.getName(),
                            fileRegion.getRegionGeometry());
                }
            } catch (Exception e) {
                logger.error("Unable to unmarshal grib model region file:"
                        + regionFile, e);
            }
        }
        long endTime = System.currentTimeMillis();
        logger.info("Grib model regions initialized: " + (endTime - startTime)
                + "ms");
    }

    public String determineRegion(GridCoverage gc) {
        // Default to "UNK". Return this if no regions intersect with the grid.
        String bestMatchRegion = "UNK";

        double bestMatchQual = 0;
        Geometry coverageGeo = gc.getGeometry();

        /*
         * If our coverage's projection is Polar Stereographic, we need to
         * create a projection in the Lat/Lon projection before we do our
         * comparison. If envelope intersection fails, log the failure and fall
         * back to using the GridCoverage geometry directly.
         */
        if (PolarStereoGridCoverage.PROJECTION_TYPE.equals(gc
                .getProjectionType())) {
            ReferencedEnvelope worldEnvelope = new ReferencedEnvelope(-180,
                    180, -90, 90, MapUtil.LATLON_PROJECTION);
            try {
                coverageGeo = EnvelopeIntersection.createEnvelopeIntersection(
                        gc.getGridGeometry().getEnvelope(), worldEnvelope);
            } catch (TransformException | FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        double coverageArea = coverageGeo.getArea();

        // Loop over all regions
        for (Map.Entry<String, Polygon> region : regionMaps.entrySet()) {
            Polygon regionGeo = region.getValue();
            Geometry intersection = coverageGeo.intersection(regionGeo);
            // Check if grid coverage intersects with this region
            if (!intersection.isEmpty()) {
                /*
                 * If coverage intersects, determine the 'quality' of the match
                 * by adding together how much of the region is covered by the
                 * intersection and how much of the grid is covered by the
                 * intersection. The higher this sum is, the closer the grid
                 * coverage is to matching the selected region.
                 */
                double matchQuality = (intersection.getArea() / coverageArea)
                        + (intersection.getArea() / regionGeo.getArea());

                /*
                 * If this quality is better than our current best match,
                 * replace our best match with this region.
                 */
                if (matchQuality > bestMatchQual) {
                    bestMatchQual = matchQuality;
                    bestMatchRegion = region.getKey();
                }
            }
        }

        return bestMatchRegion;
    }
}
