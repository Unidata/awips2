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
package com.raytheon.uf.edex.plugin.goesr.geospatial;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.plugin.satellite.dao.SatMapCoverageDao;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;
import com.raytheon.uf.edex.plugin.goesr.geospatial.crs.GeostationaryCrsFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.crs.GoesrCrsFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.crs.LambertConformalCrsFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.crs.MercatorCrsFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.crs.NorthPolarStereographicCrsFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.envelope.DimensionEnvelopeFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.envelope.GoesrEnvelope;
import com.raytheon.uf.edex.plugin.goesr.geospatial.envelope.GoesrEnvelopeFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.envelope.ImageBoundsEnvelopeFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.envelope.ProductCenterEnvelopeFactory;
import com.raytheon.uf.edex.plugin.goesr.geospatial.envelope.TileCenterEnvelopeFactory;

/**
 * 
 * In a GOESR NetCDF file the geolocation information is extracted as two
 * separate, yet equally important entities. The crs which defines the
 * translation onto a spheroid and the envelope which defines the coverage of
 * the product. This is their factory.
 * 
 * Create the {@link SatMapCoverage} corresponding to the projection information
 * contained in the GOES-R netCDF file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 01, 2012  796      jkorman     Initial creation
 * Jul 05, 2013  2123     mschenke    Refactored to have CRS factory for each type of CRS
 * Oct 29, 2014  3770     bsteffen    Pass more attributes to the projection.
 * Apr 17, 2015  4336     bsteffen    Split out crs and envelope creation into distinct factories.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class GoesrProjectionFactory {

    private static final transient Logger logger = LoggerFactory
            .getLogger(GoesrProjectionFactory.class);

    private static final boolean DEBUG_ENVELOPES = Boolean
            .getBoolean("goesr.projection.debug.envelopes");

    private final Map<String, GoesrCrsFactory> crsMap = new HashMap<>();

    private final List<GoesrEnvelopeFactory> envelopeList = new ArrayList<>(3);

    private SatMapCoverageDao satDao;

    /**
     * Create an instance of this factory.
     */
    public GoesrProjectionFactory() {
        satDao = new SatMapCoverageDao();

        GoesrCrsFactory geostationary = new GeostationaryCrsFactory();
        GoesrCrsFactory lambert = new LambertConformalCrsFactory();
        GoesrCrsFactory mercator = new MercatorCrsFactory();
        GoesrCrsFactory polar = new NorthPolarStereographicCrsFactory();
        /*
         * These are the values of the grid_mapping_name attribute within the
         * grid mapping variable. These are consistent for all known goesr
         * products.
         */
        crsMap.put("geostationary", geostationary);
        crsMap.put("lambert_conformal_conic", lambert);
        crsMap.put("polar_stereographic", polar);
        crsMap.put("mercator", mercator);
        /*
         * These are the names of the grid mapping variable for sectorized CMI,
         * these are not consistent for other data types. It may be safe to
         * remove these and rely only on the names above.
         */
        crsMap.put("lambert_projection", lambert);
        crsMap.put("mercator_projection", mercator);
        crsMap.put("polar_projection", polar);
        crsMap.put("fixedgrid_projection", geostationary);

        envelopeList.add(new ProductCenterEnvelopeFactory());
        envelopeList.add(new TileCenterEnvelopeFactory());
        envelopeList.add(new ImageBoundsEnvelopeFactory());
        envelopeList.add(new DimensionEnvelopeFactory());
    }

    public GoesrCrsFactory registerCrsFactory(String name,
            GoesrCrsFactory crsFactory) {
        crsMap.put(name, crsFactory);
        return crsFactory;
    }

    public SatMapCoverage getCoverage(NetcdfFile cdfFile, String projName)
            throws GoesrProjectionException {
        Variable projection = cdfFile.findVariable(projName);
        if (projection != null) {
            GoesrCrsFactory crsFactory = null;
            Attribute attr = projection.findAttribute("grid_mapping_name");
            if (attr != null) {
                crsFactory = crsMap.get(attr.getStringValue());
            }
            if (crsFactory == null) {
                crsFactory = crsMap.get(projName);
                if (crsFactory == null) {
                    String message;
                    if (attr != null) {
                        message = String.format(
                                "Invalid projection identifier [%s.%s].",
                                projName, attr.getStringValue());
                    } else {
                        message = String
                                .format("Invalid projection identifier [%s].",
                                        projName);
                    }
                    throw new GoesrProjectionException(message);
                }
            }
            CoordinateReferenceSystem crs = crsFactory
                    .constructCoordinateReferenceSystem(projection);
            if (DEBUG_ENVELOPES) {
                debugEnvelopes(cdfFile, crs);
            }
            for (GoesrEnvelopeFactory envelopeFactory : envelopeList) {
                GoesrEnvelope envelope = envelopeFactory.getEnvelope(cdfFile,
                        crs);
                if (envelope != null) {
                    envelope.normalize();
                    SatMapCoverage coverage = new SatMapCoverage();
                    coverage.setCrsWKT(crs.toWKT());
                    coverage.setDx(envelope.getDx());
                    coverage.setDy(envelope.getDy());
                    coverage.setMinX(envelope.getMinX());
                    coverage.setMinY(envelope.getMinY());
                    coverage.setNx(envelope.getNx());
                    coverage.setNy(envelope.getNy());
                    /* Trigger generation of a location. */
                    coverage.getLocation();
                    try {
                        return satDao.getOrCreateCoverage(coverage);
                    } catch (Exception e) {
                        throw new GoesrProjectionException(
                                "Could not create coverage", e);
                    }
                }
            }
            throw new GoesrProjectionException("Unable to create envelope.");
        } else {
            throw new GoesrProjectionException("Projection variable was null");
        }
    }

    /**
     * As long as there exists multiple envelope factories, it is useful to be
     * able to compare them for different sample files and verify the results
     * match.
     * 
     * @param cdfFile
     * @param crs
     */
    private void debugEnvelopes(NetcdfFile cdfFile,
            CoordinateReferenceSystem crs) {
        StringBuilder builder = new StringBuilder();
        builder.append("Envelope comparison for " + cdfFile.getLocation()
                + " ********************\n");
        for (GoesrEnvelopeFactory envelopeFactory : envelopeList) {
            builder.append(envelopeFactory.getClass().getSimpleName());
            builder.append(": ");
            GoesrEnvelope envelope = null;
            try {
                envelope = envelopeFactory.getEnvelope(cdfFile, crs);
            } catch (GoesrProjectionException e) {
                builder.append(e.getLocalizedMessage()).append(": ");

            }
            if (envelope != null) {
                envelope.normalize();
                builder.append(envelope.toString());
            } else {
                builder.append("No Envelope");
            }
            builder.append("\n");
        }
        builder.append("****************************************\n");
        logger.info(builder.toString());
    }


}
