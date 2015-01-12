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
package com.raytheon.uf.common.dataplugin.npp.viirs.projection;

import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSSpatialCoverage;
import com.raytheon.uf.common.geospatial.MapUtil;

/**
 * Factory for constructing CRSs for the VIIRS projection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2012            mschenke     Initial creation
 * Mar 27, 2014 2015       njensen      Made semi minor/major a constant
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSMapProjectionFactory {

    /** Using single factory is faster due to internal caching */
    private static DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();

    /**
     * MapProjection requires semi_major/minor but our projection does not use
     * them
     */
    protected static final double SEMI_MINOR_MAJOR_VALUE = 1.0;

    public static ProjectedCRS construct(VIIRSSpatialCoverage record)
            throws FactoryException {
        try {
            ParameterValueGroup group = dmtFactory
                    .getDefaultParameters(VIIRSMapProjection.VIIRS_MAP_PROJECTION_GROUP);
            group.parameter(VIIRSMapProjection.CENTER_LATITUDES).setValue(
                    record.getCenterLatitudes());
            group.parameter(VIIRSMapProjection.CENTER_LONGITUDES).setValue(
                    record.getCenterLongitudes());
            group.parameter(VIIRSMapProjection.CENTER_LENGTH).setValue(
                    record.getNy());
            group.parameter(VIIRSMapProjection.DIRECTIONS).setValue(
                    record.getDirections());
            group.parameter(VIIRSMapProjection.RESOLUTION).setValue(
                    record.getDy().doubleValue());
            group.parameter(VIIRSMapProjection.SEMI_MAJOR).setValue(
                    SEMI_MINOR_MAJOR_VALUE);
            group.parameter(VIIRSMapProjection.SEMI_MINOR).setValue(
                    SEMI_MINOR_MAJOR_VALUE);
            return MapUtil.constructProjection(
                    VIIRSMapProjection.PROJECTION_NAME, group);
        } catch (Exception e) {
            throw new FactoryException(
                    "Error constructing CRS for spatial record: "
                            + e.getLocalizedMessage(), e);
        }
    }

}
