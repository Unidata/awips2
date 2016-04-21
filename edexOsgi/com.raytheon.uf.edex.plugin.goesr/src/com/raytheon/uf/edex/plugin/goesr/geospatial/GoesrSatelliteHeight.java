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

import javax.measure.quantity.Length;
import javax.measure.unit.Unit;

import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.projection.MapProjection;
import org.opengis.parameter.ParameterValue;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.projection.Geostationary;

/**
 * Utility class for extracting the satellite height from the
 * {@link CoordinateReferenceSystem}. Several pieces of the decoder need this
 * information and don't have a better way to get it.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GoesrSatelliteHeight {

    public static double getOrbitalHeight(CoordinateReferenceSystem crs,
            Unit<Length> unit) {
        MapProjection worldProjection = CRS.getMapProjection(crs);
        if (worldProjection instanceof Geostationary) {
            ParameterValueGroup group = worldProjection.getParameterValues();
            ParameterValue<?> orbitalHeight = group
                    .parameter(Geostationary.ORBITAL_HEIGHT);
            return orbitalHeight.doubleValue(unit);
        }
        return Double.NaN;
    }
}
