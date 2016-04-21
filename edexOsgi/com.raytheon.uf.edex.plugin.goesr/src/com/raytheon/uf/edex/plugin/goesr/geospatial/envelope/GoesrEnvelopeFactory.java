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
package com.raytheon.uf.edex.plugin.goesr.geospatial.envelope;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;

/**
 * Because there are multiple ways to define an envelope within a GOES-R
 * {@link NetcdfFile} a common interface is needed for extracting this
 * inofrmation using different methodoligies.
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
public interface GoesrEnvelopeFactory {

    /**
     * Default conversion value that is used to convert from an angle between
     * the satellite and a point of earth into a meter spacing. This value is an
     * approximation and a better value can usually be determined by using the
     * actual orbital height.
     */
    public static final double RADIANS_PER_KM_SPACING = 28 * 1e-6;

    public GoesrEnvelope getEnvelope(NetcdfFile cdfFile,
            CoordinateReferenceSystem crs) throws GoesrProjectionException;

}
