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

package com.raytheon.uf.viz.core.map;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * Declares the interface for map descriptors
 * 
 * Map Descriptors contain a list of map resources and their attributes as well
 * as attributes and functions related to the map itself
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date          Ticket#     Engineer    Description
 *     ------------	----------	-----------	--------------------------
 *     7/1/06                    chammack    Initial Creation.
 *     1/12/09                   randerso    added getMapManager
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 * 
 * 
 */
public interface IMapDescriptor extends IDescriptor {

    /**
     * Convenience method to transform a set of world coordinates to pixel
     * coordinates
     * 
     * @param worldPixel
     *            an array of two of world coordinates (x, y)
     * 
     * @return the pixel coordinates (x, y)
     */
    public abstract double[] worldToPixel(double[] world,
            CoordinateReferenceSystem crs);

    /**
     * Convenience method to transform a set of pixel coordinates to world
     * coordinates
     * 
     * @param pixel
     *            the pixel coordinates (x, y)
     * @param crs
     *            a coordinate reference system the extent is in
     * @return the world coordinates (x, y)
     */
    public abstract double[] pixelToWorld(double[] pixel,
            CoordinateReferenceSystem crs);

    /**
     * Convenience method to transform a pixel extent to world extent
     * 
     * @param extent
     *            the pixel extent to transform
     * @return a latitude-longitude extent
     */
    public abstract Envelope pixelToWorld(IExtent extent);

    /**
     * Convenience method to transform a pixel extent to world extent
     * 
     * @param extent
     *            the pixel extent to transform
     * @param crs
     *            a coordinate reference system the extent is in
     * @return a latitude-longitude extent
     */
    public abstract Envelope pixelToWorld(IExtent extent,
            CoordinateReferenceSystem crs);

    /**
     * Convenience method to transform a world extent to pixel extent
     * 
     * @param extent
     *            a latitude longitude extent to transform
     * @return a pixel coverage
     */
    public abstract PixelCoverage worldToPixel(Envelope extent);

    /**
     * Convenience method to transform a world extent to pixel extent
     * 
     * @param extent
     *            a latitude longitude extent to transform
     * @param crs
     *            a coordinate reference system the extent is in
     * @return a pixel coverage
     */
    public abstract PixelCoverage worldToPixel(Envelope extent,
            CoordinateReferenceSystem crs);

    /**
     * Set the projection
     * 
     * @param crs
     *            Coordinate reference system of the projection
     * @param ll
     *            lower left coordinate of the projection in lon/lat
     * @param ur
     *            upper right coordinate of the projection in lon/lat
     * @throws FactoryException
     * @throws TransformException
     * @throws VizException
     */
    public void setProjection(CoordinateReferenceSystem crs, Coordinate ll,
            Coordinate ur) throws FactoryException, TransformException,
            VizException;

    /**
     * Get the current map width
     * 
     * @return the current map width in meters
     */
    public abstract int getMapWidth();

}
