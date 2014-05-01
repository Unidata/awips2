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
package com.raytheon.uf.viz.core.rsc.hdf5;

import java.awt.Rectangle;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;

import com.raytheon.uf.viz.core.PixelCoverage;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * ImageTile is an object that represents an image (or part of an image)
 * geospatially, it contains a grid geometry that represents the projection and
 * size of the image and a PixelCoverage which contains the screen projected
 * data for the image. Because a single ImageTile can represent multiple images,
 * this class does not contain the actual image it represents so the tile can be
 * shared between images
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 13, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ImageTile {

    public GridGeometry2D imageGeometry;

    public PixelCoverage coverage;

    /**
     * Checks to see if the x/y coordinate is contained by the ImageTile's CRS
     * Envelope
     * 
     * @param x
     * @param y
     * @return
     */
    public boolean contains(double x, double y) {
        Envelope env = imageGeometry.getEnvelope();
        return env.getMinimum(0) <= x && env.getMaximum(0) >= x
                && env.getMinimum(1) <= y && env.getMaximum(1) >= y;
    }

    /**
     * Checks to see if the Coordinate is contained by the ImageTile's CRS
     * Envelope
     * 
     * @param c
     * @return
     */
    public boolean contains(Coordinate c) {
        return contains(c.x, c.y);
    }

    /**
     * Set the grid geometry of the tile given the image rectangle and the
     * referenced envelope
     * 
     * @param rect
     * @param env
     */
    public void setGridGeometry(Rectangle rect, ReferencedEnvelope env) {
        GeneralGridEnvelope gge = new GeneralGridEnvelope(rect);
        GeneralEnvelope ge = new GeneralEnvelope(env);
        imageGeometry = new GridGeometry2D(gge, ge);
    }

    /**
     * Set the image geometry
     * 
     * @param imageGeometry
     */
    public void setGridGeometry(GridGeometry2D imageGeometry) {
        this.imageGeometry = imageGeometry;
    }

    /**
     * Get the image rectangle. This could be a subsection of a larger image so
     * x and y may not be 0,0
     * 
     * @return
     */
    public Rectangle getRectangle() {
        GridEnvelope env = imageGeometry.getGridRange();
        return new Rectangle(env.getLow(0), env.getLow(1), env.getSpan(0),
                env.getSpan(1));
    }

    /**
     * Get the spatially referenced envelope of the image tile
     * 
     * @return
     */
    public ReferencedEnvelope getEnvelope() {
        return new ReferencedEnvelope(imageGeometry.getEnvelope());
    }

    /**
     * Dispose the image tile, disposes of the coverage object associated with
     * it
     */
    public void dispose() {
        if (coverage != null) {
            coverage.dispose();
            coverage = null;
        }
    }
}