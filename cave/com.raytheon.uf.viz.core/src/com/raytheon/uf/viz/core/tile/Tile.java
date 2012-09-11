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
package com.raytheon.uf.viz.core.tile;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * Object that represents a single Tile. Contains a level and GridGeometry as
 * well as a border which is used in {@link #intersects(Geometry)}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class Tile {

    public final int tileLevel;

    public final GridGeometry2D tileGeometry;

    public final List<PreparedGeometry> tileBorder;

    /**
     * Create a Tile specifying a border for intersection checks
     * 
     * @param tileLevel
     * @param tileGeometry
     * @param tileBorder
     */
    public Tile(int tileLevel, GridGeometry2D tileGeometry, Geometry tileBorder) {
        this.tileLevel = tileLevel;
        this.tileGeometry = tileGeometry;
        if (tileBorder != null) {
            int num = tileBorder.getNumGeometries();
            this.tileBorder = new ArrayList<PreparedGeometry>(num);
            for (int n = 0; n < num; ++n) {
                this.tileBorder.add(PreparedGeometryFactory.prepare(tileBorder
                        .getGeometryN(n)));
            }
        } else {
            this.tileBorder = null;
        }
    }

    /**
     * Creates a Tile with no border. intersects calls will always return true
     * 
     * @param tileLevel
     * @param tileGeometry
     */
    public Tile(int tileLevel, GridGeometry2D tileGeometry) {
        this(tileLevel, tileGeometry, null);
    }

    public Rectangle getRectangle() {
        GridEnvelope env = tileGeometry.getGridRange();
        return new Rectangle(env.getLow(0), env.getLow(1), env.getSpan(0),
                env.getSpan(1));
    }

    /**
     * Checks if the geometry intersects the Tile. Tiles without a
     * {@link #tileBorder} will always return true
     * 
     * @param geometry
     * @return
     */
    public boolean intersects(Geometry geometry) {
        if (tileBorder == null) {
            return true;
        }
        for (PreparedGeometry border : tileBorder) {
            if (border.intersects(geometry)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks to see if the x/y coordinate is contained by the Tile's CRS
     * Envelope
     * 
     * @param x
     * @param y
     * @return
     */
    public boolean crsContains(double x, double y) {
        Envelope env = tileGeometry.getEnvelope();
        return env.getMinimum(0) <= x && env.getMaximum(0) >= x
                && env.getMinimum(1) <= y && env.getMaximum(1) >= y;
    }

    /**
     * Checks to see if the Coordinate is contained by the Tile's CRS Envelope
     * 
     * @param c
     * @return
     */
    public boolean crsContains(Coordinate c) {
        return crsContains(c.x, c.y);
    }

    /**
     * Checks to see if the x/y coordinate is contained by the Tile's grid
     * envelope
     * 
     * @param gridX
     * @param gridY
     * @return
     */
    public boolean gridContains(int gridX, int gridY) {
        GridEnvelope ge = tileGeometry.getGridRange();
        return ge.getLow(0) <= gridX && ge.getHigh(0) >= gridX
                && ge.getLow(1) <= gridY && ge.getHigh(1) >= gridY;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((tileGeometry == null) ? 0 : tileGeometry.hashCode());
        result = prime * result + tileLevel;
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Tile other = (Tile) obj;
        if (tileGeometry == null) {
            if (other.tileGeometry != null)
                return false;
        } else if (!tileGeometry.equals(other.tileGeometry))
            return false;
        if (tileLevel != other.tileLevel)
            return false;
        return true;
    }

}
