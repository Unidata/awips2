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
package com.raytheon.viz.satellite.inventory;

import java.awt.geom.Rectangle2D;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.inventory.IGridGeometryProviderComparable;

/**
 * A wrapper around {@link SatMapCoverage} that can be compared with other
 * satellite coverages for derived parameters to be able to calculate between
 * data at different resolution. It currently always chooses the higher
 * resolutioon coverage when doing comparisons.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 11, 2014  2947     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ComparableSatMapCoverage implements
        IGridGeometryProviderComparable {

    private final SatMapCoverage coverage;


    public ComparableSatMapCoverage(SatMapCoverage coverage) {
        this.coverage = coverage;
    }

    @Override
    public GridGeometry2D getGridGeometry() {
        return coverage.getGridGeometry();
    }

    @Override
    public IGridGeometryProvider compare(IGridGeometryProvider other) {
        if (other.equals(this)) {
            return this;
        }
        if (other instanceof ComparableSatMapCoverage) {
            ComparableSatMapCoverage otherCoverage = (ComparableSatMapCoverage) other;
            if (compatibleArea(otherCoverage)) {
                if (getResolution() > otherCoverage.getResolution()) {
                    return this;
                } else if (getResolution() < otherCoverage.getResolution()) {
                    return other;
                }
                /* Resolutions are the same */
                Envelope2D env = getEnvelope();
                Envelope2D otherEnv = otherCoverage.getEnvelope();
                if (area(env) < area(otherEnv)) {
                    return this;
                } else if (area(env) > area(otherEnv)) {
                    return other;
                }
                /* Area is the same. */
                /*
                 * there is no meaningful way to pick one so start checking
                 * meaningless things.
                 */
                if (env.x < otherEnv.x) {
                    return this;
                } else if (env.x > otherEnv.x) {
                    return other;
                }
                if (env.y < otherEnv.y) {
                    return this;
                } else if (env.y > otherEnv.y) {
                    return other;
                }
                if (env.width < otherEnv.width) {
                    return this;
                } else if (env.width > otherEnv.width) {
                    return other;
                }
                /* This is nuts, why aren't they equal, give up. */
            }
        }
        return null;
    }

    public SatMapCoverage getCoverage() {
        return coverage;
    }

    protected CoordinateReferenceSystem getCRS() {
        return coverage.getGridGeometry().getCoordinateReferenceSystem();
    }

    protected boolean compatibleArea(ComparableSatMapCoverage other) {
        if (!other.getCRS().equals(getCRS())) {
            return false;
        }
        Rectangle2D i = other.getEnvelope().createIntersection(getEnvelope());
        if (i == null || i.isEmpty()) {
            return false;
        }
        /* Intersection must cover at least 25% of area. */
        double a = area(i) * 4;
        if (a > area(getEnvelope()) || a > area(other.getEnvelope())) {
            return true;
        } else {
            return false;
        }
    }

    protected Envelope2D getEnvelope() {
        return coverage.getGridGeometry().getEnvelope2D();
    }

    /**
     * @return the resolution of the grid represented by this coverage in
     *         pixel/(m²).
     */
    protected double getResolution() {
        GridGeometry2D gg = coverage.getGridGeometry();
        Envelope2D e = gg.getEnvelope2D();
        GridEnvelope2D r = gg.getGridRange2D();
        return area(r) / area(e);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((coverage == null) ? 0 : coverage.hashCode());
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
        ComparableSatMapCoverage other = (ComparableSatMapCoverage) obj;
        if (coverage == null) {
            if (other.coverage != null)
                return false;
        } else if (!coverage.equals(other.coverage))
            return false;
        return true;
    }

    private static double area(Rectangle2D r) {
        return r.getHeight() * r.getWidth();
    }

}
