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
package com.raytheon.uf.common.dataplugin.warning.portions;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * Helper class for GisUtil to determine the quadrants of an area.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2013      1963    jsanchez     Initial creation
 * Jun 3, 2013      2029    jsanchez     Fixed incorrect A1 port. Added additional attributes to calculate portions of areas.
 * Dec 4, 2013      2604    jsanchez     Moved out of viz.warngen.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class ImpactedQuadrants {

    protected boolean nne;

    protected boolean ene;

    protected boolean ese;

    protected boolean sse;

    protected boolean ssw;

    protected boolean wsw;

    protected boolean wnw;

    protected boolean nnw;

    protected int nn;

    protected int ss;

    protected int ee;

    protected int ww;

    protected int ne;

    protected int nw;

    protected int se;

    protected int sw;

    /** Indicates if the north portion is impacted */
    protected boolean north;

    /** Indicates if the south portion is impacted */
    protected boolean south;

    /** Indicates if the east portion is impacted */
    protected boolean east;

    /** Indicates if the west portion is impacted */
    protected boolean west;

    /**
     * q is the accumulation of the quadrants,
     */
    protected int q;

    /**
     * qq is the accumulation of the sub quadrants
     */
    protected int qq;

    /**
     * Extreme portions for north.
     */
    protected int nnx;

    /**
     * Extreme portions for south.
     */
    protected int ssx;

    /**
     * Extreme portions for east.
     */
    protected int eex;

    /**
     * Extreme portions for west.
     */
    protected int wwx;

    /**
     * Accumulation of extreme portions.
     */
    protected int xxx;

    /**
     * Indicates if warned area is in central part regardless if warned area is
     * near parentGeom's centroid.
     */
    protected boolean cc = false;

    protected PreparedGeometry centralGeom = null;

    public ImpactedQuadrants() {
        nne = ene = ese = sse = ssw = wsw = wnw = nnw = false;
        nn = ss = ee = ww = ne = nw = se = sw = 0;
        nnx = ssx = eex = wwx = 0;
        north = south = east = west = false;
        xxx = 0;
    }

    /**
     * Divides the parentGeom into 8 quadrants. Returns an ImpactedQuadrants
     * object to identify which quadrants of parentGeom were intersected by the
     * warnedArea.
     * 
     * @param parentGeom
     *            - the parent geometry such as the impacted county/zone
     * @param warnedArea
     *            - the intersection geometry of the hatched warned geometry and
     *            the parent geometry
     * @param useCentral
     *            - boolean flag to allow CENTRAL portions to be included.
     * @return
     */
    public static ImpactedQuadrants findImpactedQuadrants(Geometry parentGeom,
            Geometry warnedArea, boolean useCentral) {
        // Find the bounds of the parentGeom to help create the bounds of a
        // quadrant.
        Envelope envelopeInternal = parentGeom.getEnvelopeInternal();
        double minLat = envelopeInternal.getMinY();
        double maxLat = envelopeInternal.getMaxY();
        double minLon = envelopeInternal.getMinX();
        double maxLon = envelopeInternal.getMaxX();

        // Create a PreparedGeomery for each quadrant in parentGeom
        Coordinate centroid = envelopeInternal.centre();
        Coordinate c1 = new Coordinate(minLon, maxLat);
        Coordinate c2 = new Coordinate(centroid.x, maxLat);
        Coordinate c3 = new Coordinate(maxLon, maxLat);
        Coordinate c4 = new Coordinate(maxLon, centroid.y);
        Coordinate c5 = new Coordinate(maxLon, minLat);
        Coordinate c6 = new Coordinate(centroid.x, minLat);
        Coordinate c7 = new Coordinate(minLon, minLat);
        Coordinate c8 = new Coordinate(minLon, centroid.y);

        PreparedGeometry nnwQuad = createQuadrant(c1, c2, centroid);
        PreparedGeometry nneQuad = createQuadrant(c2, c3, centroid);
        PreparedGeometry eneQuad = createQuadrant(c3, c4, centroid);
        PreparedGeometry eseQuad = createQuadrant(c4, c5, centroid);
        PreparedGeometry sseQuad = createQuadrant(c5, c6, centroid);
        PreparedGeometry sswQuad = createQuadrant(c6, c7, centroid);
        PreparedGeometry wswQuad = createQuadrant(c7, c8, centroid);
        PreparedGeometry wnwQuad = createQuadrant(c8, c1, centroid);

        // Determine if the warnedArea intersected the quadrant.
        ImpactedQuadrants impactedQuadrants = new ImpactedQuadrants();
        impactedQuadrants.nne = nneQuad.intersects(warnedArea);
        impactedQuadrants.ene = eneQuad.intersects(warnedArea);
        impactedQuadrants.ese = eseQuad.intersects(warnedArea);
        impactedQuadrants.sse = sseQuad.intersects(warnedArea);
        impactedQuadrants.ssw = sswQuad.intersects(warnedArea);
        impactedQuadrants.wsw = wswQuad.intersects(warnedArea);
        impactedQuadrants.wnw = wnwQuad.intersects(warnedArea);
        impactedQuadrants.nnw = nnwQuad.intersects(warnedArea);

        // The following is A1 ported code used
        if (impactedQuadrants.nne || impactedQuadrants.ene) {
            impactedQuadrants.ne = 1;
        }
        if (impactedQuadrants.sse || impactedQuadrants.ese) {
            impactedQuadrants.se = 1;
        }
        if (impactedQuadrants.nnw || impactedQuadrants.wnw) {
            impactedQuadrants.nw = 1;
        }
        if (impactedQuadrants.ssw || impactedQuadrants.wsw) {
            impactedQuadrants.sw = 1;
        }
        if (impactedQuadrants.nne || impactedQuadrants.nnw) {
            impactedQuadrants.nn = 1;
        }
        if (impactedQuadrants.sse || impactedQuadrants.ssw) {
            impactedQuadrants.ss = 1;
        }
        if (impactedQuadrants.wnw || impactedQuadrants.wsw) {
            impactedQuadrants.ww = 1;
        }
        if (impactedQuadrants.ene || impactedQuadrants.ese) {
            impactedQuadrants.ee = 1;
        }

        // Accumulates the quadrants and subquadrants
        impactedQuadrants.q = impactedQuadrants.ne + impactedQuadrants.nw
                + impactedQuadrants.se + impactedQuadrants.sw;
        impactedQuadrants.qq = impactedQuadrants.nn + impactedQuadrants.ss
                + impactedQuadrants.ee + impactedQuadrants.ww;

        // Identify if central.
        if (useCentral) {
            identifyCentral(impactedQuadrants, parentGeom, warnedArea);
        }

        // Identify extremes in use.
        identifyExtremes(impactedQuadrants, envelopeInternal, warnedArea);

        identifyAreaIntersection(impactedQuadrants, envelopeInternal,
                warnedArea);

        return impactedQuadrants;
    }

    /**
     * Creates a quadrant based on 3 coordinates.
     * 
     * @param c1
     * @param c2
     * @param c3
     * @return
     */
    private static PreparedGeometry createQuadrant(Coordinate c1,
            Coordinate c2, Coordinate c3) {
        Coordinate[] coords = new Coordinate[] { c1, c2, c3, c1 };
        GeometryFactory gf = new GeometryFactory();
        Geometry geom = gf.createPolygon(gf.createLinearRing(coords), null);
        return PreparedGeometryFactory.prepare(geom);
    }

    /**
     * Creates a geometry that is the center of the parentGeom.
     * 
     * @param parentGeom
     * @return
     */
    private static PreparedGeometry createCentralGeometry(Geometry parentGeom) {
        Envelope envelope = parentGeom.getEnvelopeInternal();
        Coordinate c = parentGeom.getCentroid().getCoordinate();
        double percentage = 0.50;
        double deltaHeight = envelope.getHeight() * percentage / 2;
        double deltaWidtth = envelope.getWidth() * percentage / 2;
        Coordinate c1 = new Coordinate(c.x - deltaWidtth, c.y + deltaHeight);
        Coordinate c2 = new Coordinate(c.x + deltaWidtth, c.y + deltaHeight);
        Coordinate c3 = new Coordinate(c.x - deltaWidtth, c.y - deltaHeight);
        Coordinate c4 = new Coordinate(c.x + deltaWidtth, c.y - deltaHeight);
        Coordinate[] coords = new Coordinate[] { c1, c2, c3, c4, c1 };
        GeometryFactory gf = new GeometryFactory();
        Geometry geom = gf.createPolygon(gf.createLinearRing(coords), null);
        geom.setUserData(parentGeom.getUserData());
        return PreparedGeometryFactory.prepare(geom);
    }

    /**
     * Identifies if the warnedArea intersects the central portions of the
     * parentGeom.
     * 
     * @param impactedQuadrants
     * @param parentGeom
     * @param warnedArea
     */
    private static void identifyCentral(ImpactedQuadrants impactedQuadrants,
            Geometry parentGeom, Geometry warnedArea) {
        impactedQuadrants.centralGeom = createCentralGeometry(parentGeom);
        Envelope centralEnvelope = impactedQuadrants.centralGeom.getGeometry()
                .getEnvelopeInternal();
        Envelope warnedEnvelope = warnedArea.getEnvelopeInternal();
        if ((warnedEnvelope.getMaxX() < centralEnvelope.getMaxX() && warnedEnvelope
                .getMinX() > centralEnvelope.getMinX())
                || (warnedEnvelope.getMaxY() < centralEnvelope.getMaxY() && warnedEnvelope
                        .getMinY() > centralEnvelope.getMinY())) {
            impactedQuadrants.cc = true;
        }
    }

    /**
     * Identifies portions as extreme if the centroid of the warnedArea is
     * within 5 % of the parentEnvelopeInternal.
     * 
     * @param impactedQuadrants
     * @param parentEnvelopeInternal
     * @param warnedArea
     */
    private static void identifyExtremes(ImpactedQuadrants impactedQuadrants,
            Envelope parentEnvelopeInternal, Geometry warnedArea) {
        Coordinate warnedAreaCentroid = warnedArea.getCentroid()
                .getCoordinate();
        double deltaY = parentEnvelopeInternal.getHeight() * 0.05;
        double deltaX = parentEnvelopeInternal.getWidth() * 0.05;
        if (warnedAreaCentroid.y > parentEnvelopeInternal.getMaxY() - deltaY) {
            impactedQuadrants.nnx = 1;
        }

        if (warnedAreaCentroid.y < parentEnvelopeInternal.getMinY() + deltaY) {
            impactedQuadrants.ssx = 1;
        }

        if (warnedAreaCentroid.x > parentEnvelopeInternal.getMaxX() - deltaX) {
            impactedQuadrants.eex = 1;
        }

        if (warnedAreaCentroid.x < parentEnvelopeInternal.getMinX() + deltaX) {
            impactedQuadrants.wwx = 1;
        }

        impactedQuadrants.xxx = impactedQuadrants.nnx + impactedQuadrants.ssx
                + impactedQuadrants.eex + impactedQuadrants.wwx;
    }

    /**
     * Identifies portions of the parent envelope which is 15% from each edge.
     * 
     * @param impactedQuadrants
     * @param parentEnvelopeInternal
     * @param warnedArea
     */
    private static void identifyAreaIntersection(
            ImpactedQuadrants impactedQuadrants,
            Envelope parentEnvelopeInternal, Geometry warnedArea) {

        double deltaY = parentEnvelopeInternal.getHeight() * 0.15;
        double deltaX = parentEnvelopeInternal.getWidth() * 0.15;

        double minLat = parentEnvelopeInternal.getMinY();
        double maxLat = parentEnvelopeInternal.getMaxY();
        double minLon = parentEnvelopeInternal.getMinX();
        double maxLon = parentEnvelopeInternal.getMaxX();

        Coordinate c1 = new Coordinate(minLon, maxLat); // upper left
        Coordinate c2 = new Coordinate(maxLon, maxLat); // upper right
        Coordinate c3 = new Coordinate(maxLon, minLat); // lower right
        Coordinate c4 = new Coordinate(minLon, minLat); // lower left

        Coordinate c5 = new Coordinate(c2.x, c2.y - deltaY);
        Coordinate c6 = new Coordinate(c1.x, c1.y - deltaY);
        Coordinate c7 = new Coordinate(c4.x, c4.y + deltaY);
        Coordinate c8 = new Coordinate(c3.x, c3.y + deltaY);
        Coordinate c9 = new Coordinate(c2.x - deltaX, c2.y);
        Coordinate c10 = new Coordinate(c3.x - deltaX, c3.y);
        Coordinate c11 = new Coordinate(c1.x + deltaX, c1.y);
        Coordinate c12 = new Coordinate(c4.x + deltaX, c4.y);

        PreparedGeometry north = createPortionMasks(c1, c2, c5, c6);
        PreparedGeometry south = createPortionMasks(c7, c8, c3, c4);
        PreparedGeometry east = createPortionMasks(c9, c2, c3, c10);
        PreparedGeometry west = createPortionMasks(c1, c11, c12, c4);

        impactedQuadrants.north = north.intersects(warnedArea);
        impactedQuadrants.south = south.intersects(warnedArea);
        impactedQuadrants.east = east.intersects(warnedArea);
        impactedQuadrants.west = west.intersects(warnedArea);
    }

    /**
     * Creates a PreparedGeometry object from 4 coordinates
     * 
     * @param c1
     *            - upper left
     * @param c2
     *            - upper right
     * @param c3
     *            - lower right
     * @param c4
     *            - lower left
     * @return
     */
    private static PreparedGeometry createPortionMasks(Coordinate c1,
            Coordinate c2, Coordinate c3, Coordinate c4) {
        Coordinate[] coords = new Coordinate[] { c1, c2, c3, c4, c1 };
        GeometryFactory gf = new GeometryFactory();
        Geometry geom = gf.createPolygon(gf.createLinearRing(coords), null);
        return PreparedGeometryFactory.prepare(geom);
    }
}
