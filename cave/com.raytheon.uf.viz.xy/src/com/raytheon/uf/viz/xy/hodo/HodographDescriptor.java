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
package com.raytheon.uf.viz.xy.hodo;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.crs.DefaultEngineeringCRS;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.serialization.adapters.GridGeometryAdapter;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class HodographDescriptor extends AbstractDescriptor {

    // The max distance of the hodograph
    private static final double MAX_RANGE = 140.0;

    @XmlElement
    @XmlJavaTypeAdapter(value = GridGeometryAdapter.class)
    protected GeneralGridGeometry geometry;

    public HodographDescriptor(IExtent anExtent) {
        super();
        GeneralEnvelope envelope = new GeneralEnvelope(2);
        envelope.setRange(0, anExtent.getMinX(), anExtent.getMaxX());
        envelope.setRange(1, anExtent.getMinY(), anExtent.getMaxY());
        envelope.setCoordinateReferenceSystem(DefaultEngineeringCRS.CARTESIAN_2D);
        geometry = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { (int) anExtent.getWidth(),
                        (int) anExtent.getHeight() }, false), envelope);
    }

    @Override
    public CoordinateReferenceSystem getCRS() {
        return null;
    }

    @Override
    public GeneralGridGeometry getGridGeometry() {
        return geometry;
    }

    @Override
    public double[] pixelToWorld(double[] pixel) {
        double x = pixel[0];
        double y = pixel[1];
        int xRange = geometry.getGridRange().getSpan(0);
        int yRange = geometry.getGridRange().getSpan(1);
        x = x - xRange / 2;
        y = y - yRange / 2;
        double r = Math.hypot(pixel[0], pixel[1]);
        double a = Math.atan2(pixel[1], pixel[0]);
        a = Math.toDegrees(a) - 90;
        r = r * MAX_RANGE * 2.0 / xRange;
        return new double[] { r, a, 0 };
    }

    @Override
    public double[] worldToPixel(double[] world) {
        double r = world[0];
        double a = world[1];
        int xRange = geometry.getGridRange().getSpan(0);
        int yRange = geometry.getGridRange().getSpan(1);
        // add 90 degrees since 0 is down, not right;
        a = Math.toRadians(a + 90);
        // make r long enough that 100 is as long as xRange
        r = r * xRange / (MAX_RANGE * 2.0);
        double x = r * Math.cos(a);
        double y = r * Math.sin(a);
        // shift 0,0 to the center of the graph
        x = x + xRange / 2;
        y = y + yRange / 2;

        return new double[] { x, y, 0 };
    }

}
