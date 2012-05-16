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

package com.raytheon.viz.core.gl.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * Represents a filled shape or set of shapes
 * 
 * <P>
 * <I><B>NOTE:</B> This is much slower than the GLWireframeShape because all
 * shapes must undergo tesselation</I>
 * 
 * <pre>
 * 
 *      SOFTWARE HISTORY
 *     
 *      Date          Ticket#     Engineer    Description
 *      ------------	----------	-----------	--------------------------
 *      7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class GLShadedShape extends GLShadedShapeBase implements IShadedShape {

    private boolean mutable;

    private List<RGB> colors = new ArrayList<RGB>();

    public GLShadedShape(GeneralGridGeometry targetGeometry, boolean mutable,
            boolean tessellate) {
        super(targetGeometry, tessellate);
        this.mutable = mutable;
    }

    public GLShadedShape(GLShadedShapeBase that) {
        super(that);
        this.mutable = false;
    }

    @Override
    public boolean isMutable() {
        return mutable;
    }

    @Override
    public boolean isDrawable() {
        return true;
    }

    @Override
    public void dispose() {
        super.dispose();
    }

    @Override
    public void reset() {
        super.dispose();
    }

    @Override
    public void addPolygon(LineString[] lineString, RGB color) {
        colors.add(color);
        super.addPolygon(lineString);
    }

    @Override
    public void addPolygonPixelSpace(LineString[] contours, RGB color) {
        colors.add(color);
        super.addPolygonPixelSpace(contours);
    }

    public void compile() {
        super.compile();
        super.color(colors);
        colors.clear();
    }

}