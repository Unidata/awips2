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
package com.raytheon.uf.common.dataplugin.pointset.traingulate;

import com.vividsolutions.jts.triangulate.quadedge.QuadEdge;
import com.vividsolutions.jts.triangulate.quadedge.Vertex;

/**
 * A simple index buffer builder which filters out invalid triangles based off a
 * maximum value of the radius of a circle circumscribed on the triangle. The
 * resulting shape is considered an "alpha shape" of the triangulation. To
 * include all triangles, and perform no alpha shaping, a max radius of
 * {@link Double#POSITIVE_INFINITY} can be used, this is done automatically when
 * using the {@link IndexBufferBuilder#IndexBufferBuilder(int)} constructor.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class IndexBufferBuilder extends AbstractIndexBufferBuilder {

    private final double maxRadius;

    public IndexBufferBuilder(int size) {
        super(size);
        this.maxRadius = Double.POSITIVE_INFINITY;
    }

    public IndexBufferBuilder(int size, double maxRadius) {
        super(size);
        this.maxRadius = maxRadius;
    }

    @Override
    public void visit(QuadEdge[] triEdges) {
        Vertex a = triEdges[0].orig();
        Vertex b = triEdges[1].orig();
        Vertex c = triEdges[2].orig();
        double circumRadius = 0.0;
        if (maxRadius > 0) {
            Vertex center = a.circleCenter(b, c);
            circumRadius = a.getCoordinate().distance(center.getCoordinate());
        }
        if (maxRadius >= circumRadius) {
            addTriangle((int) a.getZ(), (int) b.getZ(), (int) c.getZ());
        }
    }
}