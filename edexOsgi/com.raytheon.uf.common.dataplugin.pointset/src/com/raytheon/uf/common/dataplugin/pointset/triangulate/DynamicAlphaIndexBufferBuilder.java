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
package com.raytheon.uf.common.dataplugin.pointset.triangulate;

import java.nio.IntBuffer;
import java.util.Arrays;
import java.util.PriorityQueue;

import com.vividsolutions.jts.triangulate.quadedge.QuadEdge;
import com.vividsolutions.jts.triangulate.quadedge.Vertex;

/**
 * An index buffer builder which attempts to dynamically determine a correct
 * maximum radius to use for filtering triangles to achieve an alpha shape. This
 * can be useful if the spacing of the data is not known or could be
 * particularly distorted.
 * 
 * Similar to the {@link IndexBufferBuilder}, triangles are filtered off of the
 * circumradius. The dynamic filtering is achieved by automatically accepting
 * 90% of the triangles, those with the smallest circumradius. The remaining 10%
 * are only included if their circumradius is less than 4 times the largest
 * radius from the 90%. For example if your radii are 1, 2, 3, 4, 5, 6, 7, 8, 9,
 * 10, then 1-9 will be automatically included and the 10 will also be included
 * because it is less than the maximum acceptable value of 9*4 = 36.
 * 
 * This methodology should not be considered mathematically robust and could be
 * changed in the future. It was derived by fiddling with the numbers with some
 * sample data. The primary advantages of this simplistic approach is that it
 * can be achieved without iterating over the triangles multiple times to
 * calculate statistical values and it easily allows datasets with low deviation
 * to be 100% included in the result. Removing good triangles is considered more
 * dangerous than leaving in large triangles so having a very broad range of
 * acceptance is considered an advantage.
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
public class DynamicAlphaIndexBufferBuilder extends AbstractIndexBufferBuilder {


    private final int maxRadiusHeapSize;

    private final PriorityQueue<RadiusIndexPair> maxRadiusHeap;

    public DynamicAlphaIndexBufferBuilder(int size) {
        super(size);
        maxRadiusHeapSize = Math.max(1, size / 10);
        maxRadiusHeap = new PriorityQueue<>(maxRadiusHeapSize + 1);
    }

    @Override
    public IntBuffer getBuffer() {
        /*
         * Multipliers between 1.5 and 25 produce the same result on all data
         * that has been tested so there is alot of flexibility if future
         * testing reveals a need for a different number.
         */
        double radiusThreshold = maxRadiusHeap.poll().radius * 4;
        while (!maxRadiusHeap.isEmpty()
                && maxRadiusHeap.peek().radius < radiusThreshold) {
            maxRadiusHeap.poll();
        }
        int[] indicesToRemove = new int[maxRadiusHeap.size()];
        for (int i = 0; i < indicesToRemove.length; i += 1) {
            indicesToRemove[i] = maxRadiusHeap.poll().index;
        }
        Arrays.sort(indicesToRemove);
        int bufferSize = buffer.position();
        IntBuffer result = IntBuffer.allocate(bufferSize
                - indicesToRemove.length * 3);
        buffer.position(0);
        for (int index : indicesToRemove) {
            buffer.limit(index);
            result.put(buffer);
            buffer.limit(buffer.capacity());
            buffer.position(index + 3);
        }
        buffer.limit(bufferSize);
        result.put(buffer);
        result.rewind();
        return result;
    }

    @Override
    public void visit(QuadEdge[] triEdges) {
        Vertex a = triEdges[0].orig();
        Vertex b = triEdges[1].orig();
        Vertex c = triEdges[2].orig();
        Vertex center = a.circleCenter(b, c);
        double circumRadius = a.getCoordinate()
                .distance(center.getCoordinate());
        maxRadiusHeap.add(new RadiusIndexPair(buffer.position(), circumRadius));
        if (maxRadiusHeap.size() > maxRadiusHeapSize) {
            maxRadiusHeap.poll();
        }
        addTriangle((int) a.getZ(), (int) b.getZ(), (int) c.getZ());
    }

    private static class RadiusIndexPair implements Comparable<RadiusIndexPair> {

        public final int index;

        public final double radius;

        public RadiusIndexPair(int index, double radius) {
            this.index = index;
            this.radius = radius;
        }

        @Override
        public int compareTo(RadiusIndexPair o) {
            return Double.compare(radius, o.radius);
        }

    }

}