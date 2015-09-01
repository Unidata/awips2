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

import com.vividsolutions.jts.triangulate.quadedge.TriangleVisitor;

/**
 * A {@link TriangleVisitor} used by the {@link DelauneyTriangulator} to build
 * an index buffer of triangle vertices. This is an abstract class to allow
 * subclasses to implement custom filtering for invalid triangles.
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
public abstract class AbstractIndexBufferBuilder implements TriangleVisitor {

    protected final IntBuffer buffer;

    public AbstractIndexBufferBuilder(int size) {
        this.buffer = IntBuffer.allocate(size);
    }

    /**
     * This will copy the index buffer into a new buffer that is the size of the
     * consumed portion of the buffer.
     */
    public IntBuffer getBuffer() {
        int bufferSize = buffer.position();
        buffer.position(0);
        buffer.limit(bufferSize);
        IntBuffer result = IntBuffer.allocate(bufferSize);
        result.put(buffer);
        result.rewind();
        return result;
    }

    protected void addTriangle(int i0, int i1, int i2) {
        buffer.put(i0);
        buffer.put(i1);
        buffer.put(i2);
    }

}
