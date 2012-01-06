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

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;

import javax.media.opengl.glu.GLUtessellatorCallback;

import org.eclipse.swt.graphics.RGB;

/**
 * 
 * Tesselates an shape using GLU tesselation callback
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class ShapeTessellator implements GLUtessellatorCallback {

    private static final int INITIAL_ELEMENT_SIZE = 150000;

    private ByteBuffer theVertexBufferBytes;

    private FloatBuffer theVertexBuffer;

    private ByteBuffer theColorBuffer;

    private byte theRed;

    private byte theBlue;

    private byte theGreen;

    private int initialElementSize = INITIAL_ELEMENT_SIZE;

    public ShapeTessellator() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#begin(int)
     */
    public void begin(int arg0) {

    }

    /**
     * Set the color
     * 
     * @param color
     */
    public void setColor(RGB color) {
        theRed = new Integer(color.red).byteValue();
        theGreen = new Integer(color.green).byteValue();
        theBlue = new Integer(color.blue).byteValue();

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#beginData(int,
     * java.lang.Object)
     */
    public void beginData(int arg0, Object arg1) {
        // Not necessary for type of tesselation
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#combine(double[],
     * java.lang.Object[], float[], java.lang.Object[])
     */
    public void combine(double[] coordinates, Object[] v, float[] arg2,
            Object[] out) {
        double[] vertex = new double[3];
        vertex[0] = coordinates[0];
        vertex[1] = coordinates[1];
        vertex[2] = coordinates[2];
        out[0] = vertex;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#combineData(double[],
     * java.lang.Object[], float[], java.lang.Object[], java.lang.Object)
     */
    public void combineData(double[] arg0, Object[] arg1, float[] arg2,
            Object[] arg3, Object arg4) {
        // Not necessary for type of tesselation

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#edgeFlag(boolean)
     */
    public void edgeFlag(boolean arg0) {
        // No operation, but needed to force GL_TRIANGLES
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#edgeFlagData(boolean,
     * java.lang.Object)
     */
    public void edgeFlagData(boolean arg0, Object arg1) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#end()
     */
    public void end() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.media.opengl.glu.GLUtessellatorCallback#endData(java.lang.Object)
     */
    public void endData(Object arg0) {
        // Not necessary for type of tesselation

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#error(int)
     */
    public void error(int arg0) {
        System.err.println("Tess Error: " + arg0);

    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.media.opengl.glu.GLUtessellatorCallback#errorData(int,
     * java.lang.Object)
     */
    public void errorData(int arg0, Object arg1) {
        // Not necessary for type of tesselation

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.media.opengl.glu.GLUtessellatorCallback#vertex(java.lang.Object)
     */
    public void vertex(Object data) {
        if (data instanceof double[]) {
            if (theVertexBuffer == null) {
                theVertexBufferBytes = ByteBuffer
                        .allocateDirect(initialElementSize * 4 * 3);
                theVertexBufferBytes.order(ByteOrder.nativeOrder());
                theVertexBuffer = theVertexBufferBytes.asFloatBuffer();
                theVertexBuffer.position(0);

                theColorBuffer = ByteBuffer
                        .allocateDirect(initialElementSize * 3);
                theColorBuffer.order(ByteOrder.nativeOrder());
                theColorBuffer.position(0);

            }
            if (theVertexBuffer.remaining() < 3) {
                FloatBuffer oldVertexBuffer = theVertexBuffer;
                theVertexBufferBytes = ByteBuffer
                        .allocateDirect(theVertexBufferBytes.capacity() * 3 / 2 + 1);
                theVertexBufferBytes.order(ByteOrder.nativeOrder());
                theVertexBuffer = theVertexBufferBytes.asFloatBuffer();
                theVertexBuffer.position(0);
                oldVertexBuffer.limit(oldVertexBuffer.position());
                oldVertexBuffer.position(0);
                theVertexBuffer.put(oldVertexBuffer);

                ByteBuffer oldColorBuffer = theColorBuffer;
                theColorBuffer = ByteBuffer.allocateDirect(theColorBuffer
                        .capacity() * 3 / 2 + 1);
                theColorBuffer.order(ByteOrder.nativeOrder());
                theColorBuffer.position(0);
                oldColorBuffer.limit(oldColorBuffer.position());
                oldColorBuffer.position(0);
                theColorBuffer.put(oldColorBuffer);
            }
            double[] d = (double[]) data;
            theVertexBuffer.put((float) d[0]);
            theVertexBuffer.put((float) d[1]);
            theVertexBuffer.put((float) d[2]);
            theColorBuffer.put(theRed);
            theColorBuffer.put(theGreen);
            theColorBuffer.put(theBlue);
        } else {
            System.out.println("ERROR!!!! vertex got non-double");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.media.opengl.glu.GLUtessellatorCallback#vertexData(java.lang.Object
     * , java.lang.Object)
     */
    public void vertexData(Object arg0, Object arg1) {
        // Not necessary for type of tesselation
    }

    /**
     * @return the vertexBuffer
     */
    public FloatBuffer getVertexBuffer() {
        return theVertexBuffer;
    }

    /**
     * 
     * @return the vertexBuffer as a bytebuffer
     */
    public ByteBuffer getVertexBufferAsBytes() {
        return theVertexBufferBytes;
    }

    /**
     * 
     * @return the color buffer
     */
    public ByteBuffer getColorBuffer() {
        return theColorBuffer;
    }

    /**
     * Get the size
     * 
     * @return the size
     */
    public int getSize() {
        return theVertexBuffer == null ? 0 : theVertexBuffer.position();
    }

    /**
     * Reset the vertex objects
     * 
     */
    public void reset(int initialElementSize) {
        theVertexBuffer = null;
        theColorBuffer = null;
        this.initialElementSize = initialElementSize;
    }

}
