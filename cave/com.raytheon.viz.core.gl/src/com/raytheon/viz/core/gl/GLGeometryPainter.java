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
package com.raytheon.viz.core.gl;

import java.nio.IntBuffer;
import java.util.HashSet;
import java.util.Set;

import javax.media.opengl.GL;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.GLGeometryObject2D.State;

/**
 * Separated from GLGeometryObject2D. Added checks on glDrawArrays and
 * glMultiDrawArrays.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GLGeometryPainter {

    private static int maxVertices = -1;

    public static void paintGeometries(GL gl, GLGeometryObject2D... geoms)
            throws VizException {
        State state = State.INVALID;
        Set<State> states = new HashSet<State>();
        for (GLGeometryObject2D geom : geoms) {
            states.add(geom.state);
            state = geom.state;
        }

        if (states.size() > 1 || state == State.INVALID) {
            throw new VizException("Could not paint, invalid geometry states. "
                    + "All geometry states much match and not be invalid: "
                    + states);
        }

        // We are in a mutable paint state (not compiled). Make sure all geoms
        // were created as mutable
        if (state == State.MUTABLE) {
            for (GLGeometryObject2D geom : geoms) {
                if (geom.data.mutable == false) {
                    throw new VizException(
                            "Could not paint, geometry was not created "
                                    + "as mutable but in in the mutable state."
                                    + " Geometry must be compiled before use");
                }
            }
        }

        for (GLGeometryObject2D geom : geoms) {
            gl.glEnableClientState(geom.data.coordType);
        }

        switch (state) {
        case MUTABLE: {
            // A not compiled buffer, still able to add segments
            for (GLGeometryObject2D geom : geoms) {
                switch (geom.data.coordType) {
                case GL.GL_VERTEX_ARRAY: {
                    gl.glVertexPointer(geom.pointsPerCoordinate(), GL.GL_FLOAT,
                            0, geom.coordBuffer.getBuffer().rewind());
                    break;
                }
                case GL.GL_TEXTURE_COORD_ARRAY: {
                    gl.glTexCoordPointer(geom.pointsPerCoordinate(),
                            GL.GL_FLOAT, 0, geom.coordBuffer.getBuffer()
                                    .rewind());
                    break;
                }
                case GL.GL_NORMAL_ARRAY: {
                    gl.glNormalPointer(GL.GL_FLOAT, 0, geom.coordBuffer
                            .getBuffer().rewind());
                    break;
                }
                case GL.GL_COLOR_ARRAY: {
                    gl.glColorPointer(geom.pointsPerCoordinate(), GL.GL_FLOAT,
                            0, geom.coordBuffer.getBuffer().rewind());
                    break;
                }
                }
            }

            for (GLGeometryObject2D geom : geoms) {
                if (geom.data.manageIndicies) {
                    for (int i = 0; i < geom.indicies.size() - 1; ++i) {
                        int first = geom.indicies.get(i);
                        drawArrays(gl, geom.data.geometryType, first,
                                geom.indicies.get(i + 1) - first);
                    }
                    int first = geom.indicies.get(geom.indicies.size() - 1);
                    drawArrays(gl, geom.data.geometryType, first, geom.points
                            - first);
                    break;
                }
            }

            break;
        }
        case COMPILED:
        case COMPILED_HIGH_END: {
            for (GLGeometryObject2D geom : geoms) {
                if (!geom.vbo.isValid()) {
                    throw new VizException(
                            "Could not paint geometry, VBO not set!");
                }
                geom.vbo.bind(gl, GL.GL_ARRAY_BUFFER);
                switch (geom.data.coordType) {
                case GL.GL_VERTEX_ARRAY: {
                    gl.glVertexPointer(geom.pointsPerCoordinate(), GL.GL_FLOAT,
                            0, 0);
                    break;
                }
                case GL.GL_TEXTURE_COORD_ARRAY: {
                    gl.glTexCoordPointer(geom.pointsPerCoordinate(),
                            GL.GL_FLOAT, 0, 0);
                    break;
                }
                case GL.GL_NORMAL_ARRAY: {
                    gl.glNormalPointer(GL.GL_FLOAT, 0, 0);
                    break;
                }
                case GL.GL_COLOR_ARRAY: {
                    gl.glColorPointer(geom.pointsPerCoordinate(), GL.GL_FLOAT,
                            0, 0);
                    break;
                }
                }
                gl.glBindBuffer(GL.GL_ARRAY_BUFFER, 0);
            }
            for (GLGeometryObject2D geom : geoms) {
                if (geom.data.manageIndicies) {
                    if (geom.state == State.COMPILED_HIGH_END) {
                        geom.compiledIndicies.rewind();
                        geom.compiledLengths.rewind();
                        multiDrawArrays(gl, geom.data.geometryType,
                                geom.compiledIndicies, geom.compiledLengths,
                                geom.compiledIndicies.capacity());
                    } else {
                        int size = geom.compiledIndicies.capacity();
                        for (int i = 0; i < size - 1; ++i) {
                            int first = geom.compiledIndicies.get(i);
                            drawArrays(gl, geom.data.geometryType, first,
                                    geom.compiledIndicies.get(i + 1) - first);
                        }
                    }
                    break;
                }
            }
            break;
        }
        }

        for (GLGeometryObject2D geom : geoms) {
            gl.glDisableClientState(geom.data.coordType);
        }
    }

    /**
     * Calls gl.glDrawArrays(mode, first, count) after range checking first and
     * count
     * 
     * @param gl
     * @param mode
     * @param first
     * @param count
     * @throws VizException
     */
    private static void drawArrays(GL gl, int mode, int first, int count)
            throws VizException {
        if (first > -1 && count > 0) {
            gl.glDrawArrays(mode, first, count);
        } else {
            throw new VizException(
                    "Dangerous parameters passed to glDrawArrays: first="
                            + first + ", count=" + count);
        }
    }

    private static void multiDrawArrays(GL gl, int mode, IntBuffer first,
            IntBuffer count, int primcount) throws VizException {
        if (first.capacity() > 0 && count.capacity() > 0 && primcount > 0) {
            gl.glMultiDrawArrays(mode, first, count, primcount);
        } else {
            throw new VizException(
                    "Dangerous parameters passed to glMultiDrawArrays: firstCapacity="
                            + first.capacity() + ", countCapacity="
                            + count.capacity() + ", primcount=" + primcount);
        }
    }

}
