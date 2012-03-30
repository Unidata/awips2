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
package com.raytheon.viz.core.gl.glsl;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.IGLTarget;
import com.raytheon.viz.core.gl.glsl.internal.GLProgramManager;

/**
 * Factory class to obtain shader programs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author estrabal
 * @version 1.0
 */

public class GLSLFactory {

    private static final String DEFAULT_VERTEX = "vertex";

    private static GLSLFactory instance;

    private final Map<String, GLShaderProgram> shadersPrograms = new HashMap<String, GLShaderProgram>();

    private GLSLFactory() {
    }

    /**
     * 
     * @return
     */
    synchronized static public GLSLFactory getInstance() {
        if (instance == null) {
            instance = new GLSLFactory();
        }
        return instance;
    }

    /**
     * Get instance of a shader program with the specified vertex and fragment
     * shaders. Either vertex or fragment shader can be null but not both.
     * 
     * @param gl
     * @param vertexName
     * @param fragName
     * @return
     * @throws VizException
     */
    public GLShaderProgram getShaderProgram(IGLTarget target,
            String vertexName, String fragName) throws VizException {
        if (vertexName == null) {
            vertexName = DEFAULT_VERTEX;
            if (fragName == null) {
                throw new VizException(
                        "No fragment or vertex shader was specified."
                                + " Is shader being enabled needed for this?");
            }
        }
        String shaderName = new String(vertexName + "_" + fragName);
        GLShaderProgram shader = shadersPrograms.get(shaderName);
        if (shader == null
                || GLProgramManager.getInstance().hasBeenModified(fragName)
                || GLProgramManager.getInstance().hasBeenModified(vertexName)) {
            if (shader != null) {
                shader.dispose();
            }

            shader = new GLShaderProgram(target, shaderName, vertexName,
                    fragName);
            shadersPrograms.put(shaderName, shader);
        }

        return shader;
    }
}
