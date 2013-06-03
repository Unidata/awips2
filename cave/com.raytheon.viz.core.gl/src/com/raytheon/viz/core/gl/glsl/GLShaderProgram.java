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

import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.media.opengl.GL;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.glsl.internal.GLProgramManager;
import com.sun.opengl.util.BufferUtil;

/**
 * Wrapper class to using shader programs. A shader program must use one vertex
 * and/or one fragment shader.
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

public class GLShaderProgram {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GLShaderProgram.class);

    private static enum State {
        INVALID, INITIALIZED, IN_USE;
    }

    static public String NONE = "NONE";

    private final GL gl;

    private int glslContext = -1;

    private State state = State.INVALID;

    // Declaring and using variables in GLSL is similar to using variables in C.
    //
    // There are four options for variable qualifiers:
    //
    // * const - A constant.
    //
    // * varying - Read-only in a fragment shader, readable/writable in the
    // vertex shader. Used to communicate between the two shaders.
    //
    // * uniform - Global read-only variables available to vertex and fragment
    // shaders which can't be changed inside glBegin() and glEnd() calls.
    //
    // * attribute - Global read-only variables only available to the vertex
    // shader (and not the fragment shader). Passed from an OpenGL program,
    // these can be changed on a per vertex level.
    // currently we are only using uniforms variables
    private Map<String, Object> loadedUniforms = new HashMap<String, Object>();

    // TODO make configurable
    private static final int MAX_MULTIGRIDS = 8;

    private final String name;

    /**
     * Package level constructor for GLSLFactory
     * 
     * @param gl
     * @param name
     * @param vertexShader
     *            - can be null (but not if fragmentShader == null)
     * @param fragmentShader
     *            - can be null (but not if vertexShader == null)
     */
    GLShaderProgram(GL gl, String name, String vertexShader,
            String fragmentShader) throws VizException {
        this.gl = gl;
        glslContext = gl.glCreateProgram();
        if (glslContext < 1) {
            throw new VizException(
                    "Error creating glsl program, could not create program object");
        }

        List<Integer> shaderIds = new ArrayList<Integer>(2);
        if (vertexShader != null) {
            shaderIds.add(addShader(vertexShader, GL.GL_VERTEX_SHADER));
        }

        if (fragmentShader != null) {
            shaderIds.add(addShader(fragmentShader, GL.GL_FRAGMENT_SHADER));
        }

        for (Integer shaderId : shaderIds) {
            gl.glAttachShader(glslContext, shaderId);
        }
        gl.glLinkProgram(glslContext);
        if (!checkForLinkingErrors(gl)) {
            state = State.INITIALIZED;
            for (Integer shaderId : shaderIds) {
                gl.glDeleteShader(shaderId);
            }
        } else {
            throw new VizException(
                    "Error creating glsl shader program, could not link");
        }
        this.name = name;
    }

    /**
     * Make this shader program active
     */
    public void startShader() {
        if (state == State.INITIALIZED) {
            gl.glUseProgram(glslContext);
            state = State.IN_USE;
        }
        loadedUniforms.clear();
    }

    /**
     * deactivate Shader program
     */
    public void endShader() {
        if (state == State.IN_USE) {
            gl.glUseProgram(0);
            state = State.INITIALIZED;
        }
        loadedUniforms.clear();
    }

    /**
     * 
     * @return
     */
    public String getName() {
        return name;
    }

    /**
     * Load, and compile the shader program
     * 
     * @param shader
     */
    private int addShader(String shaderName, int shaderType)
            throws VizException {
        int shaderId = loadShaderProgram(shaderName, shaderType);
        if (shaderId < 1) {
            throw new VizException(
                    "Error creating shader, object not allocated");
        }
        return shaderId;
    }

    /**
     * Load, compile, and attach a shader program
     * 
     * @param program
     * @param glShaderId
     * @return the shader program id
     */
    private int loadShaderProgram(String programName, int glShaderId)
            throws VizException {
        String program = GLProgramManager.getInstance().getProgramCode(
                programName);
        int shaderId = gl.glCreateShader(glShaderId);
        if (shaderId >= 0) {
            gl.glShaderSource(shaderId, 1, new String[] { program },
                    (int[]) null, 0);
            gl.glCompileShader(shaderId);
            checkForCompileErrors(gl, shaderId, glShaderId, programName);
        }
        return shaderId;
    }

    /**
     * Set the uniform variable for the shader program
     * 
     * @param uniformName
     * @param value
     */
    private void setUniformInternal(String uniformName, Object value) {
        if (value instanceof Double) {
            gl.glUniform1f(getUniformLocation(uniformName),
                    ((Double) value).floatValue());
        } else if (value instanceof Float) {
            gl.glUniform1f(getUniformLocation(uniformName), (Float) value);
        } else if (value instanceof Integer) {
            gl.glUniform1i(getUniformLocation(uniformName), (Integer) value);
        } else if (value instanceof Boolean) {
            gl.glUniform1i(getUniformLocation(uniformName),
                    ((Boolean) value) == true ? 1 : 0);
        } else if (value instanceof int[]) {
            int[] ints = (int[]) value;
            gl.glUniform1iv(getUniformLocation(uniformName), ints.length,
                    (int[]) value, 0);
        } else if (value instanceof float[]) {
            float[] floats = (float[]) value;
            gl.glUniform1fv(getUniformLocation(uniformName), floats.length,
                    (float[]) value, 0);
        } else if (value instanceof RGB) {
            gl.glUniform3f(getUniformLocation(uniformName),
                    ((RGB) value).red / 255.0f, ((RGB) value).green / 255.0f,
                    ((RGB) value).blue / 255.0f);
        } else {
            System.err.println("Cannot set uniform for type: "
                    + value.getClass());
        }
    }

    /**
     * Direct method of setting uniforms
     * 
     * @param key
     * @param value
     */
    public void setUniform(String key, Object value) {
        if (value != null && state == State.IN_USE) {
            if (loadedUniforms.containsKey(key) == false
                    || value.equals(loadedUniforms.get(key)) == false) {
                // we haven't loaded this uniform yet or it is different
                // from last time
                loadedUniforms.put(key, value);
                setUniformInternal(key, value);
            }
        }
    }

    /**
     * Get location of uniform variable for this program
     * 
     * @param name
     * @return
     */
    private int getUniformLocation(String name) {
        return (gl.glGetUniformLocation(glslContext, name));
    }

    /**
     * Check for shader compile errors
     * 
     * @param gl
     * @param shader
     * @return
     */
    private boolean checkForCompileErrors(GL gl, int shader, int glId,
            String name) {
        String type = "unknown";
        if (glId == GL.GL_FRAGMENT_SHADER) {
            type = "fragment";
        } else if (glId == GL.GL_VERTEX_SHADER) {
            type = "vertex";
        }

        boolean rval = false;
        int[] compilecheck = new int[1];
        gl.glGetObjectParameterivARB(shader, GL.GL_OBJECT_COMPILE_STATUS_ARB,
                compilecheck, 0);
        if (compilecheck[0] == GL.GL_FALSE) {
            System.err.println("A compilation error occured in the " + type
                    + " shader source file (" + name + ")");

            IntBuffer iVal = BufferUtil.newIntBuffer(1);
            gl.glGetObjectParameterivARB(shader,
                    GL.GL_OBJECT_INFO_LOG_LENGTH_ARB, iVal);

            int length = iVal.get();
            if (length <= 1) {
                return true;
            }
            ByteBuffer infoLog = BufferUtil.newByteBuffer(length);
            iVal.flip();
            gl.glGetInfoLogARB(shader, length, iVal, infoLog);
            // Remove null termination
            byte[] infoBytes = new byte[length - 1];
            infoLog.get(infoBytes);
            statusHandler.handle(Priority.CRITICAL, "Problem occured during "
                    + type + " shader initialization of " + name + ": "
                    + new String(infoBytes));
            System.err.println(new String(infoBytes));
            rval = true;
        }

        return rval;
    }

    /**
     * Check for linking errors from the shaders
     * 
     * @param gl
     * @return
     */
    private boolean checkForLinkingErrors(GL gl) {
        boolean errors = false;
        int[] param = new int[1];
        gl.glGetProgramiv(glslContext, GL.GL_LINK_STATUS, param, 0);
        if (param[0] == GL.GL_FALSE) {
            errors = true;
            System.err.println("Error linking shader programs");
            IntBuffer iVal = BufferUtil.newIntBuffer(1);
            gl.glGetObjectParameterivARB(glslContext, GL.GL_INFO_LOG_LENGTH,
                    iVal);
            int length = iVal.get();
            if (length > 0) {
                ByteBuffer infoLog = BufferUtil.newByteBuffer(length);
                iVal.flip();
                gl.glGetProgramInfoLog(glslContext, length, iVal, infoLog);
                // Remove null termination
                byte[] infoBytes = new byte[length - 1];
                infoLog.get(infoBytes);
                statusHandler.handle(Priority.CRITICAL,
                        "Problem occured during shader program linking: "
                                + new String(infoBytes));
                System.err.println(new String(infoBytes));
            }
        }
        return errors;
    }

    /**
     * Disposes of the shader program gl resources
     */
    public void dispose() {
        if (state == State.IN_USE) {
            endShader();
        }
        if (state == State.INITIALIZED) {
            gl.glDeleteProgram(glslContext);
            glslContext = -1;
        }
        state = State.INVALID;
    }

}
