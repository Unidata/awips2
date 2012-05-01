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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
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

    private static final Map<String, Object> shaderProgramMap = new HashMap<String, Object>();

    static {
        // Construct the resource mapping from Eclipse plugins
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint("com.raytheon.viz.core.gl.shader");
        if (point != null) {
            IExtension[] extensions = point.getExtensions();

            for (int i = 0; i < extensions.length; ++i) {
                IConfigurationElement[] config = extensions[i]
                        .getConfigurationElements();

                for (int j = 0; j < config.length; j++) {
                    String name = config[j].getAttribute("programName");
                    shaderProgramMap.put(name, config[j]);
                }
            }

        }
    }

    public static void registerProgramLoader(String name, IShaderLoader loader) {
        shaderProgramMap.put(name, loader);
    }

    private GLSLFactory() {
        // Register vetex.glsl as the default vertex program loader
        IShaderLoader loader = new AbstractShaderLoader() {
            @Override
            public void loadData(IGLTarget target, GLShaderProgram program,
                    IImage image, PaintProperties paintProps)
                    throws VizException {
                // does nothing
            }
        };
        loader.setName(DEFAULT_VERTEX);
        registerProgramLoader(loader.getName(), loader);
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
                || GLProgramManager.getInstance().hasBeenModified(fragName,
                        false)
                || GLProgramManager.getInstance().hasBeenModified(vertexName,
                        false)) {
            if (shader != null) {
                shader.dispose();
            }
            // program object not cached, construct
            IShaderLoader vertexShader = null;
            if (vertexName != null) {
                vertexShader = getShaderLoaderInstance(shaderProgramMap
                        .get(vertexName));
            }
            IShaderLoader fragmentShader = null;
            if (fragName != null) {
                fragmentShader = getShaderLoaderInstance(shaderProgramMap
                        .get(fragName));
                fragmentShader.setName(fragName);
            }

            shader = new GLShaderProgram(target, shaderName, vertexShader,
                    fragmentShader);
            shadersPrograms.put(shaderName, shader);
        }

        if (shader != null) {
            Object fshader = shaderProgramMap.get(fragName);
            Object vshader = shaderProgramMap.get(vertexName);
            if (fshader != null && fshader instanceof IShaderLoader
                    && shader.getFragmentShader() != fshader) {
                shader.updateFragmentShader((IShaderLoader) fshader);
            }
            if (vshader != null && vshader instanceof IShaderLoader
                    && shader.getVertexShader() != vshader) {
                shader.updateVertexShader((IShaderLoader) vshader);
            }
        }

        return shader;
    }

    /**
     * Construct an instance of the shader loader class
     * 
     * @param className
     * @return
     */
    private IShaderLoader getShaderLoaderInstance(Object object)
            throws VizException {
        if (object instanceof IConfigurationElement) {
            IConfigurationElement configElement = (IConfigurationElement) object;
            try {
                return (IShaderLoader) configElement
                        .createExecutableExtension("loaderClass");
            } catch (CoreException e) {
                throw new VizException(
                        "Error constructing instance of loader class: "
                                + configElement.getAttribute("loaderClass"), e);
            }
        } else if (object instanceof IShaderLoader) {
            return (IShaderLoader) object;
        }
        return null;
    }
}
