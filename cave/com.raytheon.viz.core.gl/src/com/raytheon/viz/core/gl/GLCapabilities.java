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

import javax.media.opengl.GL;

import com.raytheon.viz.core.gl.internal.GLTarget;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 6, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLCapabilities {

    private static GLCapabilities caps = null;

    public static synchronized GLCapabilities getInstance(GL gl) {
        if (caps == null) {
            caps = new GLCapabilities(gl);
        }
        return caps;
    }

    /** Does the video card support high end features */
    public boolean cardSupportsHighEndFeatures = false;

    /** Does the video card support shaders */
    public boolean cardSupportsShaders = false;

    public boolean textureRectangleSupported;

    private GLCapabilities(GL gl) {
        String openGlVersion = gl.glGetString(GL.GL_VERSION);
        float glVersion = Float.parseFloat(openGlVersion.substring(0, 3));

        if (glVersion >= 1.4f) {
            System.out.println("Enabling high end GL features");
            cardSupportsHighEndFeatures = true;

        }
        boolean imagingAvailable = gl.isExtensionAvailable("GL_ARB_imaging");

        System.out.println("Imaging is available: " + imagingAvailable);
        this.textureRectangleSupported = gl
                .isExtensionAvailable("GL_ARB_texture_rectangle");

        if (glVersion >= 2.0f && !GLTarget.FORCE_NO_SHADER) {
            cardSupportsShaders = true;

            if (this.textureRectangleSupported) {
                gl.glEnable(GL.GL_TEXTURE_RECTANGLE_ARB);
            }

            if (this.textureRectangleSupported) {
                gl.glDisable(GL.GL_TEXTURE_RECTANGLE_ARB);
            }
        }
        System.out.println("Shader supported: " + cardSupportsShaders);
    }
}
