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

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat;
import com.raytheon.viz.core.gl.images.AbstractGLColormappedImage;

/**
 * Factory for creating API defined GLSL structs in a {@link GLShaderProgram}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 31, 2013 2492       mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLSLStructFactory {

    private static final String FIELD_SEPERATOR = ".";

    /**
     * Creates a DataTexture structure in the program with the given name
     * 
     * @param program
     * @param name
     * @param texBinding
     * @param image
     */
    public static void createDataTexture(GLShaderProgram program, String name,
            int texBinding, AbstractGLColormappedImage image) {
        ColorMapParameters parameters = image.getColorMapParameters();
        AbstractGLColorMapDataFormat dataFormat = image.getDataFormat();
        // Set the texture outside the struct
        setFieldUniform(program, name + "Tex", texBinding);
        // Set struct fields
        setFieldUniform(program, name, "noDataValue",
                parameters.getNoDataValue());
        setFieldUniform(program, name, "isScaled", dataFormat.isScaled());
        if (dataFormat.isScaled()) {
            setFieldUniform(program, name, "scaleMin",
                    dataFormat.getDataFormatMin());
            setFieldUniform(program, name, "scaleMax",
                    dataFormat.getDataFormatMax());
        }
        setFieldUniform(program, name, "width", (float) image.getWidth());
        setFieldUniform(program, name, "height", (float) image.getHeight());
    }

    /**
     * Creates a DataMapping structure in the program with the given name
     * 
     * @param program
     * @param name
     * @param dataMappingTexBinding
     * @param colorMappingTexBinding
     * @param numMappingValues
     */
    public static void createDataMapping(GLShaderProgram program, String name,
            int dataMappingTexBinding, int colorMappingTexBinding,
            int numMappingValues) {
        setFieldUniform(program, name + "DataValues", dataMappingTexBinding);
        setFieldUniform(program, name + "ColorValues", colorMappingTexBinding);
        setFieldUniform(program, name + "Values", numMappingValues);
    }

    /**
     * Creates a ColorMapping structure in the program with the given name
     * 
     * @param program
     * @param name
     * @param colorMapTexBinding
     * @param alphaMaskTexBinding
     * @param parameters
     */
    public static void createColorMapping(GLShaderProgram program, String name,
            int colorMapTexBinding, int alphaMaskTexBinding,
            ColorMapParameters parameters) {
        setFieldUniform(program, name + "ColorMap", colorMapTexBinding);
        setFieldUniform(program, name, "cmapMin", parameters.getColorMapMin());
        setFieldUniform(program, name, "cmapMax", parameters.getColorMapMax());

        setFieldUniform(program, name, "applyMask", parameters.isUseMask());
        setFieldUniform(program, name + "AlphaMask", alphaMaskTexBinding);

        setFieldUniform(program, name, "isMirrored", parameters.isMirror());
        setFieldUniform(program, name, "isLogarithmic",
                parameters.isLogarithmic());
        setFieldUniform(program, name, "logFactor", parameters.getLogFactor());
    }

    /**
     * Creates a ColorModifiers structure in the program with the given name
     * 
     * @param program
     * @param name
     * @param alpha
     * @param brightness
     * @param contrast
     */
    public static void createColorModifiers(GLShaderProgram program,
            String name, float alpha, float brightness, float contrast) {
        setFieldUniform(program, name, "alpha", alpha);
        setFieldUniform(program, name, "brightness", brightness);
        setFieldUniform(program, name, "contrast", contrast);
    }

    private static void setFieldUniform(GLShaderProgram program,
            String structName, String fieldName, Object fieldValue) {
        setFieldUniform(program, structName + FIELD_SEPERATOR + fieldName,
                fieldValue);
    }

    private static void setFieldUniform(GLShaderProgram program,
            String fieldName, Object fieldValue) {
        program.setUniform(fieldName, fieldValue);
    }

}
