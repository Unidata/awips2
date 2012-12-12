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
package com.raytheon.viz.core.gl.dataformat;

import java.nio.Buffer;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

import javax.media.opengl.GL;

/**
 * GL Half Float data format
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLHalfFloatDataFormat extends AbstractGLColorMapDataFormat {
    // -15 stored using a single precision bias of 127
    private static final int HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP = 0x38000000;

    // max exponent value in single precision that will be converted
    // to Inf or Nan when stored as a half-float
    private static final int HALF_FLOAT_MAX_BIASED_EXP_AS_SINGLE_FP_EXP = 0x47800000;

    // 255 is the max exponent biased value
    private static final int FLOAT_MAX_BIASED_EXP = (0xFF << 23);

    private static final int HALF_FLOAT_MAX_BIASED_EXP = (0x1F << 10);

    private static final float GL_HALF_FLOAT_MAX = 65504;

    @Override
    public int getTextureInternalFormat() {
        return GL.GL_LUMINANCE16F_ARB;
    }

    @Override
    public int getTextureType() {
        return GL.GL_HALF_FLOAT_ARB;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMin()
     */
    @Override
    public double getDataFormatMin() {
        return Double.NaN;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#
     * getDataFormatMax()
     */
    @Override
    public double getDataFormatMax() {
        return Double.NaN;
    }

    @Override
    public ShortBuffer getCopybackBuffer(GLColorMapData data) {
        int width = getAlignedWidth(data.getDimensionSize(0));
        int height = data.getDimensionSize(1);
        return ShortBuffer.allocate(height * width);
    }

    @Override
    public int getBytesPerPixel() {
        return 2;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#getValue
     * (int, int, com.raytheon.viz.core.gl.dataprep.GLColorMapData)
     */
    @Override
    public Float getValue(int x, int y, GLColorMapData data) {
        if (data.getTextureType() != GL.GL_HALF_FLOAT_ARB) {
            throw new IllegalArgumentException(
                    "Cannot process texture of type " + data.getTextureType());
        } else if (!(data.getData() instanceof ShortBuffer)) {
            throw new IllegalArgumentException(
                    "Expecting data to contain a ShortBuffer but instead it is a "
                            + data.getData().getClass().getSimpleName());
        }
        int width = getAlignedWidth(data.getDimensionSize(0));
        int index = y * width + x;
        ShortBuffer buffer = (ShortBuffer) data.getData();
        return convertFromFloat16(buffer.get(index));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataformat.AbstractGLColorMapDataFormat#formatForGL
     * (java.nio.Buffer, com.raytheon.viz.core.gl.dataformat.GLColorMapData)
     */
    @Override
    public Buffer formatForGL(Buffer buffer, GLColorMapData data) {
        Buffer buff = super.formatForGL(buffer, data);
        if (buff instanceof ShortBuffer) {
            return buff;
        } else if (buffer instanceof FloatBuffer) {
            // if super needed to resize it will handle the conversion to half
            // floats automatically, if not we should handle it here.
            Buffer oldBuffer = buffer;
            buffer = getCopybackBuffer(data);
            oldBuffer.rewind();
            copyRow(oldBuffer, buffer, 0);
            buffer.rewind();
            return (ShortBuffer) buffer;
        }
        throw new IllegalArgumentException(buffer.getClass().getSimpleName()
                + " is not a ShortBuffer");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.dataprep.AbstractGLColorMapDataFormat#copyRow
     * (java.nio.Buffer, java.nio.Buffer, int)
     */
    @Override
    protected void copyRow(Buffer fromBuffer, Buffer toBuffer, int padding) {
        if (!(toBuffer instanceof ShortBuffer)) {
            throw new IllegalArgumentException(toBuffer.getClass()
                    .getSimpleName() + " is not a ShortBuffer");
        }
        ShortBuffer dest = (ShortBuffer) toBuffer;
        if (fromBuffer instanceof FloatBuffer) {
            FloatBuffer src = (FloatBuffer) fromBuffer;
            while (src.hasRemaining()) {
                float f = src.get();
                if (Float.isNaN(f)) {
                    dest.put(convertToFloat16(GL_HALF_FLOAT_MAX));
                } else {
                    dest.put(convertToFloat16(f));
                }
            }
        } else if (fromBuffer instanceof ShortBuffer) {
            dest.put((ShortBuffer) fromBuffer);
        } else {
            throw new IllegalArgumentException(fromBuffer.getClass()
                    .getSimpleName() + " is not a ShortBuffer or a FloatBuffer");
        }
        for (int i = 0; i < padding; i++) {
            dest.put((short) 0);
        }
    }

    private static short convertToFloat16(float f) {
        int x = Float.floatToIntBits(f);
        short sign = (short) (x >> 31);
        int mantissa;
        int exp;
        short hf;

        // get mantissa
        mantissa = x & ((1 << 23) - 1);
        // get exponent bits
        exp = x & FLOAT_MAX_BIASED_EXP;
        if (exp >= HALF_FLOAT_MAX_BIASED_EXP_AS_SINGLE_FP_EXP) {
            // check if the original single precision float number is a NaN
            if ((mantissa != 0) && (exp == FLOAT_MAX_BIASED_EXP)) {
                // we have a single precision NaN
                mantissa = (1 << 23) - 1;
            } else {
                // 16-bit half-float representation stores number as Inf
                mantissa = 0;
            }
            hf = (short) ((sign << 15) | (short) (HALF_FLOAT_MAX_BIASED_EXP) | (mantissa >> 13));
        }
        // check if exponent is <= -15
        else if (exp <= HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP) {

            // store a denorm half-float value or zero
            exp = (HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP - exp) >> 23;
            // do not shoft more than 31 bits
            mantissa >>= (14 + (exp > 17 ? 17 : exp));

            hf = (short) ((sign << 15) | (short) (mantissa));
        } else {
            hf = (short) ((sign << 15)
                    | (short) ((exp - HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP) >> 13) | (short) (mantissa >> 13));
        }

        return hf;
    }

    private static float convertFromFloat16(short s) {
        int sign = (s >> 15) & 0x00000001;
        int exponent = (s >> 10) & 0x0000001f;
        int mantissa = s & 0x000003ff;
        int intBits;
        if (exponent == 0 && mantissa == 0) {
            // Plus or minus zero
            intBits = s << 31;
        } else if (exponent == 0) {
            // denorm numbers
            while ((mantissa & 0x00000400) != 0) {
                mantissa <<= 1;
                exponent -= 1;
            }
            exponent += 1;
            mantissa &= ~0x00000400;
            exponent = exponent + (127 - 15);
            mantissa = mantissa << 13;
            intBits = ((sign << 31) | (exponent << 23) | mantissa);
        } else if (exponent == 31 && mantissa == 0) {
            // Inf
            intBits = (s << 31) | FLOAT_MAX_BIASED_EXP;
        } else if (exponent == 31) {
            // NaN
            intBits = (s << 31) | FLOAT_MAX_BIASED_EXP | (mantissa << 13);
        } else {
            // Regular
            exponent = exponent + (127 - 15);
            mantissa = mantissa << 13;
            intBits = ((sign << 31) | (exponent << 23) | mantissa);

        }

        return Float.intBitsToFloat(intBits);
    }
}
