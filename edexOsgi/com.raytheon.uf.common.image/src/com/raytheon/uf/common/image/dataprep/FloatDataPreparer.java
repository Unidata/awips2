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
package com.raytheon.uf.common.image.dataprep;

import java.awt.Rectangle;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.image.data.IColormappedDataPreparer;
import com.raytheon.uf.common.image.data.resp.EDEXImageData;

/**
 * Float data preparer, prepares the float data as a short buffer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class FloatDataPreparer extends AbstractNumDataPreparer {
	// -15 stored using a single precision bias of 127
	private static final int HALF_FLOAT_MIN_BIASED_EXP_AS_SINGLE_FP_EXP = 0x38000000;

	// max exponent value in single precision that will be converted
	// to Inf or Nan when stored as a half-float
	private static final int HALF_FLOAT_MAX_BIASED_EXP_AS_SINGLE_FP_EXP = 0x47800000;

	// 255 is the max exponent biased value
	private static final int FLOAT_MAX_BIASED_EXP = (0xFF << 23);

	private static final int HALF_FLOAT_MAX_BIASED_EXP = (0x1F << 10);

	private static final float GL_HALF_FLOAT_MAX = 65504;

	public FloatDataPreparer() {
		super();
	}

	public FloatDataPreparer(FloatBuffer data, Rectangle datasetBounds,
			int[] dims) {
		super(data, datasetBounds, dims);
	}

	@Override
	public EDEXImageData prepareData() {
		EDEXImageData rval = super.prepareData();
		FloatBuffer floatBuf = (FloatBuffer) buffer;
		int paddedWidth = this.datasetBounds.width;
		if ((paddedWidth & 1) == 1) {
			paddedWidth++;
		}
		ByteBuffer bb = ByteBuffer.allocateDirect(paddedWidth
				* this.datasetBounds.height * 2);
		bb.order(ByteOrder.nativeOrder());
		bb.rewind();
		ShortBuffer sb = bb.asShortBuffer();
		boolean subset = false;
		if (datasetBounds.width * datasetBounds.height < floatBuf.capacity()) {
			subset = true;
		}
		int szX = subset ? this.datasetBounds.x + this.datasetBounds.width
				: this.datasetBounds.width;
		int szY = subset ? this.datasetBounds.y + this.datasetBounds.height
				: this.datasetBounds.height;
		int totalWidth = subset ? totalDatasetDimensions[0]
				: datasetBounds.width;
		for (int j = subset ? this.datasetBounds.y : 0; j < szY; j++) {
			for (int i = subset ? this.datasetBounds.x : 0; i < szX; i++) {
				float f = floatBuf.get(totalWidth * j + i);
				// NOTE: NaNs are converted to half precsiion float max to
				// support NVIDIA cards prior to the 8000 series which do
				// not
				// support NaNs. If support for these cards is not required
				// this
				// conversion can be removed.

				// NOTE: a corresponding check in the shader code in
				// GLTarget
				// will also have to be changed.
				if (Float.isNaN(f)) {
					sb.put(convertToFloat16(GL_HALF_FLOAT_MAX));
				} else {
					sb.put(convertToFloat16(f));
				}
			}

			if (paddedWidth != this.datasetBounds.width) {
				sb.put((short) 0);
			}
		}
		sb.rewind();
		rval.setData(sb);
		return rval;
	}

	public FloatBuffer getFloatBuffer() {
		return (FloatBuffer) buffer;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.core.gl.dataprep.AbstractNumDataPreparer#init()
	 */
	@Override
	protected void init() {
		textureFormat = LUMINANCE;
		internalTextureFormat = LUMINANCE16F_ARB;
		textureType = HALF_FLOAT_ARB;

		size = datasetBounds.width * datasetBounds.height * 2;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.data.IDataPreparer#newInstance(java.lang.Object,
	 * java.awt.Rectangle, com.raytheon.uf.viz.core.data.IColorMapper)
	 */
	@Override
	public IColormappedDataPreparer newInstance(Object data,
			Rectangle datasetBounds, int[] dims) {
		return new FloatDataPreparer((FloatBuffer) data, datasetBounds, dims);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.viz.core.data.IColorMapper#getValue(java.nio.Buffer,
	 * int, int, float, float)
	 */
	@Override
	public double getValue(Buffer rawData, int x, int y, float dataMin,
			float dataMax) {
		if (buffer == null) {
			return Double.NaN;
		}

		if (x >= this.datasetBounds.width || x < 0
				|| y >= this.datasetBounds.height || y < 0)
			return Double.NaN;

		float val = ((FloatBuffer) buffer).get((this.datasetBounds.y + y)
				* totalDatasetDimensions[0] + (x + this.datasetBounds.x));
		return val;
	}

	@Override
	public double getValue(Buffer rawData, int x, int y, float dataMin,
			float dataMax, boolean isUnsigned) {
		return getValue(rawData, x, y, dataMin, dataMax);
	}

	protected static short convertToFloat16(float f) {
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

	@Override
	public String getID() {
		return "float:" + uuid;
	}
}
