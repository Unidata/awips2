/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.util;

import java.awt.image.RenderedImage;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.Colormapper;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.edex.ogc.common.colormap.MapRange;
import com.raytheon.uf.edex.ogc.common.colormap.StyleRule;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class ColorMapUtility {

	public static final double CALCULATED_PAD_RATIO = 0.25;

    public static RenderedImage applyColorMap(ByteDataRecord record,
			ColorMap cmap, StyleRule styleRule) throws Exception {
        ColorMapData prep = buildColorMapData(record);
		return applyColorMap(prep, cmap, styleRule, record);
	}

    public static RenderedImage applyColorMap(IntegerDataRecord record,
			ColorMap cmap, StyleRule styleRule) throws Exception {
        ColorMapData prep = buildColorMapData(record);
		return applyColorMap(prep, cmap, styleRule, record);
	}

	// public static BufferedImage applyColorMap(LongDataRecord record,
	// ColorMap cmap) {
	// IColormappedDataPreparer prep = buildPreparer(record);
	// return applyColorMap(prep, cmap);
	// }

    public static RenderedImage applyColorMap(ShortDataRecord record,
			ColorMap cmap, StyleRule styleRule) throws Exception {
        ColorMapData prep = buildColorMapData(record);
		return applyColorMap(prep, cmap, styleRule, record);
	}

    public static RenderedImage applyColorMap(FloatDataRecord record,
			ColorMap cmap, StyleRule styleRule) throws Exception {
        ColorMapData prep = buildColorMapData(record);
		return applyColorMap(prep, cmap, styleRule, record);
	}

    public static RenderedImage applyColorMap(ColorMapData prep,
			ColorMap cmap, StyleRule styleRule, IDataRecord record)
			throws Exception {
		ColorMapParameters cmapParams = null;
        cmapParams = getCmapParams(cmap, styleRule, record);

        return Colormapper.colorMap(prep, cmapParams);

	}

	public static ColorMapParameters getCmapParams(ColorMap colormap,
            StyleRule rule, IDataRecord record) {

		boolean isLog = false;
		boolean doCalcRange = false;
		float min = 0.0f;
		float max = 255.0f;
        float dataMin = 0;
        float dataMax = 255.0f;

		if (rule == null) {
			// calculate proper range
			doCalcRange = true;
		} else {
			MapRange range = rule.getMapRange();
			if (range == null) {
				// calculate proper range
				doCalcRange = true;
			} else {
				if (range.getType().equalsIgnoreCase("log")) {
					// calculate correct log range
					min = range.getUpperMinimum();
					max = range.getUpperMaximum();
					isLog = true;
				} else if (range.getType().equalsIgnoreCase("linear")) {
					// use defined range
					min = range.getUpperMinimum();
					max = range.getUpperMaximum();
				} else {
					// calculate proper range
					doCalcRange = true;
				}
			}

            MapRange dataRange = rule.getDataRange();
            if (dataRange != null) {
                dataMin = dataRange.getUpperMinimum();
                dataMax = dataRange.getUpperMaximum();
            } else {
                dataMin = min;
                dataMax = max;
            }
		}

		ColorMapParameters params = new ColorMapParameters();
		params.setColorMap(colormap);
		params.setLogarithmic(isLog);

		if (doCalcRange) {
			calculateDataBounds(record, true, params);
		} else {
			params.setColorMapMin(min);
			params.setColorMapMax(max);
            params.setDataMin(dataMin);
            params.setDataMax(dataMax);
		}

		return params;
	}

    public static ColorMapData buildColorMapData(ByteDataRecord dataRecord) {
        ByteBuffer bbuff = ByteBuffer.wrap(dataRecord.getByteData());
        long[] sizes = dataRecord.getSizes();
        int[] dims = { (int) sizes[0], (int) sizes[1] };
        return new ColorMapData(bbuff, dims);
    }

    public static ColorMapData buildColorMapData(IntegerDataRecord dataRecord) {
		IntBuffer buff = IntBuffer.wrap(dataRecord.getIntData());
		long[] sizes = dataRecord.getSizes();
		int[] dims = { (int) sizes[0], (int) sizes[1] };
        return new ColorMapData(buff, dims);
	}

    public static ColorMapData buildColorMapData(
			ShortDataRecord dataRecord) {
		ShortBuffer buff = ShortBuffer.wrap(dataRecord.getShortData());
		long[] sizes = dataRecord.getSizes();
		int[] dims = { (int) sizes[0], (int) sizes[1] };
        return new ColorMapData(buff, dims);
	}

    public static ColorMapData buildColorMapData(
			FloatDataRecord dataRecord) {
		FloatBuffer buff = FloatBuffer.wrap(dataRecord.getFloatData());
		long[] sizes = dataRecord.getSizes();
		int[] dims = { (int) sizes[0], (int) sizes[1] };
        return new ColorMapData(buff, dims);
	}

    public static ColorMapData buildColorMapData(IDataRecord dataRecord)
			throws Exception {
		long[] sizes = dataRecord.getSizes();
		int[] dims = { (int) sizes[0], (int) sizes[1] };
        ColorMapData rval = null;
		if (dataRecord instanceof ByteDataRecord) {
			ByteBuffer buff = ByteBuffer.wrap(((ByteDataRecord) dataRecord)
					.getByteData());
            rval = new ColorMapData(buff, dims);
		} else if (dataRecord instanceof ShortDataRecord) {
			ShortBuffer buff = ShortBuffer.wrap(((ShortDataRecord) dataRecord)
					.getShortData());
            rval = new ColorMapData(buff, dims);
		} else if (dataRecord instanceof IntegerDataRecord) {
			IntBuffer buff = IntBuffer.wrap(((IntegerDataRecord) dataRecord)
					.getIntData());
            rval = new ColorMapData(buff, dims);
		} else if (dataRecord instanceof FloatDataRecord) {
			FloatBuffer buff = FloatBuffer.wrap(((FloatDataRecord) dataRecord)
					.getFloatData());
            rval = new ColorMapData(buff, dims);
		} else {
			throw new IllegalArgumentException(
					"Unable to apply colormap to class "
							+ dataRecord.getClass());
		}

		return rval;
	}

	/**
	 * Returns an 2D GeneralEnvelope of data bounds. Dimension 1 contains the
	 * values that map to the minimum and maximum of the colormap. Dimension 2
	 * contains the actual minimum and maximum values in the data.
	 * 
	 * @param record
	 *            IDataRecord
	 * @return GeneralEnvelope
	 */
	public static void calculateDataBounds(IDataRecord record, boolean doPad,
			ColorMapParameters params) {
		double dataMin;
		double dataMax;
		if (record instanceof ByteDataRecord) {
			dataMin = Byte.MAX_VALUE;
			dataMax = Byte.MIN_VALUE;
			byte[] data = ((ByteDataRecord) record).getByteData();
			for (byte b : data) {
				if (b > dataMax) {
					dataMax = b;
				}
				if (b < dataMin) {
					dataMin = b;
				}
			}
		} else if (record instanceof FloatDataRecord) {
			dataMin = Float.MAX_VALUE;
			dataMax = Float.MIN_VALUE;
			float[] data = ((FloatDataRecord) record).getFloatData();
			for (float b : data) {
				if (b > dataMax) {
					dataMax = b;
				}
				if (b < dataMin) {
					dataMin = b;
				}
			}
		} else if (record instanceof ShortDataRecord) {
			dataMin = Short.MAX_VALUE;
			dataMax = Short.MIN_VALUE;
			short[] data = ((ShortDataRecord) record).getShortData();
			for (short b : data) {
				if (b > dataMax) {
					dataMax = b;
				}
				if (b < dataMin) {
					dataMin = b;
				}
			}
		} else if (record instanceof IntegerDataRecord) {
			dataMin = Integer.MAX_VALUE;
			dataMax = Integer.MIN_VALUE;
			int[] data = ((IntegerDataRecord) record).getIntData();
			for (int b : data) {
				if (b > dataMax) {
					dataMax = b;
				}
				if (b < dataMin) {
					dataMin = b;
				}
			}
		} else {
			throw new IllegalArgumentException("Unsupported record class "
					+ record.getClass());
		}

		double cmapMin = dataMin;
		double cmapMax = dataMax;

		if (doPad) {
			double pad = (cmapMax - cmapMin) * CALCULATED_PAD_RATIO;
			cmapMin -= pad;
			cmapMax += pad;
		}

		params.setDataMin(new Double(dataMin).floatValue());
		params.setDataMax(new Double(dataMax).floatValue());
		params.setColorMapMin(new Double(cmapMin).floatValue());
		params.setColorMapMax(new Double(cmapMax).floatValue());
	}
}
