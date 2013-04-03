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

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;
import java.util.UUID;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.uf.common.image.colormap.ColorMapParameters;
import com.raytheon.uf.common.image.data.IColormappedDataPreparer;
import com.raytheon.uf.common.image.data.IImageDataPreparer;
import com.raytheon.uf.common.image.data.resp.EDEXImageData;
import com.raytheon.uf.common.image.data.resp.IOImageData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ColormapDataPreparer implements IImageDataPreparer {

	private IColormappedDataPreparer preparer;

	private ColorMapParameters parameters;

	private IOImageData imageData = null;

	public ColormapDataPreparer(IColormappedDataPreparer preparer,
			ColorMapParameters parameters) {
		this.preparer = preparer;
		this.parameters = parameters;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.viz.core.data.IImageDataPreparer#prepareData()
	 */
	@Override
	public IOImageData prepareData() throws Exception {
		if (imageData == null) {
			Buffer buf;
			int width, height;

			if (preparer instanceof FloatDataPreparer) {
				FloatDataPreparer fdp = (FloatDataPreparer) preparer;
				width = fdp.getDataSetBounds().width;
				height = fdp.getDataSetBounds().height;
				buf = fdp.getFloatBuffer();
			} else {
				EDEXImageData data = this.preparer.prepareData();
				buf = data.getData();
				width = data.getDatasetBounds().width;
				height = data.getDatasetBounds().height;
			}

			boolean log = parameters.isLogarithmic();

			float naturalMin = parameters.getDataMin();
			float naturalMax = parameters.getDataMax();

			float cmapMin = !log ? parameters.getColorMapMin() : (float) Math
					.log(parameters.getColorMapMin());
			float cmapMax = !log ? parameters.getColorMapMax() : (float) Math
					.log(parameters.getColorMapMax());

			int colors = parameters.getColorMap().getSize();

			float diff = Math.abs(cmapMax - cmapMin);

			byte[] cmapedData = new byte[buf.capacity()];
			if (buf instanceof ByteBuffer) {
				ByteBuffer bbuf = (ByteBuffer) buf;
				for (int i = 0; i < cmapedData.length; ++i) {
					float naturalValue = (((bbuf.get(i) & 0xFF) * Math
							.abs(naturalMax - naturalMin)) + naturalMin);
					if (log) {
						naturalValue = (float) Math.log(naturalValue);
					}
					cmapedData[i] = (byte) ((naturalValue - cmapMin) / diff);
				}
			} else if (buf instanceof FloatBuffer) {
				FloatBuffer bbuf = (FloatBuffer) buf;

				for (int i = 0; i < cmapedData.length; ++i) {
					float naturalValue = bbuf.get(i);
					double dataValue = (colors * (((log ? Math
							.log(naturalValue) : naturalValue) - cmapMin) / diff));

					// this is to handle values that are outside of the colormap
                    // range there is probably a better way to handle this
					if (dataValue >= colors) {
						dataValue = colors - 1;
					} else if (dataValue < 0) {
						dataValue = 0;
					}
					cmapedData[i] = (byte) dataValue;
				}
			} else if (buf instanceof IntBuffer) {
                // TODO This probably won't work. If so, change it to look like
                // the case for short data below.
				IntBuffer ibuf = (IntBuffer) buf;
				for (int i = 0; i < cmapedData.length; ++i) {
					float naturalValue = ((ibuf.get(i) * (naturalMax - naturalMin)) + naturalMin);
					if (log) {
						naturalValue = (float) Math.log(naturalValue);
					}
					cmapedData[i] = (byte) (colors * (naturalValue - cmapMin) / diff);
				}
			} else if (buf instanceof ShortBuffer) {
				ShortBuffer sbuf = (ShortBuffer) buf;
				for (int i = 0; i < cmapedData.length; ++i) {
                    float naturalValue = sbuf.get(i);
                    double dataValue = (colors * (((log ? Math
                            .log(naturalValue) : naturalValue) - cmapMin) / diff));
                    // this is to handle values that are outside of the colormap
                    // range
                    // there is probably a better way to handle this
                    if (dataValue >= colors) {
                        dataValue = colors - 1;
                    } else if (dataValue < 0) {
                        dataValue = 0;
                    }
                    cmapedData[i] = (byte) dataValue;
				}
			}

			IndexColorModel cm = ColorMapManager.buildColorModel(parameters
					.getColorMap());

			DataBufferByte byteArray = new DataBufferByte(cmapedData, width
					* height);

			MultiPixelPackedSampleModel sample = new MultiPixelPackedSampleModel(
					DataBuffer.TYPE_BYTE, width, height,
					ColorMapManager.NUMBER_BITS);
			WritableRaster writeRaster = Raster.createWritableRaster(sample,
					byteArray, new Point(0, 0));

			BufferedImage bi = new BufferedImage(width, height,
					BufferedImage.TYPE_BYTE_INDEXED, cm);
			bi.setData(writeRaster);

			imageData = new IOImageData(bi, 0, "colormapped_image."
					+ UUID.randomUUID().toString());
		}

		return imageData;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.viz.core.data.IDataPreparer#getID()
	 */
	@Override
	public String getID() {
		// TODO Auto-generated method stub
		return null;
	}

}
