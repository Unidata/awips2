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

import com.raytheon.uf.common.image.data.IColormappedDataPreparer;
import com.raytheon.uf.common.util.BufferUtil;

/* 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2009            mschenke     Initial creation
 *
 * </pre>
 *
 * @author mschenke
 */
/**
 * DataPreparer for byte[] and ByteBuffer
 * 
 * @version 1.0
 */
public class ByteDataPreparer extends AbstractNumDataPreparer {

	public ByteDataPreparer() {
		super();
	}

	public ByteDataPreparer(Buffer data, Rectangle datasetBounds, int[] dims) {
		super(BufferUtil.directBuffer((ByteBuffer) data), datasetBounds, dims);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.data.impls.colormap.AbstractColorMapper#getValue
	 * (int, int)
	 */
	@Override
	public double getValue(Buffer buffer, int x, int y, float dataMin,
			float dataMax) {
		return getValue(buffer, x, y, dataMin, dataMax, true);
	}

	@Override
	public double getValue(Buffer buffer, int x, int y, float dataMin,
			float dataMax, boolean isUnsigned) {
		if (buffer == null || buffer instanceof ByteBuffer == false)
			return Double.NaN;

		if (isUnsigned) {
			return ((ByteBuffer) buffer).get(y
					* BufferUtil.wordAlignedByteWidth(this.datasetBounds) + x) & 0xFF;
		}

		return ((ByteBuffer) buffer).get(y
				* BufferUtil.wordAlignedByteWidth(this.datasetBounds) + x);
	}

	@Override
	public IColormappedDataPreparer newInstance(Object data,
			Rectangle datasetBounds, int[] dims) {
		return new ByteDataPreparer((ByteBuffer) data, datasetBounds, dims);
	}

	@Override
	protected void init() {
		this.textureFormat = LUMINANCE;
		this.internalTextureFormat = LUMINANCE8;
		this.textureType = UNSIGNED_BYTE;
		this.size = this.datasetBounds.width * this.datasetBounds.height;
	}

	@Override
	public String getID() {
		return "byte:" + uuid;
	}
}
