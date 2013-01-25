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
import java.nio.IntBuffer;

import com.raytheon.uf.common.image.data.IColormappedDataPreparer;

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
 * Data Preparer for int[] and IntBuffers
 * 
 * @version 1.0
 */
public class IntDataPreparer extends AbstractNumDataPreparer {

	public IntDataPreparer() {
		super();
	}

	public IntDataPreparer(IntBuffer data, Rectangle datasetBounds, int[] dims) {
		super(data, datasetBounds, dims);
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
		if (buffer == null) {
			return Double.NaN;
		}

		if (buffer instanceof IntBuffer) {
			return ((IntBuffer) buffer).get(y * this.datasetBounds.width + x);
		} else if (buffer instanceof ByteBuffer) {
			return ((ByteBuffer) buffer).asIntBuffer().get(
					y * this.datasetBounds.width + x);
		}
		return Double.NaN;
	}

	@Override
	public double getValue(Buffer buffer, int x, int y, float dataMin,
			float dataMax, boolean isUnsigned) {
		return getValue(buffer, x, y, dataMin, dataMax);
	}

	@Override
	public IColormappedDataPreparer newInstance(Object data,
			Rectangle datasetBounds, int[] dims) {
		return new IntDataPreparer((IntBuffer) data, datasetBounds, dims);
	}

	@Override
	protected void init() {
		this.textureFormat = LUMINANCE;
		this.internalTextureFormat = LUMINANCE;
		this.textureType = INT;
		this.size = this.datasetBounds.width * this.datasetBounds.height * 4;
	}

	@Override
	public String getID() {
		return "int:" + uuid;
	}
}
