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
import java.util.UUID;

import com.raytheon.uf.common.image.data.IColormappedDataPreparer;
import com.raytheon.uf.common.image.data.resp.EDEXImageData;

/* 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            mschenke     Initial creation
 *
 * </pre>
 *
 * @author mschenke
 */
/**
 * This class is used to retrieve the data preparer that should be used based on
 * the object you have if it is a number array or number buffer, ie FLoatBuffer
 * float[]
 * 
 * @version 1.0
 */
public abstract class AbstractNumDataPreparer implements
		IColormappedDataPreparer {

	// FIXME: these are taken from GL, need a better way to handle it

	protected static final int LUMINANCE = 6409;

	protected static final int LUMINANCE8 = 32832;

	protected static final int LUMINANCE16 = 32834;

	protected static final int LUMINANCE16F_ARB = 34846;

	protected static final int UNSIGNED_BYTE = 5121;

	protected static final int INT = 5124;

	protected static final int SHORT = 5122;

	protected static final int HALF_FLOAT_ARB = 5131;

	protected int internalTextureFormat;

	protected int textureFormat;

	protected int textureType;

	protected int size;

	protected Rectangle datasetBounds;

	protected int[] totalDatasetDimensions;

	protected Buffer buffer;

	protected EDEXImageData imageData = null;

	protected String uuid = UUID.randomUUID().toString();

	protected AbstractNumDataPreparer() {
	}

	protected AbstractNumDataPreparer(Buffer data, Rectangle datasetBounds,
			int[] totalDatasetDimensions) {
		this.buffer = data;
		this.datasetBounds = datasetBounds;
		this.totalDatasetDimensions = totalDatasetDimensions;
		init();
	}

	protected abstract void init();

	public Rectangle getDataSetBounds() {
		return datasetBounds;
	}

	public EDEXImageData prepareData() {
		if (imageData == null) {
			imageData = new EDEXImageData();
			imageData.setData(buffer);
			imageData.setTextureFormat(textureFormat);
			imageData.setTextureInternalFormat(internalTextureFormat);
			imageData.setTextureType(textureType);
			imageData.setDatasetBounds(datasetBounds);
			imageData.setTotalSize(size);
		}
		return imageData;
	}

	@Override
	public abstract String getID();

}
