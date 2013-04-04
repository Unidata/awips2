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
package com.raytheon.uf.common.image.data.resp;

import java.awt.Rectangle;
import java.nio.Buffer;

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
 * Edex colormapped data, takes a data buffer, colormapper and texture values.
 * Used by GLColormappedImage
 * 
 * @version 1.0
 */
public class EDEXImageData extends ImageData {

	private Buffer data;

	private int textureFormat;

	private int textureInternalFormat;

	private int textureType;

	private int totalSize;

	private Rectangle datasetBounds;

	public EDEXImageData(Buffer data, int tf, int tif, int tt, int ts) {
		this.data = data;
		this.textureFormat = tf;
		this.textureInternalFormat = tif;
		this.textureType = tt;
		this.totalSize = ts;
	}

	public EDEXImageData() {

	}

	public Buffer getData() {
		return this.data;
	}

	public int getTextureFormat() {
		return this.textureFormat;
	}

	public int getTextureInternalFormat() {
		return this.textureInternalFormat;
	}

	public int getTextureType() {
		return this.textureType;
	}

	public int getTotalSize() {
		return this.totalSize;
	}

	public void setData(Buffer data) {
		this.data = data;
	}

	public void setTextureFormat(int textureFormat) {
		this.textureFormat = textureFormat;
	}

	public void setTextureInternalFormat(int textureInternalFormat) {
		this.textureInternalFormat = textureInternalFormat;
	}

	public void setTextureType(int textureType) {
		this.textureType = textureType;
	}

	public void setTotalSize(int totalSize) {
		this.totalSize = totalSize;
	}

	public Rectangle getDatasetBounds() {
		return datasetBounds;
	}

	public void setDatasetBounds(Rectangle datasetBounds) {
		this.datasetBounds = datasetBounds;
	}

}
