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
package com.raytheon.uf.common.spatial.reprojection;

import java.awt.Point;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferShort;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.util.Arrays;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;

/**
 * TODO Add Description
 * 
 * @author bclement
 * @version 1.0
 */
public class ShortDataReprojector extends
		AbstractDataReprojector<ShortDataRecord> {

	protected short fill = 0;

    protected short dataMaskValue = -1;

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.spatial.reprojection.DataReprojector#getGridCoverage
	 * (com.raytheon.uf.common.datastorage.records.IDataRecord,
	 * org.opengis.referencing.crs.CoordinateReferenceSystem,
	 * org.opengis.geometry.Envelope)
	 */
	@Override
	protected GridCoverage2D getGridCoverage(IDataRecord record,
			ReferencedEnvelope env) throws Exception {
		ShortDataRecord dataRecord = (ShortDataRecord) record;
		short[] data = dataRecord.getShortData();
		DataBuffer buff = new DataBufferShort(data, data.length);
		int x = (int) dataRecord.getSizes()[0];
		int y = (int) dataRecord.getSizes()[1];
		CoordinateReferenceSystem crs = env.getCoordinateReferenceSystem();
		return constructGridCoverage(crs.getName() + " Grid", buff, x, y, env);
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.spatial.reprojection.DataReprojector#getMaskCoverage
     * (com.raytheon.uf.common.datastorage.records.IDataRecord,
     * org.opengis.referencing.crs.CoordinateReferenceSystem,
     * org.opengis.geometry.Envelope)
     */
    @Override
    protected GridCoverage2D getMaskCoverage(IDataRecord record,
            ReferencedEnvelope env) throws Exception {
        int x = (int) record.getSizes()[0];
        int y = (int) record.getSizes()[1];
        short[] mask = new short[x * y];
        Arrays.fill(mask, dataMaskValue);
        DataBufferShort buff = new DataBufferShort(mask, mask.length);
        CoordinateReferenceSystem crs = env.getCoordinateReferenceSystem();
        return constructGridCoverage(crs.getName() + " Grid", buff, x, y, env);
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.spatial.reprojection.DataReprojector#extractData(org
	 * .geotools.coverage.grid.GridCoverage2D)
	 */
	@Override
	protected ShortDataRecord extractData(GridCoverage2D coverage) {
		RenderedImage image = coverage.getRenderedImage();
		Raster raster;
		if (image.getNumXTiles() == 1 && image.getNumYTiles() == 1) {
			// we can directly access data
			raster = image.getTile(0, 0);
		} else {
			// need to copy data out
			raster = image.getData();
		}
		DataBufferShort dataBuffer = (DataBufferShort) raster.getDataBuffer();
		short[] data = dataBuffer.getData();
		int height = raster.getHeight();
		int width = raster.getWidth();
		return new ShortDataRecord("", "", data, 2,
				new long[] { width, height });
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.spatial.reprojection.DataReprojector#extractData(org
     * .geotools.coverage.grid.GridCoverage2D,
     * org.geotools.coverage.grid.GridCoverage2D)
     */
    @Override
    protected ShortDataRecord extractData(GridCoverage2D coverage,
            GridCoverage2D maskCoverage) {
        RenderedImage image = coverage.getRenderedImage();
        Raster raster;
        if (image.getNumXTiles() == 1 && image.getNumYTiles() == 1) {
            // we can directly access data
            raster = image.getTile(0, 0);
        } else {
            // need to copy data out
            raster = image.getData();
        }
        DataBufferShort dataBuffer = (DataBufferShort) raster.getDataBuffer();
        short[] data = dataBuffer.getData();

        // Extract mask
        image = maskCoverage.getRenderedImage();
        if (image.getNumXTiles() == 1 && image.getNumYTiles() == 1) {
            // we can directly access data
            raster = image.getTile(0, 0);
        } else {
            // need to copy data out
            raster = image.getData();
        }
        dataBuffer = (DataBufferShort) raster.getDataBuffer();
        short[] mask = dataBuffer.getData();

        if (mask.length == data.length) {
            for (int i = 0; i < data.length; ++i) {
                if (mask[i] != dataMaskValue) {
                    data[i] = fill;
                }
            }
        }

        int height = raster.getHeight();
        int width = raster.getWidth();
        return new ShortDataRecord("", "", data, 2,
                new long[] { width, height });
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.spatial.reprojection.DataReprojector#getDataSlice(com
	 * .raytheon.uf.common.datastorage.records.IDataRecord,
	 * com.raytheon.uf.common.datastorage.Request)
	 */
	@Override
	protected ShortDataRecord getDataSlice(IDataRecord record, Request req) {
		ShortDataRecord dataRecord = (ShortDataRecord) record;
		int[] max = req.getMaxIndexForSlab();
		int[] min = req.getMinIndexForSlab();
		int toWidth = max[0] - min[0];
		int toHeight = max[1] - min[1];
		short[] from = dataRecord.getShortData();
		int fromWidth = (int) dataRecord.getSizes()[0];
		short[] to = new short[toWidth * toHeight];
		for (int fromY = min[1], toY = 0; fromY < max[1]; ++fromY, ++toY) {
			int toRow = toY * toWidth;
			int fromRow = fromY * fromWidth;
			for (int fromX = min[0], toX = 0; fromX < max[0]; ++fromX, ++toX) {
				to[toRow + toX] = from[fromRow + fromX];
			}
		}
		long[] sizes = { toWidth, toHeight };
		return new ShortDataRecord("", "", to, 2, sizes);
	}

	/**
	 * @return the fill
	 */
	public short getFill() {
		return fill;
	}

	/**
	 * @param fill
	 *            the fill to set
	 */
	public void setFill(short fill) {
		this.fill = fill;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.spatial.reprojection.AbstractDataReprojector#compatible
	 * (com.raytheon.uf.common.datastorage.records.IDataRecord)
	 */
	@Override
	protected boolean compatible(IDataRecord dataRecord) {
		return dataRecord instanceof ShortDataRecord;
	}

	@Override
	protected IDataRecord getDataPoints(IDataRecord record, Request req) {
		ShortDataRecord dataRecord = (ShortDataRecord) record;
		short[] from = dataRecord.getShortData();
		int fromWidth = (int) dataRecord.getSizes()[0];
		Point[] points = req.getPoints();
		short[] to = new short[points.length];
		for (int i = 0; i < to.length; ++i) {
			Point p = points[i];
			to[i] = from[p.y * fromWidth + p.x];
		}
		return new ShortDataRecord("", "", to, 1, new long[] { to.length });
	}

}
