/**********************************************************************
 *
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
 **********************************************************************/
package com.raytheon.uf.edex.wms.util;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.feature.FeatureCollection;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.map.DefaultMapContext;
import org.geotools.map.MapContext;
import org.geotools.renderer.GTRenderer;
import org.geotools.renderer.lite.StreamingRenderer;
import org.geotools.styling.Style;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class StyleUtility {

	public static BufferedImage applyStyle(GridCoverage2D coverage, Style style) {
		MapContext context = new DefaultMapContext();
		try {
			context.addLayer(coverage, style);
			GridGeometry2D geom = coverage.getGridGeometry();
			Rectangle rec = geom.getGridRange2D();
			return mapToImage(context, rec);
		} finally {
			if (context != null) {
				// context.dispose();
			}
		}
	}

	public static BufferedImage applyStyle(
			FeatureCollection<SimpleFeatureType, SimpleFeature> coll,
			Style style, Rectangle imageDims, ReferencedEnvelope mapBounds) {
		DefaultMapContext map = new DefaultMapContext(coll.getSchema()
				.getCoordinateReferenceSystem());
		try {
			map.addLayer(coll, style);
			return mapToImage(map, imageDims, mapBounds);
		} finally {
			if (map != null) {
				// map.dispose();
			}
		}
	}

	public static BufferedImage mapToImage(MapContext map, Rectangle imageDims,
			ReferencedEnvelope mapBounds) {
		return mapToImage(map, imageDims, mapBounds, null);
	}

	public static BufferedImage mapToImage(MapContext map, Rectangle imageDims,
			Color bgcolor) {
		return mapToImage(map, imageDims, map.getAreaOfInterest(), bgcolor);
	}

	public static BufferedImage mapToImage(MapContext map, Rectangle imageDims,
			ReferencedEnvelope mapBounds, Color bgcolor) {
		GTRenderer renderer = new StreamingRenderer();
		renderer.setContext(map);
		BufferedImage image = new BufferedImage(imageDims.width,
				imageDims.height, BufferedImage.TYPE_INT_ARGB);
		Graphics2D gr = image.createGraphics();
		if (bgcolor != null) {
			gr.setColor(bgcolor);
			gr.fill(imageDims);
		}
		renderer.paint(gr, imageDims, mapBounds);
		gr.dispose();
		return image;
	}

	public static BufferedImage mapToImage(MapContext map, Rectangle imageDims) {
		return mapToImage(map, imageDims, map.getAreaOfInterest());
	}
}
