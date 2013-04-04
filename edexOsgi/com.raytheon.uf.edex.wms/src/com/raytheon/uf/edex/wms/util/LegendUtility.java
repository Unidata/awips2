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
 */
package com.raytheon.uf.edex.wms.util;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.uf.common.colormap.ColorMap;

public class LegendUtility {

	public static BufferedImage buildLabels(int width, int height,
			String[] values, float min, float max) {
		float delta = max - min;
		BufferedImage labels = new BufferedImage(width, height,
				BufferedImage.TYPE_INT_ARGB);
		Graphics2D g2 = labels.createGraphics();
		for (String valueStr : values) {
			float value = Float.parseFloat(valueStr);
			int x = Math.round((((value - min) / delta) * width));
			int y = height / 2;
			if (0 <= x && x < width && 0 <= y && y < height) {
				drawString(g2, valueStr, x, y);
			}
		}
		return labels;
	}

	public static void drawString(Graphics2D g2, String text, int x, int y) {
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		g2.setFont(font);
		FontMetrics metrics = g2.getFontMetrics(font);
		Color bgColor = Color.BLACK;
		Color textColor = Color.WHITE;
		Rectangle2D textBounds = metrics.getStringBounds(text, g2);
		g2.setColor(bgColor);
		int x0 = (int) (x - (textBounds.getWidth() / 2) - 1);
		int w = (int) textBounds.getWidth() + 2;
		if (x0 < 0) {
			x0 = 0;
		}
		g2.fillRect(x0, (int) (y - (textBounds.getHeight() / 2) + 1), w,
				(int) textBounds.getHeight());
		g2.setColor(textColor);
		x0++;
		g2.drawString(text, x0, (int) (y + (textBounds.getHeight() / 2) - 1));
	}

	public static BufferedImage buildColorbar(ColorMap cmap, int width,
			int height) {
		IndexColorModel cm = ColorMapManager.buildColorModel(cmap);
		byte[] data = prepData(cmap.getSize(), width, height);
		DataBufferByte byteArray = new DataBufferByte(data, width * height);
		MultiPixelPackedSampleModel sample = new MultiPixelPackedSampleModel(
				DataBuffer.TYPE_BYTE, width, height,
				ColorMapManager.NUMBER_BITS);
		WritableRaster writeRaster = Raster.createWritableRaster(sample,
				byteArray, new Point(0, 0));
		BufferedImage colorBar = new BufferedImage(width, height,
				BufferedImage.TYPE_BYTE_INDEXED, cm);
		colorBar.setData(writeRaster);
		return colorBar;
	}

	private static byte[] prepData(int colormapRange, int width, int height) {
		byte[] data = new byte[width * height];

		for (int x = 0; x < width; ++x) {
			for (int y = 0; y < height; ++y) {
				int idx = (y * width) + x;
				data[idx] = (byte) ((x / (double) (width - 1)) * colormapRange);
			}
		}

		return data;
	}
}
