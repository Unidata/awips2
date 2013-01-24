package com.raytheon.uf.edex.wms.util;

import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.styling.ColorMap;
import org.geotools.styling.ColorMapEntry;
import org.geotools.styling.RasterSymbolizer;

import com.raytheon.uf.common.colormap.Color;

public class SldUtility {
	protected Log log = LogFactory.getLog(this.getClass());

	public static com.raytheon.uf.common.colormap.ColorMap getRaster(
			RasterSymbolizer rasterSymbolizer) {
		com.raytheon.uf.common.colormap.ColorMap cmap = null;
		ColorMap colorMap = rasterSymbolizer.getColorMap();
		ColorMapEntry[] colorMapEntries = colorMap.getColorMapEntries();
		Color[] colors;
		switch (colorMap.getType()) {
		case ColorMap.TYPE_RAMP: // 1
			colors = buildRampStyle(colorMapEntries);
			cmap = colors2ColorMap(colors);
			break;
		case ColorMap.TYPE_INTERVALS: // 2
			colors = buildIntervalssStyle(colorMapEntries);
			cmap = colors2ColorMap(colors);
			break;
		case ColorMap.TYPE_VALUES: // 3
			colors = buildValuesStyle(colorMapEntries);
			cmap = colors2ColorMap(colors);
			break;
		default:
			// Should never happen. If type attribute of
			// ColorMap is not specified, parser defaults to
			// TYPE_RAMP.
			break;
		}
		return cmap;
	}

	public static String[] buildLabels(RasterSymbolizer rasterSymbolizer) {
		ArrayList<String> values = null;
		ColorMap colorMap = rasterSymbolizer.getColorMap();
		ColorMapEntry[] colorMapEntries = colorMap.getColorMapEntries();
		ArrayList<ColorMapEntry> entryList = new ArrayList<ColorMapEntry>(
				Arrays.asList(colorMapEntries));
		entryList.remove(0);
		entryList.remove(entryList.size() - 1);
		int numEntries = entryList.size();
		while (numEntries > 8) {
			numEntries /= 2;
		}
		values = new ArrayList<String>(numEntries);
		int interval = entryList.size() / numEntries;
		for (int i = 0; i < entryList.size(); i = i + interval) {
			values.add(entryList.get(i).getQuantity().toString());
		}
		return values.toArray(new String[values.size()]);
	}

	protected static Color[] buildIntervalssStyle(ColorMapEntry[] entries) {
		int n = 256;
		Color[] colorMap = new Color[n];

		ColorMapEntry e0 = entries[0];
		int q0 = (int) Math.round((Double) e0.getQuantity().evaluate(null));
		Color c0 = colorMapEntry2Color(e0);
		for (int i = 0; i <= q0; ++i) {
			colorMap[i] = c0;
		}

		for (int i = 1; i < entries.length; ++i) {
			ColorMapEntry e1 = entries[i];
			int q1 = (int) Math.round((Double) e1.getQuantity().evaluate(null));
			Color c1 = colorMapEntry2Color(e1);
			int numColors = q1 - q0;
			for (int j = 1; j < numColors; ++j) {
				colorMap[q0 + j] = colorMapEntry2Color(e0);
			}
			colorMap[q1] = colorMapEntry2Color(e1);
			e0 = e1;
			q0 = q1;
			c0 = c1;
		}

		ColorMapEntry en = entries[entries.length - 1];
		int qn = (int) Math.round((Double) en.getQuantity().evaluate(null));
		Color cn = colorMapEntry2Color(en);
		if (qn < (n - 1)) {
			for (int i = (qn + 1); i < n; ++i) {
				colorMap[i] = cn;
			}
		}

		return colorMap;
	}

	protected static Color[] buildValuesStyle(ColorMapEntry[] entries) {
		Color[] colorMap = new Color[entries.length];
		for (int i = 0; i < entries.length; ++i) {
			ColorMapEntry entry = entries[i];
			Color color = colorMapEntry2Color(entry);
			colorMap[i] = color;
		}
		return colorMap;
	}

	protected static Color[] buildRampStyle(ColorMapEntry[] entries) {
		int n = 256;
		Color[] colorMap = new Color[n];

		ColorMapEntry e0 = entries[0];
		int q0 = (int) Math.round((Double) e0.getQuantity().evaluate(null));
		Color c0 = colorMapEntry2Color(e0);
		for (int i = 0; i <= q0; ++i) {
			colorMap[i] = c0;
		}

		for (int i = 1; i < entries.length; ++i) {
			ColorMapEntry e1 = entries[i];
			int q1 = (int) Math.round((Double) e1.getQuantity().evaluate(null));
			Color c1 = colorMapEntry2Color(e1);
			int numColors = q1 - q0;
			for (int j = 1; j <= numColors; ++j) {
				Color cj = new Color();
				cj.setRed(((c1.getRed() - c0.getRed()) * (j / (float) numColors))
						+ c0.getRed());
				cj.setGreen(((c1.getGreen() - c0.getGreen()) * (j / (float) numColors))
						+ c0.getGreen());
				cj.setBlue(((c1.getBlue() - c0.getBlue()) * (j / (float) numColors))
						+ c0.getBlue());
				cj.setAlpha(((c1.getAlpha() - c0.getAlpha()) * (j / (float) numColors))
						+ c0.getAlpha());
				colorMap[q0 + j] = cj;
			}
			q0 = q1;
			c0 = c1;
		}

		ColorMapEntry en = entries[entries.length - 1];
		int qn = (int) Math.round((Double) en.getQuantity().evaluate(null));
		Color cn = colorMapEntry2Color(en);
		if (qn < (n - 1)) {
			for (int i = (qn + 1); i < n; ++i) {
				colorMap[i] = cn;
			}
		}

		return colorMap;
	}

	private static com.raytheon.uf.common.colormap.ColorMap colors2ColorMap(
			Color[] colors) {
		float[] red = new float[colors.length];
		float[] green = new float[colors.length];
		float[] blue = new float[colors.length];
		float[] alpha = new float[colors.length];
		for (int i = 0; i < colors.length; ++i) {
			red[i] = colors[i].getRed();
			green[i] = colors[i].getGreen();
			blue[i] = colors[i].getBlue();
			alpha[i] = colors[i].getAlpha();
		}
		com.raytheon.uf.common.colormap.ColorMap cmap = new com.raytheon.uf.common.colormap.ColorMap(
				"", red, green, blue, alpha);
		return cmap;
	}

	private static Color colorMapEntry2Color(ColorMapEntry entry) {
		String colorStr = (String) entry.getColor().evaluate(null);
		colorStr = colorStr.replaceAll("#", "");
		Double opacity = (Double) entry.getOpacity().evaluate(null);
		java.awt.Color jColor = new java.awt.Color(Integer.parseInt(colorStr,
				16));
		Color ufColor = new Color();
		ufColor.setRed(jColor.getRed() / 255f);
		ufColor.setBlue(jColor.getBlue() / 255f);
		ufColor.setGreen(jColor.getGreen() / 255f);
		ufColor.setAlpha(opacity.floatValue());
		return ufColor;
	}
}
