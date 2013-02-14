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
* Mar 30, 2012            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.wms.styling;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.legend.Glyph;
import org.geotools.styling.Description;
import org.geotools.styling.FeatureTypeStyle;
import org.geotools.styling.RasterSymbolizer;
import org.geotools.styling.Rule;
import org.geotools.styling.Style;
import org.geotools.styling.StyledLayerDescriptor;
import org.geotools.styling.Symbolizer;
import org.geotools.styling.TextSymbolizer;
import org.opengis.filter.Filter;
import org.opengis.util.InternationalString;
import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.sld.SldParser;
import com.raytheon.uf.edex.wms.sld.SldParserRegistry;
import com.raytheon.uf.edex.wms.util.LegendUtility;
import com.raytheon.uf.edex.wms.util.SldUtility;
import com.raytheon.uf.edex.wms.util.StyleLibrary;

/**
 *
 * @author bclement
 * @version 1.0	
 */
public abstract class AbstractSldStyleProvider implements StyleLookup {

	protected String styleLibraryFileName;

	protected StyleLibrary _styleLib;

	protected String sldVersion = "1.0.0";

	protected ClassLoader loader = null;

	protected StyledLayerMatcher matcher = null;

	protected static Log log = LogFactory
			.getLog(AbstractSldStyleProvider.class);

	/**
	 * @param styleLibraryFileName
	 */
	public AbstractSldStyleProvider(String styleLibraryFileName) {
		this.styleLibraryFileName = styleLibraryFileName;
	}

	public AbstractSldStyleProvider(StyledLayerDescriptor sld) {
		this._styleLib = new StyleLibrary(sld);
	}

	protected static StyleLibrary getStyleLib(InputStream in, String sldVersion)
			throws Exception {
		ApplicationContext ctx = EDEXUtil.getSpringContext();
		String[] beans = ctx.getBeanNamesForType(SldParserRegistry.class);
		SldParserRegistry sldReg = (SldParserRegistry) ctx.getBean(beans[0]);
		SldParser parser = sldReg.getParser(sldVersion);
		StyledLayerDescriptor sld = parser.parse(in);
		return new StyleLibrary(sld);
	}

	protected StyleLibrary getStyleLib(String path, String version)
			throws Exception {
		IPathManager pathMgr = PathManagerFactory.getPathManager();
		LocalizationContext edexStaticBase = pathMgr.getContext(
				LocalizationContext.LocalizationType.EDEX_STATIC,
				LocalizationContext.LocalizationLevel.BASE);

		File sldFile = pathMgr.getFile(edexStaticBase, path);
		InputStream in = new FileInputStream(sldFile);
		return getStyleLib(in, version);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.reg.DefaultWmsSource#getStyleLibraryFileName()
	 */
	protected String getStyleLibraryFileName() {
		return styleLibraryFileName;
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.edex.ogc.common.StyleLookup#lookup(java.lang.String)
	 */
	@Override
	public String lookup(String layername) {
		try {
			StyleLibrary lib = getStyleLib();
			Style style = lib.getDefault(layername);
			if (style != null) {
				return style.getName();
			}
		} catch (Exception e) {
			log.error("Problem finding match", e);
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.ogc.common.StyleLookup#getStyles()
	 */
	@Override
	public List<OgcStyle> getStyles() {
		StyleLibrary lib;
		try {
			lib = getStyleLib();
		} catch (WmsException e) {
			log.error("Problem gettin style library", e);
			return new ArrayList<OgcStyle>(0);
		}
		Set<String> styles = lib.getAllStyles();
		List<OgcStyle> rval = new ArrayList<OgcStyle>(styles.size());
		for (String style : styles) {
			rval.add(new OgcStyle(style));
		}
		return rval;
	}

	protected StyleLibrary getStyleLib() throws WmsException {
		if (_styleLib == null) {
			try {
				_styleLib = getStyleLib(styleLibraryFileName, sldVersion);
			} catch (Exception e) {
				log.error("Unable to read style library", e);
				throw new WmsException(Code.InternalServerError);
			}
		}
		return _styleLib;
	}

	/**
	 * @param layer
	 * @param style
	 * @return
	 * @throws WmsException
	 *             if style cannot be found and there is no matching style for
	 *             layer
	 */
	public Style getStyle(String layer, String style) throws WmsException {
		StyleLibrary styleLib = getStyleLib();
		Style ns;
		if (style == null) {
			ns = getStyleForLayer(layer);
			if (ns == null) {
				throw new WmsException(Code.LayerNotDefined);
			}
		} else {
			ns = styleLib.getNamedStyle(style);
			if (ns == null) {
				throw new WmsException(Code.StyleNotDefined);
			}
		}
		return ns;
	}

	/**
	 * @param layer
	 * @return null if no style can be found for layer
	 * @throws WmsException
	 */
	protected Style getStyleForLayer(String layer) throws WmsException {
		if (layer == null) {
			return null;
		}
		StyleLibrary styleLib = getStyleLib();
		// check direct match
		Style rval = styleLib.getDefault(layer);
		if (rval == null && matcher != null) {
			String style = matcher.getStyleMatch(layer);
			if (style != null) {
				rval = styleLib.getNamedStyle(style);
			}
		}
		return rval;
	}

	public static BufferedImage getLegend(Style style, Integer width,
			Integer height) {
		List<FeatureTypeStyle> fts = style.featureTypeStyles();
		if (fts == null || fts.isEmpty()) {
			// TODO should this be a blank image instead?
			return null;
		}
		List<BufferedImage> images = new ArrayList<BufferedImage>(fts.size());
		for (FeatureTypeStyle ft : fts) {
			BufferedImage legend = getLegend(ft, width, height);
			if (legend != null) {
				images.add(legend);
			}
		}
		if (images.size() > 1) {
			log.warn("Multiple feature type styles, only processing the first one");
		}
		// TODO merge multiple results into one
		return images.get(0);
	}

	public static BufferedImage getLegend(FeatureTypeStyle ft, Integer width,
			Integer height) {
		List<Rule> rules = ft.rules();
		if (rules == null || rules.isEmpty()) {
			return null;
		}
		if (isRaster(ft)) {
			return getRasterLegend(ft, width, height);
		} else {
			return getFeatureLegend(ft, width, height);
		}
	}

	public static BufferedImage getFeatureLegend(FeatureTypeStyle ft,
			Integer width, Integer height) {
		List<Rule> rules = ft.rules();
		if (rules == null || rules.isEmpty()) {
			return null;
		}
		List<BufferedImage> ruleImgs = new ArrayList<BufferedImage>(
				rules.size());
		for (Rule r : rules) {
			BufferedImage ruleImg = getFeatureLegend(r, null, null);
			if (ruleImg != null) {
				ruleImgs.add(ruleImg);
			}
		}
		BufferedImage merged = stackImages(ruleImgs);
		return resizeIfNeeded(merged, width, height);
	}

	protected static BufferedImage resizeIfNeeded(BufferedImage img, Integer width,
			Integer height) {
		if (width == null && height == null) {
			return img;
		}
		if (width == null) {
			// height is not null, choose a width that is proportional
			double ratio = (double) img.getWidth() / img.getHeight();
			width = (int) (height * ratio);
			if (width <= 0) {
				width = 1;
			}
		}
		if (height == null) {
			// width is not null, choose a height that is proportional
			double ratio = (double) img.getHeight() / img.getWidth();
			height = (int) (width * ratio);
			if (height <= 0) {
				height = 1;
			}
		}
		return resize(img, width, height, true);
	}

	protected static BufferedImage resize(BufferedImage input, int width,
			int height, boolean quality) {
		BufferedImage rval = input;
		int type = BufferedImage.TYPE_INT_ARGB;
		Object hint = quality ? RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
				: RenderingHints.VALUE_INTERPOLATION_BILINEAR;
		int h = quality ? input.getHeight() : height;
		int w = quality ? input.getWidth() : width;

		do {
			if (quality && w > width) {
				w /= 2;
				if (w < width) {
					w = width;
				}
			} else {
				w = width;
			}
			if (quality && h > height) {
				h /= 2;
				if (h < height) {
					h = height;
				}
			} else {
				h = height;
			}
			BufferedImage tmp = new BufferedImage(w, h, type);
			Graphics2D g = tmp.createGraphics();
			g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, hint);
			g.drawImage(rval, 0, 0, w, h, null);
			g.dispose();
			rval = tmp;
		} while (w != width || h != height);
		return rval;
	}

	protected static BufferedImage stackImages(
			List<BufferedImage> ruleImages) {
		int totalHeight = 0;
		int maxWidth = Integer.MIN_VALUE;
		for (BufferedImage img : ruleImages) {
			totalHeight += img.getHeight();
			maxWidth = Math.max(maxWidth, img.getWidth());
		}
		BufferedImage rval = new BufferedImage(maxWidth, totalHeight,
				BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = rval.createGraphics();
		totalHeight = 0;
		for (BufferedImage img : ruleImages) {
			g.drawImage(img, 0, totalHeight, null);
			totalHeight += img.getHeight();
		}
		g.dispose();
		return rval;
	}

	public static BufferedImage getFeatureLegend(Rule rule, Integer width,
			Integer height) {
		if (!hasImage(rule)) {
			return null;
		}
		int spacePad = 3;
		BufferedImage icon = Glyph.geometry(rule);
		String label = getLabel(rule);
		Font font = new Font("Monospaced", Font.PLAIN, 12);
		Rectangle2D stringBounds = font.getStringBounds(label,
				new FontRenderContext(null, false, false));
		if (width == null) {
			width = (int) (icon.getWidth() + stringBounds.getWidth() + spacePad);
		}
		if (height == null) {
			height = (int) (Math
					.max(icon.getHeight(), stringBounds.getHeight()) + 1);
		}
		BufferedImage rval = new BufferedImage(width, height,
				BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = rval.createGraphics();
		g.drawImage(icon, 0, 0, null);
		g.setFont(font);
		g.setColor(Color.BLACK);
		g.drawString(label, icon.getWidth() + spacePad,
				(int) stringBounds.getHeight());
		g.dispose();
		return rval;
	}

	protected static boolean hasImage(Rule rule) {
		Symbolizer[] symbolizers = rule.getSymbolizers();
		if (symbolizers == null || symbolizers.length < 1) {
			return false;
		}
		for (Symbolizer s : symbolizers) {
			if (!(s instanceof TextSymbolizer)) {
				return true;
			}
		}
		return false;
	}

	protected static String getLabel(Rule rule) {
		Description desc = rule.getDescription();
		if (desc != null) {
			InternationalString title = desc.getTitle();
			if (title != null) {
				return title.toString();
			}
		}
		FilterLabeler labeler = new FilterLabeler();
		Filter filter = rule.getFilter();
		if (filter == null) {
			return "default";
		}
		return (String) filter.accept(labeler, null);
	}

	public static BufferedImage getRasterLegend(FeatureTypeStyle ft,
			Integer width, Integer height) {
		List<Rule> rules = ft.rules();
		if (rules == null || rules.isEmpty()) {
			return null;
		}
		List<BufferedImage> imgs = new ArrayList<BufferedImage>(rules.size());
		for (Rule rule : rules) {
			BufferedImage img = getRasterLegend(rule, width, height);
			if (img != null) {
				imgs.add(img);
			}
		}
		return stackImages(imgs);
	}

	public static BufferedImage getRasterLegend(Rule rule, Integer width,
			Integer height) {
		List<Symbolizer> symbolizers = rule.symbolizers();
		if (symbolizers == null || symbolizers.isEmpty()) {
			return null;
		}
		List<BufferedImage> imgs = new ArrayList<BufferedImage>(
				symbolizers.size());
		for (Symbolizer s : symbolizers) {
			if (s instanceof RasterSymbolizer) {
				BufferedImage img = getRasterLegend((RasterSymbolizer) s,
						width, height);
				if (img != null) {
					imgs.add(img);
				}
			}
		}
		return stackImages(imgs);
	}

	protected static BufferedImage getRasterLegend(RasterSymbolizer symbolizer, Integer width, Integer height){
		ColorMap cmap = SldUtility.getRaster(symbolizer);
		if ( width == null || height == null){
			width = ColormapStyleProvider.defaultWidth;
			height = ColormapStyleProvider.defaultHeight;
		}
		// TODO labels
		return LegendUtility.buildColorbar(cmap, width, height);
	}

	protected static boolean isRaster(FeatureTypeStyle ft) {
		for (Rule r : ft.rules()) {
			for (Symbolizer s : r.symbolizers()) {
				if (s instanceof RasterSymbolizer) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * @return the sldVersion
	 */
	public String getSldVersion() {
		return sldVersion;
	}

	/**
	 * @param sldVersion
	 *            the sldVersion to set
	 */
	public void setSldVersion(String sldVersion) {
		this.sldVersion = sldVersion;
	}

	/**
	 * @return the loader
	 */
	public ClassLoader getLoader() {
		if (loader == null) {
			return this.getClass().getClassLoader();
		}
		return loader;
	}

	/**
	 * @param loader
	 *            the loader to set
	 */
	public void setLoader(ClassLoader loader) {
		this.loader = loader;
	}

	/**
	 * @param styleLibraryFileName
	 *            the styleLibraryFileName to set
	 */
	public void setStyleLibraryFileName(String styleLibraryFileName) {
		this.styleLibraryFileName = styleLibraryFileName;
	}

	/**
	 * @return the matcher
	 */
	public StyledLayerMatcher getMatcher() {
		return matcher;
	}

	/**
	 * @param matcher
	 *            the matcher to set
	 */
	public void setMatcher(StyledLayerMatcher matcher) {
		this.matcher = matcher;
	}

}
