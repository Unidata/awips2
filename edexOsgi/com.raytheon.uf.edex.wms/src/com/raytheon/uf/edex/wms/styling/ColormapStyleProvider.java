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
 * Mar 29, 2012            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.styling;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.xml.bind.JAXBException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.filter.FilterFactoryImpl;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.styling.RasterSymbolizer;
import org.geotools.styling.Style;
import org.geotools.styling.StyleBuilder;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.edex.exception.ColorTableException;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.IPrecomputedRange;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.image.colormap.ColorMapParameters;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.colormap.ColorbarLabeling;
import com.raytheon.uf.edex.ogc.common.colormap.MapRange;
import com.raytheon.uf.edex.ogc.common.colormap.StyleRule;
import com.raytheon.uf.edex.ogc.common.colormap.StyleRuleLibrary;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.reg.WmsImage;
import com.raytheon.uf.edex.wms.util.ColorMapUtility;
import com.raytheon.uf.edex.wms.util.LegendUtility;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class ColormapStyleProvider implements CoverageStyleProvider {

    protected String styleLibraryFileName;

    protected static Log log = LogFactory.getLog(ColormapStyleProvider.class);

    protected String fallbackDefaultColormapName = "Default";

    protected Style preRendered;

    public static int defaultWidth = 512;

    public static int defaultHeight = 30;

    protected ClassLoader loader = null;

    private List<String> _cmapPaths;

    /**
     * @param styleLibraryFileName
     */
    public ColormapStyleProvider(String styleLibraryFileName,
            String defaultColormap) {
        this(styleLibraryFileName);
        fallbackDefaultColormapName = defaultColormap;
    }

    public ColormapStyleProvider(String styleLibraryFileName) {
        this.styleLibraryFileName = styleLibraryFileName;
        StyleBuilder sb = new StyleBuilder();
        RasterSymbolizer symbolizer = sb.createRasterSymbolizer();
        symbolizer.setOpacity(new FilterFactoryImpl()
                .createLiteralExpression(1.0));
        preRendered = sb.createStyle(symbolizer);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.StyleLookup#lookup(java.lang.String)
     */
    @Override
    public String lookup(String layername) {
        try {
            StyleRule match = getMatchingStyleRule(layername,
                    new HashMap<String, String>(0),
                    new HashMap<String, String>(0));
            if (match != null) {
                return match.getColorMapName();
            } else {
                return getFallbackDefaultColormapName();
            }
        } catch (Exception e) {
            log.error("Problem getting match", e);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.styling.CoverageStyleProvider#styleData(com.
     * raytheon.uf.edex.wms.styling.WmsDataRetriever,
     * com.raytheon.uf.edex.wms.styling.WmsStyleChoice,
     * com.raytheon.uf.common.dataplugin.PluginDataObject,
     * org.geotools.geometry.jts.ReferencedEnvelope)
     */
    @Override
    public WmsImage styleData(WmsDataRetriever retriever,
            WmsStyleChoice styleChoice, PluginDataObject record,
            ReferencedEnvelope envelope) throws WmsException {
        WmsImage rval;
        ReferencedDataRecord dataRecord = retriever.getDataRecord(record,
                envelope);
        if (dataRecord == null) {
            rval = new WmsImage((GridCoverage2D) null);
        } else {
            try {
                ReferencedEnvelope re = dataRecord.getEnvelope();
                StyleRule styleRule = styleChoice.getStyleRule();

                // check if the map range needs to be replaced
                if (styleRule != null && record instanceof IPrecomputedRange) {
                    IPrecomputedRange rangedRecord = (IPrecomputedRange) record;
                    if (styleRule.getMapRange() == null
                            || styleRule.getMapRange().isReplaceable()) {
                        MapRange range = new MapRange();

                        double diff = Math.abs(0.25 * (rangedRecord
                                .getDataMax() - rangedRecord.getDataMin()));
                        float min = (float) (rangedRecord.getDataMin() - diff);
                        float max = (float) (rangedRecord.getDataMax() + diff);
                        range.setUpperMaximum(max);
                        range.setUpperMinimum(min);
                        range.setType("linear");
                        range.setReplaceable(false);
                        styleRule.setMapRange(range);
                    }
                }

                BufferedImage img = applyColorMap(dataRecord.getRecord(),
                        styleChoice.getCmap(), styleRule);
                GeneralEnvelope ge = new GeneralEnvelope(2);
                ge.setCoordinateReferenceSystem(re
                        .getCoordinateReferenceSystem());
                ge.setRange(0, re.getMinX(), re.getMaxX());
                ge.setRange(1, re.getMinY(), re.getMaxY());
                GridCoverage2D gc = convert(img, ge);
                rval = new WmsImage(gc, preRendered);
            } catch (Exception e) {
                log.error("Problem applying colormap", e);
                throw new WmsException(Code.InternalServerError);
            }
        }
        return rval;
    }

    protected BufferedImage applyColorMap(IDataRecord record, ColorMap cmap,
            StyleRule styleRule) throws Exception {
        BufferedImage rval;
        if (record instanceof ByteDataRecord) {
            ByteDataRecord data = (ByteDataRecord) record;
            rval = ColorMapUtility.applyColorMap(data, cmap, styleRule);
        } else if (record instanceof FloatDataRecord) {
            FloatDataRecord data = (FloatDataRecord) record;
            rval = ColorMapUtility.applyColorMap(data, cmap, styleRule);
        } else if (record instanceof ShortDataRecord) {
            ShortDataRecord data = (ShortDataRecord) record;
            rval = ColorMapUtility.applyColorMap(data, cmap, styleRule);
        } else if (record instanceof IntegerDataRecord) {
            IntegerDataRecord data = (IntegerDataRecord) record;
            rval = ColorMapUtility.applyColorMap(data, cmap, styleRule);
        } else {
            throw new IllegalArgumentException(
                    "Unable to apply colormap to class " + record.getClass());
        }
        return rval;
    }

    protected GridCoverage2D convert(BufferedImage img, GeneralEnvelope bounds) {
        GridCoverageFactory fact = new GridCoverageFactory();
        return fact.create("", img, bounds);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.styling.CoverageStyleProvider#getStyle(java.
     * lang.String, java.lang.String, java.util.Map, java.util.Map)
     */
    @Override
    public WmsStyleChoice getStyle(String layer, String style,
            Map<String, String> dimensions, Map<String, String> levelUnits)
            throws WmsException {
        StyleRule rule = getMatchingStyleRule(layer, dimensions, levelUnits);
        ColorMap cmap;
        if (style != null) {
            cmap = getJustColormap(style);
            if (cmap == null) {
                throw new WmsException(Code.StyleNotDefined, "Unkown style: "
                        + style);
            }
            rule.setColorMapName(style);
        } else {
            cmap = getJustColormap(rule.getColorMapName());
            if (cmap == null) {
                log.error("Invalid colormap name in style library: "
                        + rule.getColorMapName());
                throw new WmsException(Code.InternalServerError);
            }
        }
        WmsStyleChoice rval = new WmsStyleChoice(cmap);
        rval.setStyleRule(rule);
        return rval;
    }

    /**
     * Get's the default fallback style rule for this plugin, in case the
     * library file fails to load or does not match the layer
     * 
     * @return
     * @throws Exception
     */
    protected StyleRule getFallbackStyleRule() throws WmsException {
        // create a default map range
        MapRange range = new MapRange();
        range.setUpperMaximum(255.0f);
        range.setUpperMinimum(0.0f);
        range.setType("linear");
        range.setReplaceable(true);
        // create style rule
        StyleRule rule = new StyleRule();
        rule.setMapRange(range);
        rule.setColorMapName(getFallbackDefaultColormapName());
        return rule;
    }

    /**
     * @param style
     * @return null if colormap is not found
     * @throws WmsException
     */
    protected ColorMap getJustColormap(String style) throws WmsException {
        try {
            return ColorMapManager.getInstance().getColorMap(style);
        } catch (ColorTableException e) {
            log.error("could not load colormap " + style, e);
            throw new WmsException(Code.InternalServerError,
                    "unable to load colormap " + style, e);
        }
    }

    /**
     * the fallback default style, when all else fails
     * 
     * @return
     */
    protected String getFallbackDefaultColormapName() {
        return fallbackDefaultColormapName;
    }

    /**
     * returns an input stream for the properties file that defines how to match
     * layer names to styles/colormaps
     * 
     * @return InputStream for properties file, null if it could not be found
     * @throws Exception
     */
    protected InputStream getStyleLibraryFile() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        File baseRuleDir = pathMgr.getFile(edexStaticBase, "styleRuleLib");

        try {
            String filename = getStyleLibraryFileName();
            if (filename == null) {
                return null;
            }
            return new FileInputStream(new File(baseRuleDir, filename));
        } catch (FileNotFoundException e) {
            log.error("Could not find style library file", e);
            return null;
        }
    }

    public String getStyleLibraryFileName() {
        return styleLibraryFileName;
    }

    /**
     * Returns the style rule defined for the layer in the style mapping
     * properties file, returns the default grayscale style if the properties
     * file does not exist or the default style if the layer does not match any
     * entry
     * 
     * @param layer
     * @throws Exception
     */
    protected StyleRule getMatchingStyleRule(String layer,
            Map<String, String> dimensions, Map<String, String> levelUnits)
            throws WmsException {
        StyleRuleLibrary lib;
        try {
            lib = getStyleLibrary(getStyleLibraryFile());
        } catch (IOException e1) {
            log.error("Problem loading library file", e1);
            throw new WmsException(Code.InternalServerError);
        }
        StyleRule rval = null;
        if (lib != null) {
            try {
                rval = lib.getMatchForLayer(layer, dimensions, levelUnits);
            } catch (ParseException e) {
                log.error("Problem parsing dimension", e);
                throw new WmsException(Code.InternalServerError);
            }
        }
        if (rval == null) {
            rval = getFallbackStyleRule();
        }
        return rval;
    }

    /**
     * @param libraryFile
     * @return null if input stream is null
     * @throws IOException
     */
    protected StyleRuleLibrary getStyleLibrary(InputStream libraryFile)
            throws IOException {
        if (libraryFile == null) {
            return null;
        }

        StyleRuleLibrary lib = null;
        try {
            lib = StyleRuleLibrary.load(libraryFile);
        } catch (JAXBException e) {
            log.error("Could not load library file.", e);
        }

        return lib;
    }

    public List<String> getCmapPaths(String folderName) {
        if (_cmapPaths == null) {
            _cmapPaths = getCmapPathsInternal(folderName);
        }
        return _cmapPaths;
    }

    protected List<String> getCmapPathsInternal(String folderName) {
        return getCmapPaths(LocalizationLevel.BASE, folderName);
    }

    protected static List<String> getCmapPaths(LocalizationLevel level,
            String folderName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC, level);

        File baseColormapDir = pathMgr.getFile(edexStaticBase, "colormaps");

        List<String> rval = new ArrayList<String>();
        File target = new File(baseColormapDir, folderName);
        findCmapRecursive(rval, baseColormapDir.toURI(), target);
        return rval;
    }

    private static void findCmapRecursive(List<String> rval, URI base, File dir) {
        File[] cmapFiles = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.isFile() && f.getName().endsWith(".cmap");
            }
        });
        getRelPaths(rval, base, cmapFiles);
        File[] subDirs = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.isDirectory();
            }
        });
        for (File f : subDirs) {
            findCmapRecursive(rval, base, f);
        }
    }

    private static void getRelPaths(List<String> rval, URI base, File[] targets) {
        for (File f : targets) {
            URI relative = base.relativize(f.toURI());
            String path = relative.getPath();
            int extIndex = path.lastIndexOf('.');
            rval.add(path.substring(0, extIndex));
        }
    }

    protected StyleRuleLibrary getStyleRuleLibrary() {
        try {
            return getStyleLibrary(getStyleLibraryFile());
        } catch (IOException e) {
            log.error("Unable to get Style Rule Library", e);
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.StyleLookup#getStyles()
     */
    @Override
    public List<OgcStyle> getStyles() {
        StyleRuleLibrary lib = getStyleRuleLibrary();
        if (lib == null) {
            return new ArrayList<OgcStyle>(0);
        }
        List<StyleRule> rules = lib.getRules();
        SortedSet<String> cmaps = new TreeSet<String>();
        for (StyleRule rule : rules) {
            cmaps.add(rule.getColorMapName());
        }
        List<OgcStyle> rval = new ArrayList<OgcStyle>(cmaps.size());
        for (String cmap : cmaps) {
            rval.add(new OgcStyle(cmap));
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.styling.CoverageStyleProvider#getLegend(java
     * .lang.String, java.lang.String, java.util.Map, java.util.Map, int, int)
     */
    @Override
    public BufferedImage getLegend(String layer, String style,
            Map<String, String> dimensions, Map<String, String> levelUnits,
            Integer width, Integer height) throws WmsException {
        if (width == null || height == null) {
            width = defaultWidth;
            height = defaultHeight;
        }
        WmsStyleChoice choice = this.getStyle(layer, style, dimensions,
                levelUnits);
        if (choice == null) {
            return null;
        }
        StyleRule styleRule = choice.getStyleRule();
        if (styleRule == null) {
            log.warn("Style without style rule, unable to create legend");
            return null;
        }
        if (styleRule.getMapRange() == null) {
            MapRange range = new MapRange();
            range.setUpperMinimum(0.0f);
            range.setUpperMaximum(255.0f);
            range.setType("linear");
            range.setReplaceable(true);
            styleRule.setMapRange(range);
        }
        return handleColormap(choice.getCmap(), styleRule, true, width, height);
    }

    public static BufferedImage handleColormap(ColorMap cmap,
            StyleRule styleRule, boolean includeLabels, Integer width,
            Integer height) {
        BufferedImage legend = null;
        // Create the color bar.
        BufferedImage colorBar = LegendUtility.buildColorbar(cmap, width,
                height);

        // Create the labels if necessary.
        BufferedImage labels = null;
        ColorMapParameters params = ColorMapUtility.getCmapParams(cmap,
                styleRule, null, null);
        float min = params.getColorMapMin();
        float max = params.getColorMapMax();
        ColorbarLabeling colorbarLabeling = styleRule.getColorbarLabeling();
        if (colorbarLabeling != null) {
            try {
                String valuesStr = colorbarLabeling.getValues();
                String incrementStr = colorbarLabeling.getIncrement();
                String[] values = null;
                if (valuesStr != null) {
                    values = valuesStr.split(" ");
                } else {
                    float increment;
                    if (incrementStr == null) {
                        increment = Math.abs((max - min) / 10);
                        if (increment < 1e-16f) {
                            increment = 1e-16f;
                        }
                        if (min + increment == min) {
                            increment = max - min;
                        }
                    } else {
                        increment = Float.parseFloat(incrementStr);
                        if (Math.abs(max - min) / increment > 16) {
                            increment *= 2;
                        }
                    }
                    ArrayList<String> valueList = new ArrayList<String>();
                    for (float f = min; f <= max; f += increment) {
                        valueList.add(Float.toString(f));
                    }
                    values = valueList.toArray(new String[valueList.size()]);
                }

                if (values != null) {
                    labels = LegendUtility.buildLabels(width, height, values,
                            min, max);
                }
            } catch (IllegalArgumentException e) {
                log.warn("Unable to create labels, style rule contains no label information.");
            }
        }

        if (colorBar != null) {
            legend = new BufferedImage(width, height,
                    BufferedImage.TYPE_INT_ARGB);
            Graphics g = legend.getGraphics();
            g.drawImage(colorBar, 0, 0, null);
            if (includeLabels && labels != null) {
                g.drawImage(labels, 0, 0, null);
            }
        }

        return legend;
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
    @Override
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
     * @param fallbackDefaultColormapName
     *            the fallbackDefaultColormapName to set
     */
    public void setFallbackDefaultColormapName(
            String fallbackDefaultColormapName) {
        this.fallbackDefaultColormapName = fallbackDefaultColormapName;
    }

}
