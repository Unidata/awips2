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
 * Apr 10, 2012            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.styling;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.Map;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.filter.FilterFactoryImpl;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.styling.RasterSymbolizer;
import org.geotools.styling.Style;
import org.geotools.styling.StyleBuilder;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.WmsImage;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class PreRenderedStyleProvider extends SingleCoverageStyleProvider {

    public static final Style preRendered;

    static {
        StyleBuilder sb = new StyleBuilder();
        RasterSymbolizer symbolizer = sb.createRasterSymbolizer();
        symbolizer.setOpacity(new FilterFactoryImpl()
                .createLiteralExpression(1.0));
        preRendered = sb.createStyle(symbolizer);
    }

    /**
     * @param style
     */
    public PreRenderedStyleProvider() {
        super(preRendered);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wms.styling.SingleStyleProvider#styleData(com
     * .raytheon.uf.edex.wms.styling.WmsDataRetriever,
     * com.raytheon.uf.edex.wms.styling.WmsStyleChoice,
     * com.raytheon.uf.common.dataplugin.PluginDataObject,
     * org.geotools.geometry.jts.ReferencedEnvelope)
     */
    @Override
    public WmsImage styleData(WmsDataRetriever retriever, WmsStyleChoice style,
            PluginDataObject record, ReferencedEnvelope envelope)
            throws WmsException {
        ReferencedDataRecord ref = retriever.getDataRecord(record, envelope);
        if (ref == null) {
            return new WmsImage((GridCoverage2D) null);
        }
        IntegerDataRecord intrec = (IntegerDataRecord) ref.getRecord();
        long[] dims = intrec.getSizes();
        int w = (int) dims[0];
        int h = (int) dims[1];
        BufferedImage img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        img.setRGB(0, 0, w, h, intrec.getIntData(), 0, w);
        GridCoverageFactory fact = new GridCoverageFactory();
        return new WmsImage(fact.create("", img, ref.getEnvelope()), this.style);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.styling.SingleCoverageStyleProvider#getLegend
     * (java.lang.String, java.lang.String, java.util.Map, java.util.Map,
     * java.lang.Integer, java.lang.Integer)
     */
    @Override
    public BufferedImage getLegend(String layer, String style,
            Map<String, String> dimensions, Map<String, String> levelUnits,
            Integer width, Integer height) throws WmsException {
        Font font = new Font("Monospaced", Font.PLAIN, 12);
        String label = "Pre-Rendered Imagery";
        Rectangle2D stringBounds = font.getStringBounds(label,
                new FontRenderContext(null, false, false));
        int strWidth = (int) stringBounds.getWidth();
        // extra 4 for bottom pad
        int strHeight = (int) (stringBounds.getHeight() + 4);
        BufferedImage rval = new BufferedImage(strWidth, strHeight,
                BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = rval.createGraphics();
        g.setFont(font);
        g.setColor(Color.BLACK);
        g.drawString(label, 0, (int) stringBounds.getHeight());
        g.dispose();
        return AbstractSldStyleProvider.resizeIfNeeded(rval, width, height);
    }

}
