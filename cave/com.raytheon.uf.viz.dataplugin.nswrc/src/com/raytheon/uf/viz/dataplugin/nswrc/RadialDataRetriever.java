/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.viz.dataplugin.nswrc;

import java.awt.Rectangle;
import java.nio.FloatBuffer;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCRadialRecord;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * This class gets the NetCDF radar data ready to display as an image.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */

public class RadialDataRetriever implements IColorMapDataRetrievalCallback {

    protected final NSWRCRadialRecord record;

    protected Rectangle rect;

    public RadialDataRetriever(NSWRCRadialRecord record, Rectangle rect) {
        this.record = record;
        this.rect = rect;
    }

    public float[] convertData() {
        float[] imageData = new float[record.getNumBins() * record.getNumRadials()];
        int i = 0;
        for (int h = 0; h < record.getNumRadials(); ++h) {
            for (int w = 0; w < record.getNumBins(); ++w) {
                if (w != 0) {
                    imageData[i] = record.getDataValue(h, w);
                } else {
                    imageData[i] = Float.NaN;
                }
                ++i;
            }
        }
        return imageData;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback#getColorMapData
     * ()
     */
    @Override
    public ColorMapData getColorMapData() throws VizException {
        return new ColorMapData(FloatBuffer.wrap(convertData()), new int[] {
                rect.width, rect.height });
    }
}
