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
package com.raytheon.uf.viz.kml.export.graphics.ext;

import java.util.Comparator;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension.IMosaicImage;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Keeps track of the images that are to be mosaiced so that the extension can
 * handle rendering them.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
class KmlMosaicImage implements IMosaicImage {

    private ColorMapParameters colorMapParameters;

    private int[] bounds;

    private IExtent imageExtent;

    private DrawableImage[] imagesToMosaic;

    private Comparator<Double> mosaicComparator;

    public KmlMosaicImage(int[] imageBounds, IExtent imageExtent,
            ColorMapParameters params, Comparator<Double> mosaicComparator) {
        this.colorMapParameters = params;
        this.bounds = imageBounds;
        this.imageExtent = imageExtent;
        this.mosaicComparator = mosaicComparator;
    }

    @Override
    public ColorMapParameters getColorMapParameters() {
        return colorMapParameters;
    }

    @Override
    public void setColorMapParameters(ColorMapParameters params) {
        this.colorMapParameters = params;
    }

    @Override
    public double getValue(int x, int y) {
        // TODO do I need this?
        return 0;
    }

    @Override
    public void stage() throws VizException {
        // TODO do I need this?
    }

    @Override
    public Status getStatus() {
        // TODO do I need this?
        return Status.LOADED;
    }

    @Override
    public void setInterpolated(boolean isInterpolated) {
        // TODO do I need this?
    }

    @Override
    public void dispose() {
        // TODO do I need this?
    }

    @Override
    public int getWidth() {
        return bounds[0];
    }

    @Override
    public int getHeight() {
        return bounds[1];
    }

    @Override
    public void setBrightness(float brightness) {
        // TODO do I need this?
    }

    @Override
    public void setContrast(float contrast) {
        // TODO do I need this?
    }

    @Override
    public Class<? extends IMosaicImageExtension> getExtensionClass() {
        return IMosaicImageExtension.class;
    }

    public Comparator<Double> getMosaicComparator() {
        return mosaicComparator;
    }

    @Override
    public void setImagesToMosaic(DrawableImage... images) {
        this.imagesToMosaic = images;
    }

    public DrawableImage[] getImagesToMosaic() {
        return this.imagesToMosaic;
    }

    @Override
    public void setImageExtent(IExtent imageExtent) {
        this.imageExtent = imageExtent;
    }

}