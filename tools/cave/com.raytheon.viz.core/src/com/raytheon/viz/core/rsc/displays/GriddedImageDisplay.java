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
package com.raytheon.viz.core.rsc.displays;

import java.awt.Rectangle;
import java.nio.Buffer;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.prep.CMDataPreparerManager;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2008            randerso     Initial creation
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GriddedImageDisplay implements IRenderable {

    private final Buffer data;

    private final IMapDescriptor descriptor;

    private final GridGeometry2D gridGeometry;

    private ColorMapParameters colorMapParameters;

    private IImage image;

    private PixelCoverage pixelCoverage;

    public GriddedImageDisplay(Buffer data, IMapDescriptor descriptor,
            GridGeometry2D gridGeometry) {

        this.data = data;
        this.descriptor = descriptor;
        this.gridGeometry = gridGeometry;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (this.image == null) {
            int w = gridGeometry.getGridRange2D().width;
            int h = gridGeometry.getGridRange2D().height;

            this.image = target.initializeRaster(CMDataPreparerManager
                    .getDataPreparer(data, new Rectangle(w, h), null),
                    this.colorMapParameters);

            try {
                if (this.pixelCoverage == null) {
                    org.opengis.geometry.Envelope ge = this.gridGeometry
                            .getEnvelope();

                    ReferencedCoordinate ll = new ReferencedCoordinate(
                            new Coordinate(ge.getMinimum(0), ge.getMinimum(1)),
                            this.gridGeometry, Type.CRS);

                    ReferencedCoordinate ul = new ReferencedCoordinate(
                            new Coordinate(ge.getMinimum(0), ge.getMaximum(1)),
                            this.gridGeometry, Type.CRS);
                    ReferencedCoordinate ur = new ReferencedCoordinate(
                            new Coordinate(ge.getMaximum(0), ge.getMaximum(1)),
                            this.gridGeometry, Type.CRS);
                    ReferencedCoordinate lr = new ReferencedCoordinate(
                            new Coordinate(ge.getMaximum(0), ge.getMinimum(1)),
                            this.gridGeometry, Type.CRS);

                    Coordinate llCoord = ll.asPixel(descriptor
                            .getGridGeometry());
                    Coordinate ulCoord = ul.asPixel(descriptor
                            .getGridGeometry());
                    Coordinate urCoord = ur.asPixel(descriptor
                            .getGridGeometry());
                    Coordinate lrCoord = lr.asPixel(descriptor
                            .getGridGeometry());

                    PixelCoverage pc = new PixelCoverage(ulCoord, urCoord,
                            lrCoord, llCoord);
                    this.pixelCoverage = pc;
                }
            } catch (Exception e) {
                throw new VizException("Error transforming raster", e);
            }
        }

        if (this.image != null && this.pixelCoverage != null) {
            if (paintProps instanceof GriddedImagePaintProperties) {
                GriddedImagePaintProperties giprops = (GriddedImagePaintProperties) paintProps;
                this.image.setInterpolated(giprops.isInterpolated);
                this.image.setBrightness(giprops.brightness);
                this.image.setContrast(giprops.contrast);
            }
            target.drawRaster(image, this.pixelCoverage, paintProps);
        }
    }

    public ColorMapParameters getColorMapParameters() {
        return this.colorMapParameters;
    }

    public void setColorMapParameters(ColorMapParameters params) {
        this.colorMapParameters = params;
    }

    public void dispose() {
        if (this.image != null) {
            this.image.dispose();
        }
    }

    /**
     * Custom paint properties for painting gridded image displays
     */
    public static class GriddedImagePaintProperties extends PaintProperties {
        protected boolean isInterpolated;

        protected float brightness;

        protected float contrast;

        public GriddedImagePaintProperties(PaintProperties paintProps,
                float brightness, float contrast, boolean interpState) {
            super(paintProps);
            this.brightness = brightness;
            this.contrast = contrast;
            this.isInterpolated = interpState;

        }

    }

    /**
     * @return the data
     */
    public Buffer getData() {
        return data;
    }

}
