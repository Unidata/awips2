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
package com.raytheon.viz.grid.rsc;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedDisplay;
import com.raytheon.viz.pointdata.PointIconFactory;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension;
import com.raytheon.viz.pointdata.drawables.IPointImageExtension.PointImage;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A renderable that can be used to display grids where every cell represents a
 * value that can be mapped toa symbol in the WXSymbol font.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GriddedIconDisplay extends AbstractGriddedDisplay<IImage> {

    private float[] values;

    private Map<Integer, IImage> images = new HashMap<Integer, IImage>();

    private IImage empty = null;

    private PointIconFactory iconFactory;

    /**
     * @param descriptor
     * @param gridGeometryOfGrid
     * @param imageSize
     */
    public GriddedIconDisplay(float[] values, IMapDescriptor descriptor,
            GeneralGridGeometry gridGeometryOfGrid, int imageSize) {
        super(descriptor, gridGeometryOfGrid, imageSize);
        this.values = values;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedImageDisplay
     * #clearImages()
     */
    @Override
    protected void disposeResources() {
        for (IImage resp : images.values()) {
            resp.dispose();
        }
        images.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedImageDisplay
     * #createImage(com.raytheon.uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    protected IImage createResource(Coordinate coord) throws VizException {
        if (iconFactory == null) {
            iconFactory = new PointIconFactory(color, size);
        }
        int i = getValue(coord);
        IImage image = images.get(i);
        if (image == null) {
            BufferedImage bImage = iconFactory.getIcon(i);
            image = target.initializeRaster(new IODataPreparer(bImage, "icon"
                    + bImage, 0), null);
            images.put(i, image);
            // keep around the image that is entirely empty/transparent so we
            // can match it up and don't waste time drawing it later
            if (i <= 0 && empty == null) {
                empty = image;
            }
        }
        return image;
    }

    private int getValue(Coordinate coord) {
        int idx = (int) (coord.x + (coord.y * gridDims[0]));
        return (int) values[idx];
    }

    public boolean setColor(RGB color) {
        if (super.setColor(color)) {
            iconFactory = null;
            disposeResources();
            if (this.target != null) {
                this.target.setNeedsRefresh(true);
            }
            return true;
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedImageDisplay
     * #getImage(com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected IImage getResource(Coordinate coord) {
        return images.get(getValue(coord));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedDisplay#paint
     * (com.raytheon.uf.viz.core.drawables.PaintProperties,
     * java.util.Collection)
     */
    @Override
    protected void paint(PaintProperties paintProps,
            Collection<GridCellRenderable> renderables) throws VizException {
        List<PointImage> images = new ArrayList<PointImage>();
        for (GridCellRenderable renderable : renderables) {
            if (renderable.resource != empty) {
                PointImage image = new PointImage(renderable.resource,
                        renderable.plotLocation);
                image.setHeight((double) size * magnification);
                image.setWidth((double) size * magnification);
                images.add(image);
            }
        }
        target.getExtension(IPointImageExtension.class).drawPointImages(
                paintProps, images);
    }

}
