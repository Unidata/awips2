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

package com.raytheon.viz.geotiff.rsc;

import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.gce.geotiff.GeoTiffFormat;
import org.geotools.gce.geotiff.GeoTiffReader;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.hdf5.PureMemoryBasedTileSet;

/**
 * Supports GeoTIFF imagery
 * 
 * NOTE: maximum of 2048 height/width in pixels.
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date         Ticket#     Engineer    Description
 *     ------------   ----------  -----------   --------------------------
 *     8/24/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class GeoTiffResource extends
        AbstractVizResource<GeoTiffResourceData, MapDescriptor> {

    /** The tiled image */
    private PureMemoryBasedTileSet image;

    private String filePath;

    protected GeoTiffResource(GeoTiffResourceData data, LoadProperties props)
            throws VizException, IOException {
        super(data, props);
        filePath = this.resourceData.getFilename();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        if (image != null) {
            image.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        try {
            GeoTiffReader gcr = (GeoTiffReader) new GeoTiffFormat()
                    .getReader(new File(filePath));

            // Read in the image itself using ImageIO as it is much more
            // reliable
            RenderedImage img = ImageIO.read(new File(filePath));
            GridCoverage2D gridCoverage = (GridCoverage2D) gcr.read(null);

            image = new PureMemoryBasedTileSet(resourceData.getNameGenerator()
                    .getName(this), this, gridCoverage.getGridGeometry(),
                    PureMemoryBasedTileSet.calculateLevels(img),
                    target.getViewType());
            image.setMapDescriptor(this.descriptor);
            image.setImage(img);
            image.init(target);

            gridCoverage = null;
            img = null;
        } catch (Exception e) {
            // TODO: Handle exception
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (image != null) {
            image.paint(target, paintProps);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        image.reproject();
    }
}
