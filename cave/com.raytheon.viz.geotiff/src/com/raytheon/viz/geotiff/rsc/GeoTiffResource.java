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

import java.io.File;
import java.io.IOException;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.gce.geotiff.GeoTiffFormat;
import org.geotools.gce.geotiff.GeoTiffReader;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.tile.TileSetRenderable;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;

/**
 * Supports GeoTIFF imagery
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date         Ticket#     Engineer    Description
 *     ------------   ----------  -----------   --------------------------
 *     8/24/06                    chammack    Initial Creation.
 *     Aug 14, 2014 3522          bclement    reworked to use TileSetRenderable
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class GeoTiffResource extends
        AbstractVizResource<GeoTiffResourceData, MapDescriptor> {

    private static final int TILE_SIZE = 512;

    private TileSetRenderable image;

    private ImageStreamTileCreator tileCreator;

    private String filePath;

    /* true since TileSetRenderable needs reproject() called on it before use */
    private boolean project = true;

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
        if (tileCreator != null) {
            try {
                tileCreator.close();
            } catch (IOException e) {
                statusHandler.error(
                        "Unable to close geotiff tile image stream", e);
            }
        }
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
        File imgFile = new File(filePath);
        if (!imgFile.canRead()) {
            String msg = "Check file permissions.";
            if (!imgFile.exists()) {
                msg = "File does not exist.";
            }
            throw new VizException("Unable to open geotiff image: "
                    + imgFile.getAbsolutePath()
                    + ". " + msg);
        }
        try {
            GeoTiffReader gcr = (GeoTiffReader) new GeoTiffFormat()
                    .getReader(imgFile);
            GridCoverage2D gridCoverage = (GridCoverage2D) gcr.read(null);

            ImagingCapability imaging = getCapability(ImagingCapability.class);
            GridGeometry2D tileSetGeometry = gridCoverage.getGridGeometry();
            TileImageCreator tileCreator = new ImageStreamTileCreator(imgFile);

            image = new TileSetRenderable(imaging, tileSetGeometry,
                    tileCreator, 1, TILE_SIZE);
            gridCoverage = null;
        } catch (Exception e) {
            throw new VizException("Unable to process geotiff image: "
                    + imgFile.getAbsolutePath(), e);
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
            if (project) {
                project = false;
                GeneralGridGeometry targetGeometry = descriptor
                        .getGridGeometry();
                if (image.getTargetGeometry() != targetGeometry) {
                    image.project(targetGeometry);
                }
            }
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
        project = true;
    }
}
