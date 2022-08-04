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

import javax.imageio.ImageIO;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.gce.geotiff.GeoTiffFormat;
import org.geotools.gce.geotiff.GeoTiffReader;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.tile.TileSetRenderable;

/**
 * Supports GeoTIFF imagery
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Aug 24, 2006           chammack  Initial Creation.
 * Aug 14, 2014  3522     bclement  reworked to use TileSetRenderable
 * Oct 25, 2017  5773     bsteffen  Support colormapped data.
 * Oct 10, 2018  7498     bsteffen  Ensure ImageIO loads geotools extensions.
 * 
 * </pre>
 * 
 * @author chammack
 */
public class GeoTiffResource
        extends AbstractVizResource<GeoTiffResourceData, MapDescriptor> {

    private static final int TILE_SIZE = 512;

    private static boolean initializedImageIO = false;

    private TileSetRenderable image;

    private ImageStreamTileCreator tileCreator;

    private String filePath;

    /* true since TileSetRenderable needs reproject() called on it before use */
    private boolean project = true;

    protected GeoTiffResource(GeoTiffResourceData data, LoadProperties props) {
        super(data, props);
        filePath = this.resourceData.getFilename();
    }

    @Override
    protected void disposeInternal() {
        if (tileCreator != null) {
            try {
                tileCreator.close();
            } catch (IOException e) {
                statusHandler.error("Unable to close geotiff tile image stream",
                        e);
            }
        }
        if (image != null) {
            image.dispose();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        File imgFile = new File(filePath);
        if (!imgFile.canRead()) {
            String msg = "Check file permissions.";
            if (!imgFile.exists()) {
                msg = "File does not exist.";
            }
            throw new VizException("Unable to open geotiff image: "
                    + imgFile.getAbsolutePath() + ". " + msg);
        }
        initializeImageIO();
        try {
            GeoTiffReader gcr = new GeoTiffFormat().getReader(imgFile);
            GridCoverage2D gridCoverage = gcr.read(null);

            ImagingCapability imaging = getCapability(ImagingCapability.class);
            GridGeometry2D tileSetGeometry = gridCoverage.getGridGeometry();
            if (ColorMappedImageStreamTileCreator
                    .checkForRawRaster(gridCoverage.getRenderedImage())) {
                ColorMapParameters colorMapParameters = ColorMappedImageStreamTileCreator
                        .initializeParameters(gridCoverage.getRenderedImage());
                getCapability(ColorMapCapability.class)
                        .setColorMapParameters(colorMapParameters);
                tileCreator = new ColorMappedImageStreamTileCreator(imgFile,
                        colorMapParameters);
            } else {
                tileCreator = new ImageStreamTileCreator(imgFile);
            }

            image = new TileSetRenderable(imaging, tileSetGeometry, tileCreator,
                    1, TILE_SIZE);
            gridCoverage = null;
        } catch (Exception e) {
            throw new VizException("Unable to process geotiff image: "
                    + imgFile.getAbsolutePath(), e);
        }
    }

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

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        project = true;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (tileCreator instanceof ColorMappedImageStreamTileCreator) {
            ColorMapParameters colorMapParameters = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            try {
                double value = image.interrogate(coord.asLatLon());
                if (Double.isNaN(value)
                        || colorMapParameters.getNoDataValue() == value) {
                    return "No Data";
                } else {
                    return Double.toString(value);
                }
            } catch (TransformException | FactoryException e) {
                throw new VizException(e);
            }
        } else {
            return super.inspect(coord);
        }
    }

    /**
     * GeotiffReader will not work if the geotools extensions to ImageIO are not
     * loaded so this method ensures the geotools extensions are loaded. The
     * GeotiffReader specifically needs to use
     * it.geosolutions.imageio.stream.input.spi.URLImageInputStreamSpi.
     * 
     * ImageIO will initialize with whatever classloader is available when it is
     * first loaded. Removing this code might appear like nothing is broken
     * since this plugin initializes ImageIO through the geotools classes so the
     * geotools classloader is used. This code is here in case ImageIO has
     * already initialized through another plugin without geotools.
     */
    private static final void initializeImageIO() {
        if (!initializedImageIO) {
            Thread currentThread = Thread.currentThread();
            ClassLoader contextClassLoader = currentThread
                    .getContextClassLoader();
            currentThread.setContextClassLoader(
                    GeoTiffReader.class.getClassLoader());
            ImageIO.scanForPlugins();
            currentThread.setContextClassLoader(contextClassLoader);
            initializedImageIO = true;
        }
    }

}
