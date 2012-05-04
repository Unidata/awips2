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
package com.raytheon.viz.core.rsc.hdf5;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.MathTransform2D;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IMeshCallback;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IImage.Status;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Describes an Abstract HDF5 Tileset
 * 
 * Must implement a preload method (optional) and a createTile method
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Feb 15, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public abstract class AbstractTileSet implements IRenderable, IMeshCallback {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractTileSet.class);

    protected static final double LEVEL_CHANGE_THRESHOLD = 2.0;

    protected static final JobPool tileCreationPool = new JobPool(
            "Creating Image Tiles", 10, false);

    /**
     * Deprecated, use DrawableImage and use target.drawImages(...)
     */
    @Deprecated
    protected static class TilePair {
        public ImageTile tile;

        public IImage image;

        public TilePair(ImageTile tile, IImage image) {
            this.tile = tile;
            this.image = image;
        }

    }

    protected Map<ImageTile, IImage> imageMap = new HashMap<ImageTile, IImage>();

    protected ImageTileList tileSet;

    protected GridGeometry2D originalGridGeometry;

    protected MathTransform originalMathTransform;

    protected GridGeometry2D[] gridGeometry;

    protected MathTransform[] mathTransform;

    protected MathTransform[] inverseMathTransform;

    /** The ratio of gl pixels / image pixel at the level */
    protected double[] pixelWidth;

    protected double elevation = 0.0;

    protected IMapDescriptor mapDescriptor;

    /**
     * Number of different levels of detail
     */
    protected int levels;

    protected MathTransform localProjToLL;

    protected MathTransform llToLocalProj;

    protected AbstractTileSet sharedGeometryTileSet;

    // protected double percentOfEarth;

    protected double interpolationFactorOnLoad = 1.0;

    protected int tileSize;

    protected int lastPaintedLevel;

    protected IGraphicsTarget lastPaintedTarget;

    protected AbstractVizResource<?, ?> rsc;

    protected RGB color = new RGB(255, 0, 0);

    protected PixelInCell cellOrientation;

    @Deprecated
    protected String viewType;

    protected Map<MultiKey, CreateTileJob> jobMap = new ConcurrentHashMap<MultiKey, CreateTileJob>();

    @Deprecated
    /** Use JobPool */
    protected volatile int threads = 0;

    private boolean disposed = true;

    @Deprecated
    /** Use JobPool */
    protected static final int MAX_THREADS = 10;

    /** The default graphics factory adapter */
    protected AbstractGraphicsFactoryAdapter graphicsAdapter;

    private Set<IMeshCallback> meshCallbacks = new HashSet<IMeshCallback>();

    public AbstractTileSet(int levels, int tileSize,
            GridGeometry2D gridGeometry, AbstractVizResource<?, ?> rsc,
            PixelInCell pixelOrientation, String viewType) throws VizException {
        this.tileSize = tileSize;
        this.rsc = rsc;
        this.cellOrientation = pixelOrientation;
        setup(levels, tileSize, gridGeometry);
    }

    public AbstractTileSet(int levels, int tileSize,
            GridGeometry2D gridGeometry, AbstractVizResource<?, ?> rsc,
            String viewType) throws VizException {
        this(levels, tileSize, gridGeometry, rsc, PixelInCell.CELL_CORNER,
                viewType);
    }

    public AbstractTileSet(AbstractTileSet sharedGeometryTileset)
            throws VizException {
        this(sharedGeometryTileset, null);
    }

    public AbstractTileSet(AbstractTileSet sharedGeometryTileset,
            AbstractVizResource<?, ?> rsc) throws VizException {
        if (rsc == null) {
            this.rsc = sharedGeometryTileset.rsc;
        } else {
            this.rsc = rsc;
        }
        setSharedGeometryTileSet(sharedGeometryTileset);
    }

    /**
     * Optional: Load the data into the tile set
     * 
     * @param level
     *            the level of the data
     * @throws StorageException
     */
    protected abstract void preloadDataObject(int level)
            throws StorageException;

    /**
     * Optional: Check to see if data has been preloaded
     * 
     * Indicates whether preloadDataObject should be called
     * 
     * @param level
     *            the level to check
     * 
     * @return true if the data has been preloaded already
     * 
     */
    public abstract boolean hasDataPreloaded(int level);

    protected abstract IImage createTile(IGraphicsTarget target, int level,
            int i, int j) throws VizException;

    protected GridGeometry2D expandGridGeometry(double factor,
            GridGeometry2D geom) {
        Rectangle rect = geom.getGridRange2D();

        rect.width *= factor * this.levels;
        rect.height *= factor * this.levels;

        GeneralGridEnvelope range = new GeneralGridEnvelope(rect, 2);
        GridGeometry2D retVal = new GridGeometry2D(range, geom.getEnvelope());

        return retVal;

    }

    protected void setup(int levels, int tileSize, GridGeometry2D gridGeometry)
            throws VizException {
        this.levels = levels;
        try {

            this.localProjToLL = CRS.findMathTransform(
                    gridGeometry.getCoordinateReferenceSystem(),
                    DefaultGeographicCRS.WGS84);
            this.llToLocalProj = this.localProjToLL.inverse();

        } catch (Exception e) {
            throw new VizException("Error setting up transforms", e);
        }

        this.gridGeometry = new GridGeometry2D[levels];

        this.originalGridGeometry = gridGeometry;

        // If interpolation factor is not 1.0, expand the grid geometry
        // out virtually for math transforms.
        if (interpolationFactorOnLoad != 1.0) {
            this.gridGeometry[0] = expandGridGeometry(
                    interpolationFactorOnLoad, originalGridGeometry);
        } else {
            this.gridGeometry[0] = gridGeometry;
        }
        int h = gridGeometry.getGridRange().getSpan(1);
        int w = gridGeometry.getGridRange().getSpan(0);

        Envelope env = this.gridGeometry[0].getEnvelope();

        for (int i = 1; i < levels; i++) {
            h /= 2;
            w /= 2;
            this.gridGeometry[i] = new GridGeometry2D(new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { w, h }, false), env);
        }

        this.originalMathTransform = this.originalGridGeometry
                .getGridToCRS(this.cellOrientation);

        this.mathTransform = new MathTransform2D[levels];
        this.inverseMathTransform = new MathTransform2D[levels];
        this.pixelWidth = new double[levels];
    }

    private void createTileLevel(int level, int inTileSize) throws VizException {

        // Get the h/w of the image we have to work with
        int h = gridGeometry[level].getGridRange().getSpan(1);
        int w = gridGeometry[level].getGridRange().getSpan(0);

        int fullTileDimX = w / inTileSize;
        int tileRemainderX = w % inTileSize;
        int fullTileDimY = h / inTileSize;
        int tileRemainderY = h % inTileSize;

        int totalTilesX = fullTileDimX;
        if (tileRemainderX > 0) {
            totalTilesX++;
        }

        int totalTilesY = fullTileDimY;
        if (tileRemainderY > 0) {
            totalTilesY++;
        }

        ImageTile[][] tiles = new ImageTile[totalTilesX][totalTilesY];

        this.tileSet.getTileSet().add(tiles);
        int startX;
        int startY;

        int effectiveWidth = 0;
        int effectiveHeight = 0;

        try {
            preloadDataObject(level);
        } catch (StorageException e1) {
            throw new VizException("Unable to load data", e1);
        }

        try {
            ReferencedEnvelope mapEnv = new ReferencedEnvelope(
                    this.mapDescriptor.getGridGeometry().getEnvelope());
            mapEnv = mapEnv.transform(MapUtil.LATLON_PROJECTION, false);
            GeneralEnvelope generalMapEnv = new GeneralEnvelope(mapEnv);
            generalMapEnv.normalize(false);
            startY = 0;
            for (int j = 0; j < totalTilesY; j++) {
                startY = j * (inTileSize);
                for (int i = 0; i < totalTilesX; i++) {
                    startX = i * (inTileSize);

                    effectiveWidth = inTileSize;

                    if ((w - (startX + effectiveWidth) < 0)) {
                        effectiveWidth = (w - startX);
                    }

                    effectiveHeight = inTileSize;
                    if ((h - (startY + effectiveHeight) < 0)) {
                        effectiveHeight = (h - startY);
                    }

                    double[] in = new double[2];
                    double[] ul = new double[2];
                    double[] lr = new double[2];

                    in[0] = startX;
                    in[1] = startY;
                    mathTransform[level].transform(in, 0, ul, 0, 1);
                    in[0] = startX + effectiveWidth;
                    in[1] = startY + effectiveHeight;

                    mathTransform[level].transform(in, 0, lr, 0, 1);

                    ReferencedEnvelope env = new ReferencedEnvelope(ul[0],
                            lr[0], ul[1], lr[1],
                            gridGeometry[0].getCoordinateReferenceSystem());
                    GeneralEnvelope generalEnv = new GeneralEnvelope(
                            env.transform(MapUtil.LATLON_PROJECTION, false));
                    // tiles which cross the dateline will almost always be
                    // created since normalizing changes their range to
                    // -180,180. If this is causing problems then instead of
                    // normalizing we should split envelopes that cross the
                    // dateline.
                    generalEnv.normalize(false);
                    // only create the tile if env overlaps the map
                    if (generalEnv.intersects(generalMapEnv, true)) {

                        tiles[i][j] = new ImageTile();
                        tiles[i][j].setGridGeometry(new Rectangle(startX,
                                startY, effectiveWidth, effectiveHeight), env);
                    }
                }

            }
        } catch (Exception e) {
            throw new VizException("Error creating tile level", e);
        }

    }

    /**
     * returns true if new tiles were requested, false if it is done loading.
     * 
     * @param lvl
     * @param extent
     * @param target
     * @return
     * @throws VizException
     */
    protected boolean createTilesWithinExtent(int lvl, IExtent extent,
            IGraphicsTarget target) throws VizException {
        List<Point> tilesToCreate = new ArrayList<Point>();

        ImageTile[][] tiles = tileSet.getTileGrid(lvl);

        for (int i = 0; i < tiles.length; i++) {
            for (int j = 0; j < tiles[i].length; j++) {
                ImageTile tile = tiles[i][j];
                if (tile != null && tile.coverage != null
                        && tile.coverage.intersects(extent)) {
                    IImage image = imageMap.get(tile);
                    if (image == null || image.getStatus() == Status.FAILED
                            || image.getStatus() == Status.INVALID) {

                        try {
                            if (!this.hasDataPreloaded(lvl)) {
                                this.preloadDataObject(lvl);
                            }
                        } catch (StorageException e) {
                            throw new VizException(
                                    "Error reading data to draw: ", e);
                        }

                        tilesToCreate.add(new Point(i, j));
                    }
                }
            }
        }
        startCreateTileJobs(lvl, target, tilesToCreate);
        return !tilesToCreate.isEmpty();
    }

    // this is in a seperate method so that subclasses can merge requests if it
    // makes it more efficient
    protected void startCreateTileJobs(int lvl, IGraphicsTarget target,
            List<Point> tilesToCreate) {
        for (Point p : tilesToCreate) {
            int i = p.x;
            int j = p.y;
            MultiKey key = new MultiKey(lvl, i, j);

            if (jobMap.get(key) == null) {
                CreateTileJob job = new CreateTileJob("tileset");
                job.setI(i);
                job.setJ(j);
                job.setLevel(lvl);
                job.setTarget(target);
                job.setTileSet(this);
                jobMap.put(key, job);
                tileCreationPool.schedule(job);
            }
        }
    }

    /**
     * 
     * @param target
     * @param extent
     * @param zoomLevel
     * @param alpha
     * @param lvl
     * @param isRecursiveCall
     * @throws VizException
     */
    protected void drawInternal(IGraphicsTarget target,
            PaintProperties paintProps, int lvl, int depth) throws VizException {

        List<ImageTile> intersectedTiles = new ArrayList<ImageTile>();
        boolean needDrawLower = false;
        ImageTile[][] tiles = tileSet.getTileGrid(lvl);

        if (depth == 0) {
            if (!createTilesWithinExtent(lvl, paintProps.getView().getExtent()
                    .intersection(paintProps.getClippingPane()), target)) {
                for (CreateTileJob job : jobMap.values()) {
                    tileCreationPool.cancel(job);
                }
                jobMap.clear();
            }
        }

        for (int i = 0; i < tiles.length; i++) {
            for (int j = 0; j < tiles[i].length; j++) {
                ImageTile tile = tiles[i][j];
                if (tile != null
                        && tile.coverage != null
                        && tile.coverage.intersects(paintProps.getView()
                                .getExtent())
                        && tile.coverage.intersects(paintProps
                                .getClippingPane())) {
                    intersectedTiles.add(tile);
                }
            }
        }

        List<DrawableImage> drawableImages = new ArrayList<DrawableImage>();

        ImagingCapability imaging = rsc.getCapability(ImagingCapability.class);

        for (ImageTile tile : intersectedTiles) {
            IImage image = imageMap.get(tile);

            if (image != null) {
                if (image instanceof IColormappedImage) {
                    ((IColormappedImage) image).setColorMapParameters(rsc
                            .getCapability(ColorMapCapability.class)
                            .getColorMapParameters());
                }
                image.setBrightness(imaging.getBrightness());
                image.setContrast(imaging.getContrast());
                image.setInterpolated(imaging.isInterpolationState());

                DrawableImage di = new DrawableImage(image, tile.coverage);
                if (paintProps.isZooming()) {
                    di.setMode(RasterMode.ASYNCHRONOUS);
                }

                drawableImages.add(di);
            } else {
                rsc.issueRefresh();
            }

            if (image == null || image.getStatus() != Status.LOADED
                    || tile.coverage.getMesh() == null) {

                needDrawLower = true;

                if (tile.coverage != null && tile.coverage.getMesh() == null) {
                    tile.coverage.setMesh(target.getExtension(
                            IMapMeshExtension.class)
                            .constructMesh(tile.imageGeometry,
                                    mapDescriptor.getGridGeometry()));
                    target.setNeedsRefresh(true);
                }
            }
        }

        // Draw lower resolution data first
        if (needDrawLower && (lvl + 1) < levels) {
            drawInternal(target, paintProps, lvl + 1, depth + 1);
        }

        // Draw all tiles at once
        target.drawRasters(paintProps, drawableImages
                .toArray(new DrawableImage[drawableImages.size()]));
    }

    /**
     * Deprecated, use DrawableImage and use target.drawImages(...)
     * 
     * @param target
     * @param paintProps
     * @param tiles
     * @param isRecursiveCall
     * @throws VizException
     */
    @Deprecated
    protected void drawTiles(IGraphicsTarget target,
            PaintProperties paintProps, List<TilePair> pairs,
            boolean isRecursiveCall) throws VizException {

        ImagingCapability imaging = rsc.getCapability(ImagingCapability.class);

        // Then draw the current level
        for (TilePair pair : pairs) {
            ImageTile tile = pair.tile;
            IImage image = pair.image;
            if (image != null) {

                if (tile.coverage == null) {
                    System.out.println("Coverage null");
                    continue;
                }

                if (image != null && tile.coverage != null) {
                    if (image instanceof IColormappedImage) {
                        ((IColormappedImage) image).setColorMapParameters(rsc
                                .getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
                    }
                    image.setBrightness(imaging.getBrightness());
                    image.setContrast(imaging.getContrast());
                    image.setInterpolated(imaging.isInterpolationState());

                    RasterMode rm = RasterMode.SYNCHRONOUS;
                    if (paintProps.isZooming()) {
                        rm = RasterMode.ASYNCHRONOUS;
                    }
                    target.drawRaster(image, tile.coverage, paintProps, rm);

                }

            } else {
                target.setNeedsRefresh(true);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public synchronized void paint(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (disposed) {
            return;
        }
        lastPaintedTarget = target;
        do2D(target, paintProps);
    }

    protected void do2D(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();

        int usedTileLevel = levels - 1;

        /*
         * pixelRatios[usedTileLevel] * screenToWorldRatio gives us
         * canvasPixels/image pixel at the level, We should use the level if
         * there are less than LEVEL_CHANGE_THRESHOLD canvas pixels per image
         * pixel
         */
        while ((pixelWidth[usedTileLevel] * screenToWorldRatio > LEVEL_CHANGE_THRESHOLD)
                && usedTileLevel > 0) {
            usedTileLevel--;
        }

        if (this.tileSet.getTileSet() != null
                && tileSet.getTileSet().size() > 0) {
            drawInternal(target, paintProps, usedTileLevel, 0);

            lastPaintedLevel = usedTileLevel;
        }

    }

    public void setMapDescriptor(IMapDescriptor descriptor) {
        this.mapDescriptor = descriptor;
    }

    public IMapDescriptor getMapDescriptor() {
        return this.mapDescriptor;
    }

    public synchronized void dispose() {
        disposed = true;

        for (IImage image : imageMap.values()) {
            if (image != null) {
                image.dispose();
            }
        }
        if (tileSet != null) {
            tileSet.dispose();
        }
        imageMap.clear();
        for (CreateTileJob job : jobMap.values()) {
            tileCreationPool.cancel(job);
        }
    }

    public void init(IGraphicsTarget target) throws VizException {
        disposed = false;
        this.lastPaintedTarget = target;

        if (this.sharedGeometryTileSet == null) {
            try {

                for (int i = 0; i < levels; i++) {
                    createMathTransform(i);
                }
            } catch (Exception e) {
                throw new VizException("Error initializing tile set", e);
            }
        }

        reproject();
    }

    /**
     * Responsible for setting the mathTransforma and inverseMathTransform for
     * this level
     * 
     * @param level
     * @throws NoninvertibleTransformException
     */
    protected void createMathTransform(int level)
            throws NoninvertibleTransformException {
        this.mathTransform[level] = this.gridGeometry[level]
                .getGridToCRS(this.cellOrientation);
        this.inverseMathTransform[level] = this.mathTransform[level].inverse();
    }

    private void setSharedGeometryTileSet(AbstractTileSet sharedGeometryTileset) {
        this.sharedGeometryTileSet = sharedGeometryTileset;
        this.levels = sharedGeometryTileset.levels;
        this.localProjToLL = sharedGeometryTileset.localProjToLL;
        this.llToLocalProj = sharedGeometryTileset.llToLocalProj;
        this.gridGeometry = sharedGeometryTileset.gridGeometry;
        this.sharedGeometryTileSet = sharedGeometryTileset;
        this.interpolationFactorOnLoad = sharedGeometryTileset.interpolationFactorOnLoad;
        this.tileSize = sharedGeometryTileset.tileSize;
        this.inverseMathTransform = sharedGeometryTileset.inverseMathTransform;
        this.pixelWidth = sharedGeometryTileset.pixelWidth;
        this.mathTransform = sharedGeometryTileset.mathTransform;
        this.originalGridGeometry = sharedGeometryTileset.originalGridGeometry;
        this.originalMathTransform = sharedGeometryTileset.originalMathTransform;
        this.cellOrientation = sharedGeometryTileset.cellOrientation;
    }

    public void reproject() throws VizException {
        // Do pixel width
        try {
            if (this.sharedGeometryTileSet != null) {
                pixelWidth = sharedGeometryTileSet.pixelWidth;
            } else {
                for (int level = 0; level < this.levels; level++) {

                    // Grab the center x, 3/4 y of the map
                    double mapYCenter = mapDescriptor.getGridGeometry()
                            .getGridRange().getSpan(0) * 0.5;
                    double mapXCenter = mapDescriptor.getGridGeometry()
                            .getGridRange().getSpan(1) * 0.75;

                    // Get the lat/lon points at center and center offset 1.
                    double[] mapCenterLL = mapDescriptor
                            .pixelToWorld(new double[] { mapXCenter, mapYCenter });
                    double[] mapCenterLL1 = mapDescriptor
                            .pixelToWorld(new double[] { mapXCenter + 1,
                                    mapYCenter });

                    double[] input = new double[] { mapCenterLL[0],
                            mapCenterLL[1], mapCenterLL1[0], mapCenterLL1[1] };
                    double[] output = new double[input.length];

                    // Convert ll to local grid projection
                    llToLocalProj.transform(input, 0, output, 0, 2);

                    // project into image space
                    if (inverseMathTransform[level] == null) {
                        createMathTransform(level);
                    }

                    // Convert lat/lon center points into image grid space
                    inverseMathTransform[level].transform(output, 0, output, 0,
                            2);

                    // compute the number of map pixels per tile pixel
                    pixelWidth[level] = 1 / Math.abs(new Coordinate(output[0],
                            output[1], 0.0).distance(new Coordinate(output[2],
                            output[3], 0.0)));
                }

            }
        } catch (TransformException e) {
            throw new VizException("Error computing pixelWidth", e);
        }

        if (tileSet != null) {
            tileSet.dispose();
        }
        tileSet = TileListFactory.getTileList(levels, tileSize,
                originalGridGeometry, mapDescriptor.getGridGeometry());
        synchronized (tileSet) {
            if (tileSet.getTileSet().isEmpty()) {
                for (int i = 0; i < levels; ++i) {
                    createTileLevel(i, tileSize);
                }

                int level = 0;
                for (ImageTile[][] tileLevel : tileSet.getTileSet()) {
                    for (int i = 0; i < tileLevel.length; i++) {
                        for (int j = 0; j < tileLevel[i].length; j++) {
                            ImageTile tile = tileLevel[i][j];
                            if (tile == null) {
                                continue;
                            }

                            ReferencedEnvelope envelope = tile.getEnvelope();
                            if (sharedGeometryTileSet != null
                                    && sharedGeometryTileSet.tileSet != tileSet) {
                                ImageTile baseTile = sharedGeometryTileSet.tileSet
                                        .getTileSet().get(level)[i][j];
                                tile.coverage = baseTile.coverage;
                            } else if (localProjToLL != null) {
                                double[] ll = new double[2];
                                double[] ul = new double[2];
                                double[] lr = new double[2];
                                double[] ur = new double[2];

                                try {
                                    envelope = envelope.transform(
                                            mapDescriptor.getCRS(), false);
                                    ll[0] = envelope.getMinX();
                                    ll[1] = envelope.getMinY();

                                    ul[0] = envelope.getMinX();
                                    ul[1] = envelope.getMaxY();

                                    ur[0] = envelope.getMaxX();
                                    ur[1] = envelope.getMaxY();

                                    lr[0] = envelope.getMaxX();
                                    lr[1] = envelope.getMinY();

                                    mapDescriptor.getGridGeometry()
                                            .getGridToCRS().inverse()
                                            .transform(ul, 0, ul, 0, 1);
                                    mapDescriptor.getGridGeometry()
                                            .getGridToCRS().inverse()
                                            .transform(ll, 0, ll, 0, 1);
                                    mapDescriptor.getGridGeometry()
                                            .getGridToCRS().inverse()
                                            .transform(lr, 0, lr, 0, 1);
                                    mapDescriptor.getGridGeometry()
                                            .getGridToCRS().inverse()
                                            .transform(ur, 0, ur, 0, 1);

                                } catch (Throwable t) {
                                    // Skip tile on error
                                    statusHandler.handle(Priority.VERBOSE,
                                            "Error reprojecting tile " + i
                                                    + ":" + j + " at level "
                                                    + level, t);
                                    ul = ll = lr = ur = null;
                                    tileLevel[i][j] = null;
                                }

                                if (ul == null || ll == null || lr == null
                                        || ur == null) {
                                    tile.coverage = null;
                                } else {

                                    Coordinate ulc = new Coordinate(ul[0],
                                            ul[1], 0);
                                    Coordinate llc = new Coordinate(ll[0],
                                            ll[1], 0);
                                    Coordinate lrc = new Coordinate(lr[0],
                                            lr[1], 0);
                                    Coordinate urc = new Coordinate(ur[0],
                                            ur[1], 0);

                                    tile.coverage = new PixelCoverage(ulc, urc,
                                            lrc, llc);
                                }
                            } else if (envelope != null) {
                                tile.coverage = mapDescriptor
                                        .worldToPixel(envelope);
                            }

                        }
                    }
                    level++;
                }
            }
        }
    }

    public double interrogate(Coordinate coord, boolean getRaw)
            throws VizException {

        double[] out = new double[2];
        try {
            if (llToLocalProj != null) {
                double[] in = new double[2];

                in[0] = coord.x;
                in[1] = coord.y;

                llToLocalProj.transform(in, 0, out, 0, 1);

            } else {
                out[0] = coord.x;
                out[1] = coord.y;
            }

            int lastLevel = this.lastPaintedLevel;

            double[] outCoords = new double[2];
            if (this.inverseMathTransform == null
                    || this.inverseMathTransform[lastLevel] == null) {
                return Double.NaN;
            }
            this.inverseMathTransform[lastLevel].transform(out, 0, outCoords,
                    0, 1);

            ImageTile[][] tiles = this.tileSet.getTileGrid(lastLevel);

            if (tiles == null) {
                return Double.NaN;
            }

            try {
                if (!this.hasDataPreloaded(lastLevel)) {
                    this.preloadDataObject(lastLevel);
                }
            } catch (StorageException e1) {
                throw new VizException("Unable to load data to interrogate", e1);
            }

            for (int i = 0; i < tiles.length; i++) {
                for (int j = 0; j < tiles[0].length; j++) {
                    ImageTile tile = tiles[i][j];
                    if (tile != null && tile.contains(out[0], out[1])) {
                        int coordX = (int) (outCoords[0]);
                        int coordY = (int) (outCoords[1]);

                        // Since createTile is asynchronous, wait to see
                        // if tile image is available
                        IImage image = imageMap.get(tile);
                        if (image != null) {
                            if (getRaw
                                    || (rsc.getCapability(
                                            ColorMapCapability.class)
                                            .getColorMapParameters()
                                            .getDataToDisplayConverter() == null)) {
                                if (image instanceof IColormappedImage) {
                                    return ((IColormappedImage) image)
                                            .getValue(coordX % this.tileSize,
                                                    coordY % this.tileSize);
                                } else {
                                    return Double.NaN;
                                }
                            } else {
                                if (image instanceof IColormappedImage) {
                                    double value = ((IColormappedImage) image)
                                            .getValue(coordX % this.tileSize,
                                                    coordY % this.tileSize);
                                    if (value <= -999999) {
                                        return Double.NaN;
                                    }
                                    return rsc
                                            .getCapability(
                                                    ColorMapCapability.class)
                                            .getColorMapParameters()
                                            .getDataToDisplayConverter()
                                            .convert(value);
                                }
                            }
                        }
                    }
                }
            }
        } catch (TransformException e) {
            throw new VizException("Error interrogating ", e);
        }

        return Double.NaN;

    }

    /**
     * @return the elevation
     */
    public double getElevation() {
        return this.elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(double elevation) {
        this.elevation = elevation;
    }

    public void addImage(final IImage image, int level, int i, int j) {
        if (image != null) {
            if (disposed) {
                // We have already been disposed, dispose images that were
                // created after dispose
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        image.dispose();
                    }
                });
            } else {
                imageMap.put(tileSet.getTile(level, i, j), image);
                rsc.issueRefresh();
            }
        }
    }

    public void remove(MultiKey key) {
        jobMap.remove(key);
    }

    public abstract void cancelRequest(int level, int i, int j);

    public boolean doneLoading() {
        return jobMap.isEmpty();
    }

    public void addMeshCallback(IMeshCallback meshCallback) {
        this.meshCallbacks.add(meshCallback);
    }

    public void removeMeshCallback(IMeshCallback meshCallback) {
        this.meshCallbacks.remove(meshCallback);
    }

    public void meshCalculated(ImageTile tile) {
        for (IMeshCallback meshCallback : meshCallbacks) {
            meshCallback.meshCalculated(tile);
        }
    }

}
