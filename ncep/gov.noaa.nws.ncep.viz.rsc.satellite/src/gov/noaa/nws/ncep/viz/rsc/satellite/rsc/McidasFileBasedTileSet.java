/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.keyvalue.MultiKey;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.crs.DefaultProjectedCRS;
import org.opengis.parameter.ParameterValue;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IImage.Status;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.uf.viz.core.rsc.hdf5.MeshCalculatorJob;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.raytheon.viz.core.rsc.hdf5.CreateTileJob;
import com.raytheon.viz.satellite.rsc.SatFileBasedTileSet;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * Basically same as SatFileBasedTileSet except that it overrides some methods
 * so that all tiles are created (and maybe displayed) instead of only creating
 * tiles within the given pixel extent.
 * 
 * @author sgilbert
 * 
 */
public class McidasFileBasedTileSet extends SatFileBasedTileSet {

    private GeometryFactory gf;

    private LineString edgeOfMap;

    private boolean isMapMercator;

    public McidasFileBasedTileSet(PluginDataObject pdo, String dataset,
            AbstractTileSet sharedGeometryTileset) throws VizException {
        super(pdo, dataset, sharedGeometryTileset);
        gf = new GeometryFactory();
    }

    public McidasFileBasedTileSet(PluginDataObject pdo, String dataset,
            int levels, int tileSize, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ?> rsc, PixelInCell pixelOrientation,
            String viewType) throws VizException {
        super(pdo, dataset, levels, tileSize, gridGeometry, rsc,
                pixelOrientation, viewType);
        gf = new GeometryFactory();
    }

    /*
     * Almost the same as method createTileLevel in AbstractTileSet. This method
     * was copied from class AbstractTileSet, and the checks for tiles in the
     * map envelope were removed so that all tiles are created.
     */
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

        this.tileSet.tileSet.add(tiles);
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

                    tiles[i][j] = new ImageTile();

                    tiles[i][j].rect = new Rectangle(startX, startY,
                            effectiveWidth, effectiveHeight);

                    tiles[i][j].envelope = env;
                    tiles[i][j].elevation = this.elevation;
                    tiles[i][j].rect.width = effectiveWidth;
                    tiles[i][j].rect.height = effectiveHeight;
                }

            }
        } catch (Exception e) {
            throw new VizException("Error creating tile level", e);
        }

    }

    /*
     * Overriding this method so that this local version of createTileLevel() is
     * called instead of the one in AbstractTileSet. This method was copied from
     * class AbstractTileSet and the references to the sharedGeometryTileSet
     * were commented out since their fields were inaccessible.
     * 
     * @see com.raytheon.viz.core.rsc.hdf5.AbstractTileSet#reproject()
     */
    @Override
    public void reproject() throws VizException {
        // Do pixel width
        try {
            // if (this.sharedGeometryTileSet != null) {
            // pixelWidth = sharedGeometryTileSet.pixelWidth;
            // } else {
            for (int level = 0; level < this.levels; level++) {

                // select a point in the center of the tile set and it's
                // neighbor
                // TODO: select a point near the center of the map instead
                double xCenter = this.gridGeometry[level].getGridRange()
                        .getSpan(0) / 2.0;
                double yCenter = this.gridGeometry[level].getGridRange()
                        .getSpan(1) / 2.0;

                double[] input = new double[] { xCenter, yCenter, xCenter + 1,
                        yCenter };
                double[] output = new double[input.length];

                if (mathTransform[level] == null) {
                    createMathTransform(level);
                }

                // convert the point s to lat/lon
                mathTransform[level].transform(input, 0, output, 0,
                        input.length / 2);

                this.localProjToLL.transform(output, 0, output, 0, 2);

                // convert the points to pixels in the map
                double[] p1 = this.mapDescriptor.worldToPixel(new double[] {
                        output[0], output[1] });
                double[] p2 = this.mapDescriptor.worldToPixel(new double[] {
                        output[2], output[3] });

                // compute the number of map pixels per tile pixel
                pixelWidth[level] = Math.abs(p2[0] - p1[0]);
            }

            // }
        } catch (TransformException e) {
            throw new VizException("Error computing pixelWidth", e);
        }

        // if (sharedGeometryTileSet == null) {
        tileSet.tileSet.clear();
        for (int i = 0; i < levels; ++i) {
            createTileLevel(i, tileSize);
        }
        // } else {
        // tileSet = sharedGeometryTileSet.tileSet;
        // }

        int level = 0;
        for (ImageTile[][] tileLevel : tileSet.tileSet) {
            for (int i = 0; i < tileLevel.length; i++) {
                for (int j = 0; j < tileLevel[i].length; j++) {
                    ImageTile tile = tileLevel[i][j];
                    if (tile == null) {
                        continue;
                    }

                    // if (this.sharedGeometryTileSet != null) {
                    // ImageTile baseTile =
                    // sharedGeometryTileSet.tileSet.tileSet
                    // .get(level)[i][j];
                    // tile.coverage = baseTile.coverage;
                    if (localProjToLL != null) {
                        double[] in = new double[2];
                        double[] ll = new double[2];
                        double[] ul = new double[2];
                        double[] lr = new double[2];
                        double[] ur = new double[2];

                        try {
                            ReferencedEnvelope envelope = ((ReferencedEnvelope) tile.envelope)
                                    .transform(mapDescriptor.getCRS(), false);
                            ll[0] = envelope.getMinX();
                            ll[1] = envelope.getMinY();

                            ul[0] = envelope.getMinX();
                            ul[1] = envelope.getMaxY();

                            ur[0] = envelope.getMaxX();
                            ur[1] = envelope.getMaxY();

                            lr[0] = envelope.getMaxX();
                            lr[1] = envelope.getMinY();

                            mapDescriptor.getGridGeometry().getGridToCRS()
                                    .inverse().transform(ul, 0, ul, 0, 1);
                            mapDescriptor.getGridGeometry().getGridToCRS()
                                    .inverse().transform(ll, 0, ll, 0, 1);
                            mapDescriptor.getGridGeometry().getGridToCRS()
                                    .inverse().transform(lr, 0, lr, 0, 1);
                            mapDescriptor.getGridGeometry().getGridToCRS()
                                    .inverse().transform(ur, 0, ur, 0, 1);

                        } catch (TransformException e) {
                            throw new VizException(
                                    "Error performing reproject", e);
                        } catch (FactoryException e) {
                            throw new VizException(
                                    "Error performing reproject", e);
                        }

                        if (ul == null || ll == null || lr == null
                                || ur == null) {
                            tile.coverage = null;
                        } else {

                            Coordinate ulc = new Coordinate(ul[0], ul[1], 0);
                            Coordinate llc = new Coordinate(ll[0], ll[1], 0);
                            Coordinate lrc = new Coordinate(lr[0], lr[1], 0);
                            Coordinate urc = new Coordinate(ur[0], ur[1], 0);

                            tile.coverage = new PixelCoverage(ulc, urc, lrc,
                                    llc);
                        }
                    } else {
                        if (tile.envelope != null) {
                            tile.coverage = mapDescriptor
                                    .worldToPixel(tile.envelope);
                        }
                    }

                }
            }
            level++;
        }

        /*
         * Set up display tests based on current map projection
         */
        setTileDisplayTest();

    }

    /*
     * If the map descriptor specifies a Mercator map projection, save the "edge
     * of the world" in the satellite image coordinates. This will be used later
     * to help determine if a specific tile should be displayed on the screen.
     */
    private void setTileDisplayTest() {

        isMapMercator = false;
        edgeOfMap = null;

        isMapMercator = mapDescriptor.getCRS().getName().toString()
                .contains("Mercator");

        if (isMapMercator
                && (mapDescriptor.getCRS() instanceof DefaultProjectedCRS)) {
            /*
             * Find central meridian of map projection
             */
            DefaultProjectedCRS crs = (DefaultProjectedCRS) mapDescriptor
                    .getCRS();
            // System.out.println( crs.getConversionFromBase().toString() );
            ParameterValue<?> pv = crs.getConversionFromBase()
                    .getParameterValues().parameter("central_meridian");
            // System.out.println( pv );
            // System.out.println( pv.getClass().getCanonicalName());
            // System.out.println( pv.getValue().getClass().getCanonicalName()
            // );
            // System.out.println( pv.getValue());
            double cm = (Double) pv.getValue();
            double longitude = cm + 180.;
            double[] imageLoc = new double[3];
            CoordinateList cdlist = new CoordinateList();

            /*
             * construct a LineString representing the edge of the world (
             * central meridian + 180 ) in the satellite image coordinates.
             */
            try {
                for (double j = 80; j >= -80; j = j - 10) {
                    double[] loc = new double[] { longitude, j, 0 };
                    llToLocalProj.transform(loc, 0, imageLoc, 0, 1);
                    if (!Double.isNaN(imageLoc[0]))
                        cdlist.add(new Coordinate(imageLoc[0], imageLoc[1]),
                                true);
                }
                edgeOfMap = gf.createLineString(cdlist.toCoordinateArray());
            } catch (Exception e) {
                isMapMercator = false;
                edgeOfMap = null;
            }
        }

    }

    /**
     * @return the originalGridGeometry
     */
    public GridGeometry2D getOriginalGridGeometry() {
        return originalGridGeometry;
    }

    /*
     * Overriding this method to add additional check when determining which
     * Tiles to draw. The new check tests whether the current tile intersects
     * the "edge of the world" when draw to a Mercator map projection. These
     * tiles will be ignored since they would be displayed as long horizontal
     * stripes across the map and disrupts the valid image area.
     */
    @Override
    protected void drawInternal(IGraphicsTarget target,
            PaintProperties paintProps, int lvl, int depth) throws VizException {

        List<ImageTile> intersectedTiles = new ArrayList<ImageTile>();
        boolean needDrawLower = false;
        ImageTile[][] tiles = tileSet.tileSet.get(lvl);

        for (int i = 0; i < tiles.length; i++) {
            for (int j = 0; j < tiles[i].length; j++) {
                MultiKey key = new MultiKey(lvl, i, j);
                ImageTile tile = tiles[i][j];
                if (tile != null && tile.occlude == false) {
                    if (tile.coverage != null
                            && tile.coverage.intersects(paintProps.getView()
                                    .getExtent())
                            && tile.coverage.intersects(paintProps
                                    .getClippingPane())) {
                        IImage image = imageMap.get(tile);
                        if (image == null
                                || (image.getStatus() != Status.LOADED
                                        && image.getStatus() != Status.STAGED && image
                                        .getStatus() != Status.LOADING)) {

                            try {
                                if (!this.hasDataPreloaded(lvl)) {
                                    this.preloadDataObject(lvl);
                                }
                            } catch (StorageException e) {
                                throw new VizException(
                                        "Error reading data to draw: ", e);
                            }

                            if (jobMap.get(key) == null && depth == 0
                                    && threads < MAX_THREADS) {
                                ++threads;
                                CreateTileJob job = new CreateTileJob("tileset");
                                job.setI(i);
                                job.setJ(j);
                                job.setLevel(lvl);
                                job.setTarget(target);
                                job.setTileSet(this);
                                jobMap.put(key, job);
                                job.setSystem(true);
                                job.schedule();
                            }
                        }
                    }
                }
            }
        }

        for (int i = 0; i < tiles.length; i++) {
            for (int j = 0; j < tiles[i].length; j++) {
                ImageTile tile = tiles[i][j];
                if (tile != null && tile.occlude == false) {

                    if (tile.coverage != null
                            && tile.coverage.intersects(paintProps.getView()
                                    .getExtent())
                            && tile.coverage.intersects(paintProps
                                    .getClippingPane())) {
                        intersectedTiles.add(tile);
                    }
                }
            }
        }

        // Join on the tile creations
        if (jobMap.isEmpty() == false) {
            for (CreateTileJob job : jobMap.values()) {
                try {
                    job.join();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }

        List<TilePair> tilePairs = new ArrayList<TilePair>();

        for (ImageTile tile : intersectedTiles) {
            IImage image = imageMap.get(tile);

            // SAG
            if (isMapMercator) {
                try {
                    CoordinateList celist = new CoordinateList();
                    celist.add(new Coordinate(tile.envelope.getMinX(),
                            tile.envelope.getMinY()), true);
                    celist.add(new Coordinate(tile.envelope.getMinX(),
                            tile.envelope.getMaxY()), true);
                    celist.add(new Coordinate(tile.envelope.getMaxX(),
                            tile.envelope.getMaxY()), true);
                    celist.add(new Coordinate(tile.envelope.getMaxX(),
                            tile.envelope.getMinY()), true);
                    celist.add(new Coordinate(tile.envelope.getMinX(),
                            tile.envelope.getMinY()), true);
                    LinearRing lrng = gf.createLinearRing(celist
                            .toCoordinateArray());
                    Polygon poly = gf.createPolygon(lrng, null);
                    if (edgeOfMap.intersects(lrng))
                        continue;
                } catch (Exception e) {
                    e.printStackTrace();
                    continue;
                }
            }
            // SAG

            tilePairs.add(new TilePair(tile, image));
            if (image == null || image.getStatus() != Status.LOADED
                    || tile.coverage.getMesh() == null) {

                needDrawLower = true;

                if (tile.coverage != null && tile.coverage.getMesh() == null) {
                    if (viewType == null) {
                        viewType = VizConstants.VIEW_2D;
                    }
                    IMesh mesh = target.getExtension(IMapMeshExtension.class)
                            .constructMesh(mapDescriptor);
                    MeshCalculatorJob.getInstance().requestLoad(mesh, tile,
                            this.localProjToLL);
                    target.setNeedsRefresh(true);
                }
            }
        }

        // Draw lower resolution data first
        if (needDrawLower && (lvl + 1) < levels) {
            drawInternal(target, paintProps, lvl + 1, depth + 1);
        }

        drawTiles(target, paintProps, tilePairs, depth > 0);
    }

    public MathTransform[] getMathTransform() {
        return this.mathTransform;
    }

    public MathTransform[] getInverseMathTransform() {
        return this.inverseMathTransform;
    }

    public int getLastPaintedLevel() {
        return this.lastPaintedLevel;
    }

    public Map<ImageTile, IImage> getImageMap() {
        return this.imageMap;
    }

    public int getTileSize() {
        return this.tileSize;
    }

    public PluginDataObject getPdo() {
        return this.pdo;
    }
}
