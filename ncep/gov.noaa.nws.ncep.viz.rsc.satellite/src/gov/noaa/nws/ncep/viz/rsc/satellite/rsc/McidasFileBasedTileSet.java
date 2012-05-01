/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;

import java.awt.Rectangle;
import java.util.Map;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.datum.PixelInCell;

import org.opengis.referencing.operation.TransformException;
import org.opengis.referencing.operation.MathTransform;


import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.raytheon.viz.core.rsc.hdf5.TileListFactory;
//import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet.TilePair;
import com.raytheon.viz.satellite.rsc.SatFileBasedTileSet;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Basically same as SatFileBasedTileSet except that it overrides some methods so
 * that all tiles are created (and maybe displayed) instead of only creating tiles
 * within the given pixel extent.
 * @author sgilbert
 *
 */
public class McidasFileBasedTileSet extends SatFileBasedTileSet {
	private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(McidasFileBasedTileSet.class);
	
    public McidasFileBasedTileSet(PluginDataObject pdo, String dataset,
            AbstractTileSet sharedGeometryTileset) throws VizException {
        super( pdo, dataset, sharedGeometryTileset);
    }
    
	public McidasFileBasedTileSet(PluginDataObject pdo, String dataset,
			int levels, int tileSize, GridGeometry2D gridGeometry,
			AbstractVizResource<?, ?> rsc, PixelInCell pixelOrientation,
			String viewType) throws VizException {
		super(pdo, dataset, levels, tileSize, gridGeometry, rsc, pixelOrientation,
				viewType);
	}

	/*
	 * Almost the same as method createTileLevel in AbstractTileSet.  
	 * This method was copied from class AbstractTileSet, and the checks for tiles in
	 * the map envelope were removed so that all tiles are created.
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
                            lr[0], ul[1], lr[1], gridGeometry[0]
                                    .getCoordinateReferenceSystem());

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
     * Overriding this method so that this local version of createTileLevel() is called
     * instead of the one in AbstractTileSet.  
     * This method was copied from class AbstractTileSet and the references to the 
     * sharedGeometryTileSet were commented out since their fields were inaccessible.
     * @see com.raytheon.viz.core.rsc.hdf5.AbstractTileSet#reproject()
     */
    @Override
    public void reproject() throws VizException {
        // Do pixel width
        try {
            //if (this.sharedGeometryTileSet != null) {
            //    pixelWidth = sharedGeometryTileSet.pixelWidth;
            //} else {
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

            //}
        } catch (TransformException e) {
            throw new VizException("Error computing pixelWidth", e);
        }

        if (tileSet != null) {
            tileSet.dispose();
        }
        tileSet = TileListFactory.getTileList(levels, tileSize,
                originalGridGeometry, mapDescriptor.getGridGeometry());

        if (sharedGeometryTileSet == null) {
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

                        //if (this.sharedGeometryTileSet != null) {
                        //    ImageTile baseTile = sharedGeometryTileSet.tileSet
                        //            .getTileSet().get(level)[i][j];
                        //    tile.coverage = baseTile.coverage;
                        //} else if (localProjToLL != null) {
                        if (localProjToLL != null) {
                            double[] ll = new double[2];
                            double[] ul = new double[2];
                            double[] lr = new double[2];
                            double[] ur = new double[2];

                            try {
                                ReferencedEnvelope envelope = ((ReferencedEnvelope) tile.envelope)
                                        .transform(mapDescriptor.getCRS(),
                                                false);
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

                            } catch (Throwable t) {
                                // Skip tile on error
                                statusHandler.handle(Priority.VERBOSE,
                                        "Error reprojecting tile " + i + ":"
                                                + j + " at level " + level, t);
                                ul = ll = lr = ur = null;
                                tileLevel[i][j] = null;
                            }

                            if (ul == null || ll == null || lr == null
                                    || ur == null) {
                                tile.coverage = null;
                            } else {

                                Coordinate ulc = new Coordinate(ul[0], ul[1], 0);
                                Coordinate llc = new Coordinate(ll[0], ll[1], 0);
                                Coordinate lrc = new Coordinate(lr[0], lr[1], 0);
                                Coordinate urc = new Coordinate(ur[0], ur[1], 0);

                                tile.coverage = new PixelCoverage(ulc, urc,
                                        lrc, llc);
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
        }

        /*
         * Set up display tests based on current map projection
        setTileDisplayTest();
         */
        
    }

	/**
	 * @return the originalGridGeometry
	 */
	public GridGeometry2D getOriginalGridGeometry() {
		return originalGridGeometry;
	}

	public MathTransform[] getMathTransform(){
		return this.mathTransform;
	}
	
	public MathTransform[] getInverseMathTransform(){
		return this.inverseMathTransform;
	}
	
	public int getLastPaintedLevel(){
		return this.lastPaintedLevel;
	}
	
	public Map<ImageTile,IImage> getImageMap(){
		return this.imageMap;
	}
	
	public int getTileSize(){
		return this.tileSize;
	}
	
	public PluginDataObject  getPdo(){
		return this.pdo;
	}
}
