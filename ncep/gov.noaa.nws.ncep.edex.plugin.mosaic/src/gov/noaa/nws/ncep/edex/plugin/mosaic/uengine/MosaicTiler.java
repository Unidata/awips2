package gov.noaa.nws.ncep.edex.plugin.mosaic.uengine;

import gov.noaa.nws.ncep.edex.plugin.mosaic.common.MosaicRecord;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import javax.measure.converter.MultiplyConverter;
import javax.measure.converter.UnitConverter;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.crs.ProjectedCRS;

/**
 * A tiler class that will allow the user to take a radial container and create
 * tiles at a particular zoom level.
 * 
 * Date         Ticket#         Engineer    	Description
 * ------------ ----------      ----------- 	--------------------------
 * 09/2009      143				L. Lin     		Initial creation
 * 12/2009		143				mgamazaychikov	Made constructor and constructGridGeometry
 * 												more suitable for raster images
 * 01/2010		204				M. Li			Set tileSize; correct geometry envelope
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class MosaicTiler {

    private int levels;

    private int tileSize;
    
    private int tileSizeX, tileSizeY;

    private int fullResolution;
    
    private int fullResolutionX, fullResolutionY;

    private int actualArrayLength;

    private byte[] blankImage;

    private ByteBuffer bImage;

    private MosaicRecord mosaicData;

    private double maxExtentW;
    
    private double maxExtentH;

    private UnitConverter dataToImage;

    public MosaicTiler(MosaicRecord data) {
        this(data, 1, data.getNy() * 2, null);
    }

    public MosaicTiler(MosaicRecord data, int levels, int tileSize) {
        this(data, levels, tileSize, null);
    }

    /**
     * Creates an instance of the mosaic tiler with the given mosaic record.
     * 
     * @param data
     *            The mosaic record to tile
     * @param fullImage
     *            Indicates if the tiler should be used to create a full image
     *            instead of tiles
     */
    public MosaicTiler(MosaicRecord data, int levels, int tileSize,
            UnitConverter dataToImage) {
        mosaicData = data;
        if ("Radial".equals(data.getFormat())) {
            this.levels = levels;
            this.tileSize = tileSize;
            this.fullResolution = (int) (this.tileSize * Math.pow(2,
                    this.levels - 1));
            actualArrayLength = this.tileSize * this.tileSize;
            blankImage = new byte[actualArrayLength];
            Arrays.fill(blankImage, (byte) 0);
        } else if ("Raster".equals(data.getFormat())) {
            this.levels = 1;
            this.tileSizeX = data.getNy();
            this.tileSizeY = data.getNx();
            this.tileSize = data.getNy(); 
            
            this.fullResolutionX = (int) (this.tileSizeX * Math.pow(2,
                    this.levels - 1));
            this.fullResolutionY = (int) (this.tileSizeY * Math.pow(2,
                    this.levels - 1));
            actualArrayLength = this.tileSizeX * this.tileSizeY;
            blankImage = new byte[actualArrayLength];
            Arrays.fill(blankImage, (byte) 0);
        }
        

        if (dataToImage == null) {
            this.dataToImage = UnitConverter.IDENTITY;
            if (data.getNumLevels() <= 16) {
                this.dataToImage = new MultiplyConverter(16);
            }
        } else {
            this.dataToImage = dataToImage;
        }

        
    }

    /**
     * @return A byte buffer representation of the image
     */
    public ByteBuffer createTile(int tileX, int tileY, int level, boolean direct) {
        createImage(direct);
        // if it's a raster product just use the raw data
        if ("Raster".equals(mosaicData.getFormat())) {
            byte[] rawData = mosaicData.getRawData();
            for (int i = 0; i < rawData.length; i++) {
                bImage.put(convertToImage(rawData[i]));
            }
            return bImage;
        }
        return bImage;
    }

    /**
     * @param b
     * @return
     */
    private byte convertToImage(byte b) {
        double image = dataToImage.convert((b) & 0xFF);
        if (Double.isNaN(image)) {
            return b;
        } else {
            return (byte) Math.round(image);
        }
    }

    /**
     * Creates a full image of the mosaic data. The class needs to be constructed
     * with the fullImage flag set to true for this to work.
     * 
     * @return A byte array of the full image
     */
    public byte[] createFullImage() {
        return createTile(0, 0, 0, false).array();
    }

    public GridGeometry2D constructGridGeometry() {
        ProjectedCRS crs = mosaicData.getCRS();

        GridGeometry2D gridGeometry2D = null;

        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);
        
        maxExtentW = mosaicData.getResolution() * mosaicData.getNy();
        maxExtentH = mosaicData.getResolution() * mosaicData.getNx();

        if ("Raster".equals(mosaicData.getFormat())) {
            maxExtentW /= 2;
            maxExtentH /= 2;
        }

        generalEnvelope.setRange(0, -maxExtentW, maxExtentW);
        generalEnvelope.setRange(1, -maxExtentH, maxExtentH);
        
        if ("Raster".equals(mosaicData.getFormat())) {
        	gridGeometry2D = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
                    0, 0 }, new int[] { fullResolutionX, fullResolutionY }, false),
                    generalEnvelope);
        }
        else if ("Radial".equals(mosaicData.getFormat())) {
        	gridGeometry2D = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
                    0, 0 }, new int[] { fullResolution, fullResolution }, false),
                    generalEnvelope);
        }
        

        return gridGeometry2D;
    }

    public double getMaxExent() {
        return maxExtentW;

    }

    /**
     * Returns the actual width of the imagery. This is typically used for the
     * full image.
     * 
     * @return The current width
     */
    public int getWidth() {
        return tileSizeX;
    }

    /**
     * Returns the actual height of the imagery. This is typically used for the
     * full image.
     * 
     * @return The current height
     */
    public int getHeight() {
        return tileSizeY;
    }

    private void createImage(boolean direct) {
        if (direct) {
            bImage = ByteBuffer.allocateDirect(actualArrayLength);
        } else {
            bImage = ByteBuffer.allocate(actualArrayLength);
        }
        bImage.order(ByteOrder.nativeOrder());
    }

    public int getLevels() {
        return levels;
    }

    public int getTileSize() {
        return tileSize;
    }

}