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

package com.raytheon.edex.uengine.chain;


/**
 * Defined attribute names for use with <code>ChainDatatype</code> during Task and Action process.
 * 
 * @author pheaberl
 */
public final class ChainAttributes {

	/**
	 * Final output passed back by the Action, <code>Object</code>.
	 */
	public static final String CHAIN_OUTPUT = "chainOutput";
	
    /**
     * Original script used to create &mu;Engine chain. Used with
     * script validation to allow the original script to be returned
     * with the validation results. 
     */
    public static final String CHAIN_INPUT = "chainInput";
    /**
     * 
     */
    public static final String VALID_TIME = "valid_time";
    
    public static final String ICAO_FILE_PREFIX = "icao_name";
    
    public static final String SHAPEFILE_ATTRIBUTES = "shapefile_attributes";
    
    /**
     * 
     */
    public static final String DATA_URI = "dataURI";
    /**
     * Contains the log of validation results. Used with script validation
     * to allow the validation log to be returned with the results.
     */
    public static final String VALIDATION_LOG = "validationLog";
    
    /**
     * File name returned following Lucene search.
     */
    public static final String SEARCH_RESULT_KEY = "key";
    
    /**
     * Contents for a ASCII type retrieval.
     */
    public static final String PLAIN_TEXT_ASCII = "plainText";
    /**
     * Contains ASCII contents after conversion to XML.
     */
    public static final String XML_FORMATTED_ASCII = "xml";
	/**
	 * Used by Tasks to pass image to be processed, <code>byte[]</code>.
	 */
    public static final String IMAGE_BYTES = "imageBytes";
    
    /**
     * Processed image ready for output, <code>BufferedImage</code>.
     */
    public static final String IMAGE_BUFFERED = "imageBuffered";
    
    /**
     * Desired output format for processed image, <code>String</code>.
     */
    public static final String IMAGE_FORMAT = "imageFormat";
    
    /**
     * Location where image was output, <code>java.net.URI</code>.
     */
    public static final String IMAGE_OUT_URI = "imageOutURI";

    /**
     * Data read in from GRIB file, <code>float[]</code>.
     */
    public static final String GRIB_FLOAT_DATA = "gribFloatData";
    
    /**
     * WorldFile info passed along, <code>com.raytheon.edex.util.WorldFile</code>.
     */
    public static final String WORLD_FILE_OBJ = "worldFileObj";
    
    /**
     * Product attribute to describe a grib record
     */
    public static final String GRIB_PRODUCT_DEFINITION = "grib_product_definition";
    
    /**
     * The coordinate reference system (projection)
     */
    public static final String COORDINATE_REFERENCE_SYSTEM = "coordinate_reference_system";
    
    /**
     * The grid geometry: contains an envelope of the extent and the dimensions
     */
    public static final String GRID_GEOMETRY = "grid_geometry";
    
    /**
     * The bounding box contains the 4 points for a radar coverage
     */
    public static final String BOUNDING_BOX = "bounding_box";
    
    /**
     * Geometries used to create a shapefile
     */
    public static final String GEOMETRIES = "geometries";
    
    /**
     * Location of the generated shapefiles
     */
    public static final String SHAPEFILE_OUT_URI = "shapefileOutURI";
    
    /**
     * Image color array
     */
    public static final String COLORS = "colors";
}
