/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.edex.common.dataRecords.NcFloatDataRecord;
import gov.noaa.nws.ncep.gempak.parameters.core.contourinterval.CINT;
import gov.noaa.nws.ncep.gempak.parameters.infill.FINT;
import gov.noaa.nws.ncep.gempak.parameters.infill.FLine;
import gov.noaa.nws.ncep.gempak.parameters.line.LineDataStringParser;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.NcgribLogger;
import gov.noaa.nws.ncep.viz.tools.contour.ContourException;
import gov.noaa.nws.ncep.viz.tools.contour.ContourGenerator;
import gov.noaa.nws.ncep.viz.tools.contour.FillException;
import gov.noaa.nws.ncep.viz.tools.contour.FillGenerator;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.locks.*;


import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.operation.projection.MapProjection;
import org.geotools.referencing.operation.projection.MapProjection.AbstractProvider;
import org.opengis.parameter.ParameterValueGroup;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.coverage.grid.GridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateArrays;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;

/**
 * ContourSupport
 * 
 * Provides contouring wrapper
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 22, 2007             chammack    Initial Creation.
 *    May 26, 2009 #2172       chammack    Use zoomLevel to calculate label spacing
 *    Mar 10, 2010 #164		   M. Li	   Control increments on zoom   
 *    May 18, 2011			   M. Li	   Add contour label frequency capability
 *    May 26, 2011			   M. Li	   Add a new method createContourLabel
 *    Aug 18, 2011             M. li       fixed reproject problems for streamline
 *    Nov 08, 2011             X. Guo      Checked centeral_meridian and 
 *                                         added vertices twice after subtract 360  
 *    Feb 15, 2012             X. Guo      Used cached contour information to re-create
 *                                         wired frame
 *    Mar 01, 2012             X. Guo      Handle five zoom levels
 *    Mar 13, 2012             X. Guo      Handle multi-threads
 *    Mar 15, 2012             X. Guo      Refactor
 *    Mar 27, 2012             X. Guo      Used contour lock instead of "synchronized" 
 *    May 23, 2012             X. Guo      Loaded ncgrib logger
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ContourSupport {

	private static NcepLogger logger = NcepLoggerManager.getNcepLogger(ContourSupport.class);

	//provided values
	private IDataRecord records;
	private int level;
	private IExtent extent;
	private double currentDensity;
	private IMapDescriptor descriptor;
	private ContourAttributes attr;
	private String cint;
	private String fint;
	private String type;
	private String fline;
	private String name;
	private float zoom;
	
	//calculated values
	private ContourGroup contourGroup = null;
	private MathTransform rastPosToWorldGrid = null;
    private MathTransform rastPosToLatLon = null;
    private MathTransform rastPosLatLonToWorldGrid = null;
    private int zoomLevelIndex;
    private ContourGridData cntrData = null;
    private List<Double> cvalues;
    private List<Double> fvalues;
    private Set<Double> svalues;
    private boolean isWorld0;
    private boolean isCntrsCreated;
    private static NcgribLogger ncgribLogger = NcgribLogger.getInstance();;
    
    /**
     * Constructor
     * 
     * @param records
     * @param level
     * @param extent
     * @param currentDensity
     * @param worldGridToCRSTransform
     * @param imageGridGeometry
     * @param mapGridGeometry
     * @param target
     * @param descriptor
     * @param attr
     * @param name
     * @param zoom
     * @param contourGp
     * */
	public ContourSupport(IDataRecord records, int level,
            IExtent extent, double currentDensity,
            MathTransform worldGridToCRSTransform,
            GeneralGridGeometry imageGridGeometry,
            GeneralGridGeometry mapGridGeometry, IGraphicsTarget target,
            IMapDescriptor descriptor, ContourAttributes attr, String name, float zoom,
            ContourGroup contourGp) {
		
		initContourSupport ( records,  level,
	             extent,  currentDensity,
	             worldGridToCRSTransform,
	             imageGridGeometry,
	             mapGridGeometry,  target,
	             descriptor,  attr,  name,  zoom,
	             contourGp);
    }
	
    /**
     * Data structure for contouring
     */
    public static class ContourGroup {
        public double zoomLevel;

        public IWireframeShape posValueShape;

        public IWireframeShape negValueShape;
        
        public IShadedShape fillShapes;

        public ContourGroup parent;

        public PixelExtent lastUsedPixelExtent;

        public double lastDensity;

        public GridGeometry gridGeometry;
        
        public List<Double> cvalues;
        
        public List<Double> fvalues;
        
        public HashMap< String, Geometry> data;
        
        public LinearRing grid;

    }

    public class ContourGridData {
    	private float minValue;
    	private float maxValue;
    	private float[] data;
    	private int szX;
    	private int szY;
    	
    	public ContourGridData ( IDataRecord record ) {
    		maxValue = Float.MIN_VALUE;
            minValue = Float.MAX_VALUE;
            float[] data1D = null;
            long[] sz = record.getSizes();
            
            data1D = ((NcFloatDataRecord) record).getXdata();
            
            szX = (int)sz[0];
            szY = (int)sz[1];
            data = new float[szX*szY];

            for (int j = 0; j < szY; j++) {
                for (int i = 0; i < szX; i++) {
                	data[szX * j + i] = data1D[(szX * j)+ i];
                	if ( data[szX * j + i] != -999999.f ) {
                		maxValue = Math.max( maxValue, data[szX * j + i]);
                		minValue = Math.min( minValue, data[szX * j + i]);
                	}
                }
            }
    	}
    	
    	public float getMinValue () {
    		return minValue;
    	}
    	
    	public float getMaxValue () {
    		return maxValue;
    	}
    	
    	public float[] getData () {
    		return data;
    	}
    	public int getX () {
    		return szX;
    	}
    	
    	public int getY () {
    		return szY;
    	}
    }
    
    public void initContourSupport(IDataRecord records, int level,
            IExtent extent, double currentDensity,
            MathTransform worldGridToCRSTransform,
            GeneralGridGeometry imageGridGeometry,
            GeneralGridGeometry mapGridGeometry, IGraphicsTarget target,
            IMapDescriptor descriptor, ContourAttributes attr, String name, float zoom,
            ContourGroup contourGp) {
    	isCntrsCreated = true;
    	if ( records == null || attr == null ) {
    		isCntrsCreated = false;
    		return;
    	}
    	if ( ! initMathTransform (imageGridGeometry,mapGridGeometry) ) {
    		isCntrsCreated = false;
    		return;
    	}
    	this.records = records;
    	this.level = level;
    	this.extent = extent;
    	this.currentDensity = currentDensity;
    	this.descriptor = descriptor;
    	this.attr = attr;
    	this.cint = attr.getCint();
    	this.type = attr.getType();
    	this.fint = attr.getFint();
    	this.fline = attr.getFline();
    	this.name = name;
    	this.zoom = zoom;
    	this.cntrData = new ContourGridData(records);
    	this.isWorld0 = isWorld0(descriptor);
    	
    	initContourGroup ( target,contourGp );
    }
    /**
     * Create contours from provided parameters
     * 
     */
    public void createContours( ) {
    	long t0 = System.currentTimeMillis();
        
        // Copy the pixel extent (deep copy required!)
        // expand by 50% to cover the subgrid expansion
/*        PixelExtent workingExtent = (PixelExtent) extent.clone();
        workingExtent.getEnvelope().expandBy(workingExtent.getWidth() * .5,
                workingExtent.getHeight() * .5);*/
        /*
         * Contours and/or color fills
         */
        if (records instanceof NcFloatDataRecord && 
        		!((NcFloatDataRecord)records).isVector()) {

            long t1 = System.currentTimeMillis();
            logger.debug("Preparing " + name + " grid data took: " + (t1-t0));
            
            /*
             * ZoomLevel. 
             */
            initZoomIndex ();
            
            long t1a = System.currentTimeMillis();
            logger.debug("new ContourGenerator took: " + (t1a-t1));
            
            /*
        	 * Get contour values from CINT
        	 */
            cvalues = calcCintValue ();
            /*
        	 * Get color fill values from FINT and FLINE
        	 */
            fvalues = calcFintValue ();
            /*
             * Combine contour and fill values
             */
            combineCintAndFillValues ();
            
            long t2 = System.currentTimeMillis();
            if ( svalues != null  && svalues.size() > 0 ) {
            	genContour ();
            	if ( ! isCntrsCreated ) return;
            }
            else {
            	logger.debug("Re-load contour line values took: " + (t2-t1));
            }
            /*
             * Create contour lines and labels wireframes
             */
            createContourLines ();
            /*
             * Create color fills
             */
            createColorFills();
            
            long t10 = System.currentTimeMillis();
//            System.out.println("Contouring/Filling took: " + (t10-t0));
            logger.debug("===Total time for ("+name+") "+ " took: " + (t10-t0) + "\n");
//            logger.info("===Total time for "+ cf_string + " " + attr.getGdpfun().trim().toUpperCase() 
//            		+ " took: " + (t10-t0) + "\n");
            
//            System.out.println("Total time for " + cf_string + " " + name  + " took: " + (t10-t0) + "\n");
        /*
         * Streamlines    
         */
        } else {
        	createStreamLines();
        }
    }

    public static GeneralEnvelope calculateSubGrid(IExtent workingExtent,
            GeneralGridGeometry mapGridGeometry,
            GeneralGridGeometry imageGridGeometry) { 
        GeneralEnvelope env = null;
        try {
            // transform screen extent to map crs
            double[] screen = new double[] { workingExtent.getMinX(),
                    workingExtent.getMinY(), workingExtent.getMaxX(),
                    workingExtent.getMaxY() };
            double[] map = new double[4];
            mapGridGeometry.getGridToCRS(PixelInCell.CELL_CORNER).transform(
                    screen, 0, map, 0, 2);
            Envelope mapEnv = new Envelope(map[0], map[2], map[1], map[3]);
            
            // transform map envelope to image crs
            ReferencedEnvelope ref = new ReferencedEnvelope(mapEnv,
                    mapGridGeometry.getCoordinateReferenceSystem());
            
            Envelope imageEnv = ref.transform(imageGridGeometry
                    .getCoordinateReferenceSystem(), true);

            if (imageEnv == null) return null;
            
            // transform image envelope to image grid cells
            double[] image = new double[] { imageEnv.getMinX(),
                    imageEnv.getMinY(), imageEnv.getMaxX(), imageEnv.getMaxY() };
            double[] grid = new double[4];
            imageGridGeometry.getGridToCRS(PixelInCell.CELL_CENTER).inverse()
                    .transform(image, 0, grid, 0, 2);

            env = new GeneralEnvelope(2);
            env.setRange(0, Math.min(grid[0], grid[2]), Math.max(grid[0],
                    grid[2]));
            env.setRange(1, Math.min(grid[1], grid[3]), Math.max(grid[1],
                    grid[3]));
        } catch (Exception e) {
//            throw new VizException("Error transforming extent", e);
            logger.error("Error transforming extent:" + e);
            return null;
        }
//        System.out.println("*** Subgrid: " + env);
        return env;
    }

    private static void createContourLabel(IExtent extent, ContourGroup contourGroup,
    		float contourValue, double[][] valsArr) {
    	
    	double minx = extent.getMinX();
	    double miny = extent.getMinY();
	    double maxx = extent.getMaxX();
	    double maxy = extent.getMaxY();
	    
	    double[][] visiblePts = new double[valsArr.length][valsArr[0].length];
	    int actualLength = 0;
	    
	    for ( double[] dl : valsArr ) {
	    	if ( dl[0] > minx && dl[0] < maxx && 
	    			dl[1] > miny && dl[1] < maxy 	) {
	    		visiblePts[actualLength][0] = dl[0];
	    		visiblePts[actualLength][1] = dl[1];
	    		actualLength++;
	    	}
	    }
	    
	    DecimalFormat df = new DecimalFormat("0.#");
	    double[] loc = {0.0, 0.0};
	    
	    if (actualLength > 0) {
	    	loc[ 0 ] = visiblePts[ actualLength/2 ][0];
			loc[ 1 ] = visiblePts[ actualLength/2 ][1];	
			
			contourGroup.negValueShape.addLabel(df
                    .format(contourValue), loc);
	    }
    	
    }
    /**
     * Create labels for a linestring
     * 
     * @param contourGroup
     * @param levelOffset
     * @param contourValue
     * @param labelPoints
     * @param valsArr
     */
    private static void prepareLabel(ContourGroup contourGroup,
            double screenToPixel, float contourValue,
            List<double[]> labelPoints, double[][] valsArr) {
    	
        double[] lastPoint = new double[] { Double.POSITIVE_INFINITY,
                Double.POSITIVE_INFINITY };

        double d = 0.0;

        final double threshold1 = (200.0 / screenToPixel);
        final double threshold2 = (100.0 / screenToPixel);

        long tAccum = 0;
        double q1, q2, p1, p2;
        DecimalFormat df = new DecimalFormat("0.#");
        for (int n = 0; n < valsArr.length; n++) {

            // Distance approximation between last label point
            // and current point

            // Absolute value logic inlined for performance
            q1 = lastPoint[0] - valsArr[n][0];
            q2 = lastPoint[1] - valsArr[n][1];
            q1 = (q1 <= 0.0D) ? 0.0D - q1 : q1;
            q2 = (q2 <= 0.0D) ? 0.0D - q2 : q2;

            d = q1 + q2;

            // If distance has been enough, add a label
            if (d > (threshold1)
            /* || (labeledAtLeastOnce == false && n == valsArr.length - 1) */) {
                // Search for any labels that are too close
                // to the current one
                boolean tooClose = false;
                p1 = valsArr[n][0];
                p2 = valsArr[n][1];
                
                q1 = 0;
                q2 = 0;
                for (double[] test : labelPoints) {
                    // Distance approximation between each label
                    // point and current point
                    // Absolute value logic inlined for performance
                    q1 = test[0] - p1;
                    q2 = test[1] - p2;
                    q1 = (q1 <= 0.0D) ? -1.0D * q1 : q1;
                    q2 = (q2 <= 0.0D) ? -1.0D * q2 : q2;
                    d = q1 + q2;
                    if (d < threshold2) {
                        tooClose = true;
                        break;
                    }
                }
                if (!tooClose
                /* || (labeledAtLeastOnce == false && n == valsArr.length - 1) */) {
                    long t0 = System.currentTimeMillis();
//                    if (contourValue >= 0) {
//                        contourGroup.posValueShape.addLabel(df
//                                .format(contourValue), valsArr[n]);
//                    } else {
                        contourGroup.negValueShape.addLabel(df
                                .format(contourValue), valsArr[n]);
//                    }

                    labelPoints.add(valsArr[n]);
                    lastPoint = valsArr[n];
                    tAccum += (System.currentTimeMillis() - t0);
                }
            }
        }

    }

    
    private static double[][] toScreen(Coordinate[] coords, MathTransform xform, int minX, int minY) {
        double[][] out = new double[coords.length][3];

        for ( int i=0; i< coords.length; i++ ) {
                double[] tmp = new double[2];
                tmp[0]=coords[i].x + minX;
                tmp[1]=coords[i].y + minY;
//                if (tmp[0] > 180) tmp[0] -= 360;
                
                try {
                        xform.transform(tmp, 0, out[i], 0, 1);
                } catch (TransformException e) {
                        // TODO Auto-generated catch block
                      //  e.printStackTrace();
                	return null;
                }
        }

        return out;
    }

    private static double[][] toScreenSubtract360(Coordinate[] coords, MathTransform xform,MathTransform xform1, int minX, int minY) {
//        Coordinate[] out = new Coordinate[coords.length];
    	double[][] out = new double[coords.length][3];
        double[] tmpout = new double[3];
        
        for ( int i=0; i< coords.length; i++ ) {
                double[] tmp = new double[2];
                tmp[0]=coords[i].x + minX;
                tmp[1]=coords[i].y + minY;
//                if (tmp[0] > 180.0) tmp[0] -= 360.0;
                
                try {
                        xform.transform(tmp, 0, tmpout, 0, 1);
                } catch (TransformException e) {
                        // TODO Auto-generated catch block
                      //  e.printStackTrace();
                	return null;
                }
                tmpout[0] -= 360.0;
                try {
                    xform1.transform(tmpout, 0, out[i], 0, 1);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                  //  e.printStackTrace();
                	return null;
                }
        }

        return out;
    }
    
    private static LineString toScreenLS(Coordinate[] coords, MathTransform xform, int minX, int minY) {

        GeometryFactory gf = new GeometryFactory();
        Coordinate[] out = new Coordinate[coords.length];
        double[] tmpout = new double[3];

                for ( int i=0; i< coords.length; i++ ) {
                        double[] tmp = new double[2];
                        tmp[0]=coords[i].x + minX;
                        tmp[1]=coords[i].y + minY;
//                        if (tmp[0] > 180) tmp[0] -= 360;
                        
                        try {
                                xform.transform(tmp, 0, tmpout, 0, 1);
                        } catch (TransformException e) {
                                // TODO Auto-generated catch block
//                                e.printStackTrace();
                        	return null;
                        }
                        out[i] = new Coordinate( tmpout[0], tmpout[1] );
                }

                return gf.createLineString(out);
        }

    private static LineString toScreenLSSubtract360(Coordinate[] coords, MathTransform xform, MathTransform xform1,int minX, int minY) {

        GeometryFactory gf = new GeometryFactory();
        Coordinate[] out = new Coordinate[coords.length];
        double[] tmpout = new double[3];
        double[] tmpout1 = new double[3];

                for ( int i=0; i< coords.length; i++ ) {
                        double[] tmp = new double[2];
                        tmp[0]=coords[i].x + minX;
                        tmp[1]=coords[i].y + minY;
//                        if (tmp[0] > 180) tmp[0] -= 360;
                        
                        try {
                                xform.transform(tmp, 0, tmpout, 0, 1);
                        } catch (TransformException e) {
                                // TODO Auto-generated catch block
//                                e.printStackTrace();
                        	return null;
                        }
                        tmpout[0] -= 360.0;
                        try {
                            xform1.transform(tmpout, 0, tmpout1, 0, 1);
                        } catch (TransformException e) {
                            // TODO Auto-generated catch block
//                            e.printStackTrace();
                        	return null;
                        }
                        out[i] = new Coordinate( tmpout1[0], tmpout1[1] );
                }

                return gf.createLineString(out);
    }
    
    private static Geometry polyToLine(Polygon poly) {
    	GeometryFactory gf = new GeometryFactory();

    	if ( poly.getNumInteriorRing() == 0 ) return poly;

    	poly.normalize();
    	LineString outerPoly = poly.getExteriorRing();

    	/*
    	 * sort interior rings
    	 */
    	TreeMap<Coordinate,LineString> orderedHoles =  new TreeMap<Coordinate,LineString>();
    	for ( int i=0; i < poly.getNumInteriorRing(); i++ ) {
    		LineString hole = poly.getInteriorRingN(i);
    		//if ( hole.getArea() == 8.0 ) System.out.println("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFound");
    		Coordinate min = CoordinateArrays.minCoordinate( hole.getCoordinates() );
    		orderedHoles.put( min, hole);
    	}

    	for ( Coordinate leftmost : orderedHoles.keySet() ) {
    		CoordinateList clist = new CoordinateList();
    		LineString hole = orderedHoles.get(leftmost);
    		//Coordinate[] connector = DistanceOp.closestPoints( outerPoly, hole);

    		Coordinate testCoord =  new Coordinate( 0, leftmost.y);
    		//      LineString testSegment = gf.createLineString( new Coordinate[] { leftmost, testCoord } );
    		LineSegment testSegment = new LineSegment( leftmost, testCoord);

    		Coordinate max = findSegments(outerPoly, leftmost.y, testSegment);
    		//      System.out.println("MAX INTX = "+max);
    		Coordinate[] connector = new Coordinate[] { max, leftmost };

    		LocationIndexedLine outerLil = new LocationIndexedLine(outerPoly);
    		LinearLocation outerLoc= outerLil.indexOf( connector[0] );
    		LocationIndexedLine innerLil = new LocationIndexedLine(hole);
    		LinearLocation innerLoc= innerLil.indexOf( connector[1] );

    		clist.add( outerLil.extractLine( outerLil.getStartIndex(), outerLoc).getCoordinates(), true );

    		clist.add( innerLil.extractLine(innerLoc, innerLil.getEndIndex()).getCoordinates(), true);
    		clist.add( innerLil.extractLine( innerLil.getStartIndex(), innerLoc).getCoordinates(), true);

    		clist.add( outerLil.extractLine( outerLoc, outerLil.getEndIndex() ).getCoordinates(), true );

    		outerPoly = gf.createLineString(clist.toCoordinateArray());

    	}

    	return outerPoly;
    	//return ls.getSequencedLineStrings();
    }

    private static Coordinate findSegments(LineString outerPoly, double y, LineSegment seg) {

        //GeometryFactory gf = new GeometryFactory();
        //List<Geometry> geoms = new ArrayList<Geometry>();
        Coordinate max = new Coordinate(0,0);
        //Geometry testGeom;

        Coordinate[] coords = outerPoly.getCoordinates();
        for ( int i=0; i<coords.length-1; i++ ) {
                Coordinate intx = null;
                if ( ((y <= coords[i].y) && (y >= coords[i+1].y)) ||  ((y >= coords[i].y) && (y <= coords[i+1].y)) )  {
                        //Geometry temp = gf.createLineString(new Coordinate[] {coords[1], coords[i+1]} );
                        LineSegment temp = new LineSegment( coords[i], coords[i+1]);
                        intx = seg.intersection(temp);
                }
                //else if ( y == coords[i].y ) {
                //      intx = coords[i];
                //}

                if ( intx != null ) {
                        if ( max.compareTo( intx ) == -1 ) max = intx;
                }

                //      testGeom = seg.intersection(temp);
                //      for ( int j=0; j < testGeom.getNumGeometries(); j++ ) {
                //              Geometry g = testGeom.getGeometryN(j);
                //              if ( max.compareTo( g.getCoordinate() ) == -1 ) max = g.getCoordinate();
                //      }
                //}

        }

        return max;
    }
    
    private static boolean isWorld0 (IMapDescriptor descriptor) {
    	MapProjection worldProjection = CRS.getMapProjection(descriptor
                .getCRS());
        if (worldProjection != null) {
            ParameterValueGroup group = worldProjection.getParameterValues();
            double centralMeridian = group.parameter(
                    AbstractProvider.CENTRAL_MERIDIAN.getName().getCode())
                    .doubleValue();
            if ( centralMeridian == 0.0 ) return true;
        }	 
    	return false;
    }
    
    private static List<Double> contourReduce ( List<Double> contour1, List<Double> contour2){
    	List<Double> tmp = new ArrayList<Double>();
    	if ( contour2 != null ) {
    		for ( Double d2 : contour2 ) {
    			boolean found = false;
    			for ( Double d1 : contour1 ) {
    				if ( Double.compare(d1, d2) == 0 ) {
    					found = true;
    					break;
    				}
    			}
    			if ( ! found ) {
    				tmp.add(d2);
    			}
    		}
    	}
    	return tmp;
    }
    
    private void initContourGroup (IGraphicsTarget target,
             ContourGroup contourGp) {
    	contourGroup = new ContourGroup();
        contourGroup.lastDensity = currentDensity;

        contourGroup.posValueShape = target.createWireframeShape(false,
                descriptor);
        contourGroup.negValueShape = target.createWireframeShape(false,
                descriptor);
        contourGroup.fillShapes = target.createShadedShape(false, descriptor, true);

        contourGroup.zoomLevel = 1.0 / Math.pow(2.0, level);
        
        contourGroup.cvalues = new ArrayList<Double>();
        
        contourGroup.fvalues = new ArrayList<Double>();
        
        contourGroup.data = new HashMap< String, Geometry>();
        
        contourGroup.grid = null;
        
        if ( contourGp != null ) {
        	if ( contourGp.cvalues != null && contourGp.cvalues.size() > 0 ) {
        		contourGroup.cvalues.addAll(contourGp.cvalues);
        	}
        	if ( contourGp.fvalues != null && contourGp.fvalues.size() > 0 ) {
        		contourGroup.fvalues.addAll(contourGp.fvalues);
        	}
        	if ( contourGp.data != null && contourGp.data.size() > 0 ) {
        		contourGroup.data.putAll(contourGp.data);
        	}
        	if ( contourGp.grid != null )
        		contourGroup.grid = contourGp.grid;
        }
        
        contourGroup.lastUsedPixelExtent = (PixelExtent) extent.clone();
        contourGroup.lastUsedPixelExtent.getEnvelope().expandBy(
                contourGroup.lastUsedPixelExtent.getWidth() * .25,
                contourGroup.lastUsedPixelExtent.getHeight() * .25);
    }
    
    private boolean initMathTransform ( GeneralGridGeometry imageGridGeometry,
            GeneralGridGeometry mapGridGeometry) {
        try {
        	DefaultMathTransformFactory factory = new DefaultMathTransformFactory();

            CoordinateReferenceSystem rastCrs = imageGridGeometry
                    .getCoordinateReferenceSystem();
            CoordinateReferenceSystem mapCrs = mapGridGeometry
                    .getCoordinateReferenceSystem();
            
            MathTransform rastGridToCrs = imageGridGeometry
                    .getGridToCRS(PixelInCell.CELL_CENTER);
            MathTransform mapCrsToGrid = mapGridGeometry.getGridToCRS(
                    PixelInCell.CELL_CORNER).inverse();

            MathTransform rastCrsToLatLon = MapUtil
                    .getTransformToLatLon(rastCrs);

            MathTransform rastCrsToWorldGrid = MapUtil
            .getTransformFromLatLon(mapCrs);
            MathTransform crs2crs = CRSCache.getInstance().findMathTransform(
                    rastCrs, mapCrs);

           rastPosToWorldGrid = factory
            .createConcatenatedTransform(
                    factory.createConcatenatedTransform(rastGridToCrs,
                            crs2crs), mapCrsToGrid);
            
            rastPosToLatLon = factory.createConcatenatedTransform(
                    rastGridToCrs, rastCrsToLatLon);
            rastPosLatLonToWorldGrid = factory.createConcatenatedTransform(
            		rastCrsToWorldGrid,mapCrsToGrid);
        } catch (Exception e) {
//            throw new VizException("Error building Transforms", e);
            logger.error("Error building Transforms:" + e);
            return false;
        }
        return true;
    }
    
    private void initZoomIndex () {
        zoomLevelIndex = level+1;//(int)(zoom / 2) + 1; // To be adjusted
		if (zoomLevelIndex < 1) zoomLevelIndex = 1;
		int maxZoomLevel = 5;
		String cint = attr.getCint();
		if (cint != null) maxZoomLevel = cint.trim().split(">").length;
		if (zoomLevelIndex > maxZoomLevel ) zoomLevelIndex = maxZoomLevel;
    }
    
    private List<Double> calcCintValue () {
    	List<Double> cvalues = null;
    	if (type.trim().toUpperCase().contains("C")) {
    		cvalues =CINT.parseCINT(cint, zoomLevelIndex, cntrData.getMinValue(), cntrData.getMaxValue());
    	}
//    	if ( cvalues != null ) {
//    		System.out.println ("******after CINT.parseCINT("+cint+").cvalues:"+ cvalues.toString());
//    		System.out.println ("******cgen.getMinValue():" + cgen.getMinValue() + " cgen.getMaxValue():"+cgen.getMaxValue());
//    	}
    	if ( contourGroup.cvalues.size() == 0 &&  cvalues != null ) {
    		contourGroup.cvalues.addAll(cvalues);
    	}
    	else if (contourGroup.cvalues.size() > 0 ) {
    		if ( cvalues != null  ) {
    			List<Double> tmp = new ArrayList<Double>(cvalues);
    			cvalues = contourReduce (contourGroup.cvalues, cvalues);
    			contourGroup.cvalues.clear();
    			contourGroup.cvalues.addAll(tmp);
    		}
    		else {
    			contourGroup.cvalues.clear();
    		}
    	}
    	return cvalues;
    }
    
    private List<Double> calcFintValue () {
    	List<Double> fvalues = null;
    	if (type.trim().toUpperCase().contains("F")) {
    		if ( !(fint.equalsIgnoreCase(cint)) ) {
    			fvalues = FINT.parseFINT(fint, zoomLevelIndex, cntrData.minValue, cntrData.getMaxValue());
    		}
    		else if ( contourGroup.cvalues != null ){
    			fvalues = contourGroup.cvalues;
    		}           	
    	}
    	if ( contourGroup.fvalues.size() == 0 && fvalues != null){
    		contourGroup.fvalues.addAll(fvalues);
    	}
    	else if ( contourGroup.fvalues.size() > 0 ) {
    		if ( fvalues != null ){
    			List<Double> tmp = new ArrayList<Double>(fvalues);
    			fvalues = contourReduce (contourGroup.fvalues, fvalues);
    			contourGroup.fvalues.clear();
    			contourGroup.fvalues.addAll(tmp);
    		}
    		else {
    			contourGroup.fvalues.clear();
    		}
    	}
    	return fvalues;
    }
    
    private void combineCintAndFillValues () {
    	if (cvalues != null && cvalues.size() > 0) svalues = new HashSet<Double>(cvalues);
        if (fvalues != null && fvalues.size() > 0) {
        	if (svalues == null) 
        		svalues = new HashSet<Double>(fvalues);
        	else
        		svalues.addAll(fvalues);
        }	
    }
    
    private void createContourLines () {
    	
    	long total_labeling_time = 0;
    	long t2 = System.currentTimeMillis();
    	if (type.trim().toUpperCase().contains("C") && contourGroup.cvalues.size() > 0) {
    		int labelFreq = 1;
    		String[] tempLineStrs = attr.getLine().split("/");
    		List<Integer> labelValues = null;
    		if (tempLineStrs.length >= 4) {
    			if (tempLineStrs[3].trim().contains(";")) {
    				LineDataStringParser lineAttr = new LineDataStringParser(attr.getLine());
    				labelValues = lineAttr.getInstanceOfLineBuilder().getLineLabelPresentList();
    			}
    			else {
    				labelFreq = Math.abs(Integer.parseInt(tempLineStrs[3].trim()));
    			}
    		}
    	
    	
    		int n = 0,minX=0,minY=0;

    		for ( Double cval : contourGroup.cvalues ) {
    			float fval = (float) (cval * 1.0f);
    			boolean toLabel = false;
    		
    			// Label frequency
    			if (labelValues != null) {
    				for(Integer value : labelValues) {
    					if (value == Math.rint(fval)) {
    						toLabel = true;
    						break;
    					}
    				}
    			}
    			else {
    				if (labelFreq == 0) 
    					toLabel = false;
    				else
    					toLabel = (n % labelFreq == 0) ? true : false;
    			}
    		
    		
//    			Geometry g = cgen.getContours(fval);
    			Geometry g = contourGroup.data.get(cval.toString());
    			if ( g == null ) continue;
//    			contourMap.put( cval, g);
    			double[][] screen1 = null;
    			for ( int i=0; i < g.getNumGeometries(); i++ ) {
    				Geometry gn = g.getGeometryN(i);
    				double[][] screen = toScreen( gn.getCoordinates(), rastPosToWorldGrid, minX, minY );
    			
    				if ( screen != null )
    					contourGroup.negValueShape.addLineSegment(screen);
                	if ( isWorld0 ) {
                		screen1 = toScreenSubtract360( gn.getCoordinates(), rastPosToLatLon,rastPosLatLonToWorldGrid, minX, minY );
                		if ( screen1 != null )
                			contourGroup.negValueShape.addLineSegment(screen1);
                	}
    				if (toLabel)  {
    					long tl0 = System.currentTimeMillis();
//    				prepareLabel(contourGroup, zoom, fval,
//    						labelPoints, screen);
    					if ( screen != null )
    						createContourLabel(extent, contourGroup, fval, screen);
    					if ( isWorld0 && screen1 != null) {
    						createContourLabel(extent, contourGroup, fval, screen1);
    					}
    					long tl1 = System.currentTimeMillis();
    					total_labeling_time += (tl1-tl0);
    				}		
    			}
    		
    			n++;
    		}
    	}
    	long t3 = System.currentTimeMillis();
    	logger.debug("===Creating label wireframes for ("+name+") took: " + total_labeling_time);
    	if ( ncgribLogger.enableCntrLogs() )
        	logger.info("===Creating contour line wireframes for ("+name+")took: " + (t3 - t2 ));
//        	System.out.println("Creating contour line wireframes took: " + (t3 - t2 - total_labeling_time));
    }
    
    private void createColorFills () {
    	
    	long t3 = System.currentTimeMillis();
    	if (type.trim().toUpperCase().contains("F") && contourGroup.fvalues.size() > 0) {
    		try {
    		
    			// Prepare colors for color fills
    			List<Integer> fillColorsIndex = new ArrayList<Integer>();
    			if (fline == null ||  fline.trim().length() < 1) {
    				for(int i = 0; i < contourGroup.fvalues.size()+2; i++) {
    					if (i <= 30)
    						fillColorsIndex.add(i + 1);
    					else
    						fillColorsIndex.add(30);
    				}
    			} else {
    				FLine flineInfo = new FLine(fline.trim());
    				fillColorsIndex = flineInfo.getFillColorList();
			
    				/*
    				 * Apply last color if not enough input color.
    				 */
    				if (contourGroup.fvalues != null && fillColorsIndex.size() < (contourGroup.fvalues.size()+1)) {
    					for (int i = fillColorsIndex.size(); i < contourGroup.fvalues.size()+2; i++) {
    						fillColorsIndex.add(i);
    					}
    				}
    			}

    			int minX=0,minY=0;
    			long t11 = System.currentTimeMillis();
    			FillGenerator fgen = new FillGenerator(contourGroup.grid);
    			long t12 = System.currentTimeMillis();
    			logger.debug(" create FillGenerator took:" + (t12-t11));
    			for ( Double cval : contourGroup.fvalues ) {
    				float fval = (float) (cval * 1.0f);
    				Geometry g = contourGroup.data.get(cval.toString());
    				if ( g == null ) continue;
    				fgen.addContours(fval, g);
    			}
    			t11 = System.currentTimeMillis();
    			logger.debug(" add Contour took:" + (t11-t12));
    			// Add color fill to contourGroup
    			for (int n=0; n <= contourGroup.fvalues.size(); n++ ) {
    				if (fillColorsIndex.get(n) <= 0 || fillColorsIndex.get(n) >= 32) continue;
			
    				RGB color = GempakColor.convertToRGB(fillColorsIndex.get(n));
    				Geometry fillPolys = null;
			
    				int index = (n < contourGroup.fvalues.size()) ? n : (n-1);
    				float fval = (float)(contourGroup.fvalues.get(index) * 1.0f);
			
    				try {
    					if (n == 0) {
    						fillPolys = fgen.fillLessThan(fval);
    					} else if (n == contourGroup.fvalues.size()) {
    						fillPolys = fgen.fillGreaterThan(fval);
    					} else {
    						float fval1 = (float)(contourGroup.fvalues.get(n-1) * 1.0f);
    						float fval2 = (float)(contourGroup.fvalues.get(n) * 1.0f);
    						fillPolys = fgen.fillBetween( fval1, fval2 );
    					}
    					for (int j=0; j<fillPolys.getNumGeometries(); j++ ) {
    						Geometry g = fillPolys.getGeometryN(j);
    						if ( g instanceof Polygon ) g = polyToLine( (Polygon)g );
    						LineString ls = toScreenLS( g.getCoordinates(), rastPosToWorldGrid, minX, minY);
    						if ( ls != null )
    							contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, color);
    						if ( isWorld0 ) {
    							ls = toScreenLSSubtract360( g.getCoordinates(), rastPosToLatLon,rastPosLatLonToWorldGrid, minX, minY);
    							if ( ls != null )
    								contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, color);
    						}
    					}
    				} catch (FillException e) {
//						e.printStackTrace();
    				}
    			}
    			t12 = System.currentTimeMillis();
    			logger.debug(" loop fvalues took:" + (t12-t11));
//				System.out.println("Creating color fills took : " + (t4-t3));
		
    		} catch (Exception e) {
    			logger.debug("Could not create FILL Polygons.");
//				e.printStackTrace();
    			return;
    		}
    	}
    	long t4 = System.currentTimeMillis();
    	if ( ncgribLogger.enableCntrLogs() )
    		logger.info("===Creating color fills for ("+name+") took : " + (t4-t3));
    }
    
    private void createStreamLines () {
    	// Step 1: Get the actual data

        float[] uW = null;
        float[] vW = null;
        long[] sz = records.getSizes();
       
        uW = ((NcFloatDataRecord) records).getXdata();
        vW = ((NcFloatDataRecord) records).getYdata();
        
//        Step 2: Determine the subgrid, if any
        int minX=0,minY=0;
        int maxX = (int)sz[0] - 1;
        int maxY = (int)sz[1] - 1;
        int szX = (maxX - minX) + 1;
        int szY = (maxY - minY) + 1;
        
        int totalSz = szX * szY;
        if (totalSz <= 0) {
        	isCntrsCreated = false;
            return ;
        }
        int x = (int) sz[0];
        
        float[] adjustedUw = new float[totalSz];
        float[] adjustedVw = new float[totalSz];

        int n = 0;

        for (int j = 0; j < szY; j++) {
            for (int i = 0; i < szX; i++) {

                adjustedUw[n] = uW[(x * (j + minY)) + (i + minX)];
                adjustedVw[n] = vW[(x * (j + minY)) + (i + minX)];
                n++;
            }
        }

        Util.flipVert(adjustedUw, szY, szX);
        Util.flipVert(adjustedVw, szY, szX);

        int arrSz = Math.max(10 * adjustedUw.length, uW.length);
        uW = null;
        vW = null;
        
        
        int[] work = new int[arrSz];
        float[] xPoints = new float[arrSz];
        float[] yPoints = new float[arrSz];
        int[] numPoints = new int[1];

        // Use ported legacy code to determine contour interval
//        contourGroup.lastDensity = currentDensity;
        
        double spadiv = 1 * contourGroup.lastDensity * 500 / 25;
        
        double minSpacing = 1.0 / spadiv;
        double maxSpacing = 3.0 / spadiv;

        float minspc = 0;
        float maxspc = 0;

        if (minSpacing > 1) {
            minspc = (float) Math.sqrt(minSpacing);
        }
        if (minspc < 0.1) {
            minspc = 0.1f;
        }
        if (maxSpacing > 1) {
            maxspc = (float) Math.sqrt(maxSpacing);
        }
        if (maxspc < 0.25) {
            maxspc = 0.25f;
        }
        
        /*
         * Fix arrow size by M. Li
         */
        float arrowSize = (float) (0.4f / Math.sqrt(zoom));
        if (arrowSize > 0.4) arrowSize = 0.4f;
        
        Controller.strmpak(adjustedUw, adjustedVw, work, szX, szX, szY,
                arrowSize, xPoints, yPoints, numPoints, minspc, maxspc,
                -1000000f, -999998f);
        
//        long t1 = System.currentTimeMillis();
//        System.out.println("Streamline Contouring took: " + (t1 - t0));

        List<double[]> vals = new ArrayList<double[]>();

        long tAccum = 0;
        try {
            for (int i = 0; i < numPoints[0] && i < xPoints.length; i++) {
                if (xPoints[i] == -99999.0) {
                    if (vals.size() > 0) {
                        double[][] valsArr = vals.toArray(new double[vals
                                .size()][2]);
                        contourGroup.posValueShape.addLineSegment(valsArr);
                        vals.clear();
                    }
                } else {
                    double[] out = new double[2];
                    try {
                        long tZ0 = System.currentTimeMillis();
                        
                        float f = maxX - xPoints[i];
                        if (f > 180) f = f - 360;
                        rastPosToWorldGrid.transform(new double[] {
                                f, yPoints[i] + minY }, 0,
                                out, 0, 1);
                        
//                        rastPosToWorldGrid.transform(new double[] {
//                                maxX - xPoints[i], yPoints[i] + minY }, 0,
//                                out, 0, 1);
                        long tZ1 = System.currentTimeMillis();
                        tAccum += (tZ1 - tZ0);
                    } catch (TransformException e) {
                        // TODO Auto-generated catch block
//                        e.printStackTrace();
                    }
                    vals.add(out);
                }
            }

//            System.out.println("streamline transformation time: " + tAccum);

            if (vals.size() > 0) {

                double[][] valsArr = vals
                        .toArray(new double[vals.size()][2]);
                contourGroup.posValueShape.addLineSegment(valsArr);
                vals.clear();
            }
        } catch (Throwable e) {
//            throw new VizException("Error postprocessing contours", e);
            logger.error("Error postprocessing contours:" + e);
            isCntrsCreated = false;
            return;
        }      
    }
    
    public void genContour () {
    	
    	ContourCalculationReentrantLock.getReentrantLock();
//    	synchronized (ContourSupport.class) {
    		List<Double> allvalues = new ArrayList<Double>(svalues);   
    		Collections.sort(allvalues);

    		long t1a = System.currentTimeMillis();
    		ContourGenerator cgen = new ContourGenerator( cntrData.getData(), cntrData.getX(), cntrData.getY());  
    		long t1b = System.currentTimeMillis();
    		logger.debug("Creating contour values took: " + (t1b-t1a));
    		cgen.setContourValues( allvalues );

    		long t1c = System.currentTimeMillis();
    		logger.debug("ContourGenerator.setContourValues(allvalues) took: " + (t1c-t1b));
//    		System.out.println("ContourGenerator init took:" + (t1c-t0));
    
    		try {
    			cgen.generateContours();
    		} catch (ContourException e1) {
    			// TODO Auto-generated catch block
//    			e1.printStackTrace();
    			cgen.dispose();
    			isCntrsCreated = false;
    			ContourCalculationReentrantLock.releaseReentrantLock();
    			return;
    		}
    
     
     
    		long t2 = System.currentTimeMillis();
    		if ( ncgribLogger.enableCntrLogs() )
    			logger.info("===ContourGenerator.generateContours() for ("+name+") took: " + (t2-t1a));
	
//    		System.out.println("Contour Computation took: " + (t2-t1c));
  
    		logger.debug("Total generating contour line values took: " + (t2-t1a));
    		if ( cvalues != null ) {
    			for ( Double cval : cvalues ) {
    				float fval = (float) (cval * 1.0f);
    				contourGroup.data.put(cval.toString(), cgen.getContours(fval));
    			}
    		}
    		if ( fvalues != null ) {
    			for ( Double cval : fvalues ) {
    				float fval = (float) (cval * 1.0f);
    				contourGroup.data.put(cval.toString(), cgen.getContours(fval));
    			}
    		}
    		
    		if ( contourGroup.grid == null ) {
				contourGroup.grid = cgen.getEdges();
			}
    		cgen.dispose();
    		ContourCalculationReentrantLock.releaseReentrantLock();
//    	}
    }
    
    public ContourGroup getContours() {
    	if ( ! isCntrsCreated ) return null;
    	return contourGroup;
    }
}   
