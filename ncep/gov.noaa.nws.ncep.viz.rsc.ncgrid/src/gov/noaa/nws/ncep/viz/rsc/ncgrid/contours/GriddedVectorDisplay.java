package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;


import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.edex.common.dataRecords.NcFloatDataRecord;
import gov.noaa.nws.ncep.viz.common.ui.color.GempakColor;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.NcgribLogger;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc.NcgridResourceData;

import java.nio.FloatBuffer;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Performs same functions as the original GriddedVectorDisplay using wireframe
 * shapes instead of svg for much faster performance. This is still slightly
 * experimental but seems to work well. It should also have the drawing code
 * extracted to a class similar to PointWindDisplay so wireframe shape barbs and
 * arrows can be used elsewhere.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2010            bsteffen     Initial creation
 * Nov 22, 2010			   M. Li		modified from RTS for NCGRID
 * Nov 02, 2011            X. Guo       Updated
 * Feb 06, 2012  #538      Q. Zhou      Changed density to filter. 
 * Feb 15, 2012  #539      Q. Zhou      Change barb tail direction on south hemisphere
 * Mar 01, 2012            X. Guo       Added isDirectional and contourAttributes
 * 										 to handle vector type changes
 * Apr 03, 2012            X. Guo       Added createWireFrame
 * May 23, 2012            X. Guo       Loaded ncgrib logger
 * Apr 26, 2013			   B. Yin		Don't plot missing values.
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GriddedVectorDisplay extends AbstractGriddedDisplay<Coordinate> {

	private static NcepLogger logger = NcepLoggerManager.getNcepLogger(GriddedVectorDisplay.class);
    private final FloatBuffer magnitude;

    private final FloatBuffer direction;

    private final ISpatialObject gridLocation;

    private final int SIZE = 64;
    
    private float lineWidth = 1;

    private LineStyle lineStyle = LineStyle.SOLID;

    private double scale = 0.6;

    private IExtent lastExtent;

    private IWireframeShape lastShape;

    private DisplayType displayType;

    private GeodeticCalculator gc;

    private NcFloatDataRecord data;
    
    private ContourAttributes contourAttributes;
    
    private Coordinate latLon;
    
    private boolean isDirectional;
    
    private static NcgribLogger ncgribLogger;
    
    public GriddedVectorDisplay(NcFloatDataRecord rec,
    		DisplayType displayType, boolean directional, IMapDescriptor descriptor, 
            ISpatialObject gridLocation, ContourAttributes attrs) {
        super(descriptor, MapUtil.getGridGeometry(gridLocation),gridLocation.getNx(),gridLocation.getNy());
        long t1 = System.currentTimeMillis();
        this.data = rec;
        this.contourAttributes = attrs;
        ncgribLogger = NcgribLogger.getInstance();
        
        if (directional) {
        	float[] dir = rec.getXdata();
    		float[] spd = new float[dir.length];
    		for (int i = 0; i < dir.length; i++) {
    			if (dir[i] == -999999.0f) {
    				dir[i] = -999999.0f;
    				spd[i] = -999999.0f;
    			} else {
    				spd[i] = 40;
    			}
    		}
    		this.magnitude     = FloatBuffer.wrap(spd);
    		this.direction = FloatBuffer.wrap(dir);
        }
        else {
        	float[] dirX = rec.getXdata();
    		float[] dirY = rec.getYdata();
    		
        	float [] spd = new float[dirX.length];
    		float [] dir = new float[dirX.length];
    		
    	
    		for (int i = 0; i < spd.length; i++) {
    			if (dirX[i] == -999999.0f || dirY[i] == -999999.0f) {
    				spd[i] = -999999.0f;
    				dir[i] = -999999.0f;
    			}
    			else {
    				spd[i] = (float) Math.hypot(dirX[i], dirY[i]);
    				dir[i] = (float) (Math.atan2(dirX[i], dirY[i]) * 180 / Math.PI) + 180;
    			}
    		}
    		
    		this.magnitude     = FloatBuffer.wrap(spd);
    		this.direction = FloatBuffer.wrap(dir);
        }
        long t2 = System.currentTimeMillis();
        logger.debug("GriddedVectorDisplay after check -999999 took:" + (t2-t1));
        this.gridLocation = gridLocation;
        this.displayType = displayType;
        this.gc = new GeodeticCalculator(descriptor.getCRS());
        this.isDirectional = directional;
        
        int colorIndex = 31;
        float sizeFactor = 1;
        lineWidth = 1;
        String windAttr = attrs.getWind();
        if (windAttr != null && !windAttr.isEmpty()) {
        	if (windAttr.contains("bk")) windAttr = windAttr.replace("bk", "");
        	String [] attr = windAttr.trim().split("/");
        	colorIndex = Integer.parseInt(attr[0].trim());
        	if (attr.length >= 2 && !attr[1].trim().isEmpty()) sizeFactor = Float.parseFloat(attr[1].trim());
        	if (attr.length >= 3 && !attr[2].trim().isEmpty()) lineWidth  = Float.parseFloat(attr[2].trim());
        	
        }
        
        setSize(sizeFactor * SIZE);
        setColor(GempakColor.convertToRGB(colorIndex)); 
    }
    

    @Override
    public void paint(NcgridResourceData gridRscData, IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	
    	if (paintProps.isZooming()) {
    		return;
    	}
    	
        if (lastExtent == null
                || !lastExtent.equals(paintProps.getView().getExtent())) {
            disposeImages();
            lastExtent = paintProps.getView().getExtent().clone();
        }
        if (lastShape == null) {
            lastShape = target.createWireframeShape(false, descriptor);
            super.paint(gridRscData, target, paintProps);
            lastShape.compile();
        }
        
        target.drawWireframeShape(lastShape, color, lineWidth, lineStyle);
    }

    public void createWireFrame (NcgridResourceData gridRscData, IGraphicsTarget target, PaintProperties paintProps) {

    	if (lastShape == null) {
    		try {
    			long t1 = System.currentTimeMillis();
    			lastShape = target.createWireframeShape(false, descriptor);
    			super.paint(gridRscData, target, paintProps);
    			lastShape.compile();
    			long t4 = System.currentTimeMillis();
    			if ( ncgribLogger.enableCntrLogs() )
    				logger.info("--GriddedVectorDisplay: create wireframe took:" + (t4-t1));
    		}
    		catch ( VizException e) {
    			lastShape = null;
    		}
    	}
    }
    	   
    protected void paintImage(int x, int y, PaintProperties paintProps, double adjSize) throws VizException {
        int idx = x + y * this.gridDims[0];

//        System.out.println("paintImage idx==="+idx+" x=="+ijcoord.x+"  y====="+ijcoord.y);
        
        if (idx < 0 || idx >= (gridDims[0] * gridDims[1])) return;
        float spd = this.magnitude.get(idx);
        float dir = this.direction.get(idx) - 180;

        if (Float.isNaN(spd) || Float.isNaN(dir)) {
            return;
        }
        
        if (this.isPlotted[idx]) return;
        
        ReferencedCoordinate newrco = new ReferencedCoordinate(
				new Coordinate(x, y),
				this.gridGeometryOfGrid, Type.GRID_CENTER);
        Coordinate plotLoc = null;

        try {
        	plotLoc = newrco.asPixel(this.descriptor
                    .getGridGeometry());
            latLon = newrco.asLatLon();
            
            if (latLon.x > 180 || latLon.x < -180 || latLon.y < -90 || latLon.y > 90) return;
            
            double[] stationLocation = { latLon.x, latLon.y };
            double[] stationPixelLocation = this.descriptor
                    .worldToPixel(stationLocation);

            if (stationPixelLocation != null) {
                stationPixelLocation[1]--;
                double[] newWorldLocation = this.descriptor
                        .pixelToWorld(stationPixelLocation);
                this.gc.setStartingGeographicPoint(stationLocation[0],
                        stationLocation[1]);
                this.gc.setDestinationGeographicPoint(newWorldLocation[0],
                        newWorldLocation[1]);
            }

            dir = dir + (float) MapUtil.rotation(latLon, gridLocation);
            dir -= this.gc.getAzimuth();
        } catch (Exception e) {
            throw new VizException(e);
        }

        dir = (float) Math.toRadians(dir);
        switch (displayType) {
        case ARROW:
            paintArrow(plotLoc, adjSize, spd, dir);
            break;
        case BARB:
            paintBarb(plotLoc, adjSize, spd, dir);
            break;
        case DUALARROW:
            paintDualArrow(plotLoc, adjSize, spd, dir);
            break;
        default:
            throw new VizException("Unsupported disply type: " + displayType);
        }
        
        this.isPlotted[idx] = true;
    }

    private void paintBarb(Coordinate plotLoc, double adjSize, double spd,
            double dir) {
    	//Don't plot missing value
    	if ( spd < 0 ) return;
    	
        if (spd < 2.5) {
            double[][] line = new double[9][2];

            double aa = adjSize * .030;
            double saa = aa * 0.707;

            line[8][0] = line[0][0] = plotLoc.x + aa;
            line[8][1] = line[0][1] = plotLoc.y;
            line[1][0] = plotLoc.x + saa;
            line[1][1] = plotLoc.y + saa;

            line[2][0] = plotLoc.x;
            line[2][1] = plotLoc.y + aa;

            line[3][0] = plotLoc.x - saa;
            line[3][1] = plotLoc.y + saa;

            line[4][0] = plotLoc.x - aa;
            line[4][1] = plotLoc.y;

            line[5][0] = plotLoc.x - saa;
            line[5][1] = plotLoc.y - saa;

            line[6][0] = plotLoc.x;
            line[6][1] = plotLoc.y - aa;

            line[7][0] = plotLoc.x + saa;
            line[7][1] = plotLoc.y - saa;

            lastShape.addLineSegment(line);

            return;
        }

        int speed = (int) (spd + 2.5);
        double staff = adjSize * .4;
        double barb = staff * 0.30;
        double add = staff * 0.105;
        // DIRECTIONS
        double uudd = -spd * Math.sin(dir);
        double vvff = -spd * Math.cos(dir);
        double dix = -uudd / spd;
        double djy = -vvff / spd;
        double dix1 = Math.cos(Math.toRadians(75)) * dix
                + Math.sin(Math.toRadians(75)) * djy;
        double djy1 = (-1) * Math.sin(Math.toRadians(75)) * dix
                + Math.cos(Math.toRadians(75)) * djy;

        // SPEED AND COUNTERS:
        int n50 = speed / 50;
        int calcSpd = speed - 50 * n50;
        int n10 = calcSpd / 10;
        calcSpd = calcSpd - 10 * n10;
        int n5 = calcSpd / 5;
        double sx = ((n50 + n50 + n10 + n5 + 2)) * add;
        staff = Math.max(adjSize * .4, sx);

        // DRAW STAFF
        double ix2 = plotLoc.x;
        double jy2 = plotLoc.y;
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;
        lastShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 } });

        // PLOT LONE HALF-BARB, IF NECESSARY
        if (n50 == 0 && n10 == 0) {
        	if (latLon.y >= 0) {
        		ix2 = ix1 + dix1 * barb / 2.0; //-
        		jy2 = jy1 - djy1 * barb / 2.0; //+
        	}
        	else {
        		ix2 = ix1 - dix1 * barb / 2.0; 
            	jy2 = jy1 + djy1 * barb / 2.0; 
        	}
        	
        	ix1 = ix1 - dix * add;
            jy1 = jy1 + djy * add;
        	lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            return;
        }

        // PLOT FLAGS, IF NECESSARY
        for (int i = 0; i < n50; i++) {
        	if (latLon.y >= 0) {
        		ix2 = ix1 + dix1 * barb; //+
                jy2 = jy1 - djy1 * barb; //-
        	}
        	else {
        		ix2 = ix1 - dix1 * barb; 
        		jy2 = jy1 + djy1 * barb; 
        	}
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            ix1 = ix1 - dix * add * 2; // 2 space
            jy1 = jy1 + djy * add * 2;
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
        }
        
        if (n50 > 0) {
            ix1 = ix1 - dix * add / 2.0;
            jy1 = jy1 + djy * add / 2.0;
        }

        // PLOT BARB, IF NECESSARY
        for (int i = 0; i < n10; i++) {
        	if (latLon.y >= 0) {
        		ix2 = ix1 + dix1 * barb; //+
        		jy2 = jy1 - djy1 * barb;//-
        	}
        	else {
        		ix2 = ix1 - dix1 * barb; 
        		jy2 = jy1 + djy1 * barb; 
        	}           
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
            ix1 = ix1 - dix * add;
            jy1 = jy1 + djy * add;
        }

        // PLOT HALF-BARB, IF NECESSARY
        if (n5 != 0) {
        	if (latLon.y >= 0) {
        		ix2 = ix1 + dix1 * barb / 2.0; //+
        		jy2 = jy1 - djy1 * barb / 2.0; //-
        	}
        	else {
        		ix2 = ix1 - dix1 * barb / 2.0; 
        		jy2 = jy1 + djy1 * barb / 2.0; 
        	}
            lastShape.addLineSegment(new double[][] { { ix2, jy2 },
                    { ix1, jy1 } });
        }
    }

    private void paintDualArrow(Coordinate plotLoc, double adjSize, double spd,
            double dir) {
        if (spd < 4.0) {
            return;
        }
        double staff = 0.0;
        if (this.scale > 0.0) {
            staff = spd * this.scale;
        } else {
            staff = Math.log10(spd * -this.scale) * 10 + 10;
        }

        double barb = 4.0;

        if (staff < barb) {
            return;
        }

        double ratio = adjSize / size;
        staff *= ratio;
        barb *= ratio;

        // DIRECTIONS
        double uudd = -spd * Math.sin(dir);
        double vvff = -spd * Math.cos(dir);
        double dix = uudd / spd;
        double djy = vvff / spd;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;

        // DRAW BODY OF ARROW
        double ix2 = plotLoc.x;
        double jy2 = plotLoc.y;
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;

        double ix3 = ix1 + dix1 * barb;
        double jy3 = jy1 - djy1 * barb;
        double ix4 = ix2 - dix1 * barb;
        double jy4 = jy2 + djy1 * barb;
        lastShape.addLineSegment(new double[][] { { ix4, jy4 }, { ix2, jy2 },
                { ix1, jy1 }, { ix3, jy3 } });

    }

    private void paintArrow(Coordinate plotLoc, double adjSize, double spd,
            double dir) {
        if (spd == 0.0) {
            return;
        }
        double staff = 0.0;
        if (this.scale > 0.0) {
            staff = spd * this.scale;
        } else {
            staff = Math.log10(spd * -this.scale) * 10 + 10;
        }

        double barb = 4.0;

        if (staff < barb) {
            return;
        }

        double ratio = adjSize / size;
        staff *= ratio;
        barb *= ratio;

        // DIRECTIONS
        double uudd = -spd * Math.sin(dir);
        double vvff = -spd * Math.cos(dir);
        double dix = uudd / spd;
        double djy = vvff / spd;
        double dix1 = -dix - djy;
        double djy1 = dix - djy;
        double dix2 = -dix + djy;
        double djy2 = -dix - djy;

        // DRAW BODY OF ARROW
        double ix2 = plotLoc.x;
        double jy2 = plotLoc.y;
        double ix1 = ix2 + dix * staff;
        double jy1 = jy2 - djy * staff;
        lastShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 } });
        // DRAW HEAD OF ARROW.
        ix2 = ix1 + dix1 * barb;
        jy2 = jy1 - djy1 * barb;
        double ix3 = ix1 + dix2 * barb;
        double jy3 = jy1 - djy2 * barb;
        lastShape.addLineSegment(new double[][] { { ix2, jy2 }, { ix1, jy1 },
                { ix3, jy3 } });

    }

    /**
     * 
     * @param color
     */
    public void setScale(double scale) {
        this.scale = scale;
    }

    public void setLineWidth(float lineWidth) {
        this.lineWidth = lineWidth;
    }

    /**
     * @param lineStyle
     */
    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    /**
     * @param filter
     *            the filter to set.  Changed from density.
     */
    public boolean setFilter(double filter) {
        if (super.setFilter(filter)) {
            disposeImages();
            if (this.target != null) {
                this.target.setNeedsRefresh(true);
            }
            return true;
        }
        return false;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public boolean setMagnification(double magnification) {
        if (super.setMagnification(magnification)) {
            disposeImages();
            if (this.target != null) {
                this.target.setNeedsRefresh(true);
            }
            return true;
        }
        return false;
    }

    /**
     * @return the magnitude
     */
    public FloatBuffer getMagnitude() {
        return magnitude;
    }

    protected void disposeImages() {
        if (lastShape != null) {
            lastShape.dispose();
            lastShape = null;
        }
    }

    protected Coordinate createImage(Coordinate coord) throws VizException {
        return coord;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedImageDisplay
     * #getImage(com.raytheon.uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    protected Coordinate getImage(Coordinate coord) {
        return coord;
    }

	public NcFloatDataRecord getData() {
		return data;
	}
    
	public boolean checkAttrsChanged (DisplayType type, boolean dir, String attr) {
		boolean isChanged = false;
		if ( this.displayType != type ||
			this.isDirectional != dir ||
			! this.contourAttributes.getWind().equalsIgnoreCase(attr)) {
			isChanged = true;
		}
		return isChanged;
	}
	
	public boolean isMatch ( ContourAttributes attr) {
		boolean match = false;
		if ( this.contourAttributes == null ) return match;
		if ( this.contourAttributes.getGlevel().equalsIgnoreCase(attr.getGlevel())&&
			 this.contourAttributes.getGvcord().equalsIgnoreCase(attr.getGvcord()) &&
			 this.contourAttributes.getScale().equalsIgnoreCase(attr.getScale()) &&
			 this.contourAttributes.getGdpfun().equalsIgnoreCase(attr.getGdpfun()) ) {
			match = true;
		}
		return match;
	}
}
