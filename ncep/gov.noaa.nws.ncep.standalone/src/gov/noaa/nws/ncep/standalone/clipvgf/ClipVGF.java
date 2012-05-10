/*
 * gov.noaa.nws.ncep.standalone.clipvgf.ClipVGF
 * 
 * Date created (as Jan 12, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.clipvgf;

import static java.lang.Math.abs;
import static java.lang.Math.atan2;
import static java.lang.Math.cos;
import static java.lang.Math.pow;
import static java.lang.Math.sin;
import static java.lang.Math.sqrt;
import static java.lang.Math.toDegrees;
import static java.lang.Math.toRadians;
import gov.noaa.nws.ncep.standalone.joinvgf.JoinVGF;
import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.KinkLine;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.geotools.geometry.jts.JTS;
//import org.geotools.referencing.GeodeticCalculator;
//import org.geotools.referencing.datum.DefaultEllipsoid;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.exception.VizServerSideException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;
import com.vividsolutions.jts.io.WKBReader;

/**
 * This is a standalone clipping application, replacement for clipvgf.
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * 06/11		213			Q. Zhou		Modified convertArcToLine() for ellipse
 * 										Due to refactoring, separate multipointLine to Line, Gfa, Circle...
 * 										Added code to change arc line to arc
 * </pre>
 * @author mlaryukhin
 */
public class ClipVGF extends Thread {

	/**
	 * Input parameter, defined by user; true to clip all the elements within the bounds, false -
	 * outside.
	 */
	public static boolean	keep			= true;

	/** Input parameters. */
	public String[]			args;

	/**
	 * The table alias to be looked up in CONFIG.CLO table, which corresponds to the table name in
	 * bounds schema.
	 */
	public String			boundsTableAlias;

	/** The columnd name which is extracted from arg1[1]. */
	private String			columnName;

	/** The column value. To search for KZNY_8 it is sufficient to use KZNY. */
	private String			columnValue;

	/** EDEX http server location. */
	public static String	httpServer;

	/** Database name, currently defaulted to "ncep". */
	public String			database		= "ncep";

	/** Schema name, currently defaulted to "bounds". */
	public String			schema			= "bounds";

	/** Help file where the program description is stored. */
	public static String	HELP_FILE_NAME	= "clipvgf.hlp";

	/** Input file name. */
	private String			inputName;

	/** Output file name to store results. */
	private String			outputName;

	/** Factory */
	private GeometryFactory	geometryFactory	= new GeometryFactory();

	/** Bounds shape is to be converted to this polygon. */
	private Polygon			polygon;

	/** Tolerance for coordinates to be treated as the same /close points. */
	private final double	PRECISION		= 0.000000001;

	/** An instance of JoinVGF to be used for joining touching parts. */
	private JoinVGF			joinvgf;
	
	static{
		// no logger needed in apache classes
		Logger.getRootLogger().setLevel(Level.OFF);
	}

	/**
	 * Constructor.
	 * 
	 * @param args
	 *            <code>String[]</code> input parameters
	 * @throws VizException
	 */
	public ClipVGF(String[] args) throws VizException {
		this.args = args;
		checkParameters();
		createPolygon();
	}

	/**
	 * Open a product file to replace or append to the current product list.
	 * 
	 * @throws FileNotFoundException
	 */
	private List<Product> openProducts() throws FileNotFoundException {
		Products products = Util.read(inputName);
		return ProductConverter.convert(products);
	}

	/**
	 * Save the products to file.
	 */
	private void saveProducts(List<Product> productList) {
		Products fileProducts = ProductConverter.convert(productList);
		if (outputName != null) {
			/*
			 * Write all products into one file.
			 */
			try {
				Util.write(outputName, fileProducts, Products.class);
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Static method to clip the xml file.
	 * 
	 * @param productList
	 * 
	 * @return
	 * 
	 * @throws FileNotFoundException
	 * 
	 * @throws VizException
	 *             if database fails
	 */
	private void clip(String inputName, String outputName) throws FileNotFoundException, VizException {
		// open
		List<Product> productList = openProducts();

		// clip each Product
		for (Product p : productList) {
			clipProduct(p);
		}
		// save
		saveProducts(productList);
	}

	/**
	 * Clips each individual product.
	 * 
	 * @param p
	 * 
	 * @return
	 */
	private void clipProduct(Product p) {

		for (Layer layer : p.getLayers()) {
			List<AbstractDrawableComponent> before = layer.getDrawables();
			List<AbstractDrawableComponent> after = new ArrayList<AbstractDrawableComponent>();
			boolean changed = false;
			for (AbstractDrawableComponent adc : before) {
				List<AbstractDrawableComponent> c = clipDrawableComponent(adc);
				if (c != null && !c.isEmpty()) {
					after.addAll(c);
				}
				changed = true;
			}
			if (changed) {
				while (getJoinVGF().joinItself(after)) {
				}
				layer.setDrawables(after);
			}
		}
	}

	/**
	 * Getter for JoinVGF instance.
	 * 
	 * @return
	 */
	private JoinVGF getJoinVGF() {
		if (joinvgf == null) {
			joinvgf = new JoinVGF(null);
		}
		return joinvgf;
	}

	/**
	 * Clips DrawableComponent.
	 * 
	 * @param adc
	 * 
	 * @return
	 */
	private List<AbstractDrawableComponent> clipDrawableComponent(AbstractDrawableComponent adc) {
		List<AbstractDrawableComponent> ret = null;
		if (adc instanceof SinglePointElement) {
			ret = clipSinglePointElement(adc);
		} 
		else if (adc instanceof MultiPointElement) {

			// circle is a special case, convert it to Line (the same way it is done in old clipvgf)
			// But cave has Arc, so add the line points to Arc to make a special arc. 
			// After clipping, convert it back to Arc with right angles and points.
			AbstractDrawableComponent adcOrig = null;
			if (adc instanceof Arc) {
				// we cannot use polymorphism here, we need pure LIne object to
				// be able to draw points
				adcOrig = adc.copy();
				adc = convertArcToLine((Arc) adc);
			}
			
//			if (((MultiPointElement) adc).isClosedLine()) {
//				adc.getPoints().add(adc.getPoints().get(0));
//			}
			// MultiPointElement include Line, Kinkline, Gfa,and Jet...Jet is not clised
			else if (adc instanceof Line) {
				if (((Line) adc).isClosedLine()) 
					adc.getPoints().add(adc.getPoints().get(0));
			}
			else if (adc instanceof Gfa) {
				if (((Gfa) adc).isClosedLine()) 
					adc.getPoints().add(adc.getPoints().get(0));
			}
			else if (adc instanceof KinkLine) {
				if (((KinkLine) adc).isClosedLine()) 
					adc.getPoints().add(adc.getPoints().get(0));
			}
					
			ret = clipMultiPointElement(adc);
			
			if (ret == null) 
				return null;
			
			/** 
			 * convert special arc to normal arc
			 */		
			for (int i=0; i<ret.size(); i++) {
				if (ret.get(i).getPgenCategory().equalsIgnoreCase("Arc")) {
					Arc newArc = (Arc)ret.get(i).getPrimaryDE();
					//get special points				
					Coordinate center = ((Arc)adcOrig).getCenterPoint();
					Coordinate circum = ((Arc)adcOrig).getCircumferencePoint();
					Coordinate startPt = newArc.getPoints().get(0);					
					Coordinate endPt = newArc.getPoints().get(newArc.getPoints().size()-1);
					
					// do conversion
					Coordinate centerProjection = new Coordinate();
					Coordinate circumProjection = new Coordinate();
					Coordinate startPtProjection =new Coordinate();					
					Coordinate endPtProjection =  new Coordinate();

					CoordinateReferenceSystem crs = MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
							MapUtil.AWIPS_EARTH_RADIUS, center.y, center.x);
					MathTransform fromLatLon = null;
					MathTransform toLatLon = null;
					
					try {
						fromLatLon = MapUtil.getTransformFromLatLon(crs);
					} catch (FactoryException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					
					if (fromLatLon != null) {
						try {
							toLatLon = fromLatLon.inverse();
						} catch (org.opengis.referencing.operation.NoninvertibleTransformException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}

					try {
						JTS.transform(center, centerProjection, fromLatLon);
						JTS.transform(circum, circumProjection, fromLatLon);
						JTS.transform(startPt, startPtProjection, fromLatLon);
						JTS.transform(endPt, endPtProjection, fromLatLon);
					} catch (TransformException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

//					double theta =  toDegrees(atan2(circumProjection.y - centerProjection.y, circumProjection.x - centerProjection.x)); //((Arc)adcOrig).getStartAngle()
//					double aStart = toDegrees(atan2(startPtProjection.y - centerProjection.y, startPtProjection.x - centerProjection.x));
//					double aEnd =   toDegrees(atan2(endPtProjection.y - center.y, endPtProjection.x - centerProjection.x));

					double x0 = centerProjection.x; // Center
					double y0 = centerProjection.y;
					double x1 = circumProjection.x; 
					double y1 = circumProjection.y;
					double xs = startPtProjection.x; 
					double ys = startPtProjection.y;
					double xe = endPtProjection.x; 
					double ye = endPtProjection.y;
					
					double radius = sqrt(pow((y1 - y0), 2) + pow((x1 - x0), 2)); 
					double radius2 = radius * newArc.getAxisRatio();
					
					// in polar coordinate
					double theta =  toDegrees(atan2(y1 - y0, x1 - x0)); 
					double beta = atan2(y1 - y0, x1 - x0); 
					double sinbeta = Math.sin(beta);
					double cosbeta = Math.cos(beta);
						
				    // Let alpha be start angle, (xs, ys) be the ellipse point at start angle. x0=y0=0.
				    // double xs = x0 + (radius * cosalpha * cosbeta - radius2 * sinalpha * sinbeta);
				    // double ys = y0 + (radius * cosalpha * sinbeta + radius2 * sinalpha * cosbeta);
					// we get cosalpha:
					// cosalpha = (ys*sinbeta+xs*cosbeta)/radius/(cosbeta*cosbeta+sinbeta*sinbeta) = (ys*sinbeta+xs*cosbeta)/radius
					// Math.acos(cosalpha) = 0~PI.  Need to treat according to xs,ys

					double cosalpha = 0;
					
					cosalpha = (ys*sinbeta+xs*cosbeta)/radius;			 
					double startAngle = Math.toDegrees(Math.acos(cosalpha));			
					cosalpha = (ye*sinbeta+xe*cosbeta)/radius;			 
					double endAngle = Math.toDegrees(Math.acos(cosalpha));
					//System.out.println("newtheta "+theta +" "+startAngle +" "+endAngle);
					
					double xTest = (radius * cos(toRadians(-startAngle)) * cosbeta - radius2 * sin(toRadians(-startAngle)) * sinbeta);
					double yTest = (radius * cos(toRadians(-startAngle)) * sinbeta + radius2 * sin(toRadians(-startAngle)) * cosbeta);
					if (abs((xs-xTest)/xs) > 0.005 || abs((ys-yTest)/ys) > 0.005)
						startAngle = 360 -startAngle;
					//System.out.println("xstest "+xTest +" "+yTest );			
					xTest = (radius * cos(toRadians(-endAngle)) * cosbeta - radius2 * sin(toRadians(-endAngle)) * sinbeta);
					yTest = (radius * cos(toRadians(-endAngle)) * sinbeta + radius2 * sin(toRadians(-endAngle)) * cosbeta);
					if (abs((xe-xTest)/xe) > 0.005 || abs((ye-yTest)/ye) > 0.005) //-90866.87 176333.57  -90448.95 176440.64
						endAngle = 360 -endAngle;
					//System.out.println("xetest "+xTest +" "+yTest );
					
					if ( abs(endAngle) <0.001) //360
						endAngle = 360 + endAngle;
					if (startAngle >= endAngle) //start to circum to end
						endAngle = 360 + endAngle;					
					//System.out.println("xe "+xs +" "+ys +" "+xe +" "+ye+" "+" "+x1 +" "+y1 +" "+xTest+" "+yTest); 
				
//					if (newArc.getStartAngle() <= 180) {
//						if (startAngle < previousEndAngle)
//							startAngle = 360 -startAngle;
//						if (startAngle >= endAngle)
//							endAngle = 360 - endAngle;
//						if (abs(startAngle - endAngle)<180 && ret.get(i).getPoints().size() >36) 
//							endAngle = 360 - endAngle;
//						previousEndAngle = endAngle;
//					}
//					else {
//						startAngle = 360 -startAngle;
//						if (startAngle < previousEndAngle)
//							startAngle = 360 -startAngle;
//						if (startAngle >= endAngle)
//							endAngle = 360 - endAngle;
//						previousEndAngle = endAngle;
//					}
					
					//System.out.println("& "+startAngle +" "+endAngle);
					
					newArc.setCenterPoint(center);
					newArc.setCircumferencePoint(circum);
					newArc.setStartAngle(startAngle);
					newArc.setEndAngle(endAngle);
					ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
					pts.add(center);
					pts.add(circum);
					newArc.setPoints(pts);
					ret.set(i, newArc);
				}
			}
		}
		return ret;
	}

	/**
	 * Clips SinglePointElement, depending on keep variable. If keep is true, then the element stays
	 * if its location is within the polygon.
	 * 
	 * @param elementToClip
	 * 
	 * @return the same element if needed to stay, a list of ofjects if this one is split in parts,
	 *         or null
	 */
	private List<AbstractDrawableComponent> clipSinglePointElement(AbstractDrawableComponent elementToClip) {
		// using JTS Polygon to determine if a point is inside the polygon
		Coordinate c = null;
		if (elementToClip instanceof SinglePointElement) {
			c = ((SinglePointElement) elementToClip).getLocation();
		} else if (elementToClip instanceof Arc) {
			c = ((Arc) elementToClip).getCenterPoint();
		}
		Point p = geometryFactory.createPoint(c);
		if (pointStays(p)) {
			ArrayList<AbstractDrawableComponent> ret = new ArrayList<AbstractDrawableComponent>();
			ret.add(elementToClip);
			return ret;
		}
		return null;
	}

	/**
	 * Converts Arc to pure Line object. We cannot use polymorphism here since Arc extends Line.
	 * 
	 * @param arc
	 * 
	 * @return
	 */
	private Arc convertArcToLine(Arc arc) {
		
		//Line line = new Line();
		Arc newArc = new Arc();
		try {
			// create points
			ArrayList<Coordinate> points = new ArrayList<Coordinate>();

			Coordinate c0 = arc.getCenterPoint();
			Coordinate c0world = new Coordinate();
			Coordinate c1 = arc.getCircumferencePoint();
			Coordinate c1world = new Coordinate();

			CoordinateReferenceSystem crs = MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
					MapUtil.AWIPS_EARTH_RADIUS, c0.y, c0.x);
			MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
			MathTransform toLatLon = fromLatLon.inverse();

			JTS.transform(c0, c0world, fromLatLon);
			JTS.transform(c1, c1world, fromLatLon);

			double x0 = c0world.x; // Center
			double y0 = c0world.y;
			double x1 = c1world.x; // CircumferencePoint
			double y1 = c1world.y;
			
			double radius = sqrt(pow((y1 - y0), 2) + pow((x1 - x0), 2)); 
			double radius2 = radius * arc.getAxisRatio();
			
			// theta in polar coordinate
			double theta = toDegrees(atan2(y1 - y0, x1 - x0));
//			double a1 = theta - arc.getStartAngle();
//			double a2 = theta - arc.getEndAngle();
			double a1 = -arc.getStartAngle();
			double a2 = -arc.getEndAngle();
			
			if (abs(a1 - a2) < 0.00001 ) a2 -= 360;     	//whole circle

			double beta = atan2(y1 - y0, x1 - x0); 
			double sinbeta = Math.sin(beta);
			double cosbeta = Math.cos(beta);
			//System.out.println("*****angles "+theta +" "+a1 +" "+a2);

								
			for ( double angle =a1; angle > a2; angle -= 5) 
			{
			    double alpha = toRadians(angle) ;			    
			    double sinalpha = Math.sin(alpha);
			    double cosalpha = Math.cos(alpha);
		   
				double x = x0 + (radius * cosalpha * cosbeta - radius2 * sinalpha * sinbeta);
			    double y = y0 + (radius * cosalpha * sinbeta + radius2 * sinalpha * cosbeta);

				Coordinate c = new Coordinate(x, y);
				JTS.transform(c, c, toLatLon);
				
				points.add(c);
				
			}
			
			double lastA2x = x0 + (radius * Math.cos(toRadians(a2)) * cosbeta - radius2 * Math.sin(toRadians(a2)) * sinbeta);
			double lastA2y = y0 + (radius * Math.cos(toRadians(a2)) * sinbeta + radius2 * Math.sin(toRadians(a2)) * cosbeta);
			Coordinate c = new Coordinate(lastA2x, lastA2y);
			JTS.transform(c, c, toLatLon);
			points.add(c);
						
			arc.setPoints(points);
			
//			line.setLineWidth(arc.getLineWidth());
//			line.setSizeScale(arc.getSizeScale());
//			line.setSmoothFactor(arc.getSmoothFactor());
//			line.setClosed(false);
//			line.setFilled(arc.isFilled());
//			line.setFillPattern(arc.getFillPattern());
//			line.setPgenCategory("Lines");
//			line.setPgenType("LINE_SOLID");
//			line.setPoints(points);
//			line.setColors(arc.getColors());
		} catch (Exception e) {
//			e.printStackTrace();
//			line = null;
		}
		return arc;
		//return line;
	}


	/**
	 * Clips MultiPointElement, depending on keep variable. If keep is true, then the element stays
	 * if its location is within the polygon.
	 * 
	 * @param elementToClip
	 * 
	 * @return
	 */
	private List<AbstractDrawableComponent> clipMultiPointElement(AbstractDrawableComponent elementToClip) {
		ArrayList<Coordinate> pointsActual = ((MultiPointElement) elementToClip).getPoints();

		/*
		 * first insert additional points which are on the border
		 */
		// and keep track of the newly inserted points
		ArrayList<Coordinate> pointsOnBorder = new ArrayList<Coordinate>();

		/*
		 * Process intersections
		 */
		ArrayList<Coordinate> pointsIncludingIntersections = processMultiPointElementIntersection(pointsActual);
		
		/*
		 * Now leave only the points which are inside of the polygon (if keep is false, then
		 * outside)
		 */
		ArrayList<Coordinate> pointsToLeave = processPointsToLeave(pointsOnBorder,
				pointsIncludingIntersections);
		
		if (!pointsToLeave.isEmpty()) {
			((MultiPointElement) elementToClip).setPoints(pointsToLeave);
			ArrayList<AbstractDrawableComponent> ret = new ArrayList<AbstractDrawableComponent>();
			
			// need to split in chunks if two consecutive points are newly
			// inserted on the border

//			boolean closed = ((MultiPointElement) elementToClip).isClosedLine();
			boolean closed = false; //jet is false
			if (elementToClip instanceof Line) {
				closed = ((Line) elementToClip).isClosedLine();
			}
//			else if (elementToClip instanceof Arc) {  closed = false
//				closed = ((Arc) elementToClip).isClosedLine();
//			}
			else if (elementToClip instanceof Gfa) {
				closed = ((Gfa) elementToClip).isClosedLine();
			}
			else if (elementToClip instanceof KinkLine) {
				closed = ((KinkLine) elementToClip).isClosedLine();
			}
			
			if (isToSplit(elementToClip, pointsOnBorder)) {
				List<AbstractDrawableComponent> splitted = split(elementToClip, pointsOnBorder);
				// combine if needed
				combineFirstAndLast(closed, splitted);
				ret.addAll(splitted);
			} else {
				((MultiPointElement) elementToClip).setClosed(false);
				ret.add(elementToClip);
			}

			return ret;
		}
		return null;
	}

	/**
	 * Process intersections
	 * 
	 * @param pointsActual
	 * 
	 * @return pointsIncludingIntersections
	 */
	private ArrayList<Coordinate> processMultiPointElementIntersection(ArrayList<Coordinate> pointsActual) {
		ArrayList<Coordinate> pointsIncludingIntersections = new ArrayList<Coordinate>();
		Coordinate[] line = new Coordinate[2];
		for (int i = 0; i < pointsActual.size() - 1; i++) {
			line[0] = pointsActual.get(i);
			line[1] = pointsActual.get(i + 1);
			CoordinateArraySequence cas = new CoordinateArraySequence(line);
			LineString ls = new LineString(cas, geometryFactory);
			Geometry intersection = null;
			if (polygon.getExteriorRing().intersects(ls)) {
				intersection = polygon.intersection(ls);
			}
			// add first point
			pointsIncludingIntersections.add(pointsActual.get(i));
			// add all the intersection points
			if (intersection != null) {
				for (Coordinate c : intersection.getCoordinates()) {
					if (!compareCoordinates(c, line[0]) && !compareCoordinates(c, line[1])) {
						// no need to add the same coordinates twice
						pointsIncludingIntersections.add(c);
					}
				}
			}
		}
		// add the last Coordinate
		pointsIncludingIntersections.add(pointsActual.get(pointsActual.size() - 1));
		return pointsIncludingIntersections;
	}

	/**
	 * Process points to leave.
	 * 
	 * @param pointsOnBorder
	 * 
	 * @param pointsIncludingIntersections
	 * 
	 * @return
	 */
	private ArrayList<Coordinate> processPointsToLeave(ArrayList<Coordinate> pointsOnBorder,
			ArrayList<Coordinate> pointsIncludingIntersections) {
		ArrayList<Coordinate> pointsToLeave = new ArrayList<Coordinate>();
		for (Coordinate c : pointsIncludingIntersections) {
			Point p = geometryFactory.createPoint(c);

			// If KEEP is false and the point is on border, it should stay.
			boolean onBorder = polygon.getExteriorRing().distance(p) < PRECISION;
			if (onBorder) {
				pointsOnBorder.add(c);
			}
			if (pointStays(p) || onBorder) {
				pointsToLeave.add(c);
			}
		}
		return pointsToLeave;
	}

	/**
	 * Decides whether the point is staying or being clipped.
	 * 
	 * <pre>
	 * Inverse XOR operation !(A&circ;B) here: INSIDE POLYGON KEEP STAYS true true true false true false true
	 * false false false false true
	 * </pre>
	 * 
	 * @param p
	 * 
	 * @param pointsOnBorder
	 * 
	 * @return true to stay, false to clip
	 */
	private boolean pointStays(Point p) {
		return !(polygon.contains(p) ^ keep);
	}

	/**
	 * Compares two Coordinate objects by comparing x and y coordinates. If the coordinates are
	 * close within a precision, returns true, otherwise false.
	 * 
	 * @param c1
	 * 
	 * @param c2
	 * 
	 * @return true is x and y are very close, false otherwise
	 */
	private boolean compareCoordinates(Coordinate c1, Coordinate c2) {
		if (abs(c1.x - c2.x) < PRECISION && abs(c1.y - c2.y) < PRECISION) {
			return true;
		}
		return false;
	}

	/**
	 * This method determines if a component needs to be splitted in chunks when two consecutive
	 * points are newly inserted on the border.
	 * 
	 * @param ret
	 * 
	 * @return true if need to split, false otherwise
	 */
	private boolean isToSplit(AbstractDrawableComponent adc, ArrayList<Coordinate> pointsOnBorder) {
		if (adc instanceof MultiPointElement) {
			ArrayList<Coordinate> points = ((MultiPointElement) adc).getPoints();
			for (int i = 0; i < points.size() - 2; i++) {
				if (pointsOnBorder.contains(points.get(i)) && pointsOnBorder.contains(points.get(i + 1))) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Splits the component, use only if isNeedToSplit method returns true.
	 * 
	 * @param adc
	 * 
	 * @param pointsOnBorder
	 * 
	 * @return
	 */
	private List<AbstractDrawableComponent> split(AbstractDrawableComponent adc,
			ArrayList<Coordinate> pointsOnBorder) {
		ArrayList<AbstractDrawableComponent> ret = new ArrayList<AbstractDrawableComponent>();
		if (adc instanceof MultiPointElement) {
			ArrayList<Coordinate> points = ((MultiPointElement) adc).getPoints();
			ArrayList<Coordinate> firstElementPoints = new ArrayList<Coordinate>();
			ArrayList<Coordinate> remainingPoints = new ArrayList<Coordinate>();
			int i = 0;
			for (i = 0; i < points.size() - 2; i++) {
				firstElementPoints.add(points.get(i));
				if (i == 0) continue;
				if (pointsOnBorder.contains(points.get(i)) && pointsOnBorder.contains(points.get(i + 1))) {
					i++;
					break;
				}
			}
			// continue looping
			for (; i < points.size(); i++) {
				remainingPoints.add(points.get(i));
			}
			AbstractDrawableComponent copy = adc.copy();
			((MultiPointElement) adc).setPoints(firstElementPoints);
			if (firstElementPoints.size() > 1) {
				((MultiPointElement) adc).setClosed(false);
				ret.add(adc);
			}

			((MultiPointElement) copy).setPoints(remainingPoints);
			// by this time we have two elements, the second one might need to
			// be splitted, recursive call to the same method
			if (copy.getPoints().size() > 2 && isToSplit(copy, pointsOnBorder)) {
				ret.addAll(split(copy, pointsOnBorder));
			} else {
				((MultiPointElement) copy).setClosed(false);
				ret.add(copy);
			}
		}
		return ret;
	}

	/**
	 * First and last object created after the clipping procedure can be combined if they share a
	 * point.
	 * 
	 * @param closed
	 * 
	 * @param splitted
	 */
	private void combineFirstAndLast(boolean closed, List<AbstractDrawableComponent> splitted) {
		AbstractDrawableComponent first = splitted.get(0);
		AbstractDrawableComponent last = splitted.get(splitted.size() - 1);

		if (compareCoordinates(first.getPoints().get(0), last.getPoints().get(last.getPoints().size() - 1))) {
			closed = true;
		}

		if (closed && !keep && splitted.size() > 1) {
			splitted.remove(0);
			// combine first and last
			last.getPoints().addAll(first.getPoints());
			// remove duplicates
			((MultiPointElement) last).setPoints(removeDuplicates(((MultiPointElement) last).getPoints()));
			((MultiPointElement) last).setClosed(false);
		}
	}

	/**
	 * Creates a bound polygon.
	 * 
	 * @throws VizException
	 */
	private void createPolygon() throws VizException {

		String sql = "select t.table_name from config.clo t where t.alias_name = upper('" + boundsTableAlias + "')";

		List<Object[]> firstResult = performQuery(sql);
		String tableName = null; // comes from config.clo
		if(firstResult.isEmpty()){
			// no record, this means use it as a table name
			tableName = boundsTableAlias;
		} else {
			for (Object o : firstResult.get(0)) {
				tableName = (String) o;
			}
		}
		if (tableName == null) {
			throw new IllegalArgumentException(
					"Second argument invalid format: no such record exists in config.clo table or invalid table name");
		}

		sql = "SELECT AsBinary(t.the_geom) FROM " + schema + "." + tableName + " t" + " WHERE t."
				+ columnName + " like '" + columnValue + "%'";

		List<Object[]> secondResult = performQuery(sql);

		WKBReader wkbReader = new WKBReader();
		if (secondResult != null && !secondResult.isEmpty()) {
			Object[] bound = secondResult.get(0); // take the very first one
			try {
				Geometry g = wkbReader.read((byte[]) bound[0]);
				if(g instanceof MultiPolygon){
					MultiPolygon mg = (MultiPolygon) g;
					int max = 0;
					int index = 0;
					for (int i = 0; i < mg.getNumGeometries(); i++) {
						if (max < mg.getGeometryN(i).getNumPoints()){
							max = mg.getGeometryN(i).getNumPoints();
							index = i;
						}
					}
					polygon = (Polygon) mg.getGeometryN(index);
					return;
				}
				CoordinateSequence sequence = new CoordinateArraySequence(g.getCoordinates());
				LinearRing ring = new LinearRing(sequence, geometryFactory);
				polygon = new Polygon(ring, null, geometryFactory);
			} catch (Exception e) {
				// cannot do anything here, wrong data
				// the same as database problem
				throw new VizException(e);
			}
		} else {
			throw new RuntimeException("No bounds found, please check parameters");
		}
	}

	/**
	 * This method is a workaround for non-Eclipse-plugin applications.
	 * 
	 * @param sql
	 * @return
	 * @throws VizException
	 * @throws VizServerSideException
	 */
	private List<Object[]> performQuery(String sql) throws VizException, VizServerSideException {
		
		NcDirectDbQuery.setHttpServer(httpServer);
		List<Object[]> res = NcDirectDbQuery.executeQuery(sql, database, QueryLanguage.SQL);

		return res;
	}

	/**
	 * Helper method to remove duplicates from array list, returns a new object with no duplicates.
	 * 
	 * @param <T>
	 * 
	 * @param list
	 * 
	 * @return
	 */
	private <T> ArrayList<T> removeDuplicates(ArrayList<T> list) {
		ArrayList<T> ret = new ArrayList<T>();
		for (T obj : list) {
			if (!ret.contains(obj)) {
				ret.add(obj);
			}
		}
		return ret;
	}

	/**
	 * Entry point.
	 * 
	 * @param args
	 */
	public void run() {
		long time = System.currentTimeMillis();
		System.out.println("\nClipping started: " + inputName);

		try {
			clip(args[0], args[3]);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Could not open " + args[0] + " file");
		} catch (VizException e) {
			throw new RuntimeException("ERROR: Database failed");
		}

		System.out.println("Clipping finished: " + (System.currentTimeMillis() - time) + "ms");
	}

	/**
	 * Checks parameters and throws IllegalArgumentException if these parameters do not pass
	 * validation.
	 * 
	 * @param args
	 * 
	 * @see ClipVGFDialog.getProgramDescription()
	 */
	private void checkParameters() {
		if (args.length < 4) {
			// ignore args[4], we always "exact", never "rough"
			throw new IllegalArgumentException("Not enough arguments");
		}

		// "STATEBNDS|<STATE>IL"
		String[] arg1 = args[1].split("\\|");
		if (arg1.length != 2 || !(arg1[1].indexOf("<") > -1)) {
			throw new IllegalArgumentException("Second argument invalid format");
		}
		boundsTableAlias = arg1[0];
		// extract <STATE> and sate as column name
		columnName = arg1[1].substring(arg1[1].indexOf("<") + 1, arg1[1].indexOf(">"));
		columnValue = arg1[1].substring(arg1[1].indexOf(">") + 1, arg1[1].length());

		if (args.length > 5) {
			httpServer = args[5];
			// VizApp.setHttpServer(httpServer);
		}

		if (boundsTableAlias.isEmpty() || columnValue.isEmpty()) {
			throw new IllegalArgumentException("boundsTable or firId are not set up correctly");
		} else if (httpServer == null) {
			throw new IllegalArgumentException("EDEX http server is not set up correctly");
		}

		inputName = args[0];
		outputName = args[3];

		if ("notkeep".equalsIgnoreCase(args[2])) {
			// explicitly leave everything what is outside of the polygon only
			// if "notkeep" is specified. everything is interpreted as "keep"
			ClipVGF.keep = false;
		} else {
			ClipVGF.keep = true;
		}
	}

	/**
	 * Description.
	 * 
	 * @return
	 */
	public static String getProgramDescription() {
		try {
			return Util.getContent(HELP_FILE_NAME).toString();
		} catch (IOException e) {
			return "";
		}
	}

	/**
	 * Entry point for command line. (Not implemented yet)
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length == 0 || "--help".equals(args[0]) || "/h".equalsIgnoreCase(args[0])) {
			System.out.println(getProgramDescription());
			return;
		} // else proceed

		try {
			ClipVGF thread = new ClipVGF(args);
			thread.start();
		} catch (VizException e) {
			System.err.println("Database error");
			e.printStackTrace();
		}
	}
}
