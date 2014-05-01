/*
 * gov.noaa.nws.ncep.ui.pgen.clip
 * 
 * January 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.clipper;

import static java.lang.Math.abs;
import static java.lang.Math.atan2;
import static java.lang.Math.cos;
import static java.lang.Math.pow;
import static java.lang.Math.sin;
import static java.lang.Math.sqrt;
import static java.lang.Math.toDegrees;
import static java.lang.Math.toRadians;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.KinkLine;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.geotools.geometry.jts.JTS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

/**
 * Class to clip a PGEN product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/2013		#966			B. Yin		Initial Creation.
 * 
 * </pre>
 * 
 * @author byin
 */

public class ClipProduct {

	/** Factory */
	private GeometryFactory	geometryFactory	= new GeometryFactory();

	/** Bounds shape is to be converted to this polygon. */
	private Polygon			boundsPoly;

	/** Tolerance for coordinates to be treated as the same /close points. */
	private final double	PRECISION		= 0.000000001;
	/**
	 * Input parameter, defined by user; true to clip all the elements within the bounds, false -
	 * outside.
	 */
	public static boolean	keep			= true;

	public ClipProduct(Polygon bounds, boolean keepInside ){

		boundsPoly = bounds;
		keep = keepInside;
	}

	/**
	 * Clips each individual product.
	 * 
	 * @param p
	 * 
	 * @return
	 */
//	public void clipProduct(Product p, MultiPolygon bounds, boolean keepInside) {
//		
//	}	
	
	/**
	 * Clips each individual product.
	 * 
	 * @param p
	 * 
	 * @return
	 */
	public void clipProduct(Product p) {
		
		if ( boundsPoly == null ) return;
		
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
			//	while (getJoinVGF().joinItself(after)) {
			//	}
				layer.setDrawables(after);
			}
		}
		
	}
	
	public List<AbstractDrawableComponent> clipDrawableComponents(List<AbstractDrawableComponent> elements) {
		List<AbstractDrawableComponent> after = new ArrayList<AbstractDrawableComponent>();
		for (AbstractDrawableComponent adc : elements) {
			List<AbstractDrawableComponent> c = clipDrawableComponent(adc);
			if (c != null && !c.isEmpty()) {
				after.addAll(c);
			}
			else {
				after.add(adc.copy());
			}
		}
		return after;
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
		else if ( adc instanceof Outlook ) {
			ret = new ArrayList<AbstractDrawableComponent>();
			ret.add(clipOutlook( (Outlook)adc ));
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
		return !(boundsPoly.contains(p) ^ keep);
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
			if (boundsPoly.getExteriorRing().intersects(ls)) {
				intersection = boundsPoly.intersection(ls);
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
			boolean onBorder = boundsPoly.getExteriorRing().distance(p) < PRECISION;
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
	
	public Outlook clipOutlook( Outlook otlk ){

		if ( boundsPoly == null ) return otlk;
		
		Outlook ret = otlk.copy();
		//ret.clear();
		List<DECollection> clipped = new ArrayList<DECollection>();
		Iterator<AbstractDrawableComponent> it = ret.getComponentIterator();
		while ( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			it.remove();
			if ( adc.getName().equals(Outlook.OUTLOOK_LABELED_LINE)){
				clipped.addAll(clipOutlookLine( (DECollection) adc ));
			}
			else if (adc.getName().equals(Outlook.OUTLOOK_LINE_GROUP)){
				clipped.add(clipOutlookLineGroup( (DECollection) adc ));
			}
		}
		
		for (DECollection dec : clipped ){
			ret.add(dec);
		}
		
		return ret;
	}
	
	private  DECollection clipOutlookLineGroup( DECollection dec ){
		
		DECollection ret = dec.copy();
		ret.clear();
		
		Iterator<AbstractDrawableComponent> it = dec.getComponentIterator();
		
		List<DECollection> clipped = new ArrayList<DECollection>();

		while ( it.hasNext() ){
			clipped.clear();
			
			AbstractDrawableComponent adc = it.next();
			it.remove();
			
			if ( adc.getName().equals(Outlook.OUTLOOK_LABELED_LINE)){
				clipped.addAll(clipOutlookLine( (DECollection) adc ));
			}
			
			for (DECollection otlkLine : clipped ){
				ret.add(otlkLine);
			}
		}
		
		return ret;
	}
	
	private  List<DECollection> clipOutlookLine( DECollection dec ){
		List<DECollection> ret = new ArrayList<DECollection>();
		Iterator<AbstractDrawableComponent> it = dec.getComponentIterator();
		
		while ( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc instanceof Line ){
				List<AbstractDrawableComponent> clippedList = clipDrawableComponent( adc );
				if ( clippedList != null && clippedList.size() >= 1 ){
					Iterator<AbstractDrawableComponent> it2 = clippedList.iterator();
					while ( it2.hasNext() ){
						AbstractDrawableComponent clippedLine = it2.next();
						if ( clippedLine instanceof Line ){
							DECollection outlookLine = dec.copy();
							
							//remove the original line
							Iterator<AbstractDrawableComponent> it3 = outlookLine.getComponentIterator();
							while ( it3.hasNext() ){
								AbstractDrawableComponent ln = it3.next();
								if ( ln instanceof Line ){
									it3.remove();
								}
							}
							
							//add the clipped line
							outlookLine.add(0, clippedLine );
							ret.add( outlookLine );
						}
					}
				}
			}
		}
		
		return ret;

	}

}
