package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import java.awt.Color;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.display.ISinglePoint;
import gov.noaa.nws.ncep.ui.pgen.display.IMultiPoint;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;


/**
 * Define a Drawable Class - containing a list of Points and a collection of 
 * properties. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/22/08					J. Wu   	Initial Creation.
 * 05/12/09        42       S. Gilbert  Added getPatternName()
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 */

public class Drawable implements IMultiPoint, ISinglePoint {
	 /** The fields */
     private List<Coordinate> points;
     private HashMap<String, Object> properties;
 
     /**
  	 *  Constructor
  	 */
  	public Drawable() {
  		this.points = new ArrayList<Coordinate>();
  		this.properties = new HashMap<String, Object>();
  	}
  	
  	/**
 	 *  Constructor
 	 */
 	public Drawable(List<Coordinate> ipoints, HashMap<String, Object> prop ) {
 	    if ( ipoints == null ) {
 	    	points = new ArrayList<Coordinate>();
 	    }
 	    else {
 	    	points = ipoints;
 	    }
 		
 		if ( prop == null ) {
 	    	properties = new HashMap<String, Object>();
 	    }
 	    else {
 	    	properties = prop;
 	    }
 	}	
     
     
     /**
	 * @return the points
	 */
	public List<Coordinate> getPoints() {
		return points;
	}
	
	/**
	 * @param points the points to set
	 */
	public void setPoints(List<Coordinate> points) {
		this.points = points;
	}
	
	/**
	 * @return the property
	 */
	public Object getProperty( String propertyName) {
		return properties.get( propertyName );
	}

	/**
	 * @return the properties
	 */
	public HashMap<String, Object> getProperties() {
		return properties;
	}
	
	/**
	 * @param properties the properties to set
	 */
	public void setProperties( HashMap<String, Object> prop) {
		properties = prop;
	}   
	
	/**
	 * @param properties the properties to match
	 */
	public boolean  matches( HashMap<String, Object> searchProperty ) {
		for ( Iterator<String> ii = searchProperty.keySet().iterator(); ii.hasNext(); ) {
			
			String propertyName = (String)ii.next();
			
			if ( !properties.get( propertyName ).equals(
					searchProperty.get( propertyName ) ) ) {
				return false;
			}
		}
		
		return true;
	}  
	
	/**
	 * @param the index and the point
	 */
	public void addPoint(int index, Coordinate point) {
		points.add(index, point);
	}
	
	/**
	 * @param the point
	 */
	public void appendPoint(Coordinate point) {
		points.add( point );
	}

	/**
	 * @param the index and the point
	 */
	public void removePoint(int index ) {
		points.remove( index );
	}
	
	/**
	 * @return the point
	 */
	public Coordinate getPoint(int index) {
		return points.get( index );
	}	
     
	/**
	 * @return the string
	 */
	public String toString() {
		StringBuilder	result = new StringBuilder( getClass().getSimpleName());
		Coordinate	onePoint;
		result.append( "\n");
		
        for ( Iterator<String> ii = properties.keySet().iterator(); ii.hasNext(); ) {
			
        	String propertyName = (String)ii.next();
			result.append( propertyName);
			result.append( ":\t");
			result.append( properties.get( propertyName ) );
			result.append( "\n");
		}
        
        result.append( "\nPoints:\t");
        result.append( points.size() );
        result.append( "\n" );
        
		for ( int ii = 0;  ii < this.points.size(); ii++ ) {
			onePoint = getPoint( ii );
			result.append ( onePoint.x );
		    result.append ( "\t" );
		    result.append ( onePoint.y );
		    result.append ( "\n" );
		}
		
		return result.toString();
	}	
	
	public String getType(){
		
		return (String)this.getProperty("Type");
		
	}
	
	public Coordinate[] getLinePoints(){
		
		Coordinate a[] = new Coordinate[ points.size() ];
		points.toArray(a);
		return a;
		
	}
	
	public Color[] getColors(){
		
	      Color[] colors = new Color[2];
          
          colors[0] = (Color)this.getProperty("color");
	      colors[1] = Color.red;
          
          return colors;
  
	}
	
	public float getLineWidth(){
		
		return Float.parseFloat(((String)this.getProperty("lineWidth")).trim());		
		
	}
	
	public String getLinePattern(){
	
		return (String)this.getProperty("linePattern");
		
	}
	
	public int getSmoothFactor(){
		
		return Integer.parseInt(((String)this.getProperty("smoothFactor")).trim());		
		
	}
	
	public Boolean isClosedLine(){
	
		return Boolean.parseBoolean(((String)this.getProperty("closed")).trim());
		
	}
	
	public Boolean isFilled(){

		return Boolean.parseBoolean(((String)this.getProperty("filled")).trim());
		
	}
	
	public FillPattern getFillPattern(){

		return (FillPattern)this.getProperty("fillPattern");

	}
	
	public double getSizeScale(){
	
		return Double.parseDouble(((String)this.getProperty("sizeScale")).trim());

	}
	
	public Coordinate getLocation(){
	
		return points.get(0);
		
	}
	
	public Boolean isClear(){
		
		return Boolean.parseBoolean(((String)this.getProperty("clear")).trim());

	}

}
