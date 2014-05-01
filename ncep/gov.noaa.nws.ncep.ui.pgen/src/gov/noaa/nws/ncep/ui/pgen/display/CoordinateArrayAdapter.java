/*
 * CoordinateArrayAdapter
 * 
 * Date created: 18 MAY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class is a JAXB XmlAdapter, which converts a Coordinate[] to/from a string
 * of Coordinate pairs, where each pair is separated by a blank " ".
 * Each Coordinate pair uses a comma as the delimiter to separate the first coordinate 
 * from the second coordinate.
 * 
 *  This Adapter can be used by JAXB when marshaling/unmarshaling a Coordinate[] array.
 * @author sgilbert
 *
 */
public class CoordinateArrayAdapter extends XmlAdapter<String, Coordinate[]> {

	/**
	 * Write out the Coordinate[] to a string.
	 */
	@Override
	public String marshal(Coordinate[] v) throws Exception {
		
		StringBuffer buffer = new StringBuffer();
		for ( int i=0; i < v.length; i++ ) {
			buffer.append(Double.toString(v[i].x)).append(",").append(Double.toString(v[i].y));
			if ( i != (v.length-1) ) buffer.append(' ');
		}

        return buffer.toString();
		
	}

	/**
	 * convert coordinate pairs in a String to Coordinate[] array
	 */
	@Override
	public Coordinate[] unmarshal(String v) throws Exception {

        Coordinate[] points = null;
        
        if (v != null) {
            String[] pairs = v.split(" ");
            points = new Coordinate[pairs.length];
            
            for ( int i=0; i < pairs.length; i++ ) {
            	String[] value = pairs[i].split(",");
            	points[i] = new Coordinate(Double.valueOf(value[0]), Double.valueOf(value[1]));
            }
        }
        
        return points;
        
	}

}
