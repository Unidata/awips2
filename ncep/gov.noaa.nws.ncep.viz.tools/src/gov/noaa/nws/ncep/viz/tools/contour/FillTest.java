package gov.noaa.nws.ncep.viz.tools.contour;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.operation.MathTransform;

//import com.raytheon.viz.core.contours.ContourSupport;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jts.io.WKTWriter;

public class FillTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
    	Map<Float,Geometry> polyMap = new HashMap<Float,Geometry>();
        float[] cvals = new float[] { 10, 20, 30, 40, 50, 60, 70 , 80, 90 };
        Geometry grid=null;
        
        try {
        FileReader data = new FileReader("/tmp/contour_data");
        BufferedReader buf = new BufferedReader(data);
       	WKTReader wkt = new WKTReader( new GeometryFactory() );
   		
       	String gr = buf.readLine();
   		grid = wkt.read(gr);
       	
       	boolean done = false;
       	while ( ! done ) {
       		String val = buf.readLine();
       		if ( val == null ) {
       			done = true;
       			continue;
       		}
       		
       		String gs = buf.readLine();
       		Geometry g = wkt.read(gs);
    	
       		polyMap.put(Float.valueOf(val), g);
       	}
        } catch(Exception e) {
        	e.printStackTrace();
        }

       	long fZ1 = System.currentTimeMillis();
    	
    //   	MathTransform rastPosToWorldGrid = 
    //    GeometryFactory gf = new GeometryFactory();
    //    Coordinate ul = new Coordinate ( 1, 1);
    //    Coordinate ur = new Coordinate ( 1, 113);
    //    Coordinate lr = new Coordinate ( 151, 113);
    //    Coordinate ll = new Coordinate ( 151, 1);
    //    Geometry grid = gf.createLinearRing(new Coordinate[] {ul, ur, lr, ll, ul });
        
        FillGenerator fillGen = new FillGenerator( grid );
        
        for ( Map.Entry<Float,Geometry> cntr: polyMap.entrySet() ) {
        	fillGen.addContours(cntr.getKey(), cntr.getValue());
        }
        
        try {
        
        float fval = cvals[0];
        //fval=30;
        Geometry fillPolys = fillGen.fillLessThan( fval );
        
        for (int j=0; j<fillPolys.getNumGeometries(); j++ ) {
        	Geometry g = fillPolys.getGeometryN(j);
//sag        	if ( g instanceof Polygon ) g = ContourSupport.polyToLine2( (Polygon)g );
        	//LineString ls = toScreenLS( g.getCoordinates(), rastPosToWorldGrid);
        	//contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, new RGB(230,230,230));
        }
        
        for (int n=1; n < cvals.length-1; n++ ) {
        	float fval1 = cvals[n];
        	float fval2 = cvals[n+1];
        	//fval1=40; fval2=50;
        	RGB color = new RGB( (70*n)%255, (110*n)%255, (50*n)%255 );
        	fillPolys = fillGen.fillBetween( fval1, fval2 );

        	for (int j=0; j<fillPolys.getNumGeometries(); j++ ) {
        		Geometry g = fillPolys.getGeometryN(j);
//sag        		if ( g instanceof Polygon ) g = ContourSupport.polyToLine2( (Polygon)g );
        		//LineString ls = toScreenLS( g.getCoordinates(), rastPosToWorldGrid);
        		//contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, color);
        	}
        }
       
        fval = cvals[cvals.length-1];
        //fval = 70;
        fillPolys = fillGen.fillGreaterThan( fval );
        
        for (int j=0; j<fillPolys.getNumGeometries(); j++ ) {
        	Geometry g = fillPolys.getGeometryN(j);
//sag        	if ( g instanceof Polygon ) g = ContourSupport.polyToLine2( (Polygon)g );
        	//LineString ls = toScreenLS( g.getCoordinates(), rastPosToWorldGrid);
        	//contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, new RGB(0,0,230));
        }
        
        long fZ2 = System.currentTimeMillis();
        System.out.println("SAGTIME fill:      N/A    :    "+(fZ2-fZ1)+"      :     N/A");

    	}
    	catch ( Exception e) { e.printStackTrace(); }

        }

	/*       	
        float fval = cvals[0];
        Geometry fill = ContourSupport.fillLessThan( fval, polyMap.get(fval), grid );
        
        for (int j=0; j<fill.getNumGeometries(); j++ ) {
        	Geometry g = fill.getGeometryN(j);
        	if ( g instanceof Polygon ) g = ContourSupport.polyToLine( (Polygon)g);
      //  	LineString ls = toScreenLS( g.getCoordinates(), rastPosToWorldGrid);
        	//contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, new RGB(230,230,230));
        }
        
        for (int n=1; n < cvals.length-1; n++ ) {
        	float fval1 = cvals[n];
        	float fval2 = cvals[n+1];
        	RGB color = new RGB( (70*n)%255, (110*n)%255, (50*n)%255 );
        	fill = ContourSupport.fillBetween( fval1, polyMap.get(fval1), fval2, polyMap.get(fval2), grid );

        	for (int j=0; j<fill.getNumGeometries(); j++ ) {
        		Geometry g = fill.getGeometryN(j);
        		if ( g instanceof Polygon ) g = ContourSupport.polyToLine( (Polygon)g);
        //		LineString ls = toScreenLS( g.getCoordinates(), rastPosToWorldGrid);
        		//contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, color);
        	}
        }
       
        fval = cvals[cvals.length-1];
    //    fill = ContourSupport.fillGreaterThan( fval, polyMap.get(fval), grid );
        
        for (int j=0; j<fill.getNumGeometries(); j++ ) {
        	Geometry g = fill.getGeometryN(j);
        	if ( g instanceof Polygon ) g = ContourSupport.polyToLine( (Polygon)g);
       // 	LineString ls = toScreenLS( g.getCoordinates(), rastPosToWorldGrid);
        	//contourGroup.fillShapes.addPolygonPixelSpace(new LineString[]{ls}, new RGB(0,0,230));
        }
        long fZ2 = System.currentTimeMillis();
        System.out.println("SAGTIME fill:      N/A    :    "+(fZ2-fZ1)+"      :     N/A");

    	*/
    	
	//

}
