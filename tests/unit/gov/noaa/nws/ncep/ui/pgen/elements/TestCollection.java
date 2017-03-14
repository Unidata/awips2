/*
 * gov.noaa.nws.ncep.ui.pgen.elements
 * 
 * 28 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import org.junit.Before;
import org.junit.Test;

import com.vividsolutions.jts.geom.Coordinate;

import static org.junit.Assert.*;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * This JUnit test is for Class DECollection
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/09		#116  		B. Yin	   Initial Creation
 *                       
 * </pre>
 * 
 * @author bingfan
 * @version 1
 */

public class TestCollection {
	
	DECollection dec = new DECollection("test collection");
	DECollection subDec = new DECollection("sub collection");
	
	DrawableElement subDe1 = new Symbol(null, new Color[]{new java.awt.Color( 255,255,255)},
			1.0f, 1, true, new Coordinate(10,10) , "symbol", "vol");
	
	DrawableElement subDe2 = new Line(null, new Color[]{new java.awt.Color( 255,255,255)},
			1.0f, 1, false, false,
			new ArrayList<Coordinate>(), 2, null,
			"line", "solid");
	DrawableElement de1 = new Symbol(null, new Color[]{new java.awt.Color( 255,255,255)},
			2.0f, 2, true, new Coordinate(70,70) , "symbol", "rain");
	DrawableElement de2 = new Line(null, new Color[]{new java.awt.Color( 255,255,255)},
			1.0f, 1, false, false,
			new ArrayList<Coordinate>(), 2, null,
			"line", "solid");
	
	@Before
	public void setUp() throws Exception {
		subDec.addElement(subDe1);
		subDec.addElement(subDe2);
		
		dec.addElement(de1);
		dec.addElement(subDec);
		dec.addElement(de2);
	}
	
	@Test
	public void testCollectionIterator() throws Exception{
		
		Iterator<DrawableElement> it = dec.createDEIterator();
		
		int total = 0;
		
		while(it.hasNext()){
			System.out.print(it.next().toString());
			total++;
		}
		
		assertEquals (4, total);

	}
	
	@Test
	public void testCollectionSearch() throws Exception{
		
		assertEquals(dec, dec.search(de1));
		assertEquals(subDec, dec.search(subDe1));

	}
}
