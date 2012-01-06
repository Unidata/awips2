/*
 * LayerTest
 * 
 * Date created: 22 October 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.List;
import java.util.ArrayList;

import java.awt.Color;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
*  JUnit test for class "Layer".
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/22/08					J. Wu   	Initial Creation.
 *
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
 *
 */
public class LayerTest {

	private Layer tlayer;
    private List<AbstractDrawableComponent> tlist;	
    
	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		tlist = new ArrayList<AbstractDrawableComponent>();
		tlayer = new Layer( "Test", true, true, Color.red, true, tlist );
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link nmap.pgen.product.Layer#addElement(int, nmap.pgen.drawable.Drawable)}.
	 */
	@Test
	public final void testAddElementIntDrawable() {
		DrawableElement oneElement = new Line();
		tlayer.addElement( oneElement );		
		assertTrue( tlayer.getElement( 0 ) == oneElement ); 
	}

	/**
	 * Test method for {@link nmap.pgen.product.Layer#addElement(nmap.pgen.drawable.Drawable)}.
	 */
	@Test
	public final void testAddElementDrawable() {
		DrawableElement oneElement = new Symbol();
		tlayer.addElement( oneElement );		
		assertTrue( tlayer.getElement( tlayer.getDrawables().size() - 1 ) == oneElement ); 		
	}

	/**
	 * Test method for {@link nmap.pgen.product.Layer#removeElement(int)}.
	 */
	@Test
	public final void testRemoveElement() {
		DrawableElement oneElement = new Line();
		int	tsize = tlayer.getDrawables().size();
		tlayer.addElement( oneElement );	
		tlayer.removeElement( 0 );			
		assertTrue( tsize == tlayer.getDrawables().size() ); 		
	}

	/**
	 * Test method for {@link nmap.pgen.product.Layer#getElement(int)}.
	 */
	@Test
	public final void testGetElement() {
		DrawableElement oneElement = new KinkLine();
		tlayer.addElement( oneElement );		
		assertTrue( tlayer.getElement( 0 ) == oneElement ); 		
	}

	/**
	 * Test method for {@link nmap.pgen.product.Layer#toString()}.
	 */
	@Test
	public final void testToString() {
		tlist = new ArrayList<AbstractDrawableComponent>();
		tlayer = new Layer( "Test", true, true, Color.red, true, tlist );
		
		System.out.println ( "\nContent in Layer Object\n" );		
		System.out.println( tlayer );
	}

}
