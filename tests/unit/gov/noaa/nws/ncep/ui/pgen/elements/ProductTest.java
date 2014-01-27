/*
 * ProductTest
 * 
 * Date created: 22 october 2008
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
 * JUnit test for class "Product".
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
public class ProductTest {
     
	private Product 			tProduct;
	private ArrayList<Layer>	tLayers;
	private ProductInfo			tInfo;	
	private ProductTime			tTime;	
	private Layer				firstLayer,  secondLayer, thirdLayer;	
	private List<AbstractDrawableComponent> 		tlist;
	
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
		tInfo = new ProductInfo();		
		tTime = new ProductTime();			
		tLayers = new ArrayList<Layer>();
		tProduct = new Product( "SFC_ANL", "Default", "Forecaster", tInfo, tTime, tLayers );
		
		tlist = new ArrayList<AbstractDrawableComponent>();
		firstLayer = new Layer( "First", true, true, Color.red, true, tlist );
		secondLayer = new Layer( "Second", true, true, Color.green, true, tlist );
		thirdLayer = new Layer( "Third", true, true, Color.blue, true, tlist );		
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link nmap.pgen.product.Product#addLayer(nmap.pgen.product.Layer)}.
	 */
	@Test
	public final void testAddLayerLayer() {
		tProduct.addLayer( thirdLayer );				
		assertTrue( tProduct.getLayer( "Third" ).equals( thirdLayer ) );  
	}

	/**
	 * Test method for {@link nmap.pgen.product.Product#addLayer(int, nmap.pgen.product.Layer)}.
	 */
	@Test
	public final void testAddLayerIntLayer() {
		int nlayers = tProduct.getLayers().size();
		tProduct.addLayer( nlayers, thirdLayer );				
		assertTrue( tProduct.getLayer( nlayers ).equals( thirdLayer ) );  
	}

	/**
	 * Test method for {@link nmap.pgen.product.Product#removeLayer(int)}.
	 */
	@Test
	public final void testRemoveLayer() {
		int nlayers = tProduct.getLayers().size();
		tProduct.addLayer( thirdLayer );	
		tProduct.removeLayer( nlayers );			
		assertTrue( tProduct.getLayers().size() == nlayers );  
	}

	/**
	 * Test method for {@link nmap.pgen.product.Product#makeProduct()}.
	 */
	@Test
	public final void testMakeProduct() {
		System.out.println( tProduct.makeProduct() );
	}

	/**
	 * Test method for {@link nmap.pgen.product.Product#toString()}.
	 */
	@Test
	public final void testToString() {
		int nlayers = tProduct.getLayers().size();		
		tProduct.addLayer( nlayers, firstLayer );
		tProduct.addLayer( nlayers + 1, secondLayer );		
		System.out.println ( "\nContent in Product Object\n" );		
		System.out.println( tProduct );
	}

}
