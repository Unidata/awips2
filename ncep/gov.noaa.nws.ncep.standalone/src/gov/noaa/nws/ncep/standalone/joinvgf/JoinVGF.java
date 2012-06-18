/*
 * gov.noaa.nws.ncep.standalone.joinvgf.JoinVGF
 * 
 * Date created (as Jan 12, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.joinvgf;

import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This is a standalone application to join PGen XML files, replacement for joinvgf.
 * 
 * @author mlaryukhin
 */
public class JoinVGF extends Thread {

	/** Input file name. */
	private String				inputName1;

	/** Input file name. */
	private String				inputName2;

	/** Output file name to store results. */
	private String				outputName;

	/** Tolerance in km. */
	public static double		tolerance		= 0.5;

	/** Input parameters. */
	public String[]				args;

	/** Help file where the program description is stored. */
	public static String		HELP_FILE_NAME	= "joinvgf.hlp";

	/** Calculator for reuse. */
	private GeodeticCalculator	gc;

	/**
	 * Constructor.
	 * 
	 * @param args
	 *            <code>String[]</code> input parameters
	 */
	public JoinVGF(String[] args) {
		this.args = args;
	}

	/**
	 * Open a product file to replace or append to the current product list.
	 * 
	 * @throws FileNotFoundException
	 */
	private List<Product> openProducts(String inputName) throws FileNotFoundException {
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
	 * This method joins the xml files.
	 * 
	 * @throws FileNotFoundException
	 * @throws VizException
	 *             if database fails
	 */
	private void joinXmls() throws FileNotFoundException, VizException {
		// open
		List<Product> productList1 = openProducts(inputName1);
		List<Product> productList2 = openProducts(inputName2);
		// all the products
		List<Product> combined = new ArrayList<Product>(productList1);

		/*
		 * Combine projects and layers based on their names
		 */

		if ((productList1 == null || productList1.isEmpty())
				|| (productList2 == null || productList2.isEmpty())) {
			throw new RuntimeException("No products found in both XML files, nothing to merge.");
		}

		for (Product p1 : productList1) {
			Iterator<Product> it = productList2.iterator();
			while (it.hasNext()) {
				Product p2 = it.next();
				if (p1.getName().equals(p2.getName())) {
					// we need to combine these two products
					joinProducts(p1, p2);
					// p1 is already in combined, no need to add to combined

					it.remove();
				}
			}
		}
		// after we joined all the products we need to add the rest from the list 2
		for (Product p2 : productList2) {
			combined.add(p2);
		}

		// save
		saveProducts(combined);
	}

	/**
	 * Joins all the layers from Product p2 to Product p1 and saves them in p1.
	 * 
	 * @param p1
	 * @param p2
	 * @return p1 object
	 */
	private Product joinProducts(Product p1, Product p2) {
		List<Layer> list1 = p1.getLayers();
		List<Layer> list2 = p2.getLayers();

		for (Layer layer1 : list1) {
			Iterator<Layer> it = list2.iterator();
			while (it.hasNext()) {
				Layer layer2 = it.next();
				if (layer1.getName().equals(layer2.getName())) {
					joinLayers(layer1, layer2);
					it.remove();
				}
			}
		}
		// after we joined the layers we need to add the rest from the list 2
		for (Layer layer2 : list2) {
			list1.add(layer2);
		}

		return p1;
	}

	/**
	 * Joins the layers, adds or combines the DrawableComponent lists.
	 * 
	 * @param layer1
	 * @param layer2
	 * @return layer1 modified
	 */
	private Layer joinLayers(Layer layer1, Layer layer2) {
		List<AbstractDrawableComponent> list1 = layer1.getDrawables();
		List<AbstractDrawableComponent> list2 = layer2.getDrawables();

		while (joinLayersSubroutine(list1, list2)) {
			// each call removes one element from list2 when returns true
			// will break out of the loop eventually
		}

		while (joinItself(list1)) {
		}

		// after we joined the layers we need to add the rest from the list2
		for (AbstractDrawableComponent adc : list2) {
			list1.add(adc);
		}

		return layer1;
	}

	private boolean joinLayersSubroutine(List<AbstractDrawableComponent> list1,
			List<AbstractDrawableComponent> list2) {
		boolean joined = false;
		for (AbstractDrawableComponent adc1 : list1) {
			Iterator<AbstractDrawableComponent> it = list2.iterator();
			while (it.hasNext()) {
				AbstractDrawableComponent adc2 = it.next();
				if (joinDrawables(adc1, adc2)) {
					it.remove();
					joined = true;
				}
			}
		}
		return joined;
	}

	public boolean joinItself(List<AbstractDrawableComponent> list) {
		for (AbstractDrawableComponent adc1 : list) {
			Iterator<AbstractDrawableComponent> it = list.iterator();
			while (it.hasNext()) {
				AbstractDrawableComponent adc2 = it.next();
				if (adc1 != adc2 && joinDrawables(adc1, adc2)) {
					it.remove();
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Join the components if they touch each other, i.e. share a point.
	 * 
	 * @param adc1
	 * @param adc2
	 * @return
	 */
	private boolean joinDrawables(AbstractDrawableComponent adc1, AbstractDrawableComponent adc2) {
		// combine only if the classes are the same
		// for example, we do not want to end up combining ColdFront with WarmFront
		boolean joined = false;
		if (adc1.getClass() == adc2.getClass()) {
			if (adc1 instanceof Arc) {
				/*
				 * !!! add other classes here which need to be ignored
				 */
			} else if (adc1 instanceof MultiPointElement) {
				MultiPointElement e1 = (MultiPointElement) adc1;
				MultiPointElement e2 = (MultiPointElement) adc2;
				ArrayList<Coordinate> points1 = e1.getPoints();
				ArrayList<Coordinate> points2 = e2.getPoints();
				// compare start and end point
				// no need to iterate through all of them

				if (compareCoordinates(points1.get(0), points2.get(0))) {
					// they both begin in the same point
					points1.remove(0);
					Collections.reverse(points2);
					points2.addAll(points1);
					e1.setPoints(points2);
					joined = true;
				} else if (compareCoordinates(points1.get(0), points2.get(points2.size() - 1))) {
					// the end of #2 is the same as the beginning of #1
					points1.remove(0);
					points2.addAll(points1);
					e1.setPoints(points2);
					joined = true;
				} else if (compareCoordinates(points1.get(points1.size() - 1), points2.get(0))) {
					// the end of #1 is the same as the beginning of #2
					points2.remove(0);
					points1.addAll(points2);
					e1.setPoints(points1); // not needed, but left for logic explanation
					joined = true;
				} else if (compareCoordinates(points1.get(points1.size() - 1), points2
						.get(points2.size() - 1))) {
					// they both end in the same point
					Collections.reverse(points1);
					points1.remove(0);
					points2.addAll(points1);
					e1.setPoints(points2); // not needed
					joined = true;
				}
				if (joined) {
					points1 = e1.getPoints();
					if (compareCoordinates(points1.get(0), points1.get(points1.size() - 1))) {
						// beginning and end are the same
						e1.setClosed(true);
					}
				}
			}
		}
		return joined;
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
		getGeodeticCalculator();
		gc.setStartingGeographicPoint(c1.x, c1.y);
		gc.setDestinationGeographicPoint(c2.x, c2.y);

		double dis = gc.getOrthodromicDistance() / 1000; // meters to km

		return dis < (tolerance + 0.001); // to avoid situations like 5.0 < 4.999999999
	}

	/**
	 * Entry point.
	 * 
	 * @param args
	 */
	public void run() {

		checkParameters();
		
		long time = System.currentTimeMillis();

		try {
			joinXmls();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (VizException e) {
			e.printStackTrace();
		}

		System.out.println("Joining finished: " + (System.currentTimeMillis() - time) + "ms");
	}

	/**
	 * Checks parameters and throws IllegalArgumentException if these parameters do not pass
	 * validation.
	 * 
	 * @param args
	 * 
	 * @see JoinVGFDialog.getProgramDescription()
	 */
	private void checkParameters() {
		System.out.println("Joining started");
		if (args.length < 3) {
			throw new IllegalArgumentException("Not enough arguments");
		}

		inputName1 = args[0];
		inputName2 = args[1];
		outputName = args[2];

		if (args.length >= 4) {
			try {
				if (args[3] != null && !args[3].isEmpty()) {
					tolerance = Double.parseDouble(args[3]);
				}
			} catch (RuntimeException e) {
				throw new IllegalArgumentException(e);
			}
		}
	}

	/**
	 * Getter for GeodeticCalculator using lazy loading.
	 * 
	 * @return
	 */
	private GeodeticCalculator getGeodeticCalculator() {
		if (gc == null) {
			gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		}
		return gc;
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
	 * Entry point for command line.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length == 0 || "--help".equals(args[0]) || "/h".equalsIgnoreCase(args[0])) {
			System.out.println(getProgramDescription());
			return;
		} // else proceed

		JoinVGF joinVGF = new JoinVGF(args);
		// no need to start a separate thread here, just call run()
		joinVGF.run();
	}
}
