/*
 * gov.noaa.nws.ncep.standalone.fop.RFOP.java
 * 
 * Date created (as Feb 01, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.rfop;

import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.elements.*;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;

import java.io.*;
import java.util.*;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Reverse This is a standalone application to read Pgen XML file and translate it to fop ASCII
 * file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer      Description
 * ------------ ----------  ------------  --------------------------
 * 
 * </pre>
 * 
 * @author mlaryukhin
 */
public class RFOP {

	/** Input file name. */
	private String				inputName;

	/** Output file name to store results. */
	private String				outputName;

	/** Input parameters. */
	public String[]				args;

	/** Help file where the program description is stored. */
	public static String		HELP_FILE_NAME	= "rfop.hlp";

	/** Calculator for reuse. */
	private GeodeticCalculator	gc;

	/**
	 * Constructor.
	 * 
	 * @param args
	 *            <code>String[]</code> input parameters
	 */
	public RFOP(String[] args) {
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
	 * Entry point.
	 * 
	 * @param args
	 * @throws IOException
	 */
	public void execute() throws IOException {

		if (!checkParameters()) return;

		System.out.println("rfop started: " + inputName);
		long time = System.currentTimeMillis();

		List<Product> productList = openProducts(inputName);

		if (productList == null || productList.isEmpty()) {
			throw new RuntimeException("No products found in both XML files, nothing to merge.");
		}

		processProducts(productList);

		System.out.println("Joining finished: " + (System.currentTimeMillis() - time) + " ms");
	}

	/**
	 * Saves the product list to the ASCII file.
	 * 
	 * @param productList
	 * @throws IOException
	 */
	private void processProducts(List<Product> productList) throws IOException {
		List<AbstractDrawableComponent> list = new ArrayList<AbstractDrawableComponent>();
		for (Product product : productList) {
			for (Layer layer : product.getLayers()) {
				if (layer.getDrawables() != null) {
					list.addAll(layer.getDrawables());
				}
			}
		}

		ArrayList<Text> texts = extractTexts(list);
		ArrayList<Line> lines = extractLines(list);
		// each Line should correspond to a Text
		// even if the sizes of the lists are different then we assign a closest Text to a Line
		HashMap<Line, Text> map = new HashMap<Line, Text>();
		for (Line line : lines) {
			Text t = findClosestText(line, texts);
			map.put(line, t);
		}

		dumpToASCII(map);
	}

	/**
	 * Returns the list of Text objects.
	 * 
	 * @param list
	 * @return
	 */
	private ArrayList<Text> extractTexts(List<AbstractDrawableComponent> list) {
		ArrayList<Text> texts = new ArrayList<Text>();
		for (AbstractDrawableComponent adc : list) {
			if (adc instanceof Text) {
				texts.add((Text) adc);
			}
		}
		return texts;
	}

	/**
	 * Returns the list of Line objects.
	 * 
	 * @param list
	 * @return
	 */
	private ArrayList<Line> extractLines(List<AbstractDrawableComponent> list) {
		ArrayList<Line> lines = new ArrayList<Line>();
		for (AbstractDrawableComponent adc : list) {
			if (adc instanceof Line) {
				lines.add((Line) adc);
			}
		}
		return lines;
	}

	/**
	 * Finds the closest text to the line object.
	 * 
	 * @param line
	 * @param texts
	 * @return
	 */
	private Text findClosestText(Line line, ArrayList<Text> texts) {
		Text ret = texts.get(0);
		double min = calculateDistance(line, texts.get(0));
		for (int i = 1; i < texts.size(); i++) {
			double d = calculateDistance(line, texts.get(i));
			if (d < min) {
				min = d;
				ret = texts.get(i);
			}
		}
		return ret;
	}

	/**
	 * Calculates the distance between the Text and the closes point of the line.
	 * 
	 * @param line
	 * @param text
	 * @return
	 */
	private double calculateDistance(Line line, Text text) {
		double min = text.getLocation().distance(line.getPoints().get(0));
		for (int i = 1; i < line.getPoints().size(); i++) {
			double d = text.getLocation().distance(line.getPoints().get(i));
			if (d < min) {
				min = d;
			}
		}
		return min;
	}

	/**
	 * Saves the map to to the output file in the format.
	 * 
	 * <pre>
	 * NWS NATIONAL FLOOD OUTLOOK PRODUCT 020321/1223
	 * 6 2 Mar 21 - Mar 23
	 * 45.53 -69.08
	 * 44.40 -67.62
	 * 44.11 -67.51
	 * 45.22 -69.02
	 * 45.39 -70.07
	 * 45.53 -69.08
	 * </pre>
	 * 
	 * @param map
	 * @throws IOException
	 */
	private void dumpToASCII(HashMap<Line, Text> map) throws IOException {
		StringBuilder sb = new StringBuilder(1000);
		// first line
		Calendar cal = Calendar.getInstance();
		sb.append("NWS NATIONAL FLOOD OUTLOOK PRODUCT " + cal.get(Calendar.YEAR)
				+ pad((cal.get(Calendar.MONTH)) + 1) + "/");
		cal.add(Calendar.HOUR, 5);
		sb.append(pad(cal.get(Calendar.HOUR_OF_DAY))).append(pad(cal.get(Calendar.MINUTE))).append(
				"\n");

		for (Line line : map.keySet()) {
			Text t = map.get(line);
			ArrayList<Coordinate> points = line.getPoints();
			int size = points.size();
			boolean isFirstAndLastSame = compareCoordinates(points.get(0), points.get(size - 1));
			if (isFirstAndLastSame) {
				sb.append(size).append(" ");
			} else {
				sb.append(size + 1).append(" ");
			}
			sb.append(findFloodCategory(line));
			sb.append(" ").append(t.getText()[0]).append("\n");
			for (Coordinate c : points) {
				sb.append(c.x).append(", ").append(c.y).append("\n");
			}
			// add first point to the end
			if (!isFirstAndLastSame) {
				// first and last points are not close, then add the last point
				sb.append(points.get(0).x).append(", ").append(points.get(0).y).append("\n");
			}
		}
		// remove the last carriage return
		sb.setLength(sb.length() - 1);
		File file = new File(outputName);
		Writer output = new BufferedWriter(new FileWriter(file));
		output.write(sb.toString());
		output.close();
	}

	/**
	 * Pads one digit int with zero and returns a two letter string.
	 * 
	 * @param in
	 * @return
	 */
	private String pad(int in) {
		if (in >= 0 && in < 10) {
			return "0" + in;
		}
		return "" + in;
	}

	/**
	 * Compares two Coordinate objects by comparing x and y coordinates. If the coordinates are
	 * close within 5 km, returns true, otherwise false.
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

		return dis < (5); // to avoid situations like 5.0 < 4.999999999
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
	 * Returns 0 for "Unfilled closed lines", 1 for "hatch filled areas", or 3 for
	 * "solid fill areas", -9999 for everything else.
	 * 
	 * @param line
	 * @return
	 */
	private int findFloodCategory(Line line) {
		if (!line.isFilled()
				&& (line.isClosedLine() || compareCoordinates(line.getPoints().get(0), line
						.getPoints().get(line.getPoints().size() - 1)))) {
			// if closed or the first point is nearby the last point
			return 0;
		} else if (line.isFilled() && line.getFillPattern() == FillPattern.FILL_PATTERN_0) {
			return 1;
		} else if (line.isFilled() && line.getFillPattern() == FillPattern.SOLID) {
			return 2;
		}
		return -9999;
	}

	/**
	 * Checks parameters and throws IllegalArgumentException if these parameters do not pass
	 * validation.
	 * 
	 * @param args
	 * 
	 * @see JoinVGFDialog.getProgramDescription()
	 * @return true if the check passed, false otherwise
	 */
	private boolean checkParameters() {
		if (args.length == 0 || "--help".equals(args[0]) || "/h".equalsIgnoreCase(args[0])) {
			System.out.println(getProgramDescription());
			return false;
		}

		inputName = args[0];
		if (args.length == 2 && args[1] != null && !args[1].isEmpty()) {
			outputName = args[1];
		} else {
			if (inputName.toLowerCase().endsWith(".xml")) {
				outputName = inputName.substring(0, inputName.length() - 4);
			} else {
				outputName = inputName + ".txt";
			}
		}
		return true;
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
		try {
			new RFOP(args).execute();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
