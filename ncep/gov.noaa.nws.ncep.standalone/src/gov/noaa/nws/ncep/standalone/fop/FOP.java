/*
 * gov.noaa.nws.ncep.standalone.fop.FOP.java
 * 
 * Date created (as Jan 29, 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.standalone.fop;

import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This is a standalone application to read fop files and create Pgen XML file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer      Description
 * ------------ ----------  ------------  --------------------------
 * 01/29/2010   220         mlaryukhin    Initial created
 * 10/31/2011   137         qzhou         Modified due to text defination changing
 * 
 * </pre>
 * 
 * @author mlaryukhin
 * 
 */
public class FOP {

	/** Input file name. */
	private String				inputName;

	/** Output file name to store results. */
	private String				outputName;

	/** Tolerance in km. */
	public static final double	TOLERANCE	= 0.5;

	/** Input parameters. */
	private String[]			args;

	/**
	 * Constructor.
	 * 
	 * @param args
	 */
	public FOP(String[] args) {
		this.args = args;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Thread#run()
	 */
	public void execute() {
		long time = System.currentTimeMillis();
		System.out.println("fop started");
		if (checkParameters()) {
			ArrayList<AbstractDrawableComponent> adcList = readFile();

			Product product = new Product();
			Layer layer = new Layer();
			product.addLayer(layer);
			layer.add(adcList);

			ArrayList<Product> list = new ArrayList<Product>();
			list.add(product);
			saveProducts(list);
		}
		System.out.println("fop finished " + (System.currentTimeMillis() - time) + " ms");
	}

	/**
	 * Reads the file line by line and creates the list of Drawables
	 * 
	 * @return list
	 */
	private ArrayList<AbstractDrawableComponent> readFile() {
		ArrayList<AbstractDrawableComponent> adcList = new ArrayList<AbstractDrawableComponent>();
		try {
			File file = new File(inputName);
			BufferedReader in = new BufferedReader(new FileReader(file));
			String str = null;
			int count = 0;
			// some input files are coming from windows, there is a lot of extra empty lines
			int numToIgnore = 1;
			while (count < numToIgnore) {
				str = in.readLine(); // ignore the first 4 lines, use 5th
				if (str == null) return null;
				if (!str.trim().isEmpty()) count++;
			}

			while ((str = in.readLine()) != null) {
				processFlood(adcList, in, str);
			}
		} catch (FileNotFoundException e) {
			throw new IllegalArgumentException(e);
		} catch (IOException e) {
			throw new IllegalArgumentException(e);
		}

		return adcList;
	}

	/**
	 * Processes one set of text and line which starts with a line like "9 0 Sat Jan 30 - Tue Feb 2"
	 * where 9 is the number of points, 0 is the flood category and the rest is the date.
	 * 
	 * @param adcList
	 * @param in
	 * @param str
	 * @throws IOException
	 */
	private void processFlood(ArrayList<AbstractDrawableComponent> adcList, BufferedReader in,
			String str) throws IOException {
		// 9 0 Sat Jan 30 - Tue Feb 2
		String[] header = str.trim().split(" ", 3);
		String[] s = null; // temp
		ArrayList<Coordinate> coordinates = new ArrayList<Coordinate>();
		for (int i = 0; i < Integer.parseInt(header[0].trim()); i++) {
			str = in.readLine().trim();
			if (str.isEmpty()) continue;
			s = str.split(",");
			if (s.length != 2) {
				throw new RuntimeException("Invalid input file format");
			}
			double x = Double.parseDouble(s[0].trim());
			double y = Double.parseDouble(s[1].trim());
			coordinates.add(new Coordinate(x, y));
		}
		// create
		createLineAndText(adcList, header, coordinates);
	}

	/**
	 * Creates Line and Text objects and adds them to adcList.
	 * 
	 * @param adcList
	 * @param header
	 * @param coordinates
	 */
	private void createLineAndText(ArrayList<AbstractDrawableComponent> adcList, String[] header,
			ArrayList<Coordinate> coordinates) {
		Color color = null;
		String pgenType = null;
		if ("1".equals(header[1])) {
			color = Color.YELLOW;
			pgenType = "LINE_SOLID";
		} else if ("2".equals(header[1])) {
			color = Color.RED;
			pgenType = "LINE_SOLID";
		} else {
			// "0" or everything else
			color = new Color(0, 255, 255);
			pgenType = "LINE_DASHED_4";
		}

		String[] textValue = { header[2] };
		Text text = new Text(null, "Courier", 14.0F, TextJustification.CENTER, coordinates.get(0),
				0.0, TextRotation.SCREEN_RELATIVE, textValue, FontStyle.REGULAR, color, 0, 0, true,
				DisplayType.NORMAL, "Text", "General Text");
		adcList.add(text);

		Color[] colors = { color, color };
		Line line = new Line(null, colors, 3.0F, 1.0, true, false, coordinates, 0,
				FillPattern.SOLID, "Lines", pgenType);
		adcList.add(line);
	}

	/**
	 * Checks the input parameters.
	 */
	private boolean checkParameters() {
		if (args.length == 0
				|| (args.length == 1 && ("--help".equals(args[0]) || "/h".equalsIgnoreCase(args[0])))) {
			System.out.println(getProgramDescription());
			return false;
		}
		inputName = args[0];
		if (args.length > 1) {
			outputName = args[1];
		} else {
			outputName = inputName + ".xml";
		}
		return true;
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
	 * Description.
	 * 
	 * @return
	 */
	public static String getProgramDescription() {
		try {
			return Util.getContent("fop.hlp").toString();
		} catch (IOException e) {
			return "";
		}
	}

	/**
	 * The entry point for the application.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			new FOP(args).execute();
		} catch (Exception e) {
			System.err.println("Error " + e.getMessage());
		}
	}
}
