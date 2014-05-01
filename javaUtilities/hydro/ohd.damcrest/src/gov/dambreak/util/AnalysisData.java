package gov.dambreak.util;

import gov.damcat.data.*;

import java.io.*;
import java.util.*;
import javax.swing.*;

/**
 * This class allows for the reading and writing of "Dambreak Analysis" input files.
 */
public class AnalysisData {

	// public fields
	public ArrayList scenarios = new ArrayList();	// ModelScenario
	public ArrayList downstream = new ArrayList();	// DownstreamPoint
	public ArrayList rootDamInfo = new ArrayList();	// DamInfo reference

	public ArrayList deletedScenarios = new ArrayList();	// ModelScenario
	public ArrayList deletedDownstream = new ArrayList();	// DownstreamPoint

	public String	damNidid;
	public String	damName;
	public String	riverName;
	public String	pointOfInterestName;
	public String 	line;
	
	public int changeFlag = 0;
	
	public float pointOfInterestDistance = 0.0f;

	public boolean bEmptyDataset = false;

/**
 * Default Constructor
 * Creation date: (7/17/2003 1:14:25 PM)
 */
public AnalysisData() {

	damNidid = "";
	damName  = "Dam Name";
	riverName = "River Name";
	pointOfInterestName = "";
	
}
/**
 * Write out .DAT file format 
 * Creation date: (7/31/2003 1:16:27 PM)
 * @param scenario int
 * @param filename java.lang.String
 */
private boolean exportDAT(int nScenario, String filename) {

	String osName = System.getProperty("os.name");
	String lowerCaseName = osName.toLowerCase();
	String lineEnd = "";
	
	if (filename == null)
			return false;
			
	if(lowerCaseName.indexOf("windows") > -1) {
		System.out.println ("OS Windows"); 
		lineEnd = "\r\n"; // The model needs this windows-style line break
	}
	else if(lowerCaseName.indexOf("linux") > -1) {
		System.out.println ("OS Linux");
		lineEnd = "\n";
	}
	else
		lineEnd = "\n";
		
	FileWriter output;

	ModelScenario scenario = (ModelScenario)scenarios.get(nScenario);
	
	try {
		// open the file
		output = new FileWriter(filename);

		damName.trim();
		output.write("C1        " + damName + lineEnd);
		riverName.trim();
		output.write("C2        " + riverName);
		for (int i=0; i < 45-riverName.length(); i++)
			output.write(" ");
		pointOfInterestName.trim();
		output.write(pointOfInterestName + lineEnd);
		output.write("C3        " + "         0         " 
								  + scenario.dbugType + "         " 
								  + scenario.damType  + "         1         0" 
								  + lineEnd);
		output.write("C4        " +
			floatToString10(2, scenario.HDE) +
			floatToString10(2, scenario.BME) +
			floatToString10(0, scenario.VOL) +
			floatToString10(2, scenario.SA) +
			floatToString10(2, scenario.BW) +
			floatToString10(2, scenario.TFM) +
			floatToString10(2, scenario.QO) + lineEnd);

		int elevationCount = 0;

		DownstreamPoint firstDown = (DownstreamPoint)downstream.get(0);
		SectionGeometry firstSection = firstDown.getBestSection();
		for ( int i = 0; i< 8; i++)											// *** assumes 8 !
		{
			if (firstSection.getElevationData(i,0) != -999.0)				// *** was > -900.0
				elevationCount++;
		}
		
		int numberXSections = downstream.size();
		
		if (numberXSections < 10)											// *** why 10 ?
			output.write("C5        " + "         " + numberXSections + "         " + elevationCount +
				floatToString10(2, scenario.CMS) +
				floatToString10(2, scenario.DISTTN) + lineEnd);
		else
			output.write("C5        " + "        " + numberXSections + "         " + elevationCount +
				floatToString10(2, scenario.CMS) +
				floatToString10(2, scenario.DISTTN) + lineEnd);
		for ( int j=0; j<numberXSections; j++)
		{
			DownstreamPoint down = (DownstreamPoint)downstream.get(j);
			SectionGeometry section = down.getBestSection();
			
			if (j+1 < 10)
				output.write("C6- " + (j+1) + "     " + floatToString10(2, down.distanceToSection) +  floatToString10(2, down.floodDepth) + floatToString10(4, down.latitude) + floatToString10(4, down.longitude) + lineEnd);
			else
				output.write("C6-" + (j+1) + "     " + floatToString10(2, down.distanceToSection) +  floatToString10(2, down.floodDepth) + floatToString10(4, down.latitude) + floatToString10(4, down.longitude) + lineEnd);

			int count = 0;
			for (int k=0; k<8; k++)
			{
				if (section.getElevationData(k, 0) != -999.0f)		// ***   was > -990.0f		
				{
					count++;
					output.write("C7-" + count + "      " + 
						floatToString10(2, section.getElevationData(k, 0)) + 
						floatToString10(2, section.getElevationData(k, 1)) +
						floatToString10(2, section.getElevationData(k, 2)) +
						floatToString10(3, section.getElevationData(k, 3)) + lineEnd);
				}
			}
		}

	// close the file
	output.close();

	} catch (IOException e) {
		System.out.println("Caught IOException in AnalysisData.exportDAT");
		e.printStackTrace();
		return false;
	}
	catch (Exception ex) {
		System.out.println("Caught Exception in AnalysisData.exportDAT");
		ex.printStackTrace();
		return false;
	}
	return true;
}
	private static String floatToString10(int decimals, float value) {
		if (value == -999.0f)
			return "       .00";
		
		java.text.DecimalFormat df;
	
		if (decimals == 0)
			df = new java.text.DecimalFormat("#########.");
		else if (decimals == 2)
			df = new java.text.DecimalFormat("#######.00");
		else if (decimals == 3)
			df = new java.text.DecimalFormat("######.000");
		else if (decimals == 4)
			df = new java.text.DecimalFormat("#####.0000");
		else
			return "<error in floatToString10(...)>";
	
		StringBuffer result = new StringBuffer(df.format(value));
		
		while (result.length() < 10)
			result.insert(0, ' ');
			
		return result.toString();
	}
/**
 * This method runs the model on all
 * scenarios stored in this data structure.
 */
public boolean generateOutput()
{
	for (int i=0; i<scenarios.size(); i++)
		
		if (!generateOutput(i))
		{
			System.out.println("In AnalysisData.generateOutput() -- false return for scenario " + i);
			return false;
		}
	return true;
}
/**
 * This method runs the SMPDBK model on a scenario
 * contained in this data structure and, if the
 * run is successful, updates the scenario's
 * output structure.
 */
public boolean generateOutput(int scenario)
{
	String osName = System.getProperty("os.name");
	String lowerCaseName = osName.toLowerCase();
	String tempPath = "";
	String methodName = "AnalysisData.generateOutput";
	
	ModelScenario theScenario = (ModelScenario)scenarios.get(scenario);
	if(theScenario.source.startsWith("#"))
	{
		
		// if theScenario.output != null
		theScenario.bOutputAvailable = true;  ////
		return true;	
	}
	
	// get a ModelScenario structure representing the given input scenario 
	//  and store the output back in the ModelScenario
	
	try {		
		// TEMPPATH variable is renamed on the Linux side to DAMCREST_DATA_DIR
		// for better readability	
		if(lowerCaseName.indexOf("windows") > -1)
			tempPath = PropertyReader.getProperty("TEMPPATH");
		else if(lowerCaseName.indexOf("linux") > -1)
			tempPath = PropertyReader.getProperty("DAMCREST_DATA_DIR");
		
		if (!tempPath.endsWith(System.getProperty("file.separator")))
			tempPath = tempPath.concat(System.getProperty("file.separator"));

		// write the scenario input out to a SMPDBK input file located in the specified TEMP directory
		
		
		if (!exportDAT(scenario, tempPath + "damin"))
		{
			System.out.println("In AnalysisData.generateOutput(i) -- false return from exportDAT");
			return false;
		}

		// attempt to run the model
		try {
			
			// System.out.println("In AnalysisData.generateOutput: input file is:" + tempPath + "damin");
		
			theScenario.output = runModel(tempPath + "damout", tempPath + "damin");
			
			theScenario.bOutputAvailable = true;
			
			// NOTE: the downstream points in the output structure are missing latitude and
			// longitude data because when the model is run, this data is lost (the fortran
			// code was not built to handle lat and lon data so it is not included in the
			// .OUT file).
			// Here, we get around this by repopulating the downstream point structure
			// with latitude and longitude data after the output file has been parsed.

			// *** This is also true of other data found in DownstreamPoint !!!

			// System.out.println(methodName + " - downstream size = " + downstream.size());

			for (int i=0; i < downstream.size(); i++) {
				
				// *** Copy data from DownstreamPoints attached to AnalysisData structure
				DownstreamPoint dx = (DownstreamPoint)downstream.get(i);
					
				// ***   to DownstreamPoints attached to ModelOutput structure 				
				DownstreamPoint dp = (DownstreamPoint)theScenario.output.inputDownstream.get(i);
		
				dp.name = dx.name;
				dp.xsecBestType = dx.xsecBestType;
				dp.comments = dx.comments;
				// dp.distanceToSection =
				// dp.floodDepth = dp.floodDepth + dx.elevation;		// ???
				// System.out.println("i = " + i);
				// System.out.println("dp.floodDepth = " + dp.floodDepth);
				// System.out.println("dx.elevation = " + dx.elevation);				
				dp.floodWidth = dx.floodWidth;
				dp.floodFlow = dx.floodFlow;
				dp.elevation = dx.elevation;
				dp.mann_oc = dx.mann_oc;
				dp.latitude = dx.latitude;
				dp.longitude = dx.longitude;
				dp.bestXS = dx.bestXS;
				// dp.bestXS = 0;			// there is only one SectionGeometry in DownstreamPoint parsed from Output !
				
				dp.changeFlag = 1;

			}
			
		} catch (Exception e) {
			System.out.println("Exception While Running Model In AnalysisData.generateOutput");
			e.printStackTrace();
			return false;
		}
	} catch (Exception ex) {
		System.out.println("Exception While Trying to set up Model Run In AnalysisData.generateOutput");
		ex.printStackTrace();
		return false;
	}
	return true;
}
/**
 * Retrieve ModelScenario from ArrayList 
 * Creation date: (7/30/2003 12:53:56 PM)
 * @param scenario int
 */
public ModelOutput getModelOutput(int scenario) {
	
	ModelScenario theScenario = (ModelScenario)scenarios.get(scenario);

	return theScenario.output;
}
/**
 * This method verifies that all the cross sections
 * marked "Best" at each point in the array of DownstreamPoints
 * has the same number of B-vs-H pairs.
 *
 * If so, it returns that number (> 0)
 *
 * If not, it returns -2
 *
 * If no downstream points exist, it returns -1
 */
public static int getNumPairs(ArrayList downstreamPts) {

	int nPairs = -1;
	
	try {
	
		String methodName = "*** AnalysisData.getNumPairs";

		for (int i=0; i < downstreamPts.size(); i++) {

			// System.out.println(">>>>>>>>>>>>> " + "getDown " + i);  
			
			DownstreamPoint down = (DownstreamPoint)downstreamPts.get(i);

			SectionGeometry xs;
		
			// System.out.println(">>>>>>>>>>>>> " + "getXS " + i);  
			if (down.xsections.size() == 0)
				continue;
			else if (down.bestXS == -1)
				xs = (SectionGeometry)down.xsections.get(0);
			else if (down.bestXS >= down.xsections.size())
				xs = (SectionGeometry)down.xsections.get(0);
			else
				xs = (SectionGeometry)down.xsections.get(down.bestXS);

			// System.out.println("Best XS is " + xs.getXSType());
			// System.out.println("Last Row Used Is " + xs.getLastRowUsed());

			int j;
			for (j=0; j < xs.getLastRowUsed(); j++)				// *** this formerly assumeD always 8 pairs of B/H !
			{
				// System.out.println(">>>>>>>>>>>>> " + "getElev " + j);  
				if (xs.getElevationData(j,0) == -999.0)			// *** was < -900.0 == -999.0
				{
					break;
				}
			}

			int jPairs = j + 1;									// nPairs is an actual count, NOT the index !

			if (nPairs == -1)
				nPairs = jPairs;
			else if (nPairs == jPairs)
				continue;
			else
				return -2;
		}
	
		return nPairs;
	} catch (Throwable t) {
		System.out.println("caught in AnalysisData.getNumPairs");
		t.printStackTrace();
	} 
	return nPairs;

	
}
/**
 * This method creates a new scenario for the
 * input data stored in the given DAT file.
 * It also creates new downstream points for
 * every cross section in the DAT file.
 */
public boolean importDAT(String filename) {
	if (!(new File(filename).exists()))
	{
		System.out.println("In AnalysisData.importDAT - existence test failed for " + filename); 
			return false;
	}
	
	ModelScenario scenario = new ModelScenario();

	// name the scenario with some default values
	scenario.source = "DAT";
	scenario.name = "MF";
	
	BufferedReader input;
	String line = "";
	String lineData[];
	
	try {
		// attempt to open the file
		input = new BufferedReader(new FileReader(filename));

		// Read C1
		line = input.readLine();

		// *** just assume that first line of file contains dam name
		/*
		if (line.charAt(0) != 'C' || line.charAt(1) != '1')
			return false;
		damName = line.substring(10,line.length());
		damName = damName.trim();
		*/
		damName = line.substring(10,line.length());
		damName = damName.trim();

		// Read C2
		line = input.readLine();
		if (line.length() > 55)
		{
			riverName = line.substring(10,55);
			pointOfInterestName = line.substring(55,line.length());
		}
		else // no point of interest
		{
			riverName = line.substring(10,line.length());
			pointOfInterestName = "";
		}
		riverName = riverName.trim();
		pointOfInterestName = pointOfInterestName.trim();

		// Read C3 = IBC, ISH, JNK, IDAM, IPLT, IREC
		line = input.readLine();
		lineData = parseLine(line);
		scenario.dbugType = Integer.parseInt(lineData[2]);
		scenario.damType = Integer.parseInt(lineData[3]);

		// Read C4 = HDE, BME, VOL, SA, BW, IFM, QO
		line = input.readLine();
		lineData = parseLine(line);
		scenario.HDE = Float.parseFloat(lineData[1]);
		scenario.BME = Float.parseFloat(lineData[2]);
		scenario.VOL = Float.parseFloat(lineData[3]);
		scenario.SA = Float.parseFloat(lineData[4]);
		scenario.BW = Float.parseFloat(lineData[5]);
		scenario.TFM = Float.parseFloat(lineData[6]);
		scenario.QO = Float.parseFloat(lineData[7]);

		// Read C5
		line = input.readLine();
		lineData = parseLine(line);
		int numberNewDowns = Integer.parseInt(lineData[1]);
		int numberPairs = Integer.parseInt(lineData[2]);
		scenario.CMS = Float.parseFloat(lineData[3]);
		scenario.DISTTN = Float.parseFloat(lineData[4]);

		// add this new scenario to the list
		scenarios.add(scenario);
		
		for (int i=0; i<numberNewDowns; i++)
		{
			DownstreamPoint down = new DownstreamPoint();
			SectionGeometry sg = new SectionGeometry();

			// Read C6-i
			line = input.readLine();
			lineData = parseLine(line);
			down.distanceToSection = NumFormat.toFloat(lineData[1]);

			if (down.distanceToSection == 0.00f)
				down.name = "At Dam";
			else
				down.name = "Town" + i;
			
			down.floodDepth = NumFormat.toFloat(lineData[2]);
			
			if (lineData[3].compareTo("") != 0) // lat/lon is available
			{
				down.latitude = NumFormat.toFloat(lineData[3]);
				down.longitude = NumFormat.toFloat(lineData[4]);
			}
			else
			{
				down.latitude = 0.0f;
				down.longitude = 0.0f;
			}

			// Read C7-i
			for (int j=0; j<8; j++)
			{
				if (j < numberPairs)
				{
					line = input.readLine();
					lineData = parseLine(line);
					sg.setElevationData(j, 0, Float.parseFloat(lineData[1]));
					sg.setElevationData(j, 1, Float.parseFloat(lineData[2]));
					sg.setElevationData(j, 2, Float.parseFloat(lineData[3]));
					sg.setElevationData(j, 3, Float.parseFloat(lineData[4]));
					sg.setLastRowUsed(j);
				}
				else
				{
					sg.setElevationData(j, 0, -999.0f);
					sg.setElevationData(j, 1, -999.0f);
					sg.setElevationData(j, 2, -999.0f);
					sg.setElevationData(j, 3, -999.0f);
				}
			}
					
			
			sg.setXSType("G2");					// *** is this a good assumption ???			
			down.xsecBestType = sg.getXSType();

			down.bestXS = 0;		// *** assume first is the best ! 
			down.xsections.add(sg);

			// add this new downstream point to the list
			downstream.add(down);
		}
		
		// close the file
		input.close();
	
	} catch (NumberFormatException numberFormatException) {
//System.out.print("junk I am here=====: " + line + "!!!!!\n");
		JOptionPane.showMessageDialog(null,"Error -- bad data in DAT file\n" + line, "Error", JOptionPane.ERROR_MESSAGE);
		return false;
	} catch (Exception e) {
		e.printStackTrace();
		JOptionPane.showMessageDialog(null,"Error -- bad DAT file\n", "Error", JOptionPane.ERROR_MESSAGE);
		return false;
	}
	return true;
}
/**
 * This method creates new downstream points
 * for each cross section entry in the given
 * FLDXS file. Each point will have one
 * cross section.
 */
public boolean importFLDXS(String filename) {
	File file = new File(filename);

	if (!file.exists())
	{
		System.out.println("In AnalysisData::importFLDXS");
		System.out.println(filename + " failed existence test");
		return false;
	}
	
	FldXSOutputFileParser fldxsParser = new FldXSOutputFileParser(file);
	
	ArrayList newDowns = fldxsParser.parse();

	if (newDowns == null)
	{
		System.out.println("In AnalysisData::importFLDXS");
		System.out.println("ArrayList returned empty from FLDXSParser");
		return false;
	}
	else
		downstream.addAll(newDowns);

	return true;
}
/**
 * This method copies data found in the ModelOutput data structure
 * Creation date: (10/7/2003 2:57:06 PM)
 * @return boolean
 * @param modelOut gov.dambreak.util.ModelOutput
 */
public boolean importModelOutput(ModelOutput modelOut) {
	

	damName = modelOut.damName;
	riverName = modelOut.riverName;	
	pointOfInterestName = modelOut.pointOfInterestName;

	modelOut.inputScenario.bOutputAvailable = true;

	modelOut.inputScenario.output = modelOut;			// scenario must also reference output   

	scenarios.add(modelOut.inputScenario);

	for (int ds = 0; ds < modelOut.inputDownstream.size(); ds++)
	{
		downstream.add(modelOut.inputDownstream.get(ds));
	}

	
	return true;
		
	
	}
/**
 * Insert the method's description here.
 * Creation date: (3/25/2004 2:14:54 PM)
 * @return java.lang.String
 * @param str java.lang.String
 * @param minLength int
 */
public String pad(String str, int maxLength) 
{
	StringBuffer sb = new StringBuffer("");
	String space = " ";
	String strPadded = "";
	
	String strInit = str.trim();
	int lengthToPad = maxLength - strInit.length();
	for ( int i = 0; i < lengthToPad; i++)
	{
		strPadded = sb.append(space).toString();
	}
	String strToDisplay = strInit.concat(strPadded);
	
	return strToDisplay;
}
	// private methods
	
	private static String[] parseLine(String line) {
		int colCount = -1;
		int position = 0;
	
		if (line.length() > 80 ){
			JOptionPane.showMessageDialog(null,"Error data exceeds 80 columns in DAT file on line: \n"
			+ line, "Error", JOptionPane.ERROR_MESSAGE);
	 		return null;
//	 		return result;
		}		

		String result[] = new String[8];			// assumes max of 8 values on line !
		for (int i=0; i < 8; i++)
			result[i] = new String();
	
		while (position < line.length())
		{
			if ((position % 10) == 0)
				colCount++;

				if (line.charAt(position) != ' ')
				result[colCount] += line.charAt(position);
	
			position++;
		}
		
		return result;
	}
	public static AnalysisData readDAM(String filename) {
		
		if (!(new File(filename).exists()))
		{
			System.out.println("In AnalysisData.readDAM - file failed existence test !");
			return null;
		}

		AnalysisData data = new AnalysisData();
		BufferedReader input;
		String line = "";
		String lineData[];
		int numberElevations;
		
		try {
			
			// attempt to open the file
			input = new BufferedReader(new FileReader(filename));

			StringTokenizer st;

			// read the type/version line
			line = input.readLine();
			st = new StringTokenizer(line,"|");
			try {
				if (!st.nextToken().equals("Dambreak Analysis"))
					return null;
				if (Integer.parseInt(st.nextToken()) != 1)
					return null;
			} catch (Exception ex) {
				return null;
			}

			// read the name line
			line = input.readLine();
			st = new StringTokenizer(line,"|");
			data.damName = st.nextToken().trim();
			data.riverName = st.nextToken().trim();
			data.pointOfInterestName = st.nextToken().trim();

			// read the number of input scenarios
			line = input.readLine();
			st = new StringTokenizer(line,"|");
			int numScenarios = Integer.parseInt(st.nextToken());
			
			// read in the input scenarios
			for (int i=0; i<numScenarios; i++) {
				line = input.readLine();
				st = new StringTokenizer(line,"|");
				ModelScenario s = new ModelScenario();
				s.source = st.nextToken().trim();
				s.name = st.nextToken().trim();
				s.HDE = NumFormat.toFloat(st.nextToken());
				s.BME = NumFormat.toFloat(st.nextToken());
				s.BW = NumFormat.toFloat(st.nextToken());
				s.CMS = NumFormat.toFloat(st.nextToken());
				s.DISTTN = NumFormat.toFloat(st.nextToken());
				s.QO = NumFormat.toFloat(st.nextToken());
				s.SA = NumFormat.toFloat(st.nextToken());
				s.TFM = NumFormat.toFloat(st.nextToken());
				s.VOL = NumFormat.toFloat(st.nextToken());
				data.scenarios.add(s);
			}

			// read the number of downstream points
			line = input.readLine();
			st = new StringTokenizer(line,"|");
			int numDownstream = Integer.parseInt(st.nextToken());

			// read in the downstream points
			for (int i=0; i<numDownstream; i++) {
				line = input.readLine();
				st = new StringTokenizer(line,"|");
				DownstreamPoint d = new DownstreamPoint();

				d.name = st.nextToken().trim();
				d.distanceToSection = NumFormat.toFloat(st.nextToken());
				d.floodDepth = NumFormat.toFloat(st.nextToken());
				d.floodFlow = NumFormat.toFloat(st.nextToken());
				d.latitude = NumFormat.toFloat(st.nextToken());
				d.longitude = NumFormat.toFloat(st.nextToken());
				d.bestXS = Integer.parseInt(st.nextToken());

				// read the number of cross sections
				line = input.readLine();
				StringTokenizer st2 = new StringTokenizer(line,"|");
				int numSections = Integer.parseInt(st2.nextToken());
				
				for (int j=0; j<numSections; j++) {
					SectionGeometry sg = new SectionGeometry();
					line = input.readLine();
					st2 = new StringTokenizer(line,"|");
					sg.setXSType(st2.nextToken());
					for (int k=0; k<8; k++) {
						line = input.readLine();
						st2 = new StringTokenizer(line,"|");
						sg.setElevationData(k,0,NumFormat.toFloat(st2.nextToken()));
						sg.setElevationData(k,1,NumFormat.toFloat(st2.nextToken()));
						sg.setElevationData(k,2,NumFormat.toFloat(st2.nextToken()));
						sg.setElevationData(k,3,NumFormat.toFloat(st2.nextToken()));
						sg.setLastRowUsed(k);
					}
					d.xsections.add(sg);
					if (j == 0)					// *** assume first is the best ! 
					{
						d.bestXS = 0;
						d.xsecBestType = sg.getXSType();
					}
				}
				
				data.downstream.add(d);
			}

			// close the file
			input.close();
		
		} catch (NoSuchElementException ne) {
			// *** this error message might be based on some faulty assumptions 
			// *** probably test should be made when values are saved from GUI 
			// *** not when DAM file is being read 
			ne.printStackTrace();
			JOptionPane.showMessageDialog(null,"Error -- DAM file is not created from GUI "
				+ " and it contains blank field(s)\n"
			+ line, "Error", JOptionPane.ERROR_MESSAGE);
			return null;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		return data;
	}
/**
 * This method runs the SMPDBK model on the given
 * input filename and writes to the given output filename.
 */
private static ModelOutput runModel(String outputFilename, String inputFilename)
	throws Exception 
{
	Runtime rt = Runtime.getRuntime();

	String modelFileName;
	String osName = System.getProperty("os.name");
	String lowerCaseName = osName.toLowerCase();
	
	if ((modelFileName = System.getProperty("damcrest.home")) == null)
		modelFileName = "";

	if (!modelFileName.endsWith(System.getProperty("file.separator")))
		modelFileName = modelFileName.concat(System.getProperty("file.separator"));
	if(lowerCaseName.indexOf("windows") > -1) {
		modelFileName = modelFileName.concat("sdbj.exe");
	}
	else if(lowerCaseName.indexOf("linux") > -1) {
		modelFileName = modelFileName.concat("sdbj.LX");
	}

	File fileModel = new File(modelFileName);
	if (!fileModel.exists()){
		System.out.println("In AnalysisData.runModel -- File Does Not Exist" + modelFileName);
		JOptionPane.showMessageDialog(null,"Error -- no SMPDBK model found in\n" + modelFileName, "Error", JOptionPane.ERROR_MESSAGE);
	  	return null;
	}
	//String runThis = modelFileName + " \"" + outputFilename + "\" \"" + inputFilename + "\"";
	String runThis = modelFileName + " " + outputFilename + " " + inputFilename;
	System.out.println("In AnalysisData::runModel, runThis is :" + runThis);


	ModelOutput outFile = null;
	
	try {
		Process ps = rt.exec(runThis);
		// wait for the output file to be generated
		ps.waitFor();

		if (ps.exitValue() != 0)
		{
			
			throw new Exception("sdbj execution Process Failed with ExitValue of " + ps.exitValue());
		}
	
		outFile = ModelOutput.readOUT(outputFilename);

		outFile.changeFlag = 1;							// set change flag so output is written
	
		if (!ModelOutput.verify(outputFilename))
			throw new Exception("False return from ModelOutput.verify");
			
	} catch (IOException e) {
		System.out.println("In AnalysisData.runModel -- Caught IOException");
		e.printStackTrace();
		// throw new IOException("IOException Rethrown In AnalysisData.runModel");
		
	
	} /* catch (Exception e) {
		System.out.println("In AnalysisData.runModel -- Caught Exception");
		e.printStackTrace();
		throw new Exception("Generic Exception rethrown In AnalysisData.runModel");
		
	}*/
	
	return outFile;
}
/**
 * Modify ArrayList of Downstream Points by sorting by Distance From Dam 
 * Creation date: (5/24/2004 1:37:00 PM)
 */
public void sortDownstreamByDistance() {

	int toGo = downstream.size();				// determine size of unsorted ArrayList 
	ArrayList sortedList = new ArrayList(toGo); // and build new ArrayList of same size

	float closestDistance = 1000000.0f;			// Assume all will be less than a million miles away
	int closestIndex = -1; 						// none selected at first

	DownstreamPoint dp = null;					// reference object
	
	while (toGo > 0)							// loop until we've found them all							
	{
		for (int j = 0; j < downstream.size(); j++)			
		{
			dp = (DownstreamPoint) downstream.get(j);
		
			if (dp.distanceToSection < closestDistance)		// find closest

			{
				closestDistance = dp.distanceToSection;		// save distance
				closestIndex = j;							// and location in ArrayList
			}
				
		}

		if (closestIndex > -1)							
		{
			dp = (DownstreamPoint) downstream.get(closestIndex);
			sortedList.add(dp);								// add to sorted ArrayList	
			downstream.remove(closestIndex);				// remove from unsortedArrayList;
			closestIndex = -1;								// reset index
			closestDistance = 1000000.0f;					// and closest distance
			toGo--;											// decrement counter
		
		}
	}	// end while

	downstream = sortedList;			// replace with sortedList
}
/**
 * This method verifies that the data contained in this AnalysisData
 * structure is ready to be run through the model.
 *
 * NOTE: If a field is empty and a default value may be calculated,
 * the data will be changed to reflect the default values.
 *
 * This may need to be changed because it assumes that some
 * values the user enters are incorrect and replaces them.
 *
 * It returns null if the data is ready or a String containing
 * error messages if it is not.
 *
 */
public String verifyInput() {
	
	String strError = "";
	float botElev;
	
	if (downstream.size() < 1)
	{
		strError = "Insufficient data to run model - downstream points must be added.";
		return strError;
	} 


	DownstreamPoint d = (DownstreamPoint)downstream.get(0);			// closest to dam
	SectionGeometry sg = (SectionGeometry)d.xsections.get(0);		// lowest elevation 
	
	// Get lowest elevation at closest downstream point to serve as channel bottom 
    if (sg.getElevationData(0,1) < 1.0f){							// testing topwidth < 1 foot 
		botElev = sg.getElevationData(0,0);							// storing elevation of first BvsH pair as bottom
    }
    else
    {
		botElev = 2.0f * sg.getElevationData(0,0) - sg.getElevationData(1,0); // storing calculation as bottom ???      
    }
     
	float previousUpstreamElevation = 100000000.0f;					// start above the Himalayas 
    
	// Examine geometry of each downstream point
	for ( int j=0; j < downstream.size(); j++)						
	{
		DownstreamPoint down = (DownstreamPoint)downstream.get(j);
		
		SectionGeometry section = down.getBestSection();			// only interested in 'best' geometry
		if (section == null || down.xsections.size() == 0) {
			strError += "There must be at least two elevation-topwidth pairs per downstream point.\n";
			break;
		}
			
		int countEqualElevations = 0;
		int countZeroWidth = 0;
		int countNegativeWidth = 0;
		int countAscendingElevations = 0;
	
		for (int k=0; k < section.getLastRowUsed(); k++)			//  only to last non-blank row in array 
		{
			float elev = section.getElevationData(k, 0);			//  retrieve elevation 
			if(k == 0)												//  but only test if first one 
			{
				
				if (elev > previousUpstreamElevation)
				{
					countAscendingElevations++;						//  increment if any first elevation ascends
				}
				previousUpstreamElevation = elev;
			}		
			else
			{
				elev = section.getElevationData(k, 0);
				
				if(elev == previousUpstreamElevation)
				{	
					countEqualElevations++;						//  increment if elevations are equal
				}
				previousUpstreamElevation = elev;	
			}
			float channelWidth = section.getElevationData(k, 1);	//  retrieve topwidth
			
			if(channelWidth == 0.0f)
			{
				countZeroWidth++;									// only 1 will be allowed	
			}

			if(channelWidth < 0.0f)
			{
				countNegativeWidth++;								// no negative topwidths allowed	
			}
			
		}
		if (countAscendingElevations > 0)
		{
			strError += "Elevations of the Cross Section must be in descending order to run model \n";
		}

		if (countEqualElevations >= 2)   
		{
			strError += "Not more than two Elevations of the Cross Section can be equal to run model \n";
		}
		
		if(countZeroWidth > 1)
		{
			strError += "More than one channel width cannot be 0.0 to run model.\n";
		}

		if(countNegativeWidth > 0)
		{
			strError += "Channel width(s) cannot be negative to run model.\n";
		}
	}
	
	// Now check the downstream points themselves
	if (downstream.size() < 2)
		strError += "There must be at least two downstream points\n with at least two elevation-topwidth pairs in each.\n";	
	else {
		int j;
		int j1;
		for (j=0; j < downstream.size(); j++) {
			j1 = j+1; // j1 points to next downstream point
			if (j1 < downstream.size()) {

				if ( ((DownstreamPoint)downstream.get(j)).distanceToSection >= ((DownstreamPoint)downstream.get(j1)).distanceToSection ){
				
				strError += "River stations must be in descending order toward downstream.\n";
				strError += "Distance to sections:\n";
				strError += ((DownstreamPoint)downstream.get(j)).distanceToSection;
				strError += "\n";
				strError += ((DownstreamPoint)downstream.get(j1)).distanceToSection;
				strError += "\n";
				}
			}
			
			if ( ((DownstreamPoint)downstream.get(j)).xsections.size() == 0 )
			{
				strError += "Each downstream point must have\n the same number of elevation / width pairs (at least 2).\n";
				// break;
			}
				
		}
		
		// if (j == downstream.size()) { 		// this can never be true here !!!

	}
	
	int howManyPairs = AnalysisData.getNumPairs(downstream);

	if (howManyPairs == -2)
	{
			strError += "Each of the cross sections marked \"Best\" must have\n the same number of elevation / width pairs (at least 2).\n";
	}
	else if (howManyPairs < 2)
	{
			strError += "There must be at least two cross sections per downstream point.\n";
	}

	// check for at least one scenario
	if (scenarios.size() == 0)
		strError += "There must be at least one scenario for model to run. \n";
	else {
		// check scenarios
		for (int i=0; i<scenarios.size(); i++) {
			ModelScenario scenario = (ModelScenario)scenarios.get(i);

			if(scenario.source.startsWith("#"))
			{
				continue;
			}
			
			if (scenario.HDE < botElev) {
				strError += "Dam breach elevation (";
				strError += scenario.HDE + ") must be greater than\n channel bottom elevation (";
				strError += botElev + ").\n";
			}
			if (scenario.BME < botElev) {
				strError += "Final breach elevation (";
				strError += scenario.BME + ") must be greater than\n channel bottom elevation (";	
				strError += botElev + ").\n";
			}

			float hdm = scenario.HDE - scenario.BME;
			if (hdm < 0.0)
			{
				strError += "Initial breach elevation (";
				strError += scenario.HDE + ") must be greater than\n final breach elevation (";	
				strError += scenario.BME + ").\n";
			}		
			if (scenario.TFM <= 0.00f)
			{
				if (scenario.damType <= 0)
					scenario.TFM = hdm/10.0f;
				else if (scenario.damType == 1)
					scenario.TFM = hdm/40.0f;
				else if (scenario.damType == 2)
					scenario.TFM = hdm/50.0f;
			}

			if (scenario.BW <= 0.00f)
			{
				if (scenario.damType == 0)
					scenario.BW = 3.0f * hdm;
				else if (scenario.damType == 1)
					scenario.BW = 5.0f * hdm;
				else if (scenario.damType == 2)
					scenario.BW = 0.0f;
			}
			
			if (scenario.VOL <= 0.00f && scenario.SA <= 0.00f)
				strError += "Either volume or surface area must be greater than zero.\n" + "(Scenario #" + (i+1) + ")\n";
			else if (scenario.VOL <= 0.00f && scenario.SA != 0.00f)
				scenario.VOL = hdm * scenario.SA / 2.0f;
			else if (scenario.VOL != 0.00f && scenario.SA <= 0.00f)
				scenario.SA = 2.0f * scenario.VOL /hdm;
//			else if (scenario.VOL <= 0.00f && scenario.SA <= 0.00f)
//				strError += "Either VOL or SA must be greater than zero. " + "(Scenario #" + (i+1) + ")\n";

			if ( scenario.DISTTN > ((DownstreamPoint)downstream.get(downstream.size()-1)).distanceToSection ){

				strError += "Scenario ";
	 			strError += scenario.name;
	 			strError += "\nPoint of interest station= ";
				strError += scenario.DISTTN;
				strError += " exceeds last river station= ";
				strError += ((DownstreamPoint)downstream.get(downstream.size()-1)).distanceToSection;
				strError += "\n";			
			}

			if (pointOfInterestName.compareTo("") == 0)
				pointOfInterestName = "Town";

			if (scenario.CMS <= 0.00f)
				scenario.CMS = 0.5f;	
		} //for loop
	}

	if (strError.length() == 0)
		return null;
	else
		return strError;
}
/**
 * Insert the method's description here.
 * Creation date: (3/1/2004 3:06:06 PM)
 * @return java.lang.String
 */
public String verifyOutput() {
	String strError = "";
	String rvrName = "";

	DamInfo damInfo = new DamInfo();
	if (rootDamInfo.size() > 0)
		damInfo = (DamInfo) this.rootDamInfo.get(0);
	
	java.text.DecimalFormat df;
	df = new java.text.DecimalFormat("#####0.00");

	if(scenarios.size() == 0)
		bEmptyDataset = true;
	else
		bEmptyDataset = false;
	
	for (int j=0; j<scenarios.size(); j++)	 
	{	
		try {
			
		// System.out.println("***** Getting scenario " + j);
		ModelScenario scenario = (ModelScenario)scenarios.get(j);
		if(scenario.source.startsWith("#"))
		{
			continue;
		}
		StringBuffer prerunGenericBuffer = new StringBuffer();
		StringBuffer prerunTextBufferOne = new StringBuffer();
		StringBuffer prerunTextBufferTwoFirst = new StringBuffer();
		StringBuffer prerunTextBufferTwoSec = new StringBuffer();
		StringBuffer prerunTextBufferTwoThird = new StringBuffer();
		StringBuffer prerunTextBufferThree = new StringBuffer();
		StringBuffer prerunTextBufferFour = new StringBuffer();
		StringBuffer prerunTextBufferFive = new StringBuffer();
		
		prerunTextBufferOne.append("\n               ");
		prerunTextBufferOne.append("DAMBREAK ANALYSIS, VIEW CATALOG RESULTS\n");
		prerunTextBufferOne.append("                  ");
		prerunTextBufferOne.append("    NATIONAL WEATHER SERVICE\n\n");
		prerunTextBufferOne.append("    Name of Dam:                                      Name of River:\n");
		prerunTextBufferOne.append("    " + damName + "                                                  \n");   
				
		//int n = prerunTextBuffer.length();
		
		int posRiver = 227;
		
		int index = riverName.indexOf("(");
		if (index != -1)
		{
			String riverNameSubStr = riverName.substring(0, index);
			prerunTextBufferOne.insert( posRiver,riverNameSubStr);
		}
		else
		{
			prerunTextBufferOne.insert( posRiver, riverName);
		}
		prerunTextBufferOne.append("\n\n");

		prerunGenericBuffer.append(prerunTextBufferOne.toString());
		
		prerunTextBufferTwoFirst.append("    Primary Point Of Interest:           Distance (mi):     Time to Flood(min):\n");
		prerunTextBufferTwoFirst.append("    " + pointOfInterestName + "                                                          ");

		int posDist = 121;
		int posTime = 140;
		prerunTextBufferTwoFirst.append("\n");

		prerunTextBufferTwoSec.append("                                                    ");

		prerunTextBufferTwoSec.append("                              \n");
		
		prerunTextBufferTwoThird.append("                                                    ");

		prerunTextBufferTwoThird.append("                              \n");
		
		prerunTextBufferThree.append("    Time and Flood Wave Forecast:\n\n");
		prerunTextBufferThree.append("    Forecast           Distance  Max        Max     Max    Time to    Time   Time     Est Flood\n");
		prerunTextBufferThree.append("    Point              from Dam  Flow       Elev    Depth  Max Depth  Flood  Deflood  Stage\n");   
		prerunTextBufferThree.append("                                                                 "); 
		
	    prerunTextBufferThree.insert(225, "                     (miles)");
		prerunTextBufferThree.append("        ");
		
		prerunTextBufferThree.insert(253, "   (cfs)");

		prerunTextBufferThree.append("        "); 
		
		prerunTextBufferThree.insert(265, " (ft-msl)");

		prerunTextBufferThree.append("        "); 
		
		prerunTextBufferThree.insert(274, " (ft)");

		prerunTextBufferThree.append("        "); 
		
		prerunTextBufferThree.insert(281, " (min)");

		prerunTextBufferThree.append("        "); 
		
		prerunTextBufferThree.insert(292, " (min)");

		prerunTextBufferThree.append("        "); 
		
		prerunTextBufferThree.insert(299, " (min)");

		prerunTextBufferThree.append("        "); 
		
		prerunTextBufferThree.insert(308, " (ft)\n");

		//System.out.println("***** Getting scenario " + j);
		
		//ModelScenario scenario = (ModelScenario)scenarios.get(j); 
	
		for ( int k=0; k < scenario.output.inputDownstream.size(); k++)
		{
			// System.out.println("***** Getting downstream point " + k);
			
			DownstreamPoint down = (DownstreamPoint)scenario.output.inputDownstream.get(k);

			String pointOfInterest = down.name;
			
			String dist = df.format(down.distanceToSection);
			//System.out.println("Distance: " + dist);
			
			if (k == scenario.output.inputDownstream.size() - 1)
			{
				prerunTextBufferTwoFirst.insert(posDist, dist);
			}
			
			String mxFlow = df.format(scenario.output.maxFlow[k]);
			//System.out.println("maxFlow: " + scenario.output.maxFlow[k]);
			
			String mxDepth = df.format(scenario.output.maxDepth[k]);
			//System.out.println("maxDepth: " + scenario.output.maxDepth[k]);

			String mxElevation = df.format(scenario.output.maxElevation[k]);
			//System.out.println("maxElevation: " + scenario.output.maxElevation[k]);
			
			String tmMxDepth = df.format(scenario.output.timeMaxDepth[k] * 60) ;
			//System.out.println("timeMaxDepth: " + scenario.output.timeMaxDepth[k]);
	
			String tmFlood = df.format(scenario.output.timeFlood[k] * 60);
			//System.out.println("timeFlood: " + scenario.output.timeFlood[k]);

			if (k == scenario.output.inputDownstream.size() - 1)
			{
				prerunTextBufferTwoFirst.insert(posTime, tmFlood);
			}
					
			String tmDeflood = df.format(scenario.output.timeDeflood[k] * 60);
			//System.out.println("timeDeflood: " + scenario.output.timeDeflood[k]);

			String flDepth = df.format(down.floodDepth);
			//System.out.println("Flood depth: " + down.floodDepth);

			if (scenario.output.maxFlow[k] == 0.0f || scenario.output.maxElevation[k] == 0.0f || scenario.output.maxDepth[k] == 0.0f)
			{
				strError = "No stored forecast in the database,\n the model should run first.\n";
				break;
			}
			
			int maxLength = 19;
			String paddedPOI = pad(pointOfInterest, maxLength);
			prerunTextBufferFour.append("\n    " + paddedPOI);

			maxLength = 10;
			String paddedDist = pad(dist, maxLength);
			prerunTextBufferFour.append(paddedDist);

			maxLength = 11;
			String paddedMxFlow = pad(mxFlow, maxLength);
			prerunTextBufferFour.append(paddedMxFlow);

			maxLength = 9;
			String paddedMxElevation = pad(mxElevation, maxLength);
			prerunTextBufferFour.append(paddedMxElevation);

			maxLength = 7;
			String paddedMxDepth = pad(mxDepth, maxLength);
			prerunTextBufferFour.append(paddedMxDepth);

			maxLength = 10;
			String paddedTmMxDepth = pad(tmMxDepth, maxLength);
			prerunTextBufferFour.append(paddedTmMxDepth);

			maxLength = 7;
			String paddedTmFlood = pad(tmFlood, maxLength);
			prerunTextBufferFour.append(paddedTmFlood);

			maxLength = 9;
			String paddedTmDeflood = pad(tmDeflood, maxLength);
			prerunTextBufferFour.append(paddedTmDeflood);

			maxLength = 11;
			String paddedFlDepth = pad(flDepth, maxLength);
			prerunTextBufferFour.append(paddedFlDepth + "\n");
		}
		prerunGenericBuffer.append(prerunTextBufferTwoFirst.toString());
		prerunGenericBuffer.append(prerunTextBufferTwoSec.toString());
		prerunGenericBuffer.append(prerunTextBufferTwoThird.toString());
		prerunGenericBuffer.append(prerunTextBufferThree.toString());
		prerunGenericBuffer.append(prerunTextBufferFour.toString());

		prerunTextBufferFive.append("\n");
		if (rootDamInfo.size() > 0)
		{
			prerunTextBufferFive.append("    Dam Information:\n");
			prerunTextBufferFive.append("           NID ID: " + damInfo.nidid + "\n");
			prerunTextBufferFive.append("           Latitude: " + damInfo.latitude_dam + "\n");
			prerunTextBufferFive.append("           Longitude: " + damInfo.longitude_dam + "\n");	
			prerunTextBufferFive.append("           State ID: " +  damInfo.stateid + "\n");
			prerunTextBufferFive.append("           County: " +  damInfo.county +"\n\n");
		}
		
		prerunTextBufferFive.append("    Inputs for this Forecast:\n");
		prerunTextBufferFive.append("           Source: " + scenario.source + "\n");
		prerunTextBufferFive.append("           Scenario: " + scenario.name + "\n");
		prerunTextBufferFive.append("           Starting Water Surface [HDE] (ft MSL): " + scenario.HDE + "\n"); 
		prerunTextBufferFive.append("           Bottom of Breach Width [BME] (ft MSL): " + scenario.BME + "\n");
		prerunTextBufferFive.append("           Starting Volume [VOL] (acre-ft): " + scenario.VOL + "\n");
		prerunTextBufferFive.append("           Starting Surface Area [SA] (acre): " + scenario.SA + "\n");
		prerunTextBufferFive.append("           Time to Failure (min): " + scenario.TFM + "\n");
		prerunTextBufferFive.append("           Additional Flow to Add [QO] (cfs): " + scenario.QO + "\n");
		prerunTextBufferFive.append("           Final Breach Width [BW] (ft): " + scenario.BW + "\n");
		prerunTextBufferFive.append("           Dam Type Code: " + scenario.damType + "\n");
			
		prerunGenericBuffer.append(prerunTextBufferFive.toString());

		
		scenario.output.prerunText = prerunGenericBuffer.toString();
		
		//scenario.output.prerunText.setFont(new java.awt.Font("Courier", Font.PLAIN, 14));
		
		scenario.output.bHasPrerunText = true;
		
		//scenario.output.fullText = prerunGenericBuffer.toString();
		//scenario.output.bHasFullText = true;
		} catch (Exception e) {
			System.out.println("Exception in AnalysisData.verifyOutput()");
			e.printStackTrace();
		}
	}
	
	return strError;
}
	public static boolean writeDAM(AnalysisData data, String filename) {
		if (filename == null)
			return false;
		
		String lineEnd = "\n";
		FileWriter output;
		try {
			
			// open the file
			output = new FileWriter(filename);

			// write out type/version information
			output.write("Dambreak Analysis|1|"+lineEnd);

			// write out name information
			output.write(data.damName+" |"+data.riverName+" |"+data.pointOfInterestName+" |"+lineEnd);

			// write out the number of input scenarios
			output.write(data.scenarios.size()+"|"+lineEnd);

			// write out input scenarios
			for (int i=0; i<data.scenarios.size(); i++) {
				ModelScenario s = (ModelScenario)data.scenarios.get(i);
				output.write(s.source+" |"+s.name+" |"+NumFormat.format(s.HDE,4)+"|"+NumFormat.format(s.BME,4)+"|"+NumFormat.format(s.BW,4)+"|"+NumFormat.format(s.CMS,4)+"|"+NumFormat.format(s.DISTTN,4)+"|"+NumFormat.format(s.QO,4)+"|"+NumFormat.format(s.SA,4)+"|"+NumFormat.format(s.TFM,4)+"|"+NumFormat.format(s.VOL,4)+"|"+lineEnd);
			}

			// write out the number of downstream points
			output.write(data.downstream.size()+"|"+lineEnd);
			
			// write out downstream points
			for (int i=0; i<data.downstream.size(); i++) {
				DownstreamPoint d = (DownstreamPoint)data.downstream.get(i);
				output.write(d.name+" |"+NumFormat.format(d.distanceToSection,4)+"|"+NumFormat.format(d.floodDepth,4)+"|"+NumFormat.format(d.floodFlow,4)+"|"+NumFormat.format(d.latitude,4)+"|"+NumFormat.format(d.longitude,4)+"|"+d.bestXS+"|"+lineEnd);

				// write out the number of cross sections at this downstream point
				output.write(d.xsections.size()+"|"+lineEnd);

				// write out cross sections
				for (int j=0; j<d.xsections.size(); j++) {
					SectionGeometry sg = (SectionGeometry)d.xsections.get(j);
					output.write(sg.getXSType()+"|"+lineEnd);
					for (int k=0; k<8; k++) {
						output.write(NumFormat.format(sg.getElevationData(k,0),4)+"|"+NumFormat.format(sg.getElevationData(k,1),4)+"|"+NumFormat.format(sg.getElevationData(k,2),4)+"|"+NumFormat.format(sg.getElevationData(k,3),4)+"|"+lineEnd);
					}
				}
			}

			///////////////////////////////////////////////////////////////////////////////////////////////
			// we don't need to write out the output scenarios because they will be regenerated if needed
			///////////////////////////////////////////////////////////////////////////////////////////////

			// *** is above statement true ???
			
			// close the file
			output.close();
			
		} catch (IOException e) {
			System.out.println("IOException Caught In AnalysisData.writeDAM");
			e.printStackTrace();
			return false;
		}
		catch (Exception ex) {
			System.out.println("Exception Caught In AnalysisData.writeDAM");
			ex.printStackTrace();
			return false;
		}
		return true;
	}
}
