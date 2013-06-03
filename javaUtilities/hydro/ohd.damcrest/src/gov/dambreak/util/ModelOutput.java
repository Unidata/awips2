package gov.dambreak.util;

import java.io.*;
import java.util.*;
import javax.swing.*;

/**
 * This class allows for the reading of SMPDBK output files.
 */
public class ModelOutput {

	// public fields
	public ModelScenario	inputScenario = new ModelScenario();
	public ArrayList		inputDownstream = new ArrayList();	// DownstreamPoint
	
	public String	source;
	public String	modelScenario;
	public String	xsectionType;
	
	public String	damName;
	public String	riverName;
	public String	pointOfInterestName;
	
	public boolean	bHasWarning;
	public String	warning;
	public boolean	bHasFullText;
	public String	fullText;
	public boolean	bHasPrerunText;
	public String	prerunText;
	
	public int changeFlag = 0;						// 0 = no changes; 1 = updated; 2 = inserted; 3 = deleted
	
	// output data
	public int		numberXSections = 0;

	public float	outSlope = 0.0f;
	public float	maxFl  = 0.0f;
	public float	maxEl  = 0.0f;
	public float	maxDep  = 0.0f;
	public float	timeMaxDep  = 0.0f;
	public float	timeFl  = 0.0f;
	public float	timeDefl  = 0.0f;
	
	public float[] maxFlow = new float[50];
	public float[] maxElevation = new float[50];
	public float[] maxDepth = new float[50];
	public float[] timeMaxDepth = new float[50];
	public float[] timeFlood = new float[50];
	public float[] timeDeflood = new float[50];
/**
 * Insert the method's description here.
 * Creation date: (7/22/2003 9:00:35 AM)
 */
public ModelOutput() {
	bHasFullText = false;
	bHasPrerunText = false;
	bHasWarning = false;
	numberXSections = 0;

	damName = "";
	riverName = "";
	pointOfInterestName = "";
	warning = "";
	fullText = "";
}
/**
 * Insert the method's description here.
 * Creation date: (7/23/2003 2:20:43 PM)
 * @return int
 */
public int getPointOfInterestXS() {
	int count = 0;
	try {
		if (inputScenario.DISTTN == 0.0)
			return inputDownstream.size()-1;

		count = 0;

		// find XS just past DISTTN
//System.out.print("junk getPointOfInterestXS: " + count + "!!!!!\n");
//System.out.print("junk getPointOfInterestXS: " + inputScenario.DISTTN + "!!!!!\n");
//System.out.print("junk getPointOfInterestXS: " + ((DownstreamPoint)inputDownstream.get(count)).distanceToSection + "!!!!!\n");
		while (	count < inputDownstream.size() &&
				inputScenario.DISTTN > ((DownstreamPoint)inputDownstream.get(count)).distanceToSection)
			count++;
	} catch (Exception e) {
		System.out.println("Exception in ModelOutput.getPointOfInterest()");
		e.printStackTrace();
	}
	return count;
}
	// private methods
	private static String[] parseLine(String line) {
		int colCount = -1;
		int position = 0;
	
		String result[] = new String[8];
		for (int i=0; i<8; i++)
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
	private static String[] parseLine2(String line) {
		int colCount = -1;
		int position = 2;
	
		String result[] = new String[9];
		for (int i=0; i<9; i++)
			result[i] = new String();
	
		while (position < line.length())
		{
			if (((position-2) % 8) == 0)
				colCount++;
	
			if (line.charAt(position) != ' ')
				result[colCount] += line.charAt(position);
	
			position++;
		}	
		return result;
	}
	/**
	 * This method reads the input portion of the *.OUT file.
	 */
	public static void readInputData(BufferedReader reader, ModelOutput retVal) throws IOException 
	{
		String line;
		String[] lineData;

		//read dam name
		line = reader.readLine();
		retVal.inputScenario = new ModelScenario();
		retVal.damName = line.substring(13,line.length());
		retVal.damName = retVal.damName.trim();

		//read river name and point of interest name
		line = reader.readLine();
		if (line.length() > 55)
		{
			retVal.riverName = line.substring(13,55);
			retVal.pointOfInterestName = line.substring(55,line.length());
		}
		else // no point of interest
		{
			retVal.riverName = line.substring(13,line.length());
			retVal.pointOfInterestName = "";
		}

		retVal.riverName = retVal.riverName.trim();
		retVal.pointOfInterestName = retVal.pointOfInterestName.trim();
	
		//read through next 4 lines 
		for (int i=0; i<4; i++)
			line = reader.readLine();
		
		// parse unique data line
		retVal.inputScenario.HDE = Float.parseFloat(line.substring(15,23).trim());
		retVal.inputScenario.BME = Float.parseFloat(line.substring(23,31).trim());
		retVal.inputScenario.VOL = Float.parseFloat(line.substring(31,41).trim());
		retVal.inputScenario.SA = Float.parseFloat(line.substring(41,49).trim());
		retVal.inputScenario.BW = Float.parseFloat(line.substring(49,57).trim());
		retVal.inputScenario.TFM = Float.parseFloat(line.substring(57,65).trim());
		retVal.inputScenario.QO = Float.parseFloat(line.substring(65,73).trim());
	}
	public static ModelOutput readOUT(String filename) {
		
		BufferedReader reader;
		String line;
		ModelOutput retVal = new ModelOutput();
		
		try {
			// first read in the entire file
			reader = new BufferedReader(new FileReader(filename));
			StringBuffer fileTextBuffer = new StringBuffer();
			while ((line = reader.readLine()) != null)
				fileTextBuffer.append(line + "\n");
			retVal.fullText = fileTextBuffer.toString();
			retVal.bHasFullText = true;
			reader.close();

			// next read and parse the file
			reader = new BufferedReader(new FileReader(filename));
			
			// skip past header lines in output file
			// "
			// SIMPLIFIED DAMBR...
			// BY D.L. FREAD, J.M LEWIS...
			// NWS HYDROLOGIC RESEA...
			//
			//                              "
			for (int i=0; i<6; i++)
				line = reader.readLine();

			// read input data into fields
			readInputData(reader,retVal);

			// read section elevation data into double type [][]
			readSectionElevations(reader,retVal);

			// look for output data table and any possible warnings
			do {
				line = reader.readLine();
				
				// check for a warning
				if (line.indexOf("W A R N I N G") != -1) {
					reader.readLine();
					for (int i=0; i<5; i++)
						retVal.warning += reader.readLine().trim() + "\n";
					retVal.warning += "\n";
					retVal.bHasWarning = true;
				}
			} while (line.compareTo("  ********  ********  ********  ********  ********  ********  ********  ********") != 0);

			// read output data into fields
			if (!readOutputData(reader,retVal))
				return null;
			
		} catch (Exception e) {
			System.out.println("Exception in OutputFileParser::parse()");
			e.printStackTrace();
			return null;
		}
		return retVal;
	}
	/**
	 * This method reads the output portion of the *.OUT file.
	 */
	public static boolean readOutputData(BufferedReader reader, ModelOutput retVal) throws IOException {
		String line;
		String lineSave = "";
		StringBuffer warningNan = new StringBuffer();
		
		String[] lineData;
		int count=0;
		
		line = reader.readLine();
		if (line == null)
		{
			System.out.println("In ModelOutput.readOutputData -- null line read !");
			return false;
		}
		while (line != null) {
			
			if (line.trim().compareTo("") == 0) {
				line = reader.readLine();
				continue;
			}

			
			if (line.trim().startsWith("ANALYSIS IS COMPLETE")) {
				line = reader.readLine();
				continue;
			}

			if (line.trim().startsWith("CANNOT CONVERGE")) {
				retVal.bHasWarning = true;
				retVal.warning += line + "\n";
				line = reader.readLine();
				continue;
			}

			if (line.trim().startsWith("(Q=")) {
				retVal.warning += line + "\n";
				line = reader.readLine();
				continue;
			}

			// *** These Lines are generated by SMPDBK whenever JNK > 0

			if (line.trim().startsWith("F1=")) {
				line = reader.readLine();
				continue;
			}

			if (line.trim().startsWith("QS=")) {
				line = reader.readLine();
				continue;
			}
	

			if (line.trim().startsWith("QA=")) {
				line = reader.readLine();
				continue;
			}
			

			if (line.trim().startsWith("XX=")) {
				line = reader.readLine();
				continue;
			}
			

			if (line.trim().startsWith("K=")) {
				line = reader.readLine();
				continue;
			}
			

			if (line.trim().startsWith("SUB=")) {
				line = reader.readLine();
				continue;
			}
			
			
			try {
				
				lineData = parseLine(line);
			
				retVal.maxFlow[count] = Float.parseFloat(lineData[1]);
				retVal.maxElevation[count] = Float.parseFloat(lineData[2]);
				retVal.maxDepth[count] = Float.parseFloat(lineData[3]);
				retVal.timeMaxDepth[count] = Float.parseFloat(lineData[4]);
				retVal.timeFlood[count] = Float.parseFloat(lineData[5]);
				retVal.timeDeflood[count] = Float.parseFloat(lineData[6]);
				count++;
				
			} catch (NumberFormatException ex) {
				System.out.println("In ModelOutput.readOutputData - Number Format Exception on this line: ");
				System.out.println(line);
				warningNan.append(line + "\n");	
				
				ex.printStackTrace();
				if (line.trim().compareTo("ANALYSIS IS COMPLETE") != 0) {
					// stuff other than output data was included in the model results
					// we will show this "stuff" to the user in the "Warnings" tab of the
					// output viewer
					retVal.bHasWarning = true;
					retVal.warning += line + "\n";
				}
			}
			catch (Exception e) {
				System.out.println("In ModelOutput.readOutputData - generic Exception on this line ;");
				System.out.println(line);
				e.printStackTrace();
				
			}
			line = reader.readLine();
		}

		if (retVal.bHasWarning) {
			if(retVal.warning.startsWith("ORIGINAL BREACH WIDTH"))
				JOptionPane.showMessageDialog(null,"Warning Warning Warning: \n\n"
				+ retVal.warning, "Warning", JOptionPane.WARNING_MESSAGE);
			else
			{
				retVal.warning = "The Model Output Results are Incomplete, See Details in the Model Run Output File.\n\n";
				JOptionPane.showMessageDialog(null,"Warning Warning Warning: \n\n"
				+ retVal.warning + warningNan + "\n", "Warning", JOptionPane.WARNING_MESSAGE);
			}	
		}

		return true;
	}
	/**
	 * This method reads cross section elevations,tWidthsBS,inactiveTwBss from *.OUT file.
	 */
	public static void readSectionElevations(BufferedReader reader, ModelOutput retVal) throws IOException {
		String line;
		String[] lineData;
		
		// read how many sections from file *.OUT.
		line = reader.readLine();

		int numberXSections = Integer.parseInt(line.substring(18,21).trim());
	
		// read how many elevations in each section from file *.OUT.
		int numberXSectionElevations = Integer.parseInt(line.substring(30,32).trim());
		
		// get DISTTN
		retVal.inputScenario.DISTTN = Float.parseFloat(line.substring(56,line.length()).trim());

		// read elevations,tWidthsBS,inactiveTwBss from *.OUT file
		for (int j = 0; j < numberXSections; j++) {
			line = reader.readLine();
			line = reader.readLine();

			DownstreamPoint down = new DownstreamPoint();
			SectionGeometry sg = new SectionGeometry();
			
			// C6-j line : Downstream point information
			down.distanceToSection = NumFormat.toFloat(line.substring(17,26).trim());
			if (down.distanceToSection == 0)
				down.name = "Dam";
			else
				down.name = "Town";		// *** This needs to be replaced with original names !
			// down.floodDepth = NumFormat.toFloat(line.substring(34,line.length()).trim());
			down.floodDepth = NumFormat.toFloat(line.substring(34,41).trim());
			
			//down.latitude = NumFormat.toFloat(line.substring(52,61).trim());
			//down.longitude = NumFormat.toFloat(line.substring(71,80).trim());
			
			
			for (int k = 0; k < 8; k++) {
				// C7-k : B-vs-H pairs
				if (k < numberXSectionElevations)
				{
					line = reader.readLine();
					lineData = parseLine2(line);
					sg.setElevationData(k, 0, NumFormat.toFloat(lineData[2]));
					sg.setElevationData(k, 1, NumFormat.toFloat(lineData[4]));
					sg.setElevationData(k, 2, NumFormat.toFloat(lineData[6]));
					sg.setElevationData(k, 3, NumFormat.toFloat(lineData[8]));
					sg.setLastRowUsed(k);
				}
				else
				{
					// -999.0 is used to represent a nonexistent pair
					sg.setElevationData(k, 0, -999.0f);
					sg.setElevationData(k, 1, -999.0f);
					sg.setElevationData(k, 2, -999.0f);
					sg.setElevationData(k, 3, -999.0f);
				}
			}

			down.bestXS = 0;
			down.xsections.add(sg);
			retVal.inputDownstream.add(down);
		}
	}
	public static boolean verify(String filename) {
		
		if (!(new File(filename).exists()))
		{
			System.out.println("In ModelOutput.verify - " + filename + " failed existence test");
			return false;
		}
		BufferedReader input;
		String line;
		try {
			input = new BufferedReader(new FileReader(filename));
			
			line = input.readLine();
			while (line != null && line.compareTo("  ********  ********  ********  ********  ********  ********  ********  ********") != 0) {
				line = input.readLine();
			}
			if (input.readLine() == null)
			{
				System.out.println("In ModelOutput.verify - null line read");
				return false;
			}
		} catch (Exception e) {
			System.out.println("Exception Caught In ModelOutput.verify");
			e.printStackTrace();
			return false;
		}
		return true;
	}
}
