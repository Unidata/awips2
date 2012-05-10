package gov.noaa.nws.ncep.standalone.miscsetConverter;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import gov.noaa.nws.ncep.standalone.miscsetConverter.AttributeTable.AttributeEntry;
import gov.noaa.nws.ncep.standalone.util.Util;


//import com.raytheon.uf.common.serialization.SerializationException;
//import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * MiscsetConvert
 * 
 * Standalone conversion program to convert legacy MISC Data Source settings table
 * ($GEMTBL/config/miscset.tbl, with possible field customizations) into corresponding
 * structures in the new system.
 * 
 * This is one of several programs in a suite intended to permit one-time conversion
 * of legacy Stored Procedure Files (SPFs) and all of their constituent subparts.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 21 Mar 2011  397         B. Hebbard  Initial creation
 * </pre>
 * 
 * @author B. Hebbard
 * @version 1
 */

public class MiscsetConvert  {

	/** Help file where the program description is stored. */
	public static String  HELP_FILE_NAME = "miscsetconvert.hlp";
	
	private final static String defaultInputFile = "$GEMTBL/config/miscset.tbl";
	
	private final static String defaultOutputDir = "$PWD";

	private SortedMap<String, List<TypeRecord>> aliasToTypesMap;
	private SortedMap<String, List<FlagRecord>> aliasToFlagsMap;
	
	/**
	 * The constructor.
	 */
	public MiscsetConvert() {
		aliasToTypesMap = new TreeMap<String, List<TypeRecord>>();
		aliasToFlagsMap = new TreeMap<String, List<FlagRecord>>();
	}
	
	/**
     * ingest the miscset table
     * 
     * @return boolean success indicator
     */
	public boolean ingestMiscsetFile(String miscsetTable) {
		File miscsetFile = new File(miscsetTable);

		//int  lineCount=0;
		try {
			BufferedReader br = new BufferedReader(new FileReader(miscsetFile));
			String nextLine = br.readLine();
			while (nextLine != null) {  // quit at EOF
				nextLine = nextLine.trim();
				if ((nextLine.startsWith("!")) || nextLine == "\n") {  // skip blank/comment lines
					nextLine = br.readLine();
				}
				else {
					if (!nextLine.startsWith("ALIAS")) {
						// sanity:  first nonblank/noncomment line should start with "ALIAS"
						System.out.println("[Error:  Expecting line starting with 'ALIAS' but found '" + nextLine + "']");
						nextLine = br.readLine();
					}
					else {
						// have an ALIAS line; make sure it matches pattern, and get model and category names
						Pattern p1 = Pattern.compile("^ALIAS[\\s]+([\\w\\-]+)$");  //TODO:  Refine pattern?
						Matcher m1 = p1.matcher(nextLine);
						if (!m1.find()) {
							System.out.println("[Error:  Malformed line '" + nextLine + "']");
							nextLine = br.readLine();
						}
						else {
							String aliasName = m1.group(1);
							// now read the following lines, to get TYPE and FLAG values
							List<TypeRecord> typeList = new ArrayList<TypeRecord>();
							List<FlagRecord> flagList = new ArrayList<FlagRecord>();
							String flagName = null;
							String flagValue = null;
							nextLine = br.readLine();
							while (nextLine != null &&
									!nextLine.trim().startsWith("ALIAS")) {
								// have read a line within the alias definition block
								if ((nextLine.trim().startsWith("!")) || nextLine.trim() == "\n") {  // skip comment or blank
									nextLine = br.readLine();
								}
								else {
									nextLine = nextLine.trim();
									if (nextLine.startsWith("TYPE")) {
										// have an TYPE line; make sure it matches pattern, and get value string -- ignoring the display name of the type!
										//Pattern p2 = Pattern.compile("^TYPE[\\s]+([\\w\\-]+)[\\s]+([\\S]+)$");  //TODO:  Refine pattern!
										//Matcher m2 = p2.matcher(nextLine);
										/* if (false) {
											System.out.println("[Error:  Malformed line '" + nextLine + "']");
											nextLine = br.readLine();
										}
										else { */
											//String typeValue = m2.group(2);
											String typeName = nextLine.substring(5,27).trim();
											String typeValue = nextLine.substring(28).trim();
											String arrowValue = null;
											String lineValue = null;
											String sym1Value = null;
											String sym2Value = null;
											nextLine = br.readLine();
											while (nextLine != null &&
													!nextLine.trim().startsWith("ALIAS") &&
													!nextLine.trim().startsWith("TYPE") &&
													!nextLine.trim().startsWith("FLAG")) {
												nextLine = nextLine.trim();
												if (nextLine.startsWith("!") || nextLine == "\n") {
													// comment or blank -- accept, but do nothing
												}
												else if (nextLine.startsWith("ARRW")) {
													if (arrowValue != null) {													
														System.out.println("[Warning:  Duplicate entry for ARRW under TYPE " + typeName + " of ALIAS " + aliasName + "]");
													}
													arrowValue = nextLine.substring(nextLine.indexOf(" ")+1);
												}
												else if (nextLine.startsWith("LINE")) {
													if (lineValue != null) {													
														System.out.println("[Warning:  Duplicate entry for LINE under TYPE " + typeName + " of ALIAS " + aliasName + "]");
													}
													lineValue = nextLine.substring(nextLine.indexOf(" ")+1);
												}
												else if (nextLine.startsWith("SYM1")) {
													if (sym1Value != null) {													
														System.out.println("[Warning:  Duplicate entry for SYM1 under TYPE " + typeName + " of ALIAS " + aliasName + "]");
													}
													sym1Value = nextLine.substring(nextLine.indexOf(" ")+1);
												}
												else if (nextLine.startsWith("SYM2")) {
													if (sym2Value != null) {													
														System.out.println("[Warning:  Duplicate entry for SYM2 under TYPE " + typeName + " of ALIAS " + aliasName + "]");
													}
													sym2Value = nextLine.substring(nextLine.indexOf(" ")+1);
												}
												else {
													System.out.println("[Error:  Unrecognized word at beginning of line:  " + nextLine + " ]");
												}
												nextLine = br.readLine();
											}
											//done with TYPE; finish structure; add to array of TYPE structures?
											TypeRecord vtr = new TypeRecord(typeName, typeValue, arrowValue, lineValue, sym1Value, sym2Value);
											typeList.add(vtr);
										/* } */
									}
									else if (nextLine.startsWith("FLAG")) {
										flagName = nextLine.substring(5,29).trim();
										//flagValue = nextLine.substring(nextLine.indexOf(" ")+1);
										flagValue = nextLine.substring(30).trim();
										//create FLAG structure?; add to array of FLAG structures?
										flagList.add(new FlagRecord(flagName, flagValue));
										nextLine = br.readLine();
									}
								}
							}
							//done with alias...?  add to map, keyed on alias name...
							aliasToTypesMap.put(aliasName, typeList);
							aliasToFlagsMap.put(aliasName, flagList);
						}
					}
					//lineCount++;
				}
			} 

		}
		catch (IOException e) {
			System.err.println("[Error: " + e + "]");
			return false;
		};
		return true;
	}

	/**
     * convert the miscset file from .tbl to .xml
     * 
     * @return nothing at all
     */
	public void convert(String inFile, String outDir) {
		
        System.out.println("\n[Info:  Beginning generation of new MISC attribute files based on " + inFile + "...]");

		if (ingestMiscsetFile(inFile)) { //"$GEMTBL/config/miscset.tbl"
		
		    writeAllAttrFiles(outDir);
		
	        System.out.println("\n[Success:  The file is converted.  " + "The Conversion is finished.]\n");
	        
		}

	}

	private void writeAllAttrFiles(String outDir) {

		for (AttributeEntry ae : AttributeTable.getInstance().getAttrList()) {
			writeOneAttrFile (ae, outDir);
		}
		
	}
	
	private void writeOneAttrFile(AttributeEntry ae, String outDir) {
		//TODO:  Review!!!
		List<TypeRecord> typeList = aliasToTypesMap.get(ae.getLegacyName());
		List<FlagRecord> flagList = aliasToFlagsMap.get(ae.getLegacyName());
		String outputFile = outDir + "/" + ae.getOutputFileName();
		
		// Create non-existing directories
		int  lind = outputFile.lastIndexOf( "/" );
  	    if ( lind >= 0 ) {
  			String fdir= outputFile.substring( 0, lind );
  			File checkDir = new File( fdir );
  		    if ( !(checkDir.exists() && checkDir.isDirectory() ) ) {
  		    	checkDir.mkdirs();	 
   		    }			
  	    }

		System.out.println( "\n[Info:  Creating " + outputFile + " using legacy ALIAS " + ae.getLegacyName() + " ...]");
		try {
			Writer output = new BufferedWriter(new FileWriter(new File(outputFile)));
			for (AttrLn al : ae.getAttrLines()) {
				String newLine = al.getNewString(typeList, flagList) + "\n";
				writeOneAttrLine(output, newLine);
			}
			output.close();
			System.out.println( "[Info:        ..." + outputFile + " created successfully]");
		}
		catch (IOException e) {			
			System.out.println( "[Error:  Could not create output file " + outputFile + "]");
		}
		finally {
		}
	}
	
	private void writeOneAttrLine(Writer output, String line) {
		try {
			output.write(line);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static String substituteEnvVars(String filepath) {
		final Map<String,String> envVars = System.getenv();
		Pattern p = Pattern.compile("\\$([\\p{Alpha}_][\\p{Alnum}_]*)");
		//Pattern p = Pattern.compile("\\$(.*?)\\/");
		Matcher m = p.matcher(filepath);
		if (m.find()) {
			String var = m.group(1);
			String dir = envVars.get(var);
			if (dir != null) {
			    filepath = filepath.replace("$" + var, dir);
			}
		}
		return filepath;
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
	
	public static void main(String[] args) {

		/**
		 * Entry point for command line.
		 * 
		 * @param args
		 */
		
		if (args.length != 0 && (args[0].equals("--help") || args[0].equalsIgnoreCase("/h"))) {
			System.out.println(getProgramDescription());
			return;
		} // else proceed
		
		boolean haveValidInput = false;
		boolean haveValidOutput = false;
		
		String inputFileStr = null;
		if (args.length > 0) {
			inputFileStr = args[0];
		}
		while (!haveValidInput) {
			//if we don't have a string, prompt for it (and validate it?)
			while (inputFileStr == null) {
				System.out.println("Enter legacy table (default " + defaultInputFile + "):  ");
				BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
				try {
					inputFileStr = in.readLine();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (inputFileStr.isEmpty()) {
					inputFileStr = defaultInputFile;
				}
				//System.out.println("Echo: " + inputFileStr);
				inputFileStr = substituteEnvVars(inputFileStr);
				//System.out.println("Echo: " + inputFileStr);
			}
			//try to open the string we have (exists? readable?)
			File inputFile = new File(inputFileStr);
			if (!inputFile.exists()) {
				System.out.println("File doesn't exist.  Try again...");
				inputFileStr = null;
			}
			else if (!inputFile.canRead()) {
				System.out.println("File can't be read.  Try again...");
				inputFileStr = null;
			}
			else {
				haveValidInput = true;
			}
		}
		
		String outputDirStr = null;
		if (args.length > 1) {
			outputDirStr = args[1];
		}
		
		while (!haveValidOutput) {
			//if we don't have a string, prompt for it (and validate it?)
			while (outputDirStr == null) {
				System.out.println("Enter output directory (default " + defaultOutputDir + "):  ");
				BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
				try {
					outputDirStr = in.readLine();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (outputDirStr.isEmpty()) {
					outputDirStr = defaultOutputDir;
				}
				//System.out.println("Echo: " + outputDirStr);
				outputDirStr = substituteEnvVars(outputDirStr);
				//System.out.println("Echo: " + outputDirStr);
			}
			//try to open the string we have (exists? writable?)
			File outputDir = new File(outputDirStr);
			if (!outputDir.exists()) {
				System.out.println("Output directory " + outputDir + " doesn't exist.  Try again...");
				outputDirStr = null;
			}
			else if (!outputDir.canWrite()) {
				System.out.println("Output directory " + outputDir + " can't be written.  Try again...");
				outputDirStr = null;
			}
			else {
				haveValidOutput = true;
			}
		}
		
		new MiscsetConvert().convert(inputFileStr, outputDirStr);
		
	}
}
