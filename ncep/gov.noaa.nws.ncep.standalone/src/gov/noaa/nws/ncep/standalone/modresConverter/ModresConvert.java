package gov.noaa.nws.ncep.standalone.modresConverter;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import gov.noaa.nws.ncep.standalone.util.Util;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroup;
import gov.noaa.nws.ncep.viz.resources.manager.AttrSetGroupList;


/**
 * ModresConvert
 * 
 * Standalone conversion program to convert legacy Model Product Selection table
 * ($GEMTBL/nmap/mod_res.tbl, with possible field customizations) into corresponding
 * files in the new system.
 * 
 * This is one of several programs in a suite intended to permit one-time conversion
 * of legacy Stored Procedure Files (SPFs) and all of their constituent subparts.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07 Feb 2011  397         B. Hebbard  Initial creation
 * 27 Oct 2011  397         B. Hebbard  Modify to follow split of single AttributeSetGroupList XML
 *                                      to multiple separate AttributeSetGroup XML files
 * 02 Feb 2012              B. Hebbard  Add FILTER to GempakParametersToTransfer (optional)
 * </pre>
 * 
 * @author B. Hebbard
 * @version 1
 */

public class ModresConvert  {

	/** Help file where the program description is stored. */
	public static String HELP_FILE_NAME = "modresconvert.hlp";
	
	private final static String defaultInputFile = "$GEMTBL/nmap/mod_res.tbl";
	
	private final static String defaultOutputDir = "$PWD";
	
	// [NO LONGER USED] private final static String attrSetGroupListOutputFileName = "ModelFcstGridContours.xml";
	
	private SortedMap<String, AttrSetGroup> attrSetGroupMap;
	private Map<String, String> restoreFileToAttrFileMap;
	
	/**
	 * The constructor.
	 */
	public ModresConvert() {
		attrSetGroupMap = new TreeMap<String, AttrSetGroup>();
		restoreFileToAttrFileMap = new HashMap<String, String>();
	}
	
	private enum GempakParameterPriority {
		REQUIRED , OPTIONAL /* , EXCLUDED */;  //TODO:  Need a default-value-required...?
	}

	private enum GempakParametersToTransfer {
		
		//  This is the list of parameters that are permitted to be transferred.
		
		//  Parameters listed here will --if present in the legacy restore file --
		//  be transferred forward to the new attribute file (along with whatever
		//  value was specified in the legacy file, even if null).  Parameters
		//  not listed here will be excluded from the transfer; that is, they will
		//  not appear at all in the generated attribute file.
		//
		//  If a parameter in this list does not appear in the legacy file at all,
		//  the "priority" parameter determines whether it will be inserted in the
		//  generated attribute file:  If REQUIRED, it will be forced into the new
		//  file, along with a default value as specified here.  If OPTIONAL, it
		//  will not appear in the converted attribute file if not in the legacy one.
		
		GLEVEL  ( GempakParameterPriority.OPTIONAL, "" ) ,
		GVCORD  ( GempakParameterPriority.OPTIONAL, "" ) ,
		SKIP    ( GempakParameterPriority.OPTIONAL, "" ) ,
		IJSKIP  ( GempakParameterPriority.OPTIONAL, "" ) ,
		SCALE   ( GempakParameterPriority.OPTIONAL, "" ) ,
		GDPFUN  ( GempakParameterPriority.OPTIONAL, "" ) ,
		TYPE    ( GempakParameterPriority.OPTIONAL, "" ) ,
		CINT    ( GempakParameterPriority.OPTIONAL, "" ) ,
		LINE    ( GempakParameterPriority.OPTIONAL, "" ) ,
		FINT    ( GempakParameterPriority.OPTIONAL, "" ) ,
		FLINE   ( GempakParameterPriority.OPTIONAL, "" ) ,
		HILO    ( GempakParameterPriority.OPTIONAL, "" ) ,
		HLSYM   ( GempakParameterPriority.OPTIONAL, "" ) ,
		WIND    ( GempakParameterPriority.OPTIONAL, "" ) ,
		TITLE   ( GempakParameterPriority.OPTIONAL, "" ) ,
		COLORS  ( GempakParameterPriority.REQUIRED, "2" ) ,
		MARKER  ( GempakParameterPriority.REQUIRED, "2" ) ,
		GRDLBL  ( GempakParameterPriority.REQUIRED, "5" ) ,
		FILTER  ( GempakParameterPriority.OPTIONAL, "" ) ;
		
		private GempakParameterPriority priority;
		private String defaultValue;
		
		//  Constructor
		GempakParametersToTransfer (GempakParameterPriority priority, String defaultValue) {
			this.priority = priority;
			this.defaultValue = defaultValue;
		}
		
		public GempakParameterPriority getPriority() {
			return priority;
		}
		
		public String getDefaultValue() {
			return defaultValue;
		}
	}

	public static Map<String, GempakParametersToTransfer> gempakParametersMap = new HashMap<String, GempakParametersToTransfer>();
	static {
		//  Dumb.  Just maps a string back to its enum value.  Why?  valueOf() conversion throws an exception
		//  if string isn't in the enum, and we want to reserve exceptions for exceptional conditions.  Here
		//  we might have this happen a lot -- a string from legacy GEMPAK that's not in the list of things we
		//  want to carry forward.  So here we can use a keySet().contains() type of thing...
		for (GempakParametersToTransfer param : GempakParametersToTransfer.values()) {
			gempakParametersMap.put(param.toString(),param);
		}
	}
	
	/**
     * ingest the modres table
     * 
     * @return boolean success indicator
     */
	public boolean ingestModres(String modresTable) {
		System.out.println("\n[Info:  Reading legacy model product selection table " + modresTable + "...]");
		File modresFile = new File(modresTable);
		
		//int  lineCount=0;
		try {
			BufferedReader br = new BufferedReader(new FileReader(modresFile));
			String t;
			while ((t = br.readLine()) != null ) { 
				t = t.trim();
				if (!(t.startsWith("!"))  && !t.isEmpty()) {  // skip comment and blank lines
		    	    String[] result = t.split("\\s+");  // split on whitespace 
		    	    if (result.length == 3) {
		    	    	System.out.println("[Warning:  Line '" + t + "' missing model name(s); ignored]");
		    	    } else if (result.length < 3) {
		    	    	System.out.println("[Warning:  Line '" + t + "' has too few fields; ignored]");
		    	    } else {
		    	    	if (result.length > 4) {
		    	    		System.out.println("[Info:  Line '" + t + "' has more than 4 fields; combining 4+ into single model field]");
		    	    		for (int i = 4; i < result.length; i++) {
		    	    			result[3] += result[i];
		    	    		}
		    	    	}
		    	    	String product  = result[0].trim();
		    	    	String filepath = result[1].trim();
		    	    	String group    = result[2].trim();
		    	    	String[] models = result[3].trim().split(";");
		    	    	filepath = substituteEnvVars(filepath);
		    	    	String attrFilename = createAttrFilename (product, filepath);
		    	    	//TODO:  Explain how things are inside-out in the new system...
		    	    	for (String model : models) {
		    	    		String key = model + " " + group; // since attrSetGroupMap is a SortedMap, contents will be ordered by model/group in output
		    	    		AttrSetGroup asg = attrSetGroupMap.get(key);
		    	    		if (asg == null) {
		    	    			asg = new AttrSetGroup();
		    	    			asg.setResource(model.toUpperCase());
		    	    			asg.setAttrSetGroupName(group);
		    	    			attrSetGroupMap.put(key, asg);
		    	    		}
		    	    		//asg.addAttrSetName(product.toLowerCase());  // or maybe filename??
		    	    		//asg.addAttrSetName(attrFilename);  // can't use this any more; new version checks for file existence, but we haven't created it yet!!

		    	    		// GH - only made compile. Not tested.
		    	    		asg.addAttrSetName( attrFilename );// , new File("bogusFile"));  //TODO:  Better way??
	    	    			//attrSetGroupMap.put(key, asg);
		    	    	}
		    	    	if (new File (filepath).exists()) {
		    	    		restoreFileToAttrFileMap.put(filepath, attrFilename);
		    	    	}
		    	    }
					//lineCount++;
				}
			} 
			
		}
		catch (IOException e) {
			   System.err.println("Error: " + e);
			   return false;
		};
		System.out.println("[Info:  ..." + modresTable + " read successfully]");
		return true;
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

	private String createAttrFilename(String product, String filepath) {
		
		// Generate a new filename, based on the product name and file path
		
		// TODO:  Better solution, maybe?  For now, the new filename (which is
		// what the user sees) is based on the legacy product name, but suffixed
		// with "_" and the parent directory of the legacy restore file name
		// so that we don't have file name conflicts with all the new files...
		
		// We also need to remove from the product name any characters that
		// can't be used in a legal file name.

		File legacyRestoreFile = new File(filepath);
		product = product.replaceAll("\\p{Punct}", "_");
		product = product.replaceAll("__", "_");
		product = product.toLowerCase();
		String attrFilename = product + "_" + legacyRestoreFile.getParentFile().getName();
		
		if (!legacyRestoreFile.exists()) {
			//TODO:  Consider whether we should abandon this entry in the XML file too
			System.out.println("[Warning:  Restore File " + filepath + " could not be located]");
		}
				
		return attrFilename;
	}

	/**
     * convert the modres file from .tbl to .xml
     * (and maybe legacy restore files to new attribute files)
     * 
     * @return nothing at all
     */
	public void convert(String inFile, String outDir) {

		if (ingestModres(inFile)) { //  usually "$GEMTBL/nmap/mod_res.tbl"

			writeAttrSetGroupsAsXMLFiles(outDir);
		
		    convertAllRestoreFilesToAttrFiles(outDir);
		
		    //  for consistency with other conversion programs...
	        System.out.println("\n[Success:  The file is converted.  " + "The Conversion is finished.]\n");
	        
		}

	}

	private void convertAllRestoreFilesToAttrFiles(String outDir) {
		
		for (String restoreFile : restoreFileToAttrFileMap.keySet()) {
			String attrFile = restoreFileToAttrFileMap.get(restoreFile);
			
			convertOneRestoreFileToAttrFile(new File(restoreFile), new File(outDir + "/" + attrFile + ".attr"));
		}
		
	}

	private void convertOneRestoreFileToAttrFile(File restoreFile, File attrFile) {

		//  Establish input (legacy restore) and output (attribute) files
		
		System.out.println("\n[Info:  Converting legacy restore file " + restoreFile + " ...]");
		System.out.println("[       ...to new attribute file " + attrFile + "]");
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(restoreFile));
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		BufferedWriter bw = null;
		try {
			bw = new BufferedWriter(new FileWriter(attrFile));
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		//  Create a set to remember the parameters (of those that we're interested in)
		//  that we've seen coming from the restore file
		
		EnumSet<GempakParametersToTransfer> parametersInLegacyRestoreFile = EnumSet.noneOf(GempakParametersToTransfer.class);
		
		String t;
		try {
			//  For each line in the restore file...
			while ((t = br.readLine()) != null ) {
				t = t.trim();
				//  Comments and blank lines get moved verbatim to the new attribute file
				if (t.startsWith("!") || t == "\n") {
					bw.write(t + "\n");
				}
				else {
					// get first word -- up to blank or end
					String[] result = t.split("\\s+");
					if (gempakParametersMap.containsKey(result[0])) {
						//  an allowed parameter specified...
						if (result.length > 1) {
							//  ...with a value -- transform and write it out
							//String cdr = t.substring(result[0].length()).trim();
							String cdr = t.substring(8);
							bw.write(result[0]);
							for (int i = result[0].length(); i < 6; i++) bw.write(" ");  // align
							bw.write(" = " + cdr + "\n");
						}
						else {
							//  ...without a value -- get the default one...?  (or not?)
							bw.write(result[0]);
							for (int i = result[0].length(); i < 6; i++) bw.write(" ");  // align
							bw.write(" = " + gempakParametersMap.get(result[0]).getDefaultValue() + "\n");
						}
						//  either way, remember that this parameter has been done
						parametersInLegacyRestoreFile.add(gempakParametersMap.get(result[0]));
					}
				}
			}
			//  Check for parameters on our 'known' list that did not appear in the legacy file.
			//  For each, if 'REQUIRED', add it to the output anyway, with its default value.
			boolean parametersAddedByConverter = false;
			for (GempakParametersToTransfer p : EnumSet.complementOf(parametersInLegacyRestoreFile)) {
				if (p.getPriority() == GempakParameterPriority.REQUIRED) {
					if (!parametersAddedByConverter) {
						bw.write("! [Following added by conversion program...]" + "\n");
						parametersAddedByConverter = true;
					}
					bw.write(p.toString());
					for (int i = p.toString().length(); i < 6; i++) bw.write(" ");
					bw.write(" = " + p.getDefaultValue() + "\n");
				}
				else {
					System.out.println("[Info:  Optional parameter " + p + " not in legacy, so not in new either]");
				}
			}
			bw.flush();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void writeAttrSetGroupsAsXMLFiles(String outDir) {
		for (AttrSetGroup asg : attrSetGroupMap.values()) {
			//Collections.sort(asg.getAttrSetNames());  // if want 'em in order...?
			String outFile = outDir + "/" + asg.getResource() + "-" + asg.getAttrSetGroupName() + ".xml";
			try {
				System.out.println("\n[Info:  Writing attribute set group as XML file " + outFile + "...]");
				//SerializationUtil.jaxbMarshalToXmlFile(colorMap, outFile);
				JAXBContext context = JAXBContext.newInstance(AttrSetGroupList.class);
				Marshaller marshaller = context.createMarshaller();
				marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
				marshaller.marshal(asg, new FileWriter(new File(outFile)));
			} catch (JAXBException e) {
				//TODO:  Maybe not this specific condition; other things can trip exception
				System.out.println("[Error:  The attrSet XML file is not writable.]");
				return;
			} catch (IOException e) {
				System.out.println("[Error:  The attrSet XML file is not writable.]");
				return;
				//e.printStackTrace();
			}
			//catch (SerializationException e) {
			//	  e.printStackTrace();
			//}
			System.out.println("[Info:  ..." + outFile + " written successfully]");
		}
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
				System.out.println("Directory doesn't exist.  Try again...");
				outputDirStr = null;
			}
			else if (!outputDir.canWrite()) {
				System.out.println("Directory can't be written.  Try again...");
				outputDirStr = null;
			}
			else {
				haveValidOutput = true;
			}
		}
		
		new ModresConvert().convert(inputFileStr, outputDirStr);
		
	}
}
