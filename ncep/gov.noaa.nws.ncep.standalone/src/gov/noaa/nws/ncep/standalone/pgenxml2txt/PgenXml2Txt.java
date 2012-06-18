/**
 * PgenXml2Txt
 * This code was developed by SIB for use in the AWIPS2 system.
 * Date created (18-February-2010) 
 */

package gov.noaa.nws.ncep.standalone.pgenxml2txt;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.File;
import java.io.Writer;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * <pre>
 * <tt>PgenXml2Txt</tt> converts a Pgen XML file into a text file.
 * The usage syntax is <code> java - jar pgenxml2txt.jar xmlfilename xltfilename textfilename</code>
 * The XML file and the xlt file (style-sheet) must be in the same directory as PgenXml2Txt.jar
 * The name of the text file is the only optional parameter
 * If the name of the text file is not provided, the program will generate
 * a default name for it, based on the names of the xml file and the xlt file. 
 *
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 18-Feb-2010    225        Archana.S   Initial Creation
 * 06-Apr-2010    225        Archana.S   Renamed the package and 
 *                                       the file to pgenxml2txt
 *                                       to indicate that it can be used for 
 *                                       any Pgen XML file with a valid 
 *                                       style-sheet.
 *                                       Updated the method 
 *                                       validateCommandLineArguments()
 *                                       to check for the file extension xslt
 *                                       as well as xlt. 
 *                               
 * </pre>
 * @author Archana.S
 * @version 1
 */
public class PgenXml2Txt {
	
	/**
	 * Name of the XML file containing the Pgen element
	 */
	private String xmlFileName;
	
	/**
	 * Name of the xml style-sheet
	 */
	private String xltFileName;
	
	/**
	 * Name of the text file
	 */
	private String textFileName;
	
	private final String README_FILE = "pgenxml2txt.hlp";
	
	private final String INVALID_STYLESHEET_EXTENSION = "Error: The second argument must be the name of the stylesheet with the extension .xlt";
	private final String INVALID_XML_FILE_EXTENSION   = "Error: The first argument must be the name of the Pgen XML file with the extension .xml";	  
	private final String LESS_NUM_ARGS                = "Error: Too few arguments";
	
	
	/***
	 * Validates the command line arguments and then invokes methods to generate the text file
	 * by formatting the XML file with the information in the style-sheet. 
	 * @param args - the command line arguments entered by the user
	 * @throws IOException
	 */
	public static void main(String args[]) throws IOException{

		PgenXml2Txt converter = new PgenXml2Txt();
        if(converter.validateCommandLineArguments(args)){
            converter.writeToFile(converter.getTxtFile().toUpperCase(),
            		              converter.convertXml2Txt(
            		                converter.getXmlFileName(),converter.getStyleSheetName()));
         
        }

	}
	
	/**
	 * @return the xmlFile
	 */
	public String getXmlFileName() {
		return xmlFileName;
	}

	/**
	 * @return the xsltFile
	 */
	public String getStyleSheetName() {
		return xltFileName;
	}

	/**
	 * @return the txtFile
	 */
	public String getTxtFile() {
		return textFileName;
	}
	
	/**
	 * @param xmlFile the xmlFile to set
	 */
	private void setXmlFileName(String xmlFile) {
		this.xmlFileName = xmlFile;
	}


	/**
	 * @param xsltFile the xsltFile to set
	 */
	private void setStyleSheetName(String xsltFile) {
		this.xltFileName = xsltFile;
	}


	/**
	 * @param txtFile the txtFile to set
	 */
	private void setTextFileName(String txtFile) {
		this.textFileName = txtFile;
	}
	  
	/***
	 * Parses the command-line parameters entered by the user to extract and validate
	 * the name of the XML file, the name of the style-sheet and the name of the text-file.
	 * If the name of the text-file is not entered, it invokes the method
	 *  <tt>generateTextFileName(String,String)</tt>. 
	 * @param commandLineArgs The user-entered command-line parameters  
	 * @return false - if the number or the type of parameters are invalid and true otherwise
	 */
	private boolean validateCommandLineArguments(String commandLineArgs[]){
		boolean isArgValid = true;
		if ( (commandLineArgs.length == 0) || ("--help".equalsIgnoreCase(commandLineArgs[0])) 
				|| ("/h".equalsIgnoreCase(commandLineArgs[0])) ) {
			System.out.println(getFileContents(this.README_FILE));
			return false;
		}
		
		if(commandLineArgs.length >= 2){
		      if (checkFile(commandLineArgs[0])) {
				if (commandLineArgs[0].toLowerCase().endsWith(".xml")) {

					setXmlFileName(commandLineArgs[0]);

					if (checkFile(commandLineArgs[1])) {
						if (commandLineArgs[1].toLowerCase().endsWith(".xlt")
								|| commandLineArgs[1].toLowerCase().endsWith(".xslt")) {

							setStyleSheetName(commandLineArgs[1]);

							if ((commandLineArgs.length >= 3)) {
								setTextFileName(commandLineArgs[2]);
							} else {

								setTextFileName(generateTextFileName(getXmlFileName(),
										getStyleSheetName()));
							}

						} else {

							System.out.println(INVALID_STYLESHEET_EXTENSION);
							isArgValid = false;
						}
					}else{
						isArgValid = false;       		
					}

				} else {  
        					System.out.println(INVALID_XML_FILE_EXTENSION);
					        isArgValid = false;
				}
			}else{
				isArgValid = false;
			}
		
		}else{  
			     System.out.println(LESS_NUM_ARGS);	          
			     isArgValid = false;			
		}
		
		return isArgValid;
		
	}
	
	/***
	 * Checks whether the <tt>File</tt> object created with the input file name is a valid file. 
	 * @param fileName - the name of the file whose validity is to be checked,
	 * @return false if the file is null/does not exist/is not a file/cannot be read
	 *         and true otherwise. 
	 */
	private boolean checkFile(String fileName){
		boolean isFileValid=true;
		if(fileName != null){
		    File file = new File(fileName);

		    if(!(file.exists())){
		    	
		    	System.out.println("Error: " + file + " does not exist in the current directory");
		    	return false;
		    }
		    
		    if(!(file.isFile())){
		    	System.out.println("Error: " + file + " is not the name of a file");
		    	return false;
		    }		    
		    
  		    if(!(file.canRead())){
		    	System.out.println("Error: " + "Cannot read " + file);
		    	return false;
		    }
		    	    
		}else{
	    	System.out.println("Error: Filename is null");
	    	return false;
	    }
	    			
		
		
		return isFileValid;
	}

	/***
	 * Generates the name of the text file as xmlFileName.xltFileName, if the
	 * xltFileName matches one of the following:
	 * <pre>wou/saw/sev/sel</pre>
	 * Else, the name of the text file is generated as xmlFileName.txt
	 * @param xmlFile - The name of the XML file
	 * @param xltFile -The name of the style-sheet file 
	 * @return The <tt>String</tt> denoting the name of the text file
	 */

	private String generateTextFileName(String xmlFile, String xltFile){

		String textFileExtension="";
		String xmlFilePrefix = xmlFile.substring(0, xmlFile.length() - 4);
		if(xltFile.substring(0,3).equalsIgnoreCase("wou")
				|| xltFile.substring(0,3).equalsIgnoreCase("saw")
				|| xltFile.substring(0,3).equalsIgnoreCase("sel")
				|| xltFile.substring(0,3).equalsIgnoreCase("sev")){
			      
			    if(xltFile.charAt(3) == '.'){    
			       textFileExtension += xltFile.substring(0, xltFile.length() - 4);
			    }else{
			    	return textFileExtension;
			    }
			    
		}else{
			textFileExtension += "txt";
		}
		
	   return (xmlFilePrefix + "." + textFileExtension );
	}
	
	  /**
	  * Creates a text file containing the Pgen element data. 
	  * @param fileName - Name of the text file
	  * @param contents - The Pgen data to be written to the text file. 
	  * @throws IOException if problem encountered during write.
	  */
	  private void writeToFile(String fileName, String contents)
	                                 throws IOException {
       File textfile = new File(fileName);   
	    Writer output = new BufferedWriter(new FileWriter(textfile));
	    try {
	           output.write(contents);
	           System.out.println( fileName + " created successfully");
	    }
	    finally {
       	      output.close();
	    }
	  }
	  
	  /**
	   * Reads the contents of a file into a string
	   * @param filePath - The file whose contents need to be read 
	   * @return The contents of the file in a <tt>String</tt> object
	   */
		public static String getFileContents(String filePath){
			StringBuffer stringBuffer = new StringBuffer();
			try{
				BufferedReader bufferedReader = new BufferedReader(
						new FileReader(
								new File(filePath)
								.getAbsoluteFile()));
				try{
					String currentLine;
					while((currentLine = bufferedReader.readLine()) != null){
						stringBuffer.append(currentLine);
						stringBuffer.append("\n");
					}
					bufferedReader.close();
				}
				catch(Exception e){
					stringBuffer.append("Error: Empty File");
					bufferedReader.close();
				}

			}catch(IOException e){
				stringBuffer.append("Error: File Not Found");
			}
			
		    return stringBuffer.toString();	
		}	  
	
	/***
	 * Creates a formatted string comprising of the contents of the XML file, to which
	 * formatting information is applied from the style-sheet.
	 * @param xmlFileName - Name of the XML file
	 * @param xltFileName - Name of the style-sheet
	 * @return A <tt>String</tt> with the formatted contents of the XML file.  
	 */
	private String convertXml2Txt(String xmlFileName, String xltFileName){
        String res = "";
		/*
         * Convert XML string into xmlSourse
         */
        Source xmlSource = new StreamSource(xmlFileName);

        /*
         * Construct xsltSource from xsltFile
         */
        Source xsltSource = new StreamSource(xltFileName);
        
        /*
         * Use the factory for XSLT transformer
         */
        TransformerFactory transFact = TransformerFactory.newInstance();
        try {
        	Transformer trans = transFact.newTransformer(xsltSource);

        	/*
        	 * Create object for the transformation product
        	 */
        	ByteArrayOutputStream baos = new ByteArrayOutputStream();

        	trans.transform(xmlSource, new StreamResult(baos));
        	/*
             * Convert transformation product to string
             */
        	res = new String(baos.toByteArray());
        }
        catch ( Exception e ) {
        	/*
        	 * Catch invalid control characters in the report
        	 */
             System.out.println("Error: File is corrupt");

        }
	return res;
	}
	

	
}

	
	
	
	
	
	
	
	
	
	
