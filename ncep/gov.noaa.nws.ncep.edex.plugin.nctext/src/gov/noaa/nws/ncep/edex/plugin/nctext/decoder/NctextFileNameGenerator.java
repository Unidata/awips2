/*
 * NctextFileNameGenerator
 * 
 * Date created 22 June 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 */
package gov.noaa.nws.ncep.edex.plugin.nctext.decoder;

import gov.noaa.nws.ncep.edex.plugin.nctext.decoder.NctextRegexMatcher;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;
import java.io.LineNumberReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
//import java.util.Collections;
//import java.util.GregorianCalendar;
import java.util.List;
import java.util.Calendar;
//import java.util.Date;
import java.util.TimeZone;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
/**
 * Generates a valid name (yyyyMMddhhmm.type or yyyyMMddhh.type) for the incoming Nctext product and then decodes it.
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 22-Jun-2010    191       Archana.S   Initial Creation
 * 21-Nov-2011              Chin Chen   change code that will not to decode nctext unknown/unsupported products, clean up code as well
 * @author Archana
 * @version 1 
 */
public class NctextFileNameGenerator {
	private final  SimpleDateFormat DATE_WITH_HOUR = new SimpleDateFormat("yyyyMMddHH");
	//private final  SimpleDateFormat DATE_WITHOUT_HOUR = new SimpleDateFormat("yyyyMMdd");
	private final  int NUM_LINES_IN_HEADER = 10;

	
	//private  Long    timeStamp;
	//private  Integer fileCreatedMonth;
	//private  Integer fileCreatedYear;
	//private  Integer fileCreatedDayOfMonth;
	//private  Date    fileCreatedDate;
	private  Calendar fileCreatedCalendar;

	private List<String> formattedFileNameList;
	private String pluginName;
    private Log logger = LogFactory.getLog(getClass());
	//private String filePath;


	/**
	 * Constructor
	 */
	public NctextFileNameGenerator() {
	    DATE_WITH_HOUR.setTimeZone( TimeZone.getTimeZone("GMT"));
		formattedFileNameList    = new ArrayList<String>(0);
	}
	
	/***
	 * Entry-point for the Nctext decoder.
	 * <pre>
	 * Reads the contents of the file header into a String and tries  to match the consecutive lines 
	 * of the file header with the regular expressions that define the Nctext products.
	 * Depending how many matches are found,  the corresponding names for the 
	 * Nctext product files are then passed along with the file itself to the Nctext decoder, 
	 * for subsequent decoding.
	 * The number of times the text product is decoded depends on the number of
	 * matches found in the regular expressions.
	 *  </pre>
	 * @param fileToRename
	 * @return an array of PluginDataObject
	 * @throws IOException
	 */
	public synchronized PluginDataObject[] renameAndDecodeFile(File fileToRename) throws IOException{
		//logger.debug("renameAndDecodeFile() called...");
		//System.out.println("renameAndDecodeFile() invoked");
		PluginDataObject[] srcArray = new PluginDataObject[0];
		List<PluginDataObject> dynamicDestArray = new ArrayList<PluginDataObject>(0);
		this.formattedFileNameList = new ArrayList<String>(0); 		
		String filePath = fileToRename.getAbsolutePath();
		String fileHeader = this.readFileHeader(NUM_LINES_IN_HEADER,filePath);
		//System.out.println("file header="+ fileHeader);
		String[] linesOfFileHeader = fileHeader.split(System.getProperty("line.separator"));
		if(linesOfFileHeader != null && linesOfFileHeader.length > 1){
			//			System.out.println("Date of file creation: "+ this.fileCreatedCalendar.getTime().toString());
			//for(int k =0; k < linesOfFileHeader.length; k++){
			//	System.out.println("Line "+ k + " ="+ linesOfFileHeader[k]);
			//}
			
			int i=0;
			 while( i < linesOfFileHeader.length - 1){
				int j = i+1;
				if(linesOfFileHeader[j].isEmpty()){
					j++;
				}
				//Chin: add \n to separate the 2 lines to make regular expression work "correctly" on those products with
				//product type coded at 2nd line
				String consecutive2Lines = new String(linesOfFileHeader[i]+"\n" +linesOfFileHeader[j]); 
				//	    	  System.out.println(consecutive2Lines);
				if( createFormattedFileName(consecutive2Lines, this.fileCreatedCalendar)){
					//    		 System.out.println("The match was found at: "+ consecutive2Lines);
					//	    		 System.out.println("The final list of formatted file names: "+ this.formattedFileNameList);
					break;
				}
				i++;
			}
			
		}
		List<PluginDataObject>  pdoList = new ArrayList<PluginDataObject>(0);
		if (this.formattedFileNameList.size() > 0) {
			//			System.out.println("The final list of formatted file names: "+ this.formattedFileNameList);
			boolean isFileRenamedInPostProcess = false;
			//for(int p =0; p < formattedFileNameList.size(); p++){
			//	System.out.println("File name "+p+ " ="+formattedFileNameList.get(p));
			//}
			int index = 0;
			int listSize = this.formattedFileNameList.size();
			while(index <  listSize && !isFileRenamedInPostProcess){
				String outFilePath = this.formattedFileNameList.get(index);
				//logger.debug("New name for file: "+ outFilePath);
				/*
				 * OMR and cgr files have similar regular expressions, hence the same raw data could be decoded as either product. 
				 * OMR files have the words "OTHER MARINE REPORTS" placed either in the file header or the file body, 
				 * while cgr (coast guard report) files lack this feature.
				 * Hence if either one of these extensions are encountered, the entire file is read to check if it contains the
				 * words "OTHER MARINE REPORTS"
				 */
				if( outFilePath.endsWith(".cgr")  || outFilePath.endsWith(".OMR") 

				) {
					outFilePath = new String(outFilePath.substring(0, 11) + getFileNameAfterPostProcess(fileToRename, outFilePath));
					isFileRenamedInPostProcess = true;
				}

				//System.out.println("New name for file: "+ outFilePath);
				srcArray = decodeFile(fileToRename, outFilePath);
				pdoList =   new ArrayList<PluginDataObject>(Arrays.asList(srcArray));
				dynamicDestArray.addAll(pdoList);
				index++;
			}
		}else{
			//System.out.println("No match found with any regular expression- file= "+ fileToRename.getName());
			//Chin:TESTING 
			//srcArray = decodeFile(fileToRename, fileToRename.getName()); 
			// pdoList =   new ArrayList<PluginDataObject>(Arrays.asList(srcArray));
			//dynamicDestArray.addAll(pdoList);
		}
		
		//System.out.println("The final array length : " + dynamicDestArray.size());
		PluginDataObject[] returnArray = new PluginDataObject[0];
		
		return dynamicDestArray.toArray(returnArray);

	}
	
	/***
	 * Reads the entire file until it encounters the words "OTHER MARINE REPORTS" to decide
	 * if the Nctrext product type is OMR or cgr.
	 * @param file - the input file
	 * @param fileNameToCheck - the current name of the file
	 * @return The correct Nctext product type
	 */
	private String getFileNameAfterPostProcess(File file, String fileNameToCheck) {
		String productType = "";
		boolean matchFound = false;
		try{
			boolean isFileExists = file.exists();
			boolean isFileAFile  = file.isFile();
			boolean isFileReadable = file.canRead();
			
            if(isFileExists &&  isFileAFile && isFileReadable){
                String currentLine;
                FileReader fileReader = new FileReader(file.getAbsoluteFile());
    			BufferedReader bufferedReader = new BufferedReader(fileReader);    				
    			if (fileNameToCheck.endsWith(".cgr") || fileNameToCheck.endsWith(".OMR")) {
					while ((currentLine = bufferedReader.readLine()) != null) {
						if (currentLine.compareTo("OTHER MARINE REPORTS") == 0) {
							productType = new String("OMR");
							matchFound = true;
							break;
						}
					}
					
					if(!matchFound){
						productType = new String("cgr");
					}
				}
			
    			bufferedReader.close();
            }
		}catch(IOException e){
				System.out.println("IOException thrown from NctextFileNameGenerator.getFileNameAfterPostProcess()");
		}		
		return productType;
	}

	/***
	 * Invokes the NctextDecoder's decodeTextInputFileWithName(File, String) method
	 * @param fileToDecode - the Nctext product to be decoded
	 * @param derivedFileName - the derived name of the file.
	 * @return array of PluginDataObject with 0 elements.
	 *@throws DecoderException
	 *@throws PluginException
	 */
	private synchronized PluginDataObject[] decodeFile(File fileToDecode, String derivedFileName){
    	try {
    		//System.out.println("decodeFile() entered...");
    	      return (new NctextDecoder().decodeTextInputFileWithName(fileToDecode, derivedFileName));
        	} catch (DecoderException e) {
			
			logger.error("DecoderException!\n"+e);
			return (new PluginDataObject[0]);
		} catch (PluginException e) {
			logger.error("PluginException!\n"+e);
			return (new PluginDataObject[0]);
		}		
	}
	
	/**
	 * <pre>
	 * Generates a file name and extension for the Nctext product
	 * The file name is based on the date extracted from the file header and the file's time-stamp
	 * and obeys the format yyyymmddhh The file extension is the nctext product type. 
	 * If a match is found with one or more of the regular expressions with specific lines
	 * from the file's header, then the parsed date, hour and minute information are extracted
	 * from the matched lines.
	 * The year and month information are supplied by the file's last modified date.
	 * The same file name is prepended to different extensions, one for each match
	 * found in the regular expressions. 
	 * </pre>
	 * @param fileContent - consecutive lines of the file header
	 * @param productCreationDateCalendar - time-stamp of the file (time that the file was last modified)
	  */

	private synchronized boolean createFormattedFileName(String fileContent, Calendar productCreationDateCalendar){
		if(NctextRegexMatcher.matchFileRegex(fileContent)){
//			System.out.println("productCreationDateCalendar= "+ productCreationDateCalendar.getTime().toString());
			List<String>  nctextProductTypeList   = new ArrayList<String>( NctextRegexMatcher.getProductType());
			int parsedDayOfMonth     =  NctextRegexMatcher.getParsedDayOfMonth().intValue();
			int parsedHour                  =  NctextRegexMatcher.getParsedHour().intValue();
			int parsedMinute               =  NctextRegexMatcher.getParsedMinute().intValue();
// 		    Calendar tempCal = productCreationDateCalendar;
			Calendar tempCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
//   		    System.out.println("Before updating tempCal = "+ tempCal.getTime().toString());
//            System.out.println("Before updating tempCal File derived time in seconds = "+  tempCal.getTimeInMillis()/1000);

   		    tempCal.clear(Calendar.DAY_OF_MONTH);
    		tempCal.set(Calendar.DAY_OF_MONTH, parsedDayOfMonth);
    		tempCal.clear(Calendar.HOUR_OF_DAY);
   			tempCal.set(Calendar.HOUR_OF_DAY, parsedHour);
    		tempCal.clear(Calendar.MINUTE);
    		tempCal.set(Calendar.MINUTE, parsedMinute);

    		tempCal.clear(Calendar.YEAR);
    		tempCal.set(Calendar.YEAR, productCreationDateCalendar.get(Calendar.YEAR));
    		tempCal.clear(Calendar.MONTH);
    		tempCal.set(Calendar.MONTH, productCreationDateCalendar.get(Calendar.MONTH));
    		
    		//long derivedTime = tempCal.getTimeInMillis()/1000;
			//long fileCreatedTime   = productCreationDateCalendar.getTimeInMillis()/1000;
//            System.out.println("File created time in seconds = "+ fileCreatedTime);
//            System.out.println("File derived time after updating tempCal  = "+ derivedTime);
			
//            if(  fileCreatedTime - derivedTime > ( 3*NUM_SECS_PER_DAY/2 ) ){  // logic derived from legacy
//					tempCal.set( Calendar.MONTH,  productCreationDateCalendar.get(Calendar.MONTH) - 1 ) ;
//			}

//			System.out.println("After updating tempCal = "+ tempCal.getTime().toString());
	  		if(nctextProductTypeList.size() > 0){
	  			for(String fileExtension: nctextProductTypeList ){
	  				this.formattedFileNameList.add( DATE_WITH_HOUR.format(tempCal.getTime()) + "." + fileExtension);
	  			}
	  		}
	  		//else
	  		//	System.out.println("product size =0");
		}
		
		return (this.formattedFileNameList.size() > 0 ? true: false );
	}


	/**
	 * @param fileCreatedYear the fileCreatedYear to set
	 */
	//private void setFileCreatedYear(Integer fileCreatedYear) {
	//	this.fileCreatedYear = fileCreatedYear;
	//}

	/**
	 * @param fileCreatedDayOfMonth the fileCreatedDayOfMonth to set
	 */
	//private void setFileCreatedDayOfMonth(Integer fileCreatedDayOfMonth) {
	//	this.fileCreatedDayOfMonth = fileCreatedDayOfMonth;
	//}

	/**
	 * @param fileCreatedMonth the fileCreatedMonth to set
	 */
	//private void setFileCreatedMonth(Integer fileCreatedMonth) {
	//	this.fileCreatedMonth = fileCreatedMonth;
	//}

	/**
	 * Reads a fixed number of lines from a file into a String
	 * <pre>
	 * If the line-count of the file is less than the number of lines to be read
	 * then the number of lines to be read is restricted to the line-count of the file.
	 * This method also stores a time-stamp for the file based on when the file was last modified.
	 * </pre>
	 * @param numLinesToRead  - number of lines to read from the file
	 * @param filePath - the location of the file to be read
	 * @return the String representation of the lines of the file that were read
	 */
	private synchronized String readFileHeader(int numLinesToRead, String filePath){
		
        String fileHeaderContents = "";
		try{
			File file = new File(filePath);
			boolean isFileExists = file.exists();
			boolean isFileAFile  = file.isFile();
			boolean isFileReadable = file.canRead();
			
            if(isFileExists &&  isFileAFile && isFileReadable){
    			long localTimeStamp             = file.lastModified();
    			storeTimeStampForFile(localTimeStamp);
            	
   			    int numLinesInFile = getNumLinesInFile(file);
    			if(numLinesToRead > numLinesInFile){
    				numLinesToRead = numLinesInFile;
    			}
    			
    			int lineCounter = 0;
                String currentLine;
                FileReader fileReader = new FileReader(file.getAbsoluteFile());
    			BufferedReader bufferedReader = new BufferedReader(fileReader);    				
    			StringBuffer stringBuffer = new StringBuffer();
    			while((currentLine = bufferedReader.readLine()) != null && lineCounter < numLinesToRead){
    					stringBuffer.append(currentLine);
    					stringBuffer.append(System.getProperty("line.separator"));
    					lineCounter++;
    			}
    			bufferedReader.close();
    			fileHeaderContents = stringBuffer.toString();
    				           	
            }
		}catch(IOException e){
				System.out.println("IOException thrown from NctextFileNameGenerator.readFileHeader() for file:"+ filePath);
		}
		
	    return fileHeaderContents;
	}
	/**
	 * Returns the number of lines in the input file
	 * @param file whose line-count is to be determined
	 * @return the number of lines in the file if there was no exception thrown or zero otherwise
	 * @throws IOException, FileNotFoundException
	 * 
	 */
	private synchronized int getNumLinesInFile(File file){
		int numLinesInFile = 0;
		try {
			FileReader fileReader = new FileReader(file.getAbsoluteFile());
			LineNumberReader lineNumberReader = new LineNumberReader(fileReader);
			try {
				lineNumberReader.skip(Long.MAX_VALUE);
				numLinesInFile = lineNumberReader.getLineNumber();
				lineNumberReader.close();
			} catch (IOException e1) {
				System.out.println("IOException thrown from NctextFileNameGenerator.getNumLinesInFile() for file : " + file.toString());
			}

		} catch (FileNotFoundException e) {
          System.out.println("FileNotFoundException thrown from NctextFileNameGenerator.getNumLinesInFile() for file :  " + file.toString());
		}
	
		return numLinesInFile;
	}
	
	/***
	 * Stores the time-stamp for the file as a Calendar object and also
	 * stores the individual components of the time-stamp separately.
	 * @param theFileTimeStamp - the time when the file was last modified (in milli-seconds). 
	 */
	private synchronized void storeTimeStampForFile(long theFileTimeStamp){
		setFileCreatedCalendar(Calendar.getInstance());
		getFileCreatedCalendar().setTimeZone(TimeZone.getTimeZone("GMT"));
		getFileCreatedCalendar().setTimeInMillis(theFileTimeStamp);
		//setFileCreatedDate(new Date(theFileTimeStamp));
		//setFileCreatedDayOfMonth( new Integer (getFileCreatedCalendar().get(Calendar.DAY_OF_MONTH)));
		//setFileCreatedMonth( new Integer (getFileCreatedCalendar().get(Calendar.MONTH)));
		//setFileCreatedYear( new Integer (getFileCreatedCalendar().get(Calendar.YEAR)));
	}
	
     /**
	 * @return the fileCreatedDate
	 */
	//private Date getFileCreatedDate() {
	//	return fileCreatedDate;
	//}

	/**
	 * @param fileCreatedDate the fileCreatedDate to set
	 */
	//private void setFileCreatedDate(Date fileCreatedDate) {
	//	this.fileCreatedDate = fileCreatedDate;
	//}

	/**
	 * @return the fileCreatedCalendar
	 */
	private Calendar getFileCreatedCalendar() {
		return fileCreatedCalendar;
	}

	/**
	 * @param fileCreatedCalendar the fileCreatedCalendar to set
	 */
	private void setFileCreatedCalendar(Calendar fileCreatedCalendar) {
		this.fileCreatedCalendar = fileCreatedCalendar;
	}

	/**
	 * @return the timeStamp
	 */
	//private Long getTimeStamp() {
	//	return timeStamp;
	//}

	/**
	 * @param timeStamp the timeStamp to set
	 */
	//private void setTimeStamp(Long timeStamp) {
	//	this.timeStamp = timeStamp;
	//}

	/** 
      * @return the pluginName
      */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * 
     * @param pluginName
     *            the pluginName to set
     */
    
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }	
	
}

