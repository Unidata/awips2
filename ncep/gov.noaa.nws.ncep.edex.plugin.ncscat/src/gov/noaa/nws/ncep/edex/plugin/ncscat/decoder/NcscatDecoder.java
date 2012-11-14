/**
 * 
 * Ncscat Decoder
 * 
 * This java class decodes Ascat quikscat (NESDIS File) raw data. 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 11/2009		Uma Josyula		Initial creation
 * 01/2011		B. Hebbard 		Handle ambiguity variants
 * 07/2012		B. Hebbard 		Handle OSCAT / OSCAT_HI
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.edex.plugin.ncscat.decoder;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import gov.noaa.nws.ncep.common.dataplugin.ncscat.NcscatRecord;
import gov.noaa.nws.ncep.edex.plugin.ncscat.util.*;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.edex.esb.Headers; 



public class NcscatDecoder extends AbstractDecoder {
	String  plugin;
	String  ambigNumber;
    private Matcher matcher;
    private Pattern pattern;
	private static  String pluginName ;//= "NCSCAT";
	/**
	 * Constructor
	 * 
	 * @throws DecoderException
	 */
	public NcscatDecoder(String name) throws DecoderException {
		 pluginName = name; 
	}


	//public PluginDataObject[] decode(byte[] data, Headers headers) throws DecoderException {
		public PluginDataObject[] decode(byte[] data) throws DecoderException {

		String traceId = ""; 
       /* if (headers != null) { 
                traceId = (String) headers.get("traceId"); 
        } */
		
		NcscatRecord record = null;
		byte[] messageData = null;
		int recLength=688;
		NcscatProcessing sProcess = new NcscatProcessing();
		sProcess.setInputFileName(plugin);
		messageData = sProcess.separate(data);
		recLength =NcscatProcessing.getRecordLength();
		if(messageData!=null){

			record = new NcscatRecord();
			record.setConvertedMessage(messageData);
			record.setStartTime(sProcess.getStartTime());
			record.setEndTime(sProcess.getEndTime());
			record.setRecordLength(recLength);
			record.setDataTime(new DataTime(sProcess.getStartTime()));
			
			if (record.getRecordLength()==688){

				if(plugin.equalsIgnoreCase("oscat")){
					record.setReportType("oscat-hi");
				}else{
				record.setReportType("quikscat");
				}
			}else if(record.getRecordLength()==1372){
				record.setReportType("quikscat-hi");
			}else if(record.getRecordLength()==382){

				if(plugin.equalsIgnoreCase("ascatx")){

					record.setReportType("Exasct");
				}else{			

				record.setReportType("ascat");
				}
			}else if(record.getRecordLength()==742){

				if(plugin.equalsIgnoreCase("ascatx")){

					record.setReportType("Exasct-hi");
				}else{	
				record.setReportType("ascat-hi");
				}
			}else if(record.getRecordLength()==328){
				record.setReportType("oscat");
			}else if(record.getRecordLength()==715){
				record.setReportType("wscat");
			}

			//  If this is an (numbered) 'ambiguity' variant, add suffix to reportType 
			if (!ambigNumber.isEmpty()) {
				record.setReportType(record.getReportType() + "-ambig" + ambigNumber);
			}

			if (record != null) {
				try {
					record.setPluginName(pluginName);
					record.constructDataURI();	

				}
				catch (PluginException e) {
					throw new DecoderException("Error constructing dataURI", e);
				}
			}
		}
		if (record == null) {
			return new PluginDataObject[0];
		} else {
			return new PluginDataObject[] {(PluginDataObject) record};
		}


	}
	/**
     * 
     * @param inputFile : ingest file to be decoded
     * @return
     * @throws DecoderException
     * @throws PluginException 
     * 
     */
    public synchronized PluginDataObject[] decodeNcscatInputFile(File inputFile) throws DecoderException, PluginException {
        byte [] fileData = null;
        InputStream inputStream = null;
        try {
            
        	inputStream = new FileInputStream(inputFile);

                fileData = new byte[(int) inputFile.length()];
                int bytesRead = inputStream.read(fileData);
                if (bytesRead != fileData.length) {
                    fileData = null;
                }
                fileName = inputFile.getName();

        }
        catch (IOException ioe) {
        	logger.error("Error reading input file " + inputFile.getName(), ioe);
        	fileData = null;
        }
        finally {
        	if (inputStream != null) {
        		try {
        			inputStream.close();
        		} catch (IOException ioe) {
        			logger.error("Could not close input file " + inputFile.getName());
        		}
        	}
        }
        if(fileData != null){
        	pattern = Pattern.compile("\\.([a-z]{5,6})");
        	matcher = pattern.matcher(fileName);
        	if (matcher.find()) {
        		plugin= matcher.group(1) ;
        	}
            //TODO:  Review this temporary fix by BH to see if a broader refactor might be better.
            //       Problem:  Without the added 'else' below, non-local variable "plugin" will
        	//       retain its previous value if the pattern matcher fails above; this will happen
        	//       if the file suffix/extension (after the ".") is shorter than 5 characters.
        	//       Scenario:  A file with suffix ".ascatx" is ingested, then later a ".qsct" file.
        	//       Variable "plugin" will be set to "ascatx" by the first, but will retain this
        	//       same value for the second; logic in NcscatProcessing.doSeparate() will as a
        	//       result interpret the big-endian ".qsct" data as little endian format.
        	//       Alternate (better):  Could make "plugin" local to this method
        	else {
        	    plugin = "";
        	}
        	//END of temporary fix by BH; see also endianess fixes in NcscatProcessing
        	
        	//  Handle ambiguity variants:  In addition to files containing the primary
        	//  (greatest likelihood) wind vector solutions, for some data types we also
        	//  receive (typically 2 or 4) 'ambiguity' variants for the same point locations,
        	//  but with (lesser likelihood) solutions.  Since these cannot be distinguished
        	//  from the primary files based on data format/content alone, we look for a
        	//  (prearranged) telltale pattern somewhere in the file name.  If found, we
        	//  remember the indicated ambiguity number, for later tagging of the database
        	//  record.
        	pattern = Pattern.compile("ambig([1-9])");
        	matcher = pattern.matcher(fileName);
        	if (matcher.find()) {
        		ambigNumber = matcher.group(1);
        	}
        	else {
        		ambigNumber = "";
        	}
        	//[debug] System.out.println("fileName="+fileName + " ambigNumber="+ambigNumber);
        	
        	return decode(fileData);
        	
        }

        System.out.println("decodeTextInputFile: bad input file reading");
        return (new PluginDataObject[0]);


    }
}











