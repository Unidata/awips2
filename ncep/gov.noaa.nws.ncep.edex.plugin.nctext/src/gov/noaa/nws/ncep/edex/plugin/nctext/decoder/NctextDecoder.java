/**
 * 
 * NctextDecoder
 * 
 * This java class performs text data type decoding function
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date             Ticket#   	Engineer    Description
 * -------		     ------- 	   --------------   ------------------
 * 10/22/2009	 191		   Chin Chen	Initial coding
 * 07/29/2010    191        Archana       Commented out debug code in
 *                                                         decodeTextInputFileWithName() and 
 *                                                         decodeTextInputFile()
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.nctext.decoder;

import gov.noaa.nws.ncep.edex.plugin.nctext.common.NctextRecord;
import gov.noaa.nws.ncep.edex.plugin.nctext.common.dao.NctextRecordDao;
import gov.noaa.nws.ncep.edex.plugin.nctext.query.NctextQuery;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;



public class NctextDecoder extends AbstractDecoder {

    /**
     * Default constructor
     * 
     * @throws DecoderException
     */    
    public NctextDecoder() throws DecoderException {
    }
    
    private Log logger = LogFactory.getLog(getClass());


    String fileName;
    //String fileExt;
    private String pluginName;
    private NctextRecordDao dao;

    public static final byte ASCII_RS = 0x1E; //record separator "^^"
    public static final byte ASCII_ETX = 0x03; //record separator "^^"
    public static final byte ASCII_SOH = 0x01; //record separator "^^"


	/**
     *
     * 
     * @see com.raytheon.edex.plugin.IMessageDecoder#decode()
     */
    public PluginDataObject[] decode(byte[] data) throws DecoderException {
        PluginDataObject[] returnObjects = null;
        //System.out.println("decode(): entered!!!! ");
        if((data != null)&&(data.length > 0)) {

            String traceId = null;
            if(fileName != null) {
                traceId = fileName;
            } else {
                traceId = UUID.randomUUID().toString();
            }

            NctextSeparator separator = new NctextSeparator(traceId);
            
            if(separator != null) {
                //separator.setData(data);Chin - old way
                separator.setRecordData(data);
                List<PluginDataObject> rtnReports = new ArrayList<PluginDataObject>();
                
                while (separator.hasNext()) {

                    NctextRecord nctextRecord = (NctextRecord) separator.next();

                    if (nctextRecord != null) {
                    	
                    	try { //Construct URI for this record
							nctextRecord.constructDataURI();
						} catch (PluginException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
                        //add  textRecord to PluginDataObject list
                        rtnReports.add(nctextRecord);
                    }
                }//While loop 
                
                
              //Chin Debug
              //  System.out.println("NctextDecoder.decode:  report size = "+ rtnReports.size());
                
                //Chin comment, convert PluginDataObject list to PluginDataObject[].
                if(rtnReports.isEmpty()== false){
                	returnObjects = new PluginDataObject[rtnReports.size()];
                	rtnReports.toArray(returnObjects);
                }
                else {
                	returnObjects = new PluginDataObject[0];
                    //Chin Debug
                      System.out.println("TextDecoder.decode: return with 0 record");
                }
                	
            } else {
                returnObjects = new PluginDataObject[0];
              //Chin Debug
                System.out.println("TextDecoder.decode: no separator, return with 0 record");
            }
        }
        else{
            returnObjects = new PluginDataObject[0];
            //Chin Debug
              System.out.println("TextDecoder.decode: bad input");
        }
        System.out.println("decode(): return with size of  "+ returnObjects.length);
        return returnObjects;
    }

    /**
     * 
     * @param inputFile : ingest file to be decoded
     * @return
     * @throws DecoderException
     * @throws PluginException 
     * 
     */
    public synchronized PluginDataObject[] decodeTextInputFile(File inputFile) throws DecoderException, PluginException {
        byte [] fileData = null;
        InputStream is = null;
        logger.debug("decodeTextInputFile: enterred!");
        System.out.println("decodeTextInputFile: enterred!");
        try {
            try {
                is = new FileInputStream(inputFile);

                fileData = new byte[(int) inputFile.length()];
                int bytesRead = is.read(fileData);
                // If we didn't or couldn't read all the data, signal the
                // fact by setting the data to null;
                if (bytesRead != fileData.length) {
                    fileData = null;
                }
                fileName = inputFile.getName();
                //int ind = fileName.indexOf('.');
                //if (ind > 0)
                //	fileExt = fileName.substring(ind+1);
            } catch (IOException ioe) {
                logger.error("Error reading input file " + inputFile.getName(), ioe);
                fileData = null;
            }
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                    logger.error("Could not close input file " + inputFile.getName());
                }
            }
        }
        if(fileData != null){
        	//Chin debug

//        	if(fileName.indexOf("test") != -1) {
//        		//This route for testing 
//        		System.out.println("NctextDecoder.decodeFile(): fileName= "+ fileName);
//        		int rsCount =0;
//        		int ctlCCount =0;
//        		int ctlACount =0;
//        		int pos = -1;
//        		
//        		String rawMsg = new String(fileData, 0, fileData.length);
//        		while(true){
//        			pos = rawMsg.indexOf(ASCII_RS,pos+1); // RS ^^
//        			if(pos != -1){
//        				//rawMsg = rawMsg.substring(pos);
//        				rsCount++;
//        				//System.out.println(" RS number = "+rsCount);
//        				//if(pos >= fileData.length)
//        					//break;
//        			}
//        			else
//        				break;
//        		}
//        		System.out.println(" RS number = "+rsCount);
//
//        		pos = -1;
//        		String rawMsg2 = new String(fileData);
//        		while(true){
//        			pos = rawMsg2.indexOf(ASCII_SOH, pos+1); // ctl-A
//        			if(pos != -1){
//        				//rawMsg2 = rawMsg2.substring(pos);
//        				ctlACount++;
//        				if(pos >= fileData.length)
//        					break;
//        			}
//        			else
//        				break;
//        		}
//        		System.out.println(" CtlA number = "+ctlACount);
//        		pos = -1;
//        		String rawMsg1 = new String(fileData);
//        		while(true){
//        			pos = rawMsg1.indexOf(ASCII_ETX,pos+1); // ctl-C
//        			if(pos != -1){
//        				//rawMsg1 = rawMsg1.substring(pos);
//        				ctlCCount++;
//        				if(pos >= fileData.length)
//        					break;
//        			}
//        			else
//        				break;
//        		}
//        		System.out.println( " CtlC number = "+ctlCCount);
//        		PluginDataObject[] returnObjects = new PluginDataObject[0];
//        		return returnObjects;        	
//        	}
//        	else if(fileName.indexOf("query") != -1) {
//        		//This route for using query file to retrieve database
//        		NctextQuery query;
//        		try {
//        			query = new NctextQuery();
//        			/*return, disable this return until later*/ query.queryInput(fileName, fileData);
//        			return (new PluginDataObject[0]);
//
//        		} catch (PluginException e) {
//        			// TODO Auto-generated catch block
//        			e.printStackTrace();
//        			throw (e);
//        		}
//
//
//        	}
//        	else
        		return decode(fileData);
        }
        else {
        	System.out.println("decodeTextInputFile: bad input file reading");
        	return (new PluginDataObject[0]);
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
    public synchronized PluginDataObject[] decodeTextInputFileWithName(File inputFile, String inputfileName) throws DecoderException, PluginException {
        byte [] fileData = null;
        InputStream is = null;
        System.out.println("decodeTextInputFileWithName: enterred!");
        try {
            try {
                is = new FileInputStream(inputFile);

                fileData = new byte[(int) inputFile.length()];
                int bytesRead = is.read(fileData);
                // If we didn't or couldn't read all the data, signal the
                // fact by setting the data to null;
                if (bytesRead != fileData.length) {
                    fileData = null;
                }
                fileName = inputfileName;
                //int ind = fileName.indexOf('.');
                //if (ind > 0)
                //	fileExt = fileName.substring(ind+1);
            } catch (IOException ioe) {
            	System.out.println("Error reading input file " + fileName);
                fileData = null;
            }
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                	System.out.println("Could not close input file " + fileName);
                }
            }
        }
        if(fileData != null){
        	//Chin debug

//        	if(fileName.indexOf("test") != -1) {
//        		//This route for testing 
//        		System.out.println("NctextDecoder.decodeFile(): fileName= "+ fileName);
//        		int rsCount =0;
//        		int ctlCCount =0;
//        		int ctlACount =0;
//        		int pos = -1;
//        		
//        		String rawMsg = new String(fileData, 0, fileData.length);
//        		while(true){
//        			pos = rawMsg.indexOf(ASCII_RS,pos+1); // RS ^^
//        			if(pos != -1){
//        				//rawMsg = rawMsg.substring(pos);
//        				rsCount++;
//        				//System.out.println(" RS number = "+rsCount);
//        				//if(pos >= fileData.length)
//        					//break;
//        			}
//        			else
//        				break;
//        		}
//        		System.out.println(" RS number = "+rsCount);
//
//        		pos = -1;
//        		String rawMsg2 = new String(fileData);
//        		while(true){
//        			pos = rawMsg2.indexOf(ASCII_SOH, pos+1); // ctl-A
//        			if(pos != -1){
//        				//rawMsg2 = rawMsg2.substring(pos);
//        				ctlACount++;
//        				if(pos >= fileData.length)
//        					break;
//        			}
//        			else
//        				break;
//        		}
//        		System.out.println(" CtlA number = "+ctlACount);
//        		pos = -1;
//        		String rawMsg1 = new String(fileData);
//        		while(true){
//        			pos = rawMsg1.indexOf(ASCII_ETX,pos+1); // ctl-C
//        			if(pos != -1){
//        				//rawMsg1 = rawMsg1.substring(pos);
//        				ctlCCount++;
//        				if(pos >= fileData.length)
//        					break;
//        			}
//        			else
//        				break;
//        		}
//        		System.out.println( " CtlC number = "+ctlCCount);
//        		PluginDataObject[] returnObjects = new PluginDataObject[0];
//        		return returnObjects;        	
//        	}
//        	else if(fileName.indexOf("query") != -1) {
//        		//This route for using query file to retrieve database
//        		NctextQuery query;
//        		try {
//        			query = new NctextQuery();
//        			/*return, disable this return until later*/ query.queryInput(fileName, fileData);
//        			return (new PluginDataObject[0]);
//
//        		} catch (PluginException e) {
//        			// TODO Auto-generated catch block
//        			e.printStackTrace();
//        			throw (e);
//        		}
//
//
//        	}
//        	else
        		return decode(fileData);
        }
        else {
        	System.out.println("decodeTextInputFile: bad input file reading");
        	return (new PluginDataObject[0]);
        }
        	
    }

    
    public NctextRecordDao getDao() {
		return dao;
	}

	public void setDao(NctextRecordDao dao) {
		this.dao = dao;
	}

	   /**
     * 
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