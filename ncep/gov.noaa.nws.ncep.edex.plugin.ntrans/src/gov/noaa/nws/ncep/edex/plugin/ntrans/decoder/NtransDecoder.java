/**
 * 
 * NTRANS Decoder
 * 
 * This class decodes legacy NTRANS Metafiles.
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 03/2013		B. Hebbard  Initial creation
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.edex.plugin.ntrans.decoder;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import gov.noaa.nws.ncep.common.dataplugin.ntrans.NtransRecord;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.edex.esb.Headers; 


public class NtransDecoder extends AbstractDecoder {

	private static  String pluginName ;//= "NTRANS";
	/**
	 * Constructor
	 * 
	 * @throws DecoderException
	 */
	public NtransDecoder(String name) throws DecoderException {
		 pluginName = name; 
	}

	private class FrameHeader {
		String validTimeString;
		String productNameString;
		int startPos;
		int endPos;
	}
		
	/**
     * 
     * @param inputFile : ingest file to be decoded
     * @return
     * @throws DecoderException
     * @throws PluginException 
     * 
     */
    public synchronized PluginDataObject[] decodeNtransMetafile(File inputFile) throws DecoderException, PluginException {
        byte [] fileData = null;
        byte [] headerData = null;
        InputStream inputStream = null;
    	String fileTitle = null;
    	short fileMaxFrame = 0;
    	short fileVersion = 0;
    	short fileMachineTypes = 0;
    	byte [] fileReservedSpace = new byte[42];
		
		List<FrameHeader> frameHeaders = new ArrayList<FrameHeader>();
		List<NtransRecord> records = new ArrayList<NtransRecord>();
		List<PluginDataObject> pdolist = new ArrayList<PluginDataObject>();
    	
        try {
            
            fileName = inputFile.getName();
        	inputStream = new FileInputStream(inputFile);
        	
        	//  Read NTRANS metafile header
        	
        	/*
        	headerData = new byte[80];
        	int headerBytesRead = inputStream.read(headerData);
        	*/
    		
    		byte[] fileTitleBytes = new byte[32];
    		int fileTitleBytesRead = inputStream.read(fileTitleBytes);
    		fileTitle = new String(fileTitleBytes);
    		//System.out.println("[File title:  " + fileTitle + "]");
        	
    		byte[] remHeaderBytes = new byte[48];
    		int remHeaderBytesRead = inputStream.read(remHeaderBytes);
    		ByteBuffer byteBuffer = null;
    		byteBuffer = ByteBuffer.allocate(48);
    		byteBuffer.put(remHeaderBytes,0,48);
    		byteBuffer.rewind();
    		
    		fileMaxFrame = byteBuffer.getShort();
    		fileVersion = byteBuffer.getShort();
    		fileMachineTypes = byteBuffer.getShort();
    		
    		//  Read NTRANS frame headers (follow file header; precede frame contents)
    		
    		for (int frame = 0 ; frame < fileMaxFrame-1 ; frame++) {
    			if (frame>=500) break;
    			byte[] frameHeaderBytes = new byte[72];
    			int frameHeaderBytesRead = inputStream.read(frameHeaderBytes);
    			if (frameHeaderBytes[0] == 0x00 && frameHeaderBytes[1] == 0x20) break;  // hit data
    			byteBuffer = ByteBuffer.allocate(72);
    			byteBuffer.put(frameHeaderBytes);
    			byteBuffer.rewind();
    			byte[] labelTitleBytes = new byte[64];
    			byteBuffer.get(labelTitleBytes);
    			String labelTitle = new String(labelTitleBytes);
    			int startPos = byteBuffer.getInt();
    			int endPos   = byteBuffer.getInt();
    			//System.out.println("[startPos " + startPos + " endPos " + endPos + "]");
    			FrameHeader fh = new FrameHeader ();
    			fh.validTimeString = labelTitle.substring(0, 8);
    			fh.productNameString = labelTitle.substring(9);
    			fh.startPos = startPos;
    			fh.endPos = endPos;
    			frameHeaders.add(fh);
    		}
    	
    		System.out.println("this");
    		
    		//  Read NTRANS frames (CGM coded images)
    		
    		for (FrameHeader fh : frameHeaders) {
    			int startPos = fh.startPos;
    			int endPos = fh.endPos;
    			byte[] frameImage = { 0x04 , 0x02 };  // test
    			//byte[] frameImage = new byte[endPos - startPos + 0];
    			//int imageBytesRead = inputStream.read(frameImage);
    			System.out.println("stop");
    			
    			//  Create NTRANS record (PDO)
    			
    			NtransRecord record = new NtransRecord();
    			record.setInputFile(inputFile.getName());
    			record.setConvertedMessage(frameImage);
    			record.setValidTimeString(fh.validTimeString);
    			record.setProductNameString(fh.productNameString);
    			Calendar dummyTBD = Calendar.getInstance();
    			record.setStartTime(dummyTBD);
    			record.setEndTime(dummyTBD);
    			record.setRecordLength(0);
    			record.setDataTime(new DataTime(dummyTBD));

    			if (record != null) {
    				try {
    					record.setPluginName(pluginName);
    					record.constructDataURI();	
    				}
    				catch (PluginException e) {
    					throw new DecoderException("Error constructing dataURI", e);
    				}
    			}

    			records.add(record);
    		}

        }
        catch (IOException ioe) {
        	logger.error("Error reading input file " + inputFile.getName(), ioe);
        	fileData = null;
        }
        catch (Exception e) {
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

        if (records.isEmpty()) {
			return new PluginDataObject[0];
		} else {
			PluginDataObject[] pdos = records.toArray(new PluginDataObject[0]);
			return pdos;
		}


    }
}


