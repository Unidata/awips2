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
 * 04/2013		B. Hebbard  IOC version (for OB13.4.1)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.edex.plugin.ntrans.decoder;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
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
import com.raytheon.uf.common.time.TimeRange;
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
		long startPos;
		long endPos;
	}
		
	/**
     * 
     * @param inputFile : ingest file to be decoded
     * @return
     * @throws DecoderException
     * @throws PluginException 
     * 
     */
    public 
         //synchronized
                        PluginDataObject[] decode(File inputFile) throws DecoderException, PluginException {
        byte [] fileData = null;
        byte [] headerData = null;
        InputStream inputStream = null;
    	String fileTitle = null;
    	int fileMaxFrame = 0;
    	int fileVersion = 0;
    	int fileMachineType = 0;
    	int frameSizeX = 0;
    	int frameSizeY = 0;
    	byte [] fileReservedSpace = new byte[38];  //TODO  symbolic
		
		List<FrameHeader> frameHeaders = new ArrayList<FrameHeader>();
		List<NtransRecord> records = new ArrayList<NtransRecord>();
		
        try {
        	
            fileName = inputFile.getName();
        	inputStream = new FileInputStream(inputFile);
        	
        	//  Read the entire file
        	//  TODO:  Wish we didn't have to do that, but decode method
        	//         wants to return all PDOs at once, so it's all got
        	//         to sit in memory anyway.  Propose architecture change (?)
        	//         say, decode(File,IDataReturners) to allow sending up
        	//         PDOs as completed, so memory can be released?
        	//         Anyway, this allows us to use NIO buffer tricks...
        	
        	long inputFileCount = inputFile.length();
        	if (inputFileCount > Integer.MAX_VALUE) {
				throw new DecoderException("Input file too big! " + inputFileCount + " bytes");
        	}
        	byte[] fileBytes = new byte[(int)inputFileCount];
        	int fileBytesReadIn = inputStream.read(fileBytes);
        	if (fileBytesReadIn != inputFileCount) {
				throw new DecoderException("Input file not read correctly/completely! " + fileBytesReadIn + " of " + inputFileCount + " bytes read");
        	}
        	
        	ByteBuffer byteBuffer = ByteBuffer.wrap(fileBytes);
			
        	byteBuffer.order ( determineEndianess (byteBuffer) );
			
			byteBuffer.rewind();
        	
        	//  Read NTRANS metafile header
			
    		byte[] fileTitleBytes = new byte[32];  //TODO  symbolic
    		byteBuffer.get(fileTitleBytes);
    		fileTitle = new String(fileTitleBytes).trim();
    		//System.out.println("[File title:  " + fileTitle + "]");
    		
    		fileMaxFrame     = toUnsigned(byteBuffer.getShort());
    		fileVersion      = toUnsigned(byteBuffer.getShort());
    		fileMachineType  = toUnsigned(byteBuffer.getShort());
    		frameSizeX       = toUnsigned(byteBuffer.getShort());
    		frameSizeY       = toUnsigned(byteBuffer.getShort());
    		
    		byte[] fileReserved = new byte[38];
    		byteBuffer.get(fileReserved);
    		
    		//  Read NTRANS frame headers (follow file header; precede frame contents)
    		
    		for (int frame = 0 ; frame < fileMaxFrame ; frame++) {
    			byte[] labelTitleBytes = new byte[64];  //TODO  symbolic
    			byteBuffer.get(labelTitleBytes);
    			StringBuffer sb = new StringBuffer();
    			for (int i=0 ; i < 64 && labelTitleBytes[i] != 0x00 ; i++) {  //TODO  symbolic
    				sb.append((char)labelTitleBytes[i]);
    			}
    			String labelTitle = new String(sb);
    			long startPos = toUnsigned(byteBuffer.getInt());
    			long endPos   = toUnsigned(byteBuffer.getInt());
    			if (startPos == 0 && endPos == 0) break;
    			//System.out.println("[startPos " + startPos + " endPos " + endPos + "]");
    			FrameHeader fh = new FrameHeader ();
    			if (labelTitle.length() < 8) {  //TODO check!
        			fh.validTimeString = labelTitle;
        			fh.productNameString = "";
    			}
    			else {
        			fh.validTimeString = labelTitle.substring(0, 9);
        			fh.productNameString = labelTitle.substring(9);
    			}
    			fh.startPos = startPos;
    			fh.endPos = endPos;
    			frameHeaders.add(fh);
    		}
    	
    		//  Read NTRANS frames (CGM coded images)
    		
    		//for (FrameHeader fh : frameHeaders) {
    		for (int frame = 0 ; frame < frameHeaders.size() ; frame++) {
    			FrameHeader fh = frameHeaders.get(frame);
    			int startPos = (int) (fh.startPos - 4);
    			int endPos = (int) (fh.endPos + 0);
    			int imageLength = (int) (endPos - startPos + 0);
    			byte[] frameImage = new byte[imageLength];
    			//int imageBytesRead = inputStream.read(frameImage);
    			byteBuffer.position(startPos);
    			byteBuffer.get(frameImage);
    			
    			//  Create NTRANS record (PDO)
    			
    			NtransRecord record = new NtransRecord();
    			
    			record.setReportType("NTRANS");
    			record.setModelName(inferModel(inputFile.getName())
    					.replaceAll("_", "-"));
    			record.setMetafileName(inputFile.getName()
    					.replaceAll("_", "-"));
    			record.setProductName(fh.productNameString
    					.trim()
    					.replaceAll("_", "-")  //TODO:  Cleanup/Combine in regex.  (Wanted to test one at a time.)
    					.replaceAll(" ", "-")
    					.replaceAll(":", "-")
    					.replaceAll("\\.", "-")
    					.replaceAll("&", "and")  //TODO  acceptable??
    					.replaceAll(",", "-")
    					.replaceAll("--", "-")
    					.replaceAll("--", "-")); // twice
    			record.setDataTime(createDataTime(fh.validTimeString));
    			record.setValidTimeString(fh.validTimeString);
    			record.setImageData(frameImage);
    			record.setImageSizeX(frameSizeX);
    			record.setImageSizeY(frameSizeY);
    			record.setImageByteCount(frameImage.length);
    			record.setFrameNumberInFile(frame);
    			record.setTotalFramesInFile(frameHeaders.size());

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

	private DataTime createDataTime(String validTimeString) {
		
		//  Create a standard DataTime object, with proper timing
		//  determined from valid time string (e.g., "27/06V042")
		
		//  Get a Calendar object.  Fields default to current time.
		
		Calendar calendar = Calendar.getInstance();
		Calendar now      = Calendar.getInstance();
		
		//  Get components of validTimeString as 'int's
		
		//  TODO -- generalize to take "F" as well as "V" strings?
		//          if so, may want to put in central utilities.
		//  TODO -- use more general/flexible pattern matching?
		//  TODO -- improve error handling/recovery
		
		//try {
			String validDateString = validTimeString.substring(0, 2);
			int validDate = Integer.parseInt(validDateString);
			String validHourString = validTimeString.substring(3, 5);
			int validHour = Integer.parseInt(validHourString);
			String fcstHourString = validTimeString.substring(6, 9);
			int fcstHour = Integer.parseInt(fcstHourString);
		//}
		//catch (Exception e) {
			//  TODO
			//return new DataTime(calendar, 0);
		//}
		
		//  Alter specific fields to set to valid time.
		//  TODO -- use cycle time string if available to set year, month
		//          but must be very careful about applying rules 
		
		calendar.set(Calendar.DAY_OF_MONTH, validDate);
		calendar.set(Calendar.HOUR_OF_DAY, validHour);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		
		//  Now subtract forecast hours to get initial (reference) or cycle time
		
		calendar.add(Calendar.HOUR_OF_DAY, -fcstHour);
		
		//  Careful here:  Calendar assumed valid time is in the current month
		//  (which, even with current data, could be off one month either way).
		
		//  What we want for the month of the valid time is the latest month
		//  such that the (deduced) cycle time is not in the future (that is,
		//  not later than decode time "now").
		
		//  The following two steps are designed to get us there.  (Note that
		//  either or both can execute.)  First, if the inferred cycle time is
		//  in the past, try adding a month...
		
		if (calendar.before(now)) {
			calendar.add(Calendar.MONTH, 1);
		}
		
		//  Now -- regardless of whether the previous step executed (no "else" here)
		//  if the inferred cycle time is in the future, back up one month.
		
		if (calendar.after(now)) {
			calendar.add(Calendar.MONTH, -1);
		}
		
		//  Return DataTime, constructed from cycle time and forecast hour.
		
		DataTime dataTime = new DataTime(calendar, fcstHour * 3600);
		
		return dataTime;
	}

	private DataTime createDataTime(String initialTimeString, String validTimeString) {
		
		//  FUTURE -- use initialTimeString to influence deduction of
		//            full initial (reference) time from validTimeString
		
		//  For now, initialTimeString not used...
		
		return createDataTime(validTimeString);
		
		/*
		
		//  Create a standard DataTime object, with proper timing
		//  determined from valid time string (e.g., "27/06V042")
		
		//  Get components of validTimeString as 'int's
		//  TODO -- generalize to take "F" as well as "V" strings?
		//          if so, may want to put in central utilities.
		//  TODO -- use more general/flexible pattern matching?
		
		String validDateString = validTimeString.substring(0, 1);
		int validDate = Integer.parseInt(validDateString);
		String validHourString = validTimeString.substring(3, 4);
		int validHour = Integer.parseInt(validHourString);
		String fcstHourString = validTimeString.substring(6, 8);
		int fcstHour = Integer.parseInt(fcstHourString);
		
		//  Get a Calendar object.  Fields default to current time.
		
		Calendar calendar = Calendar.getInstance();
		
		//  Alter specific fields to set to valid time.
		//  TODO -- use cycle time string if available to set year, month
		//          but must be very careful about applying rules 
		
		calendar.set(Calendar.DAY_OF_MONTH, validDate);
		calendar.set(Calendar.HOUR_OF_DAY, validHour);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		
		//  Now subtract forecast hours to get initial (reference) time...
		
		calendar.add(Calendar.HOUR_OF_DAY, -fcstHour);
		
		//  ...for DataTime constructor that wants it
		//  (DataTime has many constructors, but none that
		//   takes validTime and fcstHour)
		
		DataTime dataTime = new DataTime(calendar, fcstHour);
		
		return dataTime;
		
		*/
	}

	private String inferModel(String fileName) {
		
		//  Infer the model name from the file name
		//  (Use heuristics gleaned from $NTRANS_META contents)
		//  TODO -- continuous improvement!
		
		String modelName = "";  //TODO "default" model...?
		if (fileName.startsWith("ecens_prob")) {
			modelName = "ecens";
		}
		else if (fileName.startsWith("Day") ||
				 fileName.startsWith("Night") ||
				 fileName.startsWith("Official")) {
			modelName = "medrt";
		}
		else if (fileName.startsWith("meta_sst")) {
			modelName = "sst";
		}
		else if (/* fileName.matches("^[A-Z]") */
				fileName.contains("_GFS")) {
			modelName = "vaftad";
		}
		else if (fileName.contains("_2")) {
			modelName = fileName.substring(0, fileName.indexOf("_2"));
			if (modelName.equals("jma")) {
				modelName = "jmap";
			}
		}

		return modelName;
	}

	private ByteOrder determineEndianess(ByteBuffer byteBuffer) {
		//TODO Review This -- I think this is the same criterion
		//     used in legacy; see scan_hmeta in process_meta.c
		//     (where do_flip is set)
		byte lowAddressByteOfTheMachineTypeField = byteBuffer.get(36);
    	if (lowAddressByteOfTheMachineTypeField == 0) {
    		return ByteOrder.BIG_ENDIAN;
    	}
    	else {
    		return ByteOrder.LITTLE_ENDIAN;
    	}
	}

	private int toUnsigned(short shorty) {
		return shorty & 0xffff;
	}

	private long toUnsigned(int inty) {
		return inty & 0xffffffffL;
	}

}


