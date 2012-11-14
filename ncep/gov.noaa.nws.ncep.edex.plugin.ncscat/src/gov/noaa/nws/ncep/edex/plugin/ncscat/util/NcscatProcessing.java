/**
 * NcscatProcessing
 *
 * This class sets the raw data to  Arraylist of bytes
 * separated by fixed length
 *
 * <pre>
 * Uma Josyula                               11/2009         Creation
 * B. Hebbard                                07/2012         Handle OSCAT / OSCAT_HI
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS system.
 */


package gov.noaa.nws.ncep.edex.plugin.ncscat.util;

import gov.noaa.nws.ncep.common.dataplugin.ncscat.NcscatPoint;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.NoSuchElementException;

import javax.xml.bind.JAXBException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.Logger;

import com.raytheon.uf.common.serialization.SerializationUtil;


public class NcscatProcessing {



	private final Logger log = Logger.getLogger(getClass().getName());
	private final Log theLogger = LogFactory.getLog(getClass());
	private List<NcscatPoint> item;
	List<Byte> newMessage=null;
	NcscatPoint sPointObj ;
	private Calendar endTime ; 
	private static int scatNumber ;
	private Calendar startTime;
	private static int recordLength;
	private String inputFileName;


	public NcscatProcessing() {
	}

	public  byte[]  separate(byte[] data) {
		doSeparate(data);
		try {
			if(newMessage ==null){
				return (byte[])null;
			} 
			else {
				byte[] tempBytes =	listByteTobyteArray(newMessage);
				return tempBytes;
			}

		} catch (NoSuchElementException e) {
			return (byte[])null;
		}

	}


	/**
	 * @param message
	 *            separate bulletins
	 */
	private void doSeparate(byte[] message) {
		int ji=0;
		int n = 0,doubleNcscat =0,scatXLen =0;
		ByteBuffer byteBuffer = null;
		byteBuffer = ByteBuffer.allocate(message.length);
		byteBuffer.put(message,0,message.length);
		int bitNum =0;
		int tempLength=message.length;
		int day,hour,min,sec;

		startTime = Calendar.getInstance(); 
		endTime = Calendar.getInstance(); 

		// Attempt to discriminate type of data by looking for 'period' of repeating date+hour fields.
		// TODO !! Guard against false negative which could occur if first 2 times straddle hour boundary,
		//      !! possibly by checking for match EITHER 1st-&-2nd OR 2nd-&-3rd
		if ((byteBuffer.getShort(0) == byteBuffer.getShort(1484)) && (byteBuffer.getShort(2) == byteBuffer.getShort(1486))) {
			// ASCAT_HI or EXASCT_HI (...which are big and little endian, respectively)
			n=1476;
			scatNumber=82;
			recordLength=742;
		}
		else if((byteBuffer.getShort(0)==  byteBuffer.getShort(764)) &&(byteBuffer.getShort(2)==  byteBuffer.getShort(766))) {
			// ASCAT or EXASCT (...which are big and little endian, respectively)
			n=756;
			scatNumber=42;
			recordLength=382;			
		}
		else if ((byteBuffer.getShort(0)==  byteBuffer.getShort(1376)) &&(byteBuffer.getShort(2)==  byteBuffer.getShort(1378))) {
			// QUIKSCAT -OR- OSCAT_HI (...which are big and little endian, respectively)
			n=1368;
			scatNumber = 76;
			recordLength =688;
		}
		else if ((byteBuffer.getShort(0) == byteBuffer.getShort(656)) && (byteBuffer.getShort(2) == byteBuffer.getShort(658))) {
			// OSCAT
			n=648;
			scatNumber = 36;
			recordLength = 328;
		}
		else if ((byteBuffer.getShort(0) == byteBuffer.getShort(2744)) && (byteBuffer.getShort(2) == byteBuffer.getShort(2746))) {
			// QUIKSCAT_HI
			n=2736;
			recordLength = 1372;
			scatNumber = 152;
		}
		else if((byteBuffer.getShort(56)==  byteBuffer.getShort(1486)) &&(byteBuffer.getShort(58)==  byteBuffer.getShort(1488))){
			// WSAT
			n=1422;
			scatNumber=79;
			recordLength=715;
	        //TODO:  Review this temporary fix by BH to see if a broader refactor might be better.
			//       For WindSat -- which has a 56-byte header once per file, remove it from the
			//       message array here, so we don't have to mess with it separately later.
            tempLength=message.length - 56;
            byte[] xmessage = new byte[tempLength];
            for (int i = 0; i<tempLength; i++) {
                xmessage[i] = message[i+56];
            }
            message = xmessage;
			byteBuffer = null;
			byteBuffer = ByteBuffer.allocate(tempLength);
			byteBuffer.put(message,0,tempLength);
			/* Original code as follows...
            byteBuffer = null;
            tempLength=message.length -56;
            byteBuffer = ByteBuffer.allocate(tempLength);
            byteBuffer.put(message,56,tempLength);
			 */
			//END of temporary fix by BH - Part 1 (compare Part 4)
		}

        //TODO:  Review this temporary fix by BH to see if a broader refactor might be better.
		//       Endianess correction must be applied to the 4-short date/time field on each
		//       line.  Here we use set the ByteOrder of the byteBuffer, so that the 4
		//       getShort(...) calls below will do the byte-flipping for us.  We also
		//       remember byteOrder for a bit later...  (Part 3)
		//       
		ByteOrder byteOrder;
		if ( (n == 1422) || // WSAT
		    ((n == 1368) && "oscat".equalsIgnoreCase(getInputFileName())) || // OSCAT_HI
		   "ascatx".equalsIgnoreCase(getInputFileName()) ) { // EXASCT or EXASCT_HI
			byteOrder = ByteOrder.LITTLE_ENDIAN;
		}
		else {
			byteOrder = ByteOrder.BIG_ENDIAN;
		}
		
		byteBuffer.order(byteOrder);
		//END of temporary fix by BH - Part 2 (new code added only)

		try {
			if(message!=null){
				int dNcscatMult =0;
				doubleNcscat = scatNumber*2;
				scatXLen =doubleNcscat*9+8;
				while (ji < tempLength) {
					day = byteBuffer.getShort(ji) ;
					hour= byteBuffer.getShort(ji+2);
					min = byteBuffer.getShort(ji+4);
					sec = byteBuffer.getShort(ji+6);

					if (day<0) break;
					
					if(ji<8){
						//stTime = endTime;
						startTime.set(Calendar.DAY_OF_YEAR, day);
						startTime.set(Calendar.HOUR_OF_DAY, hour);
						startTime.set(Calendar.MINUTE, min);
						startTime.set(Calendar.SECOND, sec);
					}
					//else{
						endTime.set(Calendar.DAY_OF_YEAR, day);
						endTime.set(Calendar.HOUR_OF_DAY, hour);
						endTime.set(Calendar.MINUTE, min);
						endTime.set(Calendar.SECOND, sec);
					//}

					ji=ji+scatXLen;

				}//for while

				newMessage= new ArrayList<Byte>(tempLength);

				while(bitNum<tempLength){
					int consBitNum = bitNum;
					while (bitNum<consBitNum+8)
					{
                        //TODO:  Review this temporary fix by BH to see if a broader refactor might be better.
						//       Here we again apply endianess correction to the 4-short date/time field on each
						//       line.  Above (via the ByteBuffer) was just for determining startTime and endTime
						//       of the entire data set.  Here we do it again as the 'message' array is moved to
						//       newMessage (ArrayList) for later return and eventual writing to the HDF5 data
						//       for each scan row.  Note that the byte-swapping was already done below for the
						//       data fields in each row following the leading date/time (in the process of
						//       regrouping the data so data for each point is together); here we do it for the
						//       date as well (where no other regrouping is required).
						//       
                        int offsetInDate = bitNum - consBitNum;  //  0 thru 7
                        if (byteOrder == ByteOrder.LITTLE_ENDIAN) {
                            //  need to flip the 2 bytes of each short; map 0 1 2 3 4 5 6 7 to 1 0 3 2 5 4 7 6, respectively
                            offsetInDate = offsetInDate/2*2+(1-offsetInDate%2);
                        }
                        newMessage.add((Byte)message[consBitNum+offsetInDate]);
					    /* Original code as follows...
						newMessage.add((Byte)message[bitNum]);
						*/
                        //END of temporary fix by BH - Part 3
						bitNum++;
					}//date field 8 bytes
					consBitNum = bitNum;
					while((scatXLen*dNcscatMult+7)<=bitNum && bitNum<(scatXLen*(dNcscatMult+1))){
						int calc=0;
						for (int qNoIndex =0; qNoIndex<doubleNcscat;qNoIndex++){
							for(int rou=0;rou<=8;rou++){

								if(bitNum<tempLength){
									calc=consBitNum+doubleNcscat*rou+qNoIndex;
			                        //TODO:  Review this temporary fix by BH to see if a broader refactor might be better.
									//       Problem with the following is it applies the 56-byte bias (for WindSat) to the
									//       address to copy from, but for the date transfer code to work above, bitNum
									//       must already have had this bias applied (fix above moved it), and so consBitNum
									//       -- the base address for this row of data, after the date field -- must also
									//       have already reflected this correction.  (Note that the 56-byte header is once
									//       per file, rather than once per row.)
									/* Original code [removed] as follows...
                                    if(recordLength==715){
                                        calc=calc+56;
                                    }
                                    */
							        //END of temporary fix by BH - Part 4 (compare Part 1)
									if(byteOrder == ByteOrder.LITTLE_ENDIAN) {// swap the bytes
										newMessage.add(message[calc+1]);
										bitNum++; 
										newMessage.add(message[calc]);//since 2 bytes form a short
										bitNum++;
									}
									else{
										newMessage.add(message[calc]);
										bitNum++; 
										newMessage.add(message[calc+1]);//since 2 bytes form a short
										bitNum++;
									}

								}//end of if

							}//end of for
							qNoIndex++;
						}
						dNcscatMult++;

					}

				}

			}//if message is not null  						
		}// end of try block
		catch (Exception e) {
			e.printStackTrace();
			if (log.isInfoEnabled()) {
				log.info("No valid records found!");
			}
			theLogger.warn("No valid records found!");
		}
		return;
	}





    /**
     * processHDF5Data  --  SHOULD NOT BE USED IN PRESENT FORM 
     *                      Please see NcscatResource.processHDF5Data(...)
     * 
     * @return List<NcscatPoint>
     * 
     * @deprecated Please see NcscatResource.processHDF5Data(...)
     */
    @Deprecated


	public List<NcscatPoint> processHDF5Data(byte[] hdf5Msg){
		int ji=0, bitNum=0;
		int day,hour,min,sec;
		item= new ArrayList<NcscatPoint>();
		// TODO - Caution!  Separate startTime Calendar object needs to be allocated
		//                  for each point row, since will be shared by all points
		//                  in that row.  See below.
		startTime = Calendar.getInstance();
		ByteBuffer byteBuffer = null;
		byteBuffer = ByteBuffer.allocate(hdf5Msg.length);
		byteBuffer.put(hdf5Msg,0,hdf5Msg.length);

		while (  ji < hdf5Msg.length) {

			day = byteBuffer.getShort(bitNum) ;
			hour= byteBuffer.getShort(bitNum+2);
			min = byteBuffer.getShort(bitNum+4);
			sec = byteBuffer.getShort(bitNum+6);
			ji=ji+8;
			bitNum=bitNum+8;
			// TODO - Caution!  Need to allocate new startTime here...
			startTime.set(Calendar.DAY_OF_YEAR, day);
			startTime.set(Calendar.HOUR_OF_DAY, hour);
			startTime.set(Calendar.MINUTE, min);
			startTime.set(Calendar.SECOND, sec);
			for(int j=ji;j<ji+scatNumber*18 && bitNum <hdf5Msg.length;j=j+18){
				sPointObj = new NcscatPoint();
				// TODO - continued -  otherwise all points in all rows get same time here
				sPointObj.setStTime(startTime);
				sPointObj.setLat(byteBuffer.getShort(j));
				sPointObj.setLon(byteBuffer.getShort(j+2));
				sPointObj.setIql(byteBuffer.getShort(j+4));
				sPointObj.setIsp(byteBuffer.getShort(j+6));
				sPointObj.setIdr(byteBuffer.getShort(j+8));
				sPointObj.setIrn(byteBuffer.getShort(j+10));
				sPointObj.setIb1(byteBuffer.getShort(j+12));
				sPointObj.setIb2(byteBuffer.getShort(j+14));
				sPointObj.setIb3(byteBuffer.getShort(j+16));
				bitNum=bitNum+18;
				item.add(sPointObj);

			}//for

			ji=bitNum;

		}//for while

		return item;

	}

	public  Calendar getStartTime() {
		return startTime;
	}


	public  void setStartTime(Calendar startTime) {
		this.startTime = startTime;
	}

	public Calendar getEndTime() {
		return endTime;
	}
	public void setEndTime(Calendar endTime) {
		this.endTime = endTime;
	}

	private static byte[] listByteTobyteArray(List<Byte> temp) {
		byte[] byteArray = new byte[temp.size()];
		int jkl=0;
		for(Byte current:temp){
			byteArray[jkl] =current;
			jkl++;
		}

		return byteArray;
	}

	public static int getRecordLength() {
		return recordLength;
	}

	public static void setRecordLength(int recordLength) {
		NcscatProcessing.recordLength = recordLength;
	}
	public String getInputFileName() {
		return inputFileName;
	}

	public void setInputFileName(String inputFileName) {
		this.inputFileName = inputFileName;
	}

}
