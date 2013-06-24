/**
 * This code has been developed by NWS/OST to support AWIPS II
 * 
 */
package gov.noaa.nws.ost.edex.plugin.binlightning;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.binlightning.impl.BinLightningFactory;
import com.raytheon.edex.plugin.binlightning.impl.IBinLightningDecoder;
import com.raytheon.edex.plugin.binlightning.impl.LightningDataSource;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgMsgType;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.edex.decodertools.core.BasePoint;
import com.raytheon.uf.edex.decodertools.core.IBinDataSource;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * BinLigntningDecoderUtil
 * 
 * Utility method to decode legacy (bit-shifted) or new encrypted bin lightning
 * data
 * 
 * Some utility code were adapted from vendor supplied sample code
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20130503        DCS 112 Wufeng Zhou To handle both the new encrypted data and legacy bit-shifted data
 * 
 * </pre>
 * 
 * @author Wufeng Zhou
 * 
 */
public class BinLigntningDecoderUtil {

	/**
	 * Message type for keep alive data records. 
	 */
	final static short KEEP_ALIVE_TYPE = 0x0000;

	/**
	 * Message type for lightning data records. 
	 */
	final static short LIGHTNING_TYPE = 0x00ff;

	/**
	 * If there are more elements within the data record, this terminator is used.
	 */
	final static byte[] MORE_TERM_BYTES = {0x0d, 0x0d, 0x0a, 0x00};
	/**
	 * Last element within data records should be terminated by these 4 bytes.
	 */
	final static byte[] LAST_TERM_BYTES = {0x0d, 0x0d, 0x0a, 0x03};

	/**
	 * WMO header start bytes, optional (it is known that TG will strip this away)
	 */
	final static byte[] WMO_HEADER_START_BYTES = {0x01, 0x0d, 0x0d, 0x0a};

	/* Size of binary NWS lightning data record. */
	static final int BINLIGHTNING_RECORD_SIZE = 32;

    private static Log logger = LogFactory.getLog(BinLigntningDecoderUtil.class);

	/**
	 * extracted from the decode() of the original
	 * com.raytheon.edex.plugin.binlightning.BinLightningDecoder class
	 * 
	 * @param pdata
	 * @return
	 */
	public static List<LightningStrikePoint> decodeBitShiftedBinLightningData(byte[] pdata) {
		List<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();

		IBinDataSource msgData = new LightningDataSource(pdata);

		boolean continueDecode = true;
		while (continueDecode) {
			IBinLightningDecoder decoder = BinLightningFactory.getDecoder(msgData);

			switch (decoder.getError()) {
			case IBinLightningDecoder.NO_ERROR: {
				for (LightningStrikePoint strike : decoder) {
					strikes.add(strike);
				}
				break;
			}
			default: {
				continueDecode = false;
			}
			}
		}
		return strikes;
	}
	
	/**
	 * decode the new bin lightning data, after the data record is decrypted, and it is not keep-alive record
	 * @param pdata
	 * @return
	 */
	public static List<LightningStrikePoint> decodeDecryptedBinLightningData(byte[] data) {
		List<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();
		
		int offset = 0;
		// length of data to be put in ByteBuffer for easier reading of the little-endian data
		//   data put into ByteBuffer would be byte 2 to byte 27 (skipping leading 2 type bytes and trailing 4 terminator bytes) 
		int dataLen = BINLIGHTNING_RECORD_SIZE - 2 - LAST_TERM_BYTES.length;
		
		for (int i = 0; i < data.length / BINLIGHTNING_RECORD_SIZE; i++) {
			ByteBuffer buffer = ByteBuffer.allocate(dataLen);
			buffer.order(ByteOrder.LITTLE_ENDIAN);
	
			// put the data into ByteBuffer			
			buffer.put(data, offset + 2, dataLen);

			// Reset buffer position to read in data we just stored.
			buffer.position(0);
			
			// Do NOT change the read order below
			// read signed 16 bit integer as short and assigned to short
			// read other 16 bit (unsigned) integer as short, but assign to int after bit & with 0xffff, so no negatives when first bit is 1 
			// Read count of seconds first
			long epochSeconds = buffer.getInt() & 0xffffffffL;
			// Convert to millisecond and add on millisecond offset
			int miliseconds = buffer.getShort() & 0xffff; // valid range: 0 to 999
			long epochTime = epochSeconds * 1000 + miliseconds;
	
			// read lat/lon as float 
			float lat = buffer.getFloat(); // valid range: -90.0 to 90.0
			float lon = buffer.getFloat(); // valid range: -180.0 to 180.0
	
			// vendor, 0x01 for CONUS, i.e. NLD data;
			//         0x02 for long range source, GLD360? 
			int vendor = buffer.getShort() & 0xffff; // valid values: 0x0001 (CONUS) or 0x0002 (long range source)

			int strokeType = buffer.getShort() & 0xffff; // 0x0000 for cloud-to-ground, 0x00ff for cloud-to-cloud, 0xffff for total flash
			short strokeKiloAmps = buffer.getShort(); // valid range: -254 to 254, specifically 16 bit signed integer
			int strokeMultiplicity = buffer.getShort() & 0xffff; // i.e. stroke count, valid range: 0 to 15
			int strokeDuration = buffer.getShort() & 0xffff; // valid range: 0 to 65535 (i.e., looks like unsigned short)
			int reserved = buffer.getShort() & 0xffff;

            // Create the strike record from the report info and base time information. 
			BasePoint base = new BasePoint(lat, lon);
			Calendar cal = Calendar.getInstance();
			cal.setTimeInMillis(epochTime);
			base.setYear(cal.get(Calendar.YEAR));
			base.setMonth(cal.get(Calendar.MONTH) + 1);
			base.setDay(cal.get(Calendar.DAY_OF_MONTH));
			base.setHour(cal.get(Calendar.HOUR_OF_DAY));
			base.setMinute(cal.get(Calendar.MINUTE));
			base.setSecond(cal.get(Calendar.SECOND));
			base.setMillis(cal.get(Calendar.MILLISECOND));
			
			// new spec does not seem to have lightning message type indicator such as FL (Flash Lightning) or RT (Real Time flash lightning)
			// The source of lightning data in the vendor specific data bytes (byte 16-17) may related to this (???),
			// and it is used here for now.  04/182/013 Wufeng Zhou
			/** 05/02/2013, found DSI-9603 Spec (http://www1.ncdc.noaa.gov/pub/data/documentlibrary/tddoc/td9603.pdf) on NLDN lightning data format,
			 *   on Message Type and Stroke Type:
			 *    POS: 37-38 Message Type
			 *         This field identifies whether this record was U.S. continental data or an international location.
			 *         Values are “FL” and “RT”.
			 *         A value of “FL” stands for FLASH and identifies this record as U.S. data.
			 *         A value of “RT” stands for Real-Time data type and identifies this record as international data.
			 *    POS: 40-41 Stroke Type
			 *         This field identifies whether this lightning stroke was cloud-to-ground or cloud-to-cloud.
			 *         Values are “CG” for cloud-to-ground and “CC” for cloud-to-cloud. FLASH (FL) data are always cloud-to-ground 
			 *         while REAL-TIME (RT) data can be either type. 
			 */
			LtgMsgType msgType = LtgMsgType.STRIKE_MSG_FL; // as default
			if (vendor == ((short)0x0001)) { // CONUS source
				msgType = LtgMsgType.STRIKE_MSG_FL; 
			} else if (vendor == ((short)0x0002)) { // long range source 
				msgType = LtgMsgType.STRIKE_MSG_RT;
			}
			
            LightningStrikePoint lsp = new LightningStrikePoint(base, lat, lon, msgType);
            LtgStrikeType ltgStrikeType = LtgStrikeType.STRIKE_CG; // default ??
            if (strokeType == 0x0000) {
            	ltgStrikeType = LtgStrikeType.STRIKE_CG;
            } else if (strokeType == 0x00ff) {
            	ltgStrikeType = LtgStrikeType.STRIKE_CC;
            } else if (strokeType == 0xffff) {
            	ltgStrikeType = LtgStrikeType.STRIKE_TF;
            }
            lsp.setType(ltgStrikeType);
            
            // as of OB13.3 for World Wide Lightning Location Network (WWLLN) data (decoded by textlightning though, not this bin lightning decoder),
            // added lightning source field in LightningStrikePoint, as well as column in binlightning database table defaults to NLDN 
			if (vendor == ((short)0x0001)) { // CONUS source
				lsp.setLightSource("NLDN"); 
			} else if (vendor == ((short)0x0002)) { // long range source, i.e., GLD360. 
				// However, since the database table column for lightning source is 5 characters  
				lsp.setLightSource("GLD"); 
			}

            lsp.setStrikeCount(strokeMultiplicity);
            lsp.setStrikeStrength(strokeKiloAmps);
            // stroke duration does not seem to be used
            
            strikes.add(lsp);
		}
		return strikes;
	}
	
	/**
	 * Decode bin lightning data, able to handle both legacy bit-shifted and new encryted data
	 * 
	 * The modified BinLightningDecoder.decode() method will use this method to decode data, which 
	 *   will try to decrypt first, and decode the old fashioned way when decryption fails   
	 * 
	 * @param data - data content from file, including WMO header section
	 * @param pdata - data with WMO header stripped, optional, if null, will strip WMO header internally from passed in data parameter 
	 * @param traceId - the file name of the data to be deoced
	 * @param dataDate - date of the data, optional, used as a hint to find appropriate encryption key faster
	 * @return null if keep-alive record, otherwise a list (could be empty) of LightningStrikePoint
	 */
	public static List<LightningStrikePoint> decodeBinLightningData(byte[] data, byte[] pdata, String traceId, Date dataDate) {
		if (pdata == null) { // if data without header not passed, we'll strip the WMO header here
			WMOHeader header = new WMOHeader(data);
			if (header.isValid() && header.getMessageDataStart() > 0) {
				pdata = new byte[data.length - header.getMessageDataStart()];
				System.arraycopy(data, header.getMessageDataStart(), pdata, 0, data.length - header.getMessageDataStart());
			} 
		}
		
        List<LightningStrikePoint> strikes = new ArrayList<LightningStrikePoint>();
        boolean needDecrypt = true; // set as default unless clear evidence says otherwise
        boolean decodeDone = false;
		EncryptedBinLightningCipher cipher = new EncryptedBinLightningCipher();

		//
		// Using different WMO headers to indicate whether the data is encrypted or not would be a nice option. 
		// However, that idea has been discussed but not adopted.   
        // If in the future, WMO header can be different for legacy and encrypted data, or some other metadata can be used to decide
        //   whether deceyption is needed, logic can be added here.
        //
        // Before that happens, we'll use hints and trial & error to decode the data
		//   Hints: Per lightning data format spec, there are 3 bytes in the WMO header starting line that indicates the size of the encrypted block 
		//          or the ASCII sequence # for legacy bit-shifted data 
		// However, the starting line is optional and AWIPS decode may not see it at all because TG will strip that starting line away
		// We'll try to use this hint first, if is is not found, then trial and error way to decrypt and decode
        if (data != null) {
			byte[] sizeSeqBytes = BinLigntningDecoderUtil.findSizeOrSeqBytesFromWMOHeader(data);
	        if (sizeSeqBytes != null) { 
	        	// if this is in the header (which may not), use that as a hint to determine which decoding route to go
	        	if (BinLigntningDecoderUtil.isPossibleWMOHeaderSequenceNumber(sizeSeqBytes)  
	        			&& BinLigntningDecoderUtil.getEncryptedBlockSizeFromWMOHeader(sizeSeqBytes) != pdata.length) {
	        		// looks like a sequence #, and if treat as size, it does not equal to the data block size, so most likely legacy data
	        		needDecrypt = false;
	        	}
	        }
        }
        
        if (needDecrypt) {
    		try {
    			byte[] decryptedData = cipher.decryptData(pdata, dataDate);	                    
    			// decrypt ok, then decode, first check if keep-alive record
    			if (BinLigntningDecoderUtil.isKeepAliveRecord(decryptedData)) {
    				logger.info(traceId + " - Keep-alive record detected, ignore for now.");
    				decodeDone = true;
    				return null; 
    			}
    			// not keep-alive record, then decode into an ArrayList<LightningStrikePoint> of strikes
    			strikes = BinLigntningDecoderUtil.decodeDecryptedBinLightningData(decryptedData); 
    			decodeDone = true;
    		} catch (IllegalBlockSizeException e) {
				logger.info(traceId + " - " + e.getMessage() + ": Decryption failed, will try decode the old-fashioned way.");
    			decodeDone = false;
    		} catch (BadPaddingException e) {
				logger.info(traceId + " - " + e.getMessage() + ": Decryption failed, will try decode the old-fashioned way.");
    			decodeDone = false;
    		} catch (BinLightningDataDecryptionException e) {
				logger.info(traceId + " - " + e.getMessage() + ": Decryption failed, will try decode the old-fashioned way.");
    			decodeDone = false;
			}
        }

        if (decodeDone == false) { // not decoded through decrypt->decode process, try the legacy decoder
        	strikes = BinLigntningDecoderUtil.decodeBitShiftedBinLightningData(pdata); 
        }
        
        return strikes;
	}
	
	/**
	 * Determines if the bytes passed are a standard "NWS Keep Alive" message.
	 * 
	 * Note, record type in data are represented as 16-bit little-endian integer
	 *       i.e., for keep alive record type (0x0000), byte[0] will be 0x00, byte[1] will be ox00 
	 * 
	 * @param raw - Buffer containing bytes to check.
	 * 
	 * @return true if bytes specified match a full "keep alive" message. 
	 */

	public static boolean isKeepAliveRecord(byte[] data) {
		return (data.length == 6) && ((data[0] & 0xff) == (KEEP_ALIVE_TYPE & 0xff))
				&& ((data[1] & 0xff) == ((KEEP_ALIVE_TYPE >> 8) & 0xff))
				&& isLastTerminator(data, 2, data.length - 2);
	}
	
	/**
	 * Determines if the bytes passed are a standard "NWS Last Element" terminator sequence.
	 * 
	 * Note, record type in data are represented as 16-bit little-endian integer
	 *       i.e., for lightning data record type (0x00ff), byte[0] will be 0xff, byte[1] will be ox00 
	 *       
	 * @param raw - Buffer containing bytes to check.
	 * @param ofs - Offset within buffer to start check at.
	 * @param len - How many bytes from offset are available in the buffer.
	 * 
	 * @return true if enough bytes are available and match the expected sequence. 
	 */
	public static boolean isLastTerminator(byte[] raw, int ofs, int len) {
		return compareBytes(LAST_TERM_BYTES, raw, ofs, len);
	}

	/**
	 * Checks to see if raw data looks like a NWS lightning record.
	 * 
	 * @param raw 	Buffer of bytes to inspect.
	 * @param ofs	Current offset into the buffer.
	 * @param len	Length of bytes left in the buffer.
	 * 
	 * @return -1 if not bin lightning record, 
	 *         0 if bin lightning record with continuation terminator (terminated with bytes 0x0D 0x0D, 0x0A, 0x00) 
	 *         3 if it is the last bin lightning record (terminated with bytes 0x0D 0x0D, 0x0A, 0x03)
	 */
	public static int checkBinLightningRecord(byte[] raw, int ofs, int len) {
		if (len < 0) {
			len = raw.length - ofs;
		}
		if (len < BINLIGHTNING_RECORD_SIZE) {
			return -1;
		}
		int terminatorOffset = BINLIGHTNING_RECORD_SIZE - 4;
		if (((raw[ofs] & 0xff) == (LIGHTNING_TYPE & 0xff)) && ((raw[ofs + 1] & 0xff) == ((LIGHTNING_TYPE >> 8) & 0xff))) {
			// record type indicates lightning record, now check record terminator
			if (isMoreTerminator(raw, ofs + terminatorOffset, len - terminatorOffset)) {
				return 0;
			} else if (isLastTerminator(raw, ofs + terminatorOffset, len - terminatorOffset)) {
				return 3;
			}
		}
		return -1;
	}
	
	/**
	 * check if the decrypted data is valid, i.e. contains either keep-alive record or a series of lightning data records
	 *  
	 * @param data
	 * @return
	 */
	public static boolean isLightningDataRecords(byte[] data) {
		if (data == null) return false;
		
		if (data.length % BINLIGHTNING_RECORD_SIZE != 0) { // not a multiple of bin lightning data record size (32)
			return false;
		}
		
		// check all records
		int recordCount = data.length / BINLIGHTNING_RECORD_SIZE;
		for (int i = 0; i < recordCount; i++) {
			int offset = i * BINLIGHTNING_RECORD_SIZE;
			int lenLeft = data.length - offset;
			if (i < (recordCount - 1) && checkBinLightningRecord(data, offset, lenLeft) != 0) { 
				return false; 
			} else if (i == (recordCount - 1) && checkBinLightningRecord(data, offset, lenLeft) != 3) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * check if the decrypted data is valid, check record by record for either keep-alive record or lightning data record
	 *    
	 * NOTE: use this to check data validity only if keep-alive record is allowed to be mixed with lightning record (as in one sample file).
	 *       However, generally as email communication cleared, keep-alive record should be in its own file.    
	 * @param data
	 * @return
	 */
	public static boolean isValidMixedRecordData(byte[] data) {
		if (data == null) return false;
		int ofs = 0;
		while ((ofs + 1) < data.length) {
			// check record type bytes
			if (((data[ofs] & 0xff) == (KEEP_ALIVE_TYPE & 0xff)) && ((data[ofs + 1] & 0xff) == ((KEEP_ALIVE_TYPE >> 8) & 0xff))) {
				// keep-alive record, check its ending bytes after 2 bytes
				if (isLastTerminator(data, ofs+2, data.length - ofs - 2)) {
					ofs += 6;
					if (data.length == ofs) return true; // reach the end
				} else {
					return false;
				}
			} else if (((data[ofs] & 0xff) == (LIGHTNING_TYPE & 0xff)) && ((data[ofs + 1] & 0xff) == ((LIGHTNING_TYPE >> 8) & 0xff))) {
				// lightning record, check ending bytes after 28 bytes
				if (isMoreTerminator(data, ofs + 28, data.length - ofs - 28)) {
					ofs += 32;
					if (data.length == ofs) return false; // reach the end but not last terminator
				} else if (isLastTerminator(data, ofs + 28, data.length - ofs - 28)) {
					ofs += 32;
					if (data.length == ofs) return true; // reach the end
				} else {
					return false;
				}
			}
		}
		return false; 
	}

	/**
	 * Determines if the bytes passed are a standard "NWS Last Element" OR a "NWS More Elements" terminator sequence.
	 * 
	 * @param raw - Buffer containing bytes to check.
	 * @param ofs - Offset within buffer to start check at.
	 * @param len - How many bytes from offset are available in the buffer.
	 * 
	 * @return true if enough bytes are available and match the expected sequence. 
	 */

	public static boolean isTerminator(byte[] raw, int ofs, int len) {
		return isMoreTerminator(raw, ofs, len) || isLastTerminator(raw, ofs, len);
	}

	/**
	 * Determines if the bytes passed are a standard "NWS More Element" terminator sequence.
	 * 
	 * @param raw - Buffer containing bytes to check.
	 * @param ofs - Offset within buffer to start check at.
	 * @param len - How many bytes from offset are available in the buffer.
	 * 
	 * @return true if enough bytes are available and match the expected sequence. 
	 */

	public static boolean isMoreTerminator(byte[] raw, int ofs, int len) {
		return compareBytes(MORE_TERM_BYTES, raw, ofs, len);
	}


	/**
	 * Helper method to compare bytes in a buffer to a known source.
	 * 
	 * @param ref - Reference set of bytes you want to check against
	 * (all bytes in this array must be present in the src array starting
	 * at the offset specified and the length of the reference array
	 * must be equal to or less than the 'len' specified).
	 * 
	 * @param raw - Buffer containing bytes to check.
	 * @param ofs - Offset within buffer to start check at.
	 * @param len - How many bytes from offset are available in the buffer.
	 * 
	 * @return true if byte range specified matches the reference array. 
	 */
	private static boolean compareBytes(byte[] ref, byte[] src,	int ofs, int len) {
		
		int sizeToCompare = ref.length;
		if (len < sizeToCompare) {
			return false;
		}
		for (int i = 0; i < sizeToCompare; i++) {
			if (ref[i] != src[ofs + i]) {
				return false;
			}
		}
		// Source bytes to check matched if this point reached
		return true;
	}

	/**
	 * find from the optional WMO header line (which could be stripped by switching system such as TG) the size or sequence bytes (3 bytes)
	 * the 3 bytes should be after the WMO header start bytes if they all exist
	 * 
	 * @param data - data including the WMO header section
	 * @return null if not found, 
	 */
	public static byte[] findSizeOrSeqBytesFromWMOHeader(byte[] data) {
		if (compareBytes(WMO_HEADER_START_BYTES, data, 0, data.length) == true && data.length > 32) {
			// found the [SOH] [CR] [CR] [LF] byte sequence at the beginning, then the next 3 bytes is what we looking for
			return Arrays.copyOfRange(data, WMO_HEADER_START_BYTES.length, WMO_HEADER_START_BYTES.length + 3);
		}
		return null;
	}
	
	/**
	 * convert the 3 bytes encrypted block size in WMO header to an integer according to spec.
	 * @param sizeBytes
	 * @return -1 if invalid sizeBytes
	 */
	public static int getEncryptedBlockSizeFromWMOHeader(byte[] sizeBytes) {
		if (sizeBytes == null || sizeBytes.length != 3) return -1;
		
		return (sizeBytes[0] & 0xff) + ((sizeBytes[1] & 0xff) << 8) + ((sizeBytes[2] & 0xff) << 16); 
	}
	
	/**
	 * check if the bytes looks like a sequence number in 3 ASCII characters
	 * 
	 * @param seqBytes
	 * @return 
	 */
	public static boolean isPossibleWMOHeaderSequenceNumber(byte[] seqBytes) {
		if (seqBytes == null || seqBytes.length != 3) return false;
		
		if (Character.isDigit(seqBytes[0]) &&  Character.isDigit(seqBytes[1]) && Character.isDigit(seqBytes[2])) {
			return true;
		}
		return false;
	}
	
}
