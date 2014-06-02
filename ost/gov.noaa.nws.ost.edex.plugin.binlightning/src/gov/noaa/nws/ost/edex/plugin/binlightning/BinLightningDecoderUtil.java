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
import java.util.List;

import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgMsgType;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * 20140501	               Wufeng Zhou Fix the encrypted decoding with correct offset	
 * Jun 03, 2014 3226       bclement    renamed from BinLigntningDecoderUtil to BinLightningDecoderUtil
 *                                     moved from com.raytheon.edex.plugin.binlightning to gov.noaa.nws.ost.edex.plugin.binlightning
 *                                     moved decodeBinLightningData() and decodeBitShiftedBinLightningData() 
 *                                          to BinLightningDecoder to solve circular dependency
 * Jun 05, 2014  3226      bclement    LightningStrikePoint refactor
 * </pre>
 * 
 * @author Wufeng Zhou
 * 
 */
public class BinLightningDecoderUtil {

	/**
	 * Message type for keep alive data records. 
	 */
	static final short KEEP_ALIVE_TYPE = 0x0000;

	/**
	 * Message type for lightning data records. 
	 */
	static final short LIGHTNING_TYPE = 0x00ff;

	/**
	 * If there are more elements within the data record, this terminator is used.
	 */
	static final byte[] MORE_TERM_BYTES = {0x0d, 0x0d, 0x0a, 0x00};
	/**
	 * Last element within data records should be terminated by these 4 bytes.
	 */
	static final byte[] LAST_TERM_BYTES = {0x0d, 0x0d, 0x0a, 0x03};

	/**
	 * WMO header start bytes, optional (it is known that TG will strip this away)
	 */
	static final byte[] WMO_HEADER_START_BYTES = {0x01, 0x0d, 0x0d, 0x0a};

	/* Size of binary NWS lightning data record. */
	static final int BINLIGHTNING_RECORD_SIZE = 32;

	/** legacy lightning data types */
    static final byte FLASH_RPT = (byte)0x96;
    static final byte RT_FLASH_RPT = (byte)0x97;
    static final byte OTHER_RPT = (byte)0xD0;
    static final byte COMM_RPT = (byte)0xD1;

    private static IUFStatusHandler logger = UFStatus
            .getHandler(BinLightningDecoderUtil.class);


	
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
			offset = i * BINLIGHTNING_RECORD_SIZE;
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
            // int strokeDuration = buffer.getShort() & 0xffff; // valid range:
            // 0 to 65535 (i.e., looks like unsigned short)
            // int reserved = buffer.getShort() & 0xffff;

            // Create the strike record from the report info and base time information. 
			Calendar cal = Calendar.getInstance();
			cal.setTimeInMillis(epochTime);
			
            /*
             * new spec does not seem to have lightning message type indicator
             * such as FL (Flash Lightning) or RT (Real Time flash lightning)
             * The source of lightning data in the vendor specific data bytes
             * (byte 16-17) may related to this (???),
             * and it is used here for now. 04/182/013 Wufeng Zhou
             */
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
			
            LightningStrikePoint lsp = new LightningStrikePoint(lat, lon, cal,
                    msgType);
            LtgStrikeType ltgStrikeType = LtgStrikeType.CLOUD_TO_GROUND; // default ??
            if (strokeType == 0x0000) {
            	ltgStrikeType = LtgStrikeType.CLOUD_TO_GROUND;
            } else if (strokeType == 0x00ff) {
            	ltgStrikeType = LtgStrikeType.CLOUD_TO_CLOUD;
            } else if (strokeType == 0xffff) {
            	ltgStrikeType = LtgStrikeType.TOTAL_FLASH;
            }
            lsp.setType(ltgStrikeType);
            
            /*
             * as of OB13.3 for World Wide Lightning Location Network (WWLLN)
             * data (decoded by textlightning though, not this bin lightning
             * decoder), added lightning source field in LightningStrikePoint,
             * as well as column in binlightning database table defaults to NLDN
             */
			if (vendor == ((short)0x0001)) { // CONUS source
				lsp.setLightSource("NLDN"); 
			} else if (vendor == ((short)0x0002)) { // long range source, i.e., GLD360. 
				// However, since the database table column for lightning source is 5 characters  
				lsp.setLightSource("GLD"); 
			}

            lsp.setPulseCount(strokeMultiplicity);
            lsp.setStrikeStrength(strokeKiloAmps);
            // stroke duration does not seem to be used
            
            strikes.add(lsp);
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
	
	/**
	 * Based on the data format described below, trying to get an estimate of bit-shifted data strike count without actually decoding the data
	 * 
	 * Legacy bit-shifted data format has the following known data pattern:
	 * The bit-shifted data will have multiple groups of the following patterns:
	 *      1-byte (unsigned byte): for size count. Count be 0, in that case, will only have 4-bytes data-time following (sounds like keep-alive)
	 *      1-byte (unsigned byte): for flash type:
	 *                              0x96 for FLASH_RPT (message size is 6 bytes each)
	 *                              0x97 for RT_FLASH_RPT (message size is 8 bytes each)
     *                              0xd0 for OTHER_RPT (The D2D decoders declare but do not define this message, so unimplemented decoder)
	 *                              0xd1 for COMM_RPT  (The D2D decoders declare but do not define this message, so unimplemented decoder)
	 *     4-bytes: date time
	 *     multiple of 6 or 8 bytes (as determined by 2nd byte flash type) with count indicated in 1st byte
	 *
	 * NOTE: as test file SFUS41_KWBC_241059_53594581.nldn.2013042411 revealed, it is bit-shifted data but somehow with 4 few extra bytes at 
	 *          the end of the file (0x0d 0x0d 0x0a 0x03).  It still can be decoded as bit-shifted data.  
	 *       So this method of checking data format is not foolproof.  
	 *       - 11/07/2013 WZ
	 * 
	 *  @param pdata - binary data byte array
	 *  @return true if conforms to bit-shifted data format, false otherwise 
	 */
	public static int getBitShiftedDataStrikeCount(byte[] pdata) {
		int pos = 0;
		int strikeCount = 0;
		int totalGroupCount = 0; // groups that conforms to the data pattern described above in comment
		while (pos < pdata.length) {
			int recCount = (int)(pdata[pos] & 0xff);
			if ((pos+1) >= pdata.length) break; // end of data before even read the flash type
			byte flashType = pdata[pos + 1];
			if (flashType == FLASH_RPT) { // FLASH_RPT 0x96, record size is 6 bytes
				totalGroupCount++;
				pos = pos + 6 + 6*recCount; // next record start position
			} else if (flashType == RT_FLASH_RPT) { // RT_FLASH_RPT 0x97, record size is 8 bytes
				totalGroupCount++;
				pos = pos + 6 + 8*recCount; // next record start position
			} else {
				break; // not a known type
			}
			// only when the next record start position is exactly right after the end of data, we get a well conformed data
			if (pos <= pdata.length) {
				strikeCount += recCount;
			} else if (pos > pdata.length) {
				break;
			}
		}
		if (totalGroupCount == 0 && strikeCount == 0) {
			logger.info("getBitShiftedDataStrikeCount(): no bit-shifted data pattern found, not likely to be bit-shifted data.");
			return -1;
		} else {
			logger.info("getBitShiftedDataStrikeCount(): found " + totalGroupCount + " groups of bit-shifted data pattern, which contains " + strikeCount + " strikes.");
			return strikeCount;
		}
	}
}
