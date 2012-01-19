/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.rcm.message;

import java.nio.ByteBuffer;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * Base class for representing Nexrad messages.  Also provides utility routines
 * for encoding and decoding messages.
 */
public class Message {
	public static final int PRODUCT_REQUEST = 0;
	public static final int GSM = 2;
	public static final int REQUEST_RESPONSE = 3;
	public static final int MAX_CONNECT_TIME_DISABLE_REQUEST = 4;
	public static final int EXTERNAL_DATA = 5;
	public static final int ALERT_ADAPTATION_PARAMETERS = 6;
	public static final int ALERT_REQUEST = 7;
	public static final int PRODUCT_LIST = 8;
	public static final int SIGN_ON = 11;
	public static final int BIAS_TABLE = 15;
	public static final int FREE_TEXT_MESSAGE = 75;
	
	public static byte[] encode(int code, int source, int dest, byte[] block) {
		Message msg = new Message();
		msg.messageCode = (short) code;
		msg.sourceId = (short) source;
		msg.destId = (short) dest;
		msg.time = Calendar.getInstance();		
		msg.blocks = new byte[1][];
		msg.blocks[0] = block;
		return msg.encode();
	}
	
	public static byte[] encode(int code, Calendar time, int source, int dest, byte[][] blocks) {
		Message msg = new Message();
		msg.messageCode = (short) code;
		msg.sourceId = (short) source;
		msg.destId = (short) dest;
		msg.time= time;		
		msg.blocks = blocks;
		return msg.encode();
	}
	
	public short messageCode;
	public short sourceId;
	public short destId;
	public Calendar time;
	protected byte[][] blocks;

	protected static final int HEADER_SIZE = 9 * 2; // Does not include block divider
	
	public byte[] encode() {

		int payloadSize = 0;
		for (byte[] block : blocks) {
			if (block.length > 65535)
				throw new MessageFormatException("Block too large");
			payloadSize += 4 /* block divider and block size*/ + block.length;
		}
		
		int totalSize = HEADER_SIZE + payloadSize;
		ByteBuffer buf = ByteBuffer.allocate(totalSize);
		
		encodeHeader(buf);
		for (byte[] block : blocks) {
			buf.putShort((short) -1);
			buf.putShort((short) (block.length + 4));
			buf.put(block);
		}
		return buf.array();
	}
	
	protected void encodeHeader(ByteBuffer buf) {
		int totalSize = buf.limit() - buf.position();
		buf.putShort(messageCode);
		encodeTime(buf, time);
		buf.putInt(totalSize);
		buf.putShort(sourceId);
		buf.putShort(destId);
		buf.putShort((short) (blocks.length + 1)); // header counts as one block
	}
	
	public static int messageCodeOf(byte[] msg) {
		return ((msg[0] & 0xff) << 8) | (msg[1] & 0xff);
	}
	
	public static short sourceIdOf(byte[] msg) {
		ByteBuffer buf = ByteBuffer.wrap(msg);
		return buf.getShort(12);
	}
	
	public static void setSourceIdOf(byte[] msg, short id) {
		ByteBuffer buf = ByteBuffer.wrap(msg);
		buf.putShort(12, id);
	}
	
	protected static final TimeZone gmt = new SimpleTimeZone(0, "GMT"); // TimeZone.getTimeZone("GMT+0");

	public static void encodeTime(ByteBuffer buf, Calendar time) {
		// The provided date may not be GMT...  
		GregorianCalendar gmtDate = new GregorianCalendar(gmt);
		gmtDate.setTimeInMillis(time.getTimeInMillis());
		
		/* Despite the documentation specifying "Julian days", it appears to 
		 * use the Gregorian calendar. */
		short julianDays;
		int year = gmtDate.get(Calendar.YEAR);
		
		if (year >= 1970)
			julianDays = (short)(gmtDate.getTimeInMillis() / (24*60*60*1000) + 1);
		else
			julianDays = 0; // TODO: is this worth throwing an exception over?			
		
		int seconds = gmtDate.get(Calendar.HOUR_OF_DAY) * 3600 +
			gmtDate.get(Calendar.MINUTE) * 60 + gmtDate.get(Calendar.SECOND);

		buf.putShort(julianDays);
		buf.putInt(seconds);
	}
	
	public static Calendar decodeTime(ByteBuffer buf) {
		int julianDays = buf.getShort() & 0xffff;
		int seconds = buf.getInt();
		Calendar time = new GregorianCalendar(gmt);

		time.setTimeInMillis(0);
		time.set(1970, Calendar.JANUARY, 1);
		time.add(Calendar.SECOND, (julianDays - 1) * 24 * 60 * 60 + seconds);		
		
		return time;
	}
	
	/** 
	 * Encodes a signed elevation angle in tenths of degrees into the unsigned
	 * format used in Nexrad messages.  Does not account for the 
	 * multiple-elevation flags.  See {@link com.raytheon.rcm.request.Request} 
	 * for those.
	 * 
	 * @param elev the elevation angle
	 * @return the encoded value
	 */
	public static int encodeElevation(int elev) {
		if (elev >= 0)
			return Math.min(elev, 1800);
		else {
			if (elev > -1800)
				return 3600 + elev;
			else
				return 1801;
		}
	}

	public static int decodeElevation(int elev) {
		if (elev > 1800)
			return elev - 3600;
		else
			return elev;
	}
	
	public static Message decodeHeader(byte[] msg) {
		Message result = new Message();
		result.decodeHeader(ByteBuffer.wrap(msg));
		return result;
	}
	
	protected void decode(ByteBuffer buf) {
		decodeHeader(buf);
		decodeBlocks(buf);
	}
	
	// TODO needs to throw something
	protected void decodeHeader(ByteBuffer buf) {
		messageCode = buf.getShort();
		time = decodeTime(buf);
		buf.getInt(); // total size field; unused
		sourceId = buf.getShort();
		destId = buf.getShort();
	}
	
	/**
	 * Decodes the blocks following the header block.  Not all messages
	 * have the same block structure so this method may be overridden.
	 * 
	 * @param buf must be positioned at the block count
	 */
	protected void decodeBlocks(ByteBuffer buf) {
		decodeSimpleBlocks(buf);
	}

	/**
	 * Decodes the blocks that are prefixed by a length.  This is the
	 * structure of most messages received from an RPG that are not
	 * products.
	 */
	protected void decodeSimpleBlocks(ByteBuffer buf) {
		int nBlocks = buf.getShort();
		// Start at 1 because the header block counts as 1
		for (int i = 1; i < nBlocks; ++i) {
			int divider = buf.getShort();
			Message.checkFormat(divider == -1, "expected block divider");
			int blockSize = buf.getShort();
			ByteBuffer block = buf.duplicate();
			block.limit(block.position() + blockSize);
			decodeBlock(i, block);
			buf.position(buf.position() + blockSize);			
		}
	}

	/**
	 * Decode a message block.  Default implementation does nothing.
	 * 
	 * @param index Block index starting at one (The header block would
	 * be index zero.)
	 */
	protected void decodeBlock(int index, ByteBuffer buf) {
		// nothing
	}
	
	protected static void checkFormat(boolean condition, String message) 
			throws MessageFormatException {
		if (! condition)
			throw new MessageFormatException(message);
	}
}
