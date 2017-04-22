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

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Calendar;

public class ExternalData extends GenericProduct {
    
    public GenericData genericData;
    
    public static interface ICompressor {
        byte[] compress(byte[] data);
        int getCompressionMethod();
    }
    
    public static byte[] encode(GenericData data) {
        return encode(data, null);
    }
    
    public static GenericData decodeExternalData(byte[] msgData) {
        ExternalData msg = new ExternalData();
        ByteBuffer buf = ByteBuffer.wrap(msgData);
        msg.decode(buf);
        return msg.genericData;
    }
    
	public static byte[] encode(GenericData data, ICompressor compressor) {
			
	    byte[] packetData = encodeExternalDataPacket(data);
	    int uncompressedSize = packetData.length; 
	    int compressionMethod = 0;
	    
	    if (compressor != null) {
	        byte[] compressedData = compressor.compress(packetData);
	        if (compressedData != null) {
	            packetData = compressedData;
	            compressionMethod = compressor.getCompressionMethod();
	        }
	    }
	    
		ByteBuffer buf = ByteBuffer.allocate(HEADER_SIZE +
				+ 6 * 2 + packetData.length);
		
		Message msg = new Message();
		msg.messageCode = Message.EXTERNAL_DATA;
		msg.time = Calendar.getInstance();		
		msg.blocks = new byte[1][];
		msg.encodeHeader(buf);
		buf.putShort((short) -1);
		
		/* Value of 4 indicates Environmental Data from 40-km RUC Model.
		 * - so shouldn't block id be configurable?
         */
		buf.putShort((short) 4);
		buf.putShort((short) 0); // Spare
		buf.putShort((short) compressionMethod); // Compression method
		buf.putInt(uncompressedSize);
		buf.put(packetData);
		
		return buf.array();
	}
	
	public static byte[] encodeExternalDataPacket(GenericData data) {
        ByteArrayOutputStream outStream = new ByteArrayOutputStream(4096);
        DataOutputStream out = new DataOutputStream(outStream);
        
        final int packetCode = 
            GenericProduct.EXTERNAL_DATA_DESCRIPTION_PACKET;
        
        try {
            out.writeShort((short) packetCode);
            out.writeShort((short) 0); // "Reserved"
            out.writeInt(0); // Placeholder for data length
            
            data.encode(out, packetCode);
    
            out.flush();
        } catch (IOException e) {
            throw new RuntimeException("Encoding failed");
        }
        
        ByteBuffer buf = ByteBuffer.wrap(outStream.toByteArray());
        buf.putInt(4, buf.capacity() - 8); // Set the data length 
        
        return buf.array();
	}

	@Override
    protected void decodeBlocks(ByteBuffer buf) {
        buf.getShort(); // Number of blocks
        int divider = buf.getShort();
        Message.checkFormat(divider == -1, "expected block divider");
        buf.getShort(); // block ID. don't care. have to assume it is 4
        buf.getShort(); // Spare
        int compressionMethod = buf.getShort();
        Message.checkFormat(compressionMethod == 0, "compression not supported");
        buf.getInt();
        int packetCode = buf.getShort();
        Message.checkFormat(packetCode == GenericProduct.EXTERNAL_DATA_DESCRIPTION_PACKET,
                "unexpected packet type");
        buf.getShort(); // Reserved
        int length = buf.getInt();
        ByteBuffer gdBuf = buf.slice();
        gdBuf.limit(length);
        genericData = new GenericData();
        genericData.decode(gdBuf, packetCode);
    }
	
}
