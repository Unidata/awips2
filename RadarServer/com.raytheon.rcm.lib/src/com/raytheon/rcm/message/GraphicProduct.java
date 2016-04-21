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

public class GraphicProduct extends Message {
	
	public static class PDB {
		public int lat; // In thousandths of degrees 
		public int lon;
		public int height; // In feet
		public int productCode;
		public int opMode;
		public int vcp;
		public int sequence;
		public int volumeScan;
		public Calendar volumeScanTime;
		public Calendar productGenerationTime;
		public int elevationNumber;
		public short[] productDependent = new short[10];
		public short[] thresholds = new short[16];
		public int version;
		public boolean spotBlanking;
		// not bothering with offsets for now
		
		public int getElevationAngle() {
			return Message.decodeElevation(productDependent[2]);
		}
		
		public int getElevationSegmentNumber() {
			// Assumes only one bit is set.
			
			int v = productDependent[0];
			for (int i = 0; i < 5; ++i) {
				if ((v & (2 << i)) != 0) // First segment bit is 2nd LSB in word.
					return i + 1; // Return one-based result.
			}
			return 0;
		}
		
		// 31 (USP), 151 (USD), 150 (USW) - Hours
		// 173 (DUA) - Minutes
		public int getTimeSpan() {
			return productDependent[1];
		}
		
		public boolean isBzip2Compressed() {
			return productDependent[7] == 1;
		}
		
		public int getUncompressedSize() {
			return ((productDependent[8] & 0xffff) << 16)
					| (productDependent[9] & 0xffff);
		}
	}
	
	public static PDB pdbOfMessage(byte[] msg) {
		ByteBuffer buf = ByteBuffer.wrap(msg);
		buf.position(18);
		int divider = buf.getShort();
		Message.checkFormat(divider == -1, "expected block divider");
		return decodePDB(buf);
	}
	
	public static byte[] extractHeaderAndPDB(byte[] msg) {
		byte[] result = new byte[Math.min(msg.length, 120)];
		System.arraycopy(msg, 0, result, 0, result.length);
		return result;
	}

	public PDB pdb;
	
	protected static PDB decodePDB(ByteBuffer buf) {
		PDB pdb = new PDB();
		pdb.lat = buf.getInt();
		pdb.lon = buf.getInt();
		pdb.height = buf.getShort();
		pdb.productCode = buf.getShort();
		pdb.opMode = buf.getShort();
		pdb.vcp = buf.getShort();
		pdb.sequence = buf.getShort();
		pdb.volumeScan = buf.getShort();
		pdb.volumeScanTime = decodeTime(buf);
		pdb.productGenerationTime = decodeTime(buf);
		pdb.productDependent[0] = buf.getShort();
		pdb.productDependent[1] = buf.getShort();
		pdb.elevationNumber = buf.getShort();
		pdb.productDependent[2] = buf.getShort();
		buf.asShortBuffer().get(pdb.thresholds, 0, 16);
		buf.position(buf.position() + 32);
		buf.asShortBuffer().get(pdb.productDependent, 3, 7);
		buf.position(buf.position() + 14);
		pdb.version = buf.get() & 0xff;
		pdb.spotBlanking = buf.get() == 1;
		return pdb;
	}
	
	@Override
	protected void decodeBlock(int index, ByteBuffer buf) {
		if (index == 1) {
			int divider = buf.getShort();
			Message.checkFormat(divider == -1, "expected block divider");
			pdb = decodePDB(buf);
		}
	}

	@Override
	protected void decodeBlocks(ByteBuffer buf) {
		ByteBuffer block;
		
		// The header counts as one block
		int nBlocks = buf.getShort();
		
		// Read the PDB which is in a fixed-size block
		if (nBlocks < 2)
			return;
		block = buf.slice();
		block.limit(50 * 2);
		decodeBlock(1, block);
		buf.position(buf.position() + 50 * 2);

		/* // Don't bother with content blocks for now
		for (int i = 1; i < nBlocks; ++i) {
			int divider = buf.getShort();
			Message.checkFormat(divider == -1, "expected block divider");
			int blockSize = buf.getInt(buf.position() + 2);
			block = buf.slice();
			block.limit(blockSize);
			decodeBlock(i, block);
			buf.position(buf.position() + blockSize);			
		}
		*/
	}
	
	
}
