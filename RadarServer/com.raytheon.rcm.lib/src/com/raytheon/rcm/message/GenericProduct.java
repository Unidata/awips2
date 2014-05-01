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
import java.util.ArrayList;

import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.Message;

public class GenericProduct extends GraphicProduct {
	
	public static final int PRODUCT_DESCRIPTION_PACKET = 28;
	public static final int EXTERNAL_DATA_DESCRIPTION_PACKET = 29;
	
	public static GenericProduct decode(byte[] bytes) {
		GenericProduct gp = new GenericProduct();
		gp.decode(ByteBuffer.wrap(bytes));
		return gp;
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
		block.limit(51 * 2);
		decodeBlock(1, block);
		buf.position(buf.position() + 51 * 2);

		for (int i = 2; i < nBlocks; ++i) {
			int divider = buf.getShort(buf.position()); // Note: does not consume the divider
			Message.checkFormat(divider == -1, "expected block divider");
			int blockSize = buf.getInt(buf.position() + 4); // Skip divider and block ID
			block = buf.slice();
			block.limit(blockSize);
			decodeBlock(i, block);
			buf.position(buf.position() + blockSize);			
		}
	}
	
	@Override
	protected void decodeBlock(int index, ByteBuffer buf) {
		if (index > 1) {
			checkFormat(buf.getShort() == -1, "expected block divider");
			if (buf.getShort() != 1) // 1 == Symbology block
				return;
			buf.getInt(); // symbology size
			int nLayers = buf.getShort();
			layers = new Object[nLayers][]; 
			for (int iLayer = 0; iLayer < nLayers; ++iLayer) {
				checkFormat(buf.getShort() == -1, "expected layer divider");
				
				int layerSize = buf.getInt();
				ByteBuffer layerBuf = buf.slice();
				layerBuf.limit(layerSize);
				buf.position(buf.position() + layerSize);
				
				Object[] layer = decodeLayer(layerBuf);
				layers[iLayer] = layer;
			}
		} else
			super.decodeBlock(index, buf);
	}

	private Object[] decodeLayer(ByteBuffer buf) {
		ArrayList<Object> packets = new ArrayList<Object>();
		
		while (buf.remaining() > 0) {
			short packetCode = buf.getShort();
			if (packetCode != 28 && packetCode != 29) {
				System.err.format("Unknown display packet code %d\n", packetCode); // TODO: throw exception?
				break;
			}
			buf.getShort(); // "reserved (=0)"

			int packetSize = buf.getInt();
			ByteBuffer packetBuf = buf.slice();
			packetBuf.limit(packetSize);
			buf.position(buf.position() + packetSize);

			Object packet = decodeGenericPacket(packetCode, packetBuf);
			packets.add(packet);
		}
		return packets.toArray();
	}
	
	public Object[][] layers; // n layers of m packets

	private Object decodeGenericPacket(short packetCode, ByteBuffer packet) {
		GenericData g = new GenericData();
		g.decode(packet, packetCode);
		return g;
	}
	
}
