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

public class ProductList extends Message {
	public static class Product {
		public short productCode;
		public short elevation; // elevation in 10ths of degrees or altitude in ???
		public short interval; // "Distribution class";
		public short p1, p2, p3, p4;
	}
	
	public Product[] products;
	
	public static Product[] decode(byte[] msg) {
		return ((ProductList) MD.decode(msg)).products;
	}	

	@Override
	protected void decodeBlock(int index, ByteBuffer buf) {
		if (index == 1) {
			int nProducts = buf.getShort();
			buf.getShort(); // reserved field
			products = new Product[nProducts];
			for (int i = 0; i < nProducts; ++i) {
				Product p = new Product();
				p.productCode = buf.getShort();
				p.elevation = buf.getShort();
				p.p1 = buf.getShort();
				p.p2 = buf.getShort();
				p.p3 = buf.getShort();
				p.p4 = buf.getShort();
				p.interval = buf.getShort();
				products[i] = p;
			}
		}
	}
}
