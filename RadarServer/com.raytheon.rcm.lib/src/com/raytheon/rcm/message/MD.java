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

/**
 * Factory class for decoding messages.  Only works on message currently of
 * interest to the RadarServer.
 */
public class MD {
	public static Message decode(byte[] msg) {
		int messageCode = Message.messageCodeOf(msg);
		
		Class<?> clazz;
		switch (messageCode) {
		case Message.GSM:
			clazz = GSM.class;
			break;
		case Message.REQUEST_RESPONSE: 
			clazz = RequestResponse.class;
			break;
		case Message.PRODUCT_LIST:
			clazz = ProductList.class;
			break;
		case Message.ALERT_ADAPTATION_PARAMETERS:
			clazz = AlertAdaptationParameters.class;
			break;
		default:
			/* TODO: Is it safe to assume >16 is a graphic product?  Could check by
			 * comparing the pdb's productCode (or not? -- is it always the same?) and
			 * the offsets in the pdb to the determined offsets.. 
			 */
			clazz = Message.class;
		}
		
		Message result;
		try {
			result = (Message) clazz.newInstance();
		} catch (Exception e) {
			throw new MessageFormatException(e);
		}
		
		ByteBuffer buf = ByteBuffer.wrap(msg);
		result.decode(buf);
		
		return result;
	}
}
