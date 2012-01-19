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
package com.raytheon.rcm.coll;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.MessageFormatException;
import com.raytheon.rcm.message.StandAloneText;
import com.raytheon.rcm.server.Log;

public class TextRoutines {

	/**
	 * Format the text portion of a radar product.
	 * 
	 * See AWIPS1 textRoutines.C:createTextProd. Only implements functionality
	 * for Free Text Messages.
	 * 
	 * Returns a byte array because this is currently only used for generated
	 * messages for distribution.
	 * 
	 * @param msgData
	 * @return null if the product could not be formatted
	 */
	public static final byte[] formatTextProduct(byte[] msgData) {
		ByteArrayOutputStream result = new ByteArrayOutputStream(
				msgData.length + 64);

		Exception ex = null;
		try {
			Message msg = Message.decodeHeader(msgData);
			if (msg.messageCode == Message.FREE_TEXT_MESSAGE) {

				// TODO: Shouldn't this be in all caps and using \r\r\n?
				String prefix = String
						.format(
								"Message Date:  %1$tb %1$td %1$tY %1$tH:%1$tM:%1$tS\n\n",
								msg.time);

				result.write(prefix.getBytes("ISO-8859-1"));
				result.write(StandAloneText.decodeTextBytes(msgData));
			}
		} catch (IOException e) {
			ex = e;
		} catch (MessageFormatException e) {
			ex = e;
		}
		if (ex != null) {
			Log.errorf("Error formatting product: %s", ex);
			return null;
		}

		return result.toByteArray();
	}

}
