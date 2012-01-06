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

package com.raytheon.edex.util;

/**
 * Deserializer implementation used during JiBX compilation. The identifier
 * field in the PersistableDataObject class is used to store the data URI for plugin data
 * types. JiBX needs a deserializer to correctly convert a String to an Object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 				353			bphillip	Initial creation	
 * 
 * </pre>
 * 
 */
public class ObjectToStringConv {

	/**
	 * Takes a String and returns it as an Object to help JiBX
	 * @param value The value as a String
	 * @return The dataURI as an Object
	 */
	public static Object deserializer(String value) {
		return (Object) value;
	}

}
