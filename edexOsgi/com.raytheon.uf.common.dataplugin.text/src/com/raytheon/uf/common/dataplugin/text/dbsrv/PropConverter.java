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
package com.raytheon.uf.common.dataplugin.text.dbsrv;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;

import com.raytheon.uf.common.message.Header;

/**
 * Utilities for hexadecimal property values
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2014 2536       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PropConverter {

    /**
     * ASCII to HEX conversion.
     * 
     * @param string
     *            ascii string
     * @return hex code
     */
    public static String asciiToHex(String string) {
        return new HexBinaryAdapter().marshal(string.getBytes());
    }

    /**
     * HEX To ASCII conversion
     * 
     * @param hexString
     * @return ascii string
     */
    public static String hexToAscii(String hexString) {

        byte[] b = new HexBinaryAdapter().unmarshal(hexString);

        return new String(b);
    }

    /**
     * Get hexadecimal property value. Converts value from hex to ASCII (if
     * non-null).
     * 
     * @param header
     *            contains message header.
     * @param propName
     *            contains property name.
     * @return property value or null if property is not in header
     */
    public static String getProperty(Header header, String propName) {
        String result = null;

        String value = header.getProperty(propName);

        if (value != null) {
            result = hexToAscii(value);
        }
        return result;
    }

}
