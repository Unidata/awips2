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
package com.raytheon.edex.subscription.util;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;

import com.raytheon.uf.common.message.Property;

/**
 * Utility class for the subscription manager.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * 14Sep2010    3944       cjeanbap    Trim the newline char from value.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public final class Tools {

    /**
     * Constructor.
     */
    private Tools() {
        super();
    }
    /**
     * 
     * @param properties
     * @return
     */
    public static final List<Property> adjustMessageProperties(Property[] properties) {
        List<Property> retVal = new ArrayList<Property>();
        for (Property prop : properties) {
            if (!"operation".equalsIgnoreCase(prop.getName())) {
                if ("file".equalsIgnoreCase(prop.getName())) {
                    prop.setName("filepath");                    
                    if (prop.getValue().indexOf('\n') > 0) {
                        int len = prop.getValue().length();
                        prop.setValue(prop.getValue().substring(0, len-1));
                    }
                }
                if ("index".equalsIgnoreCase(prop.getName())) {
                    prop.setName("id");
                }
                retVal.add(prop);
            }
        }

        return retVal;
    }
    public static final String AsciiToHex(String string) {
        return new HexBinaryAdapter().marshal(string.getBytes());
    }
    
    public static final String hexToAscii(String hexString) {
        byte[] b = new HexBinaryAdapter().unmarshal(hexString);
        return new String(b);
    }
}
