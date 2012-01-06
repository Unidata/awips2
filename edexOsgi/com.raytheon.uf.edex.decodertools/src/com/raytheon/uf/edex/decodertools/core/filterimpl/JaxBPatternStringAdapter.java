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
package com.raytheon.uf.edex.decodertools.core.filterimpl;

import java.util.regex.Pattern;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public final class JaxBPatternStringAdapter extends XmlAdapter<String, Pattern> {
    
    /**
     * Marshall a Pattern to a String.
     * @param pattern A Pattern to be marshalled. 
     * @see javax.xml.bind.annotation.adapters.XmlAdapter#marshal(java.lang.Object)
     */
    @Override
    public String marshal(Pattern pattern) throws Exception {
        String p = null;
        try {
            if(pattern != null) {
                p = pattern.pattern();
            }
        } catch(Exception e) {
        }
        return p;
    }

    /**
     * Unmarshall a String representation of a Pattern.
     * @see javax.xml.bind.annotation.adapters.XmlAdapter#unmarshal(java.lang.Object)
     */
    @Override
    public Pattern unmarshal(String pattern) throws Exception {
        Pattern p = null;
        try {
            if(pattern != null) {
                p = Pattern.compile(pattern);
            }
        } catch(Exception e) {
        }
        return p;
    }
}
