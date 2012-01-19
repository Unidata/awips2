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
package com.raytheon.uf.edex.plugin.mesowest.decoder;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.edex.plugin.mesowest.common.MESOWestRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class MWIntElement extends MESOWestElement {

    private Integer intValue = null;
    
    public MWIntElement(String name) {
        super(name);
    }

    /**
     * 
     * @param value
     */
    @Override
    public void setElementValue(String value) {
        super.setElementValue(value);
        
        try {
            intValue = Integer.parseInt(value);
        } catch(NumberFormatException nfe) {
            
        }
    }
    
    /**
     * 
     * @param record
     * @return
     */
    @Override
    public MESOWestRecord getDecodedData(MESOWestRecord record) {
        
        StringBuilder sb = new StringBuilder(getElementName());
        sb.setCharAt(0,Character.toUpperCase(sb.charAt(0)));
        sb.insert(0,"get");
        
        String methodName = sb.toString();
        Method m = null;
        
        try {
            m = record.getClass().getDeclaredMethod(methodName, Integer.class);
            if(m != null) {
                
                try {
                    m.invoke(record,  intValue);
                } catch (IllegalArgumentException e) {
                    e.printStackTrace();
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        } catch (SecurityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        return record;
    }
    
}
