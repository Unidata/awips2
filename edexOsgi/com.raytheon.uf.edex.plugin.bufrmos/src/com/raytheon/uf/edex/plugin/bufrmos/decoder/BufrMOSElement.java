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
package com.raytheon.uf.edex.plugin.bufrmos.decoder;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;



/**

 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20020221            861 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BufrMOSElement {

    // Forecast model this element is defined for i.e. "GFS"
    private final String modelName;
    
    // The access name for this element i.e. "temperature"
    private final String elementName;
    
    // The index position within the BUFRDataDocument data list for this element.
    private final Integer elementIndex;

    // 
    private final Integer descriptor;
    
    private BufrMOSElement(String model, String name, Integer index, Integer descrip) {
        modelName = model;
        elementName = name;
        elementIndex = index;
        descriptor = descrip;
    }
    
    /**
     * @return the modelName
     */
    public String getModelName() {
        return modelName;
    }

    /**
     * @return the elementName
     */
    public String getElementName() {
        return elementName;
    }

    /**
     * @return the elementIndex
     */
    public Integer getElementIndex() {
        return elementIndex;
    }

    /**
     * @return the descriptor
     */
    public Integer getDescriptor() {
        return descriptor;
    }

    public static BufrMOSElement createElement(String model, String name, Integer index, String descrip) {
        BufrMOSElement element = new BufrMOSElement(model,name,index,parseDescriptor(descrip));
    
        return element;
    }
 
    
    /**
     * 
     * f\tx\ty
     * @param descrip A potential descriptor as a String.
     * @return
     */
    private static Integer parseDescriptor(String descrip) {
        Integer descriptor = null;

        String [] ss = descrip.split(" ");
        if((ss != null) && (ss.length == 3)) {
            Integer f = Integer.parseInt(ss[0]);
            Integer x = Integer.parseInt(ss[1]);
            Integer y = Integer.parseInt(ss[2]);
            
            descriptor = BUFRDescriptor.createDescriptor(f,x,y);
        }
        return descriptor;
    }
    
    
    
    
    
    
}
