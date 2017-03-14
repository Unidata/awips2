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
package com.raytheon.viz.hydro.pointdatacontrol;

import java.util.ArrayList;


/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2008            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0 
 */

public class AdHocElements {
    /**
     * Elements type.
     */
    private String elementType;
    
    /**
     * Array of data elements.
     */
    private ArrayList<String>  dataElementArray;
    
    /**
     * Constructor.
     * @param elementType Element type.
     */
    public AdHocElements(String elementType)
    {
        this.elementType = elementType;
        dataElementArray = new ArrayList<String>();
    }
    
    /**
     * Constructor.
     * @param elementType Element type.
     * @param array Array of data elements.
     */
    public AdHocElements(String elementType, ArrayList<String> array)
    {
        this.elementType = elementType;
        dataElementArray = array;
    }
    
    /**
     * Get the element type.
     * @return The element type.
     */
    public String getElementType()
    {
        return elementType;
    }
    
    /**
     * Get the data element array.
     * @return The data element array.
     */
    public ArrayList<String> getDataElementArray()
    {
        return dataElementArray;
    }
    
    /**
     * Add a data element to the data element array.
     * @param dataElement Data element.
     */
    public void addDataElement(String dataElement)
    {
        dataElementArray.add(dataElement);
    }
}
