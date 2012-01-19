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

public class MWFloatElement extends MESOWestElement {

    private Double floatValue = null;
    
    public MWFloatElement(String name) {
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
            floatValue = Double.parseDouble(value);
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
        
        
        
        return record;
    }
}
