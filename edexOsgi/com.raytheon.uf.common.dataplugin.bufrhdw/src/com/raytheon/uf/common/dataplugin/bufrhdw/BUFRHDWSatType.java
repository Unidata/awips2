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
package com.raytheon.uf.common.dataplugin.bufrhdw;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public enum BUFRHDWSatType {
    
    IR("IR"),
    VIS("Vis"),
    WV("WV"),
    IR3_9u("IR3.9u"),
    WV7_0u("WV7.0u"),
    WV7_4u("WV7.4u"),
    ERROR("ERROR");
    
    private final String satType;
    
    private BUFRHDWSatType(String type) {
        satType = type;
    }
    
    /**
     * 
     */
    @Override
    public String toString() {
        return satType;
    }
    
    /**
     * 
     * @param freq
     * @return
     */
    public static BUFRHDWSatType getType(Double freq) {
        BUFRHDWSatType type = null;
        
        if((freq >= 2.79374e+13)&&(freq <= 2.81374e+13)) {
            type = IR;
        } else if((freq >= 4.60538e+14)&&(freq <= 4.62538e+14)) {
            type = VIS;
        } else if((freq >= 4.40176e+13)&&(freq <= 4.42176e+13)) {
            type = WV;
        } else if((freq >= 7.67699e+13)&&(freq <= 7.69699e+13)) {
            type = IR3_9u;
        } else if((freq >= 4.27571e+13)&&(freq <= 4.29571e+13)) {
            type = WV7_0u;
        } else if((freq >= 4.04405e+13)&&(freq <= 4.06405e+13)) {
            type = WV7_4u;
        } else {
            type = ERROR;
        }
        
        return type;
    }
    
}
