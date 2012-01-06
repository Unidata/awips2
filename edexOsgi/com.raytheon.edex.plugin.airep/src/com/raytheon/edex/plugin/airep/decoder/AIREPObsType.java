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
package com.raytheon.edex.plugin.airep.decoder;

import java.util.HashMap;

import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * TODO Change this to a Java 1.5 enum.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * 20080103            384 jkorman     Initial Coding.
 * </pre>
 */
public class AIREPObsType
{
    private static final HashMap<String,Integer> AIREP_TYPES =
        new HashMap<String,Integer>();
    static {
        AIREP_TYPES.put("ARP",IDecoderConstants.AIREP_NORMAL);
        AIREP_TYPES.put("AIREP",IDecoderConstants.AIREP_NORMAL);
        AIREP_TYPES.put("ARS",IDecoderConstants.AIREP_SPECIAL);
    }
    
    private final String obsType;
    
    /**
     * 
     * @param aType
     */
    private AIREPObsType(String aType) {
        obsType = aType;
    } // AIREPObsType()

    /**
     * 
     * @param anObsType
     * @return
     */
    public static AIREPObsType obsTypeFactory(String anObsType) {
        AIREPObsType obsTypeInstance = null;

        if(AIREP_TYPES.containsKey(anObsType)) {
            obsTypeInstance = new AIREPObsType(anObsType);
        }
        return obsTypeInstance;
    } // obsTypeFactory()
    
    /**
     * 
     * @return
     */
    public Integer getValue() {
        return AIREP_TYPES.get(obsType);
    } // getValue()

    public String getType() {
        return obsType;
    }
}
