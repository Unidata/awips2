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
package com.raytheon.uf.common.dataplugin.lsr;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public enum LSRUnits {
    
    FUJITA("enum"),
    FAHRENHEIT("f"),
    INCH("in"),
    FT("ft"),
    MPH("mi/hr"),
    NOUNITS("enum");
    
  //eventType:units_0 = "NO_UNITS" ;
  //eventType:units_1 = "NO_UNITS" ;
  //eventType:units_2 = "NO_UNITS" ;
  //eventType:units_3 = "NO_UNITS" ;
  //eventType:units_4 = "NO_UNITS" ;
  //eventType:units_5 = "NO_UNITS" ;
  //eventType:units_6 = "F" ;
  //eventType:units_7 = "F" ;
  //eventType:units_8 = "F" ;
  //eventType:units_9 = "NO_UNITS" ;
  //eventType:units_10 = "NO_UNITS" ;
  //eventType:units_11 = "F" ;
  //eventType:units_12 = "in" ;
  //eventType:units_13 = "NO_UNITS" ;
  //eventType:units_14 = "in" ;
  //eventType:units_15 = "in" ;
  //eventType:units_16 = "in" ;
  //eventType:units_17 = "in" ;
  //eventType:units_18 = "NO_UNITS" ;
  //eventType:units_19 = "NO_UNITS" ;
  //eventType:units_20 = "mi/hr" ;
  //eventType:units_21 = "NO_UNITS" ;
  //eventType:units_22 = "NO_UNITS" ;
  //eventType:units_23 = "NO_UNITS" ;
  //eventType:units_24 = "NO_UNITS" ;
  //eventType:units_25 = "in" ;
  //eventType:units_26 = "mi/hr" ;
  //eventType:units_27 = "NO_UNITS" ;
  //eventType:units_28 = "mi/hr" ;
  //eventType:units_29 = "NO_UNITS" ;
  //eventType:units_30 = "NO_UNITS" ;
  //eventType:units_31 = "in" ;
  //eventType:units_32 = "in" ;
  //eventType:units_33 = "ft" ;
  //eventType:units_34 = "Fujita" ;
  //eventType:units_35 = "NO_UNITS" ;
  //eventType:units_36 = "NO_UNITS" ;
  //eventType:units_37 = "mi/hr" ;
  //eventType:units_38 = "NO_UNITS" ;
  //eventType:units_39 = "NO_UNITS" ;
  //eventType:comment_0 = "N is a flag for no units";
  //eventType:comment_1 = "Y is a flag for Fujita Scale";

    private final String unitsId;
    
    private LSRUnits(String id) {
        unitsId = id;
    }
    
    
    
}
