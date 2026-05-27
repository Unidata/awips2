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
package com.raytheon.uf.common.dataplugin.warning;

/**
 * Helps manage and identify emergency products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep  4, 2013  2176      jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class EmergencyType {

    public static final String EMER = "EMER";

    private static final EmergencyType TORNADO = new EmergencyType(
            "TORNADO EMERGENCY", "TO.W");

    private static final EmergencyType FLASH_FLOOD = new EmergencyType(
            "FLASH FLOOD EMERGENCY", "FF.W");

    private final String value;

    private final String phensig;

    private final static EmergencyType[] values = new EmergencyType[] {
            TORNADO, FLASH_FLOOD };

    private EmergencyType(String type, String phensig) {
        this.value = type;
        this.phensig = phensig;
    }

    public static EmergencyType valueOf(String phensig) {
        EmergencyType type = null;
        for (EmergencyType t : values) {
            if (t.phensig.equals(phensig)) {
                type = t;
                break;
            }
        }
        return type;
    }

    /**
     * Checks to see if the text product is an emergency product.
     * 
     * @param rawmessage
     * @return
     */
    public static boolean isEmergency(String rawmessage) {
        for (EmergencyType type : values) {
            if (rawmessage != null && rawmessage.contains(type.getValue())) {
                return true;
            }
        }
        return false;
    }

    public String getValue() {
        return value;
    }

}
