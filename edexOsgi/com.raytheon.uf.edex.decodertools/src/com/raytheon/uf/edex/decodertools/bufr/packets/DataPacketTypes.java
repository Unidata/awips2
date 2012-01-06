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
package com.raytheon.uf.edex.decodertools.bufr.packets;

import java.util.ArrayList;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080407           1026 jkorman     Initial implementation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public enum DataPacketTypes {

    RepSubList("RepSubList"),
    SubSetList("SubSetList"),
    UNKNOWN("Unknown"),
    INVALID("invalid");
    
    private final String packetType;

    // Only have a couple of enums so an array list will be fine.
    private static final ArrayList<DataPacketTypes> implementedTypes =
        new ArrayList<DataPacketTypes>();
    static {
        implementedTypes.add(RepSubList);
        implementedTypes.add(SubSetList);
        implementedTypes.add(UNKNOWN);
    }

    /**
     * Private constuctor for use when creating enum instances only.
     * @param type The data type identifier associated with the packet type.
     */
    private DataPacketTypes(String type) {
        packetType = type;
    }

    /**
     * Get the data type identifier associated with the packet type.
     * @return The data type identifier associated with the packet type.
     */
    public String getPacketType() {
        return packetType;
    }
    
    /**
     * 
     * @param type
     * @return
     */
    public static DataPacketTypes getType(String type) {
        DataPacketTypes dataType = INVALID;

        for(DataPacketTypes dType : implementedTypes) {
            if(dType.packetType.equals(type)) {
                dataType = dType;
                break;
            }
        }
        return dataType;
    }
}
