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
package com.raytheon.uf.common.dataplugin.binlightning.impl;

/**
 * Enums for the lightning strike type, cloud-to-cloud or cloud-to-ground.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 20130425        DCS 112 Wufeng Zhou Added STRIKE_TF (for Total Flash) in definition for Total Flash 
 * Jun 3, 2014  3226       bclement    added id, changed to more descriptive names, added KEEP_ALIVE
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public enum LtgStrikeType
{
    CLOUD_TO_CLOUD((byte)0, "CC"), CLOUD_TO_GROUND((byte)1, "CG"), TOTAL_FLASH((byte)2, "TF"), KEEP_ALIVE((byte)9,
            "");
    
    private final String strikeType;

    private final byte id;
    
    /**
     * Construct the type.
     * @param id unique id for type
     * @param type Lightning strike type.
     */
    private LtgStrikeType(byte id, String type)
    {
        this.id = id;
        this.strikeType = type;
    }
    
    /**
     * @param id
     *            unique id for type
     * @return null if no type is found for id
     */
    public static LtgStrikeType getById(byte id) {
        for (LtgStrikeType type : LtgStrikeType.values()) {
            if (type.id == id) {
                return type;
            }
        }
        return null;
    }
    
    /**
     * Get the strike type value.
     * @return The strike type value.
     */
    public String getType()
    {
        return strikeType;
    }

    /**
     * @return id
     */
    public int getId() {
        return this.id;
    }
}
