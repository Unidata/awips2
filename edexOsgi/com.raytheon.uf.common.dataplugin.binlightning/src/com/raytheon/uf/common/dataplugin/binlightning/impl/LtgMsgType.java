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
 * Enums for the lightning message type, Flash or RT Flash.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * Jun 3, 2014  3226      bclement     added id, added TOTAL_LIGHTNING
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public enum LtgMsgType
{
    STRIKE_MSG_FL((byte) 0, "FL"), STRIKE_MSG_RT((byte) 1, "RT"), TOTAL_LIGHTNING(
            (byte) 2, "TL");

    private final String msgType;

    private final byte id;

    /**
     * Construct the type.
     * 
     * @param type
     *            Lightning message type.
     */
    private LtgMsgType(byte id, String type)
    {
        this.msgType = type;
        this.id = id;
    }

    /**
     * @param id
     * @return null if no type found for id
     */
    public static LtgMsgType getById(byte id) {
        for (LtgMsgType type : LtgMsgType.values()) {
            if (type.id == id) {
                return type;
            }
        }
        return null;
    }

    /**
     * Get the message type value.
     * @return The strike message value.
     */
    public String getType()
    {
        return msgType;
    }

    /**
     * @return unique id for type
     */
    public byte getId() {
        return this.id;
    }
}
