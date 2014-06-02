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
 * Stroke type enum for lightning pulses. Mainly categorizes between return and
 * non-return strokes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2014  3226      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public enum LtgPulseType {

    RETURN_STROKE((byte) 0), NON_RETURN_STROKE((byte) 1), KEEP_ALIVE((byte) 9);

    private final byte id;

    /**
     * @param id
     */
    private LtgPulseType(byte id) {
        this.id = id;
    }

    /**
     * @param id
     * @return null if no type is found for id
     */
    public static LtgPulseType getById(byte id) {
        for (LtgPulseType type : LtgPulseType.values()) {
            if (type.id == id) {
                return type;
            }
        }
        return null;
    }

    /**
     * @return unique id for this type
     */
    public byte getId() {
        return this.id;
    }
}
