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
package com.raytheon.uf.common.dataplugin.radar.level3;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * A Layer represnets a group of {@link SymbologyPacket}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 08, 2009           chammack    Initial creation
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class Layer {
    @DynamicSerializeElement
    private int layerId;

    @DynamicSerializeElement
    SymbologyPacket[] packets;

    /**
     * @return the layerId
     */
    public int getLayerId() {
        return layerId;
    }

    /**
     * @param layerId
     *            the layerId to set
     */
    public void setLayerId(int layerId) {
        this.layerId = layerId;
    }

    /**
     * @return the packets
     */
    public SymbologyPacket[] getPackets() {
        return packets;
    }

    /**
     * @param packets
     *            the packets to set
     */
    public void setPackets(SymbologyPacket[] packets) {
        this.packets = packets;
    }

}