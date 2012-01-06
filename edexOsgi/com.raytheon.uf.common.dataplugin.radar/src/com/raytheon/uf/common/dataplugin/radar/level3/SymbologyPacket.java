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

import java.io.DataInputStream;
import java.io.IOException;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Abstract class that defines the binary values for the various symbology
 * layers that are sent out in Level III format.
 * 
 * Copyright 2006 Raytheon Corporation
 * 
 * @author Bryan Rockwood
 * @version 1.0
 */
public abstract class SymbologyPacket {

    @DynamicSerializeElement
    protected int packetId;

    public SymbologyPacket(int packetId, DataInputStream in) throws IOException {
        this.packetId = packetId;
        init(in);
    }

    protected SymbologyPacket() {

    }

    protected abstract void init(DataInputStream in) throws IOException;

    /**
     * @return the packetId
     */
    public int getPacketId() {
        return packetId;
    }

    /**
     * @param packetId
     *            the packetId to set
     */
    public void setPacketId(int packetId) {
        this.packetId = packetId;
    }

    @Override
    public String toString() {
        String s = String.format("\tPacket ID: 0x%04X", packetId);
        return s;
    }

}
