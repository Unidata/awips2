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
package com.raytheon.uf.edex.decodertools.bufr;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class EngineData {
    private List<IBUFRDataPacket> packetData = null;

    private int logicalDescriptorCount = 0;

    private int physicalDescriptorCount = 0;

    // private int descriptorCount = 0;

    public EngineData() {
        packetData = new ArrayList<IBUFRDataPacket>();
    }

    public List<IBUFRDataPacket> getPacketData() {
        return packetData;
    }

    public void addPacket(IBUFRDataPacket packet) {
        packetData.add(packet);
    }

    public void addAll(List<IBUFRDataPacket> packets) {
        packetData.addAll(packets);
    }

    public int getLogicalCount() {
        return logicalDescriptorCount;
    }

    public void incLogicalCount() {
        logicalDescriptorCount++;
    }

    public void incLogicalCountBy(int increment) {
        logicalDescriptorCount += increment;
    }

    public int getPhysicalCount() {
        return physicalDescriptorCount;
    }

    public void incPhysicalCount() {
        physicalDescriptorCount++;
    }

    public void incPhysicalCountBy(int increment) {
        physicalDescriptorCount += increment;
    }

}
