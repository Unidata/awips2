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
import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericUtil;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/2013   DCS51      zwang       Handle GFM product
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class PacketFactory {
    /** The logger */
    private static final IUFStatusHandler handler = UFStatus.getHandler(PacketFactory.class);

    private static Map<Integer, Class<? extends SymbologyPacket>> classMap = new HashMap<Integer, Class<? extends SymbologyPacket>>();

    private static Map<Integer, Class<? extends SymbologyPacket>> genericClassMap = new HashMap<Integer, Class<? extends SymbologyPacket>>();

    public synchronized static void registerPacketType(
            Class<? extends SymbologyPacket> packetClass, int... packetIds) {
        for (int packetId : packetIds) {
            if (handler.isPriorityEnabled(Priority.DEBUG)) {
                handler.handle(Priority.DEBUG, "Registering packet ID: " + packetId
                                + " with class " + packetClass.getName());
            }
            classMap.put(packetId, packetClass);
        }
    }

    public synchronized static void registerGenericPacketType(
            Class<? extends SymbologyPacket> packetClass, int... productIds) {
        for (int productId : productIds) {
            if (handler.isPriorityEnabled(Priority.DEBUG)) {
                handler.handle(Priority.DEBUG, "Registering product ID: " + productId
                                + " with class " + packetClass.getName());
            }
            genericClassMap.put(productId, packetClass);
        }
    }

    static {
        // TODO: get appropriate directory. See recordFactory.loadRecordFromUri

        // TODO !!! Had to hardcode when in cave

        String[] v = new String[] { LinkedVectorPacket.class.getName(),
                UnlinkedVectorPacket.class.getName(),
                RasterPacket.class.getName(), RadialPacket.class.getName(),
                TextSymbolPacket.class.getName(),
                RadialPacket8bit.class.getName(),
                HdaHailPacket.class.getName(), StormIDPacket.class.getName(),
                MesocyclonePacket.class.getName(),
                CorrelatedShearPacket.class.getName(),
                TVSPacket.class.getName(), ETVSPacket.class.getName(),
                WindBarbPacket.class.getName(),
                STICirclePacket.class.getName(),
                SpecialGraphicSymbolPacket.class.getName(),
                SCITDataPacket.class.getName(),
                PrecipDataPacket.class.getName(),
                CellTrendVolumeScanPacket.class.getName(),
                VectorArrowPacket.class.getName(),
                CellTrendDataPacket.class.getName(),
                HailPositivePacket.class.getName(),
                HailProbablePacket.class.getName(),
                GenericDataPacket.class.getName(),
                LinkedContourVectorPacket.class.getName(),
                UnlinkedContourVectorPacket.class.getName(),
                GFMPacket.class.getName(),
                DMDPacket.class.getName() };

        // Properties props =
        // PropertiesFactory.getInstance().getPluginProperties(
        // "RADAR");
        //
        // List<String> list = props.getPluginPropertyList("PacketList");
        //
        for (String className : v) {
            try {
                Class.forName(className);
            } catch (ClassNotFoundException e) {
                handler.handle(Priority.ERROR, "No class found: " + className);
            }
        }
    }

    public static SymbologyPacket createPacket(int packetId, DataInputStream in)
            throws IOException {
        SymbologyPacket packet = null;
        Class<? extends SymbologyPacket> packetClass = classMap.get(packetId);

        // Check for Generic Packet so the correct Generic Packet subclass can
        // be used.
        if (packetClass != null && packetClass.equals(GenericDataPacket.class)) {
            int productId = GenericUtil.getProductID(in);
            packetClass = genericClassMap.get(productId);
        }

        if (packetClass != null) {
            try {
                Constructor<? extends SymbologyPacket> ctor = packetClass
                        .getConstructor(int.class, DataInputStream.class);
                packet = ctor.newInstance(packetId, in);
            } catch (Exception e) {
                handler.handle(Priority.ERROR,
                        "Unable to construct class " + packetClass.getName(), e);
            }
        } else {
            handler.handle(Priority.ERROR, "No class registered for packet ID: " + packetId);
        }
        return packet;
    }
}
