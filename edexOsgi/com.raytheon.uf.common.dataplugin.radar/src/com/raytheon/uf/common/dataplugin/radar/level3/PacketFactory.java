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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericUtil;

/**
 * A factory for creating packets based on the packet ID. In the case of generic
 * packets, it further uses the product ID to determine the packet to create.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/04/2013   DCS51      zwang       Handle GFM product
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * 03/04/2014   2870       njensen     Log product ID when no class found for generic packet
 * 06/27/2017   6316       njensen     Switched from ufstatus to slf4j for logging
 * 
 * </pre>
 * 
 * @author randerso
 */

public class PacketFactory {

    /** The logger */
    private final Logger logger = LoggerFactory.getLogger(PacketFactory.class);

    private Map<Integer, Class<? extends SymbologyPacket>> classMap = new HashMap<>();

    private Map<Integer, Class<? extends SymbologyPacket>> genericClassMap = new HashMap<>();

    private static final PacketFactory instance = new PacketFactory();

    public static synchronized PacketFactory getInstance() {
        return instance;
    }

    public synchronized Class<? extends SymbologyPacket> registerPacketType(
            Class<? extends SymbologyPacket> packetClass, int... packetIds) {
        for (int packetId : packetIds) {
            if (logger.isDebugEnabled()) {
                logger.debug("Registering packet ID: " + packetId
                        + " with class " + packetClass.getName());
            }
            classMap.put(packetId, packetClass);
        }
        return packetClass;
    }

    public synchronized Class<? extends SymbologyPacket> registerGenericPacketType(
            Class<? extends SymbologyPacket> packetClass, int... productIds) {
        for (int productId : productIds) {
            if (logger.isDebugEnabled()) {
                logger.debug("Registering product ID: " + productId
                        + " with class " + packetClass.getName());
            }
            genericClassMap.put(productId, packetClass);
        }
        return packetClass;
    }

    public SymbologyPacket createPacket(int packetId, DataInputStream in)
            throws IOException {

        SymbologyPacket packet = null;
        Class<? extends SymbologyPacket> packetClass = classMap.get(packetId);

        /*
         * Check for Generic Packet so the correct Generic Packet subclass can
         * be used.
         */
        if (packetClass != null && packetClass.equals(GenericDataPacket.class)) {
            int productId = GenericUtil.getProductID(in);
            packetClass = genericClassMap.get(productId);
            if (packetClass == null) {
                logger.warn("No class registered for generic packet ID: "
                        + packetId + " with product ID: " + productId);
                return packet;
            }
        }

        if (packetClass != null) {
            try {
                Constructor<? extends SymbologyPacket> ctor = packetClass
                        .getConstructor(int.class, DataInputStream.class);
                packet = ctor.newInstance(packetId, in);
            } catch (Exception e) {
                logger.error(
                        "Unable to construct class "
                        + packetClass.getName(), e);
            }
        } else {
            logger.warn("No class registered for packet ID: " + packetId);
        }
        return packet;
    }
}
