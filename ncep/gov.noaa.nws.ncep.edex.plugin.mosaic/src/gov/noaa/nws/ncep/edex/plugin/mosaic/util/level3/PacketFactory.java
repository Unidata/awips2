
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import java.io.DataInputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This is an abstract class to define mosaic packets and blocks.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class PacketFactory {
    /** The logger */
    private static final Log logger = LogFactory.getLog(PacketFactory.class);

    private static Map<Integer, Class<? extends SymbologyPacket>> classMap = new HashMap<Integer, Class<? extends SymbologyPacket>>();

    private static Map<Integer, Class<? extends SymbologyPacket>> genericClassMap = new HashMap<Integer, Class<? extends SymbologyPacket>>();

    public synchronized static void registerPacketType(
            Class<? extends SymbologyPacket> packetClass, int... packetIds) {

        for (int packetId : packetIds) {
            logger.debug("Registering packet ID: " + packetId + " with class "
                    + packetClass.getName());
            classMap.put(packetId, packetClass);
        }
    }

    public synchronized static void registerGenericPacketType(
            Class<? extends SymbologyPacket> packetClass, int... productIds) {

        for (int productId : productIds) {
            logger.debug("Registering product ID: " + productId
                    + " with class " + packetClass.getName());
            genericClassMap.put(productId, packetClass);
        }
    }

    static {
        // TODO: get appropriate directory. See recordFactory.loadRecordFromUri

        // TODO !!! Had to hardcode when in cave

        String[] v = new String[] { RasterPacket.class.getName() };
       
        for (String className : v) {
            try {
                Class.forName(className);
            } catch (ClassNotFoundException e) {
                logger.error("No class found: " + className);
            }
        }
    }

    public static SymbologyPacket createPacket(int packetId, DataInputStream in)
            throws IOException {
        SymbologyPacket packet = null;
        Class<? extends SymbologyPacket> packetClass = classMap.get(packetId);
    	
        if (packetClass != null) {
            try {
                Constructor<? extends SymbologyPacket> ctor = packetClass
                        .getConstructor(int.class, DataInputStream.class);
                packet = ctor.newInstance(packetId, in);
            } catch (Exception e) {
                logger.error("Unable to construct class "
                        + packetClass.getName(), e);
            }
        } else {
            logger.error("No class registered for packet ID: " + packetId);
        }
        return packet;
    }
}
