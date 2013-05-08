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
package com.raytheon.edex.plugin.redbook.common.blocks;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import com.raytheon.edex.plugin.redbook.common.blocks.RedbookBlock.RedbookBlockFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.PropertiesUtil;
import com.raytheon.uf.common.util.ReflectionException;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * 
 * Build RedbookBlocks from a buffer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080516           1131 jkorman     Initial Coding.
 * Apr 29, 2013 1958       bgonzale    Added class RedbookBlockHeader,
 *                                     and moved reflective calls to the
 *                                     mapping population method.  Map now
 *                                     contains factory objects.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class RedbookBlockBuilder {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookBlockBuilder.class);

    private static final int MIN_REMAINING = 4;

    private static final String FACTORY_NAME = "$Factory";

    private final Map<String, RedbookBlockFactory> blockFactoryMap = new HashMap<String, RedbookBlockFactory>();

    public RedbookBlockBuilder() {
        populateMappings();
    }

    /**
     * 
     * @return
     */
    public RedbookBlock getBlock(ByteBuffer dataBuffer) {
        RedbookBlock blockInstance = null;

        if (dataBuffer != null) {
            RedbookBlockHeader header = getHeader(dataBuffer);
            RedbookBlockFactory factory = blockFactoryMap
                    .get(header.blockFactoryKey);

            if (factory == null) {
                blockInstance = new DefaultBlock(header, dataBuffer);
            } else {
                blockInstance = factory.createBlock(header, dataBuffer);
            }
        }
        return blockInstance;
    }

    /**
     * 
     * @param dataBuffer
     * @return
     */
    public static RedbookBlockHeader getHeader(ByteBuffer dataBuffer) {
        RedbookBlockHeader header = null;
        short rawHdr = dataBuffer.getShort();
        byte rawMode = dataBuffer.get();
        byte rawSubMode = dataBuffer.get();

        // Must have at least MIN_REMAINING
        if (dataBuffer.remaining() >= MIN_REMAINING) {
            header = new RedbookBlockHeader(rawHdr, rawMode, rawSubMode);
        } else {
            header = RedbookBlockHeader.DEFAULT;
        }
        return header;
    }

    /**
     * 
     */
    private void populateMappings() {
        final String redbookBlockProperties = "/res/conf/RedbookBlocks.properties";

        try {
            Properties redbookClassProps = PropertiesUtil.read(this.getClass()
                    .getResourceAsStream(redbookBlockProperties));

            for (String key : redbookClassProps.stringPropertyNames()) {
                String factoryClassName = redbookClassProps.get(key)
                        + FACTORY_NAME;

                try {
                    RedbookBlockFactory factory = ReflectionUtil
                            .newInstanceOfAssignableType(
                                    RedbookBlockFactory.class, factoryClassName);

                    blockFactoryMap.put((String) key, factory);
                } catch (ReflectionException e) {
                    statusHandler.error("Could not instantiate "
                            + factoryClassName, e);
                }
            }
        } catch (IOException e) {
            statusHandler.error(
                    "Could not load properties from the property file "
                            + redbookBlockProperties, e);
        }
    }
}
