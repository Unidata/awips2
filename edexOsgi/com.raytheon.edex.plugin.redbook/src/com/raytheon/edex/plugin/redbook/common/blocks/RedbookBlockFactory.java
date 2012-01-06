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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.ByteBuffer;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080516           1131 jkorman     Initial Coding.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class RedbookBlockFactory {
    private Log logger = LogFactory.getLog(getClass());

    private static final String MODE_KEY_FMT = "%03d_%03d";

    private static final String DEFAULT_KEY = "default";

    private static final int MIN_REMAINING = 4;

    private Properties RptClassMap;

    private static RedbookBlockFactory factoryInstance = null;

    private boolean loaded = false;

    private RedbookBlockFactory() {
        populateMappings();
    }

    /**
     * 
     * @return
     */
    public RedbookBlock getBlock(ByteBuffer dataBuffer) {
        RedbookBlock blockInstance = null;

        if (dataBuffer != null) {
            String blockId = RptClassMap.getProperty(getBlockKey(dataBuffer));

            try {
                if ((blockId != null) && (!DEFAULT_KEY.equals(blockId))) {

                    Class<?> c = null;
                    try {
                        c = Class.forName(blockId);
                        try {
                            Constructor<?> con = c
                                    .getConstructor(ByteBuffer.class);

                            blockInstance = (RedbookBlock) con
                                    .newInstance(dataBuffer);
                        } catch (InstantiationException e) {
                            logger.error("Could not instantiate " + blockId, e);
                        } catch (IllegalAccessException e) {
                            logger.error("Illegal access " + blockId, e);
                        } catch (SecurityException e) {
                            logger.error("Could not instantiate " + blockId, e);
                        } catch (NoSuchMethodException e) {
                            logger.error("Could not instantiate " + blockId, e);
                        } catch (IllegalArgumentException e) {
                            logger.error("Could not instantiate " + blockId, e);
                        } catch (InvocationTargetException e) {
                            logger.error("Could not instantiate " + blockId, e);
                        }
                    } catch (ClassNotFoundException e1) {
                        logger.error("No class found for " + blockId);
                    }
                }
            } finally {
                if (blockInstance == null) {
                    blockInstance = new DefaultBlock(dataBuffer);
                }
            }
        }
        return blockInstance;
    }

    /**
     * @return the loaded
     */
    public boolean isLoaded() {
        return loaded;
    }

    /**
     * 
     * @return
     */
    public static synchronized RedbookBlockFactory getInstance() {
        if (factoryInstance == null) {
            factoryInstance = new RedbookBlockFactory();
        }
        return factoryInstance;
    }

    /**
     * 
     * @param dataBuffer
     * @return
     */
    private String getBlockKey(ByteBuffer dataBuffer) {
        String blockKey = DEFAULT_KEY;
        // Must have at least MIN_REMAINING
        if (dataBuffer.remaining() >= MIN_REMAINING) {
            dataBuffer.mark();
            // Dummy read for the flags/length
            dataBuffer.getShort();

            int mode = (dataBuffer.get() & 0xFF);
            int subMode = (dataBuffer.get() & 0xFF);

            dataBuffer.reset();

            blockKey = String.format(MODE_KEY_FMT, mode, subMode);
        }
        return blockKey;
    }

    /**
     * 
     */
    private void populateMappings() {
        InputStream strm = null;
        BufferedReader bf = null;

        RptClassMap = new Properties();
        try {
            try {
                strm = this.getClass().getResourceAsStream(
                        "/res/conf/RedbookBlocks.properties");

                if (strm != null) {
                    bf = new BufferedReader(new InputStreamReader(strm));
                    RptClassMap.load(bf);
                    loaded = true;
                } else {
                    loaded = false;
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }
    }
}
