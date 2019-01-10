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
package com.raytheon.edex.plugin.binlightning.impl;


import static com.raytheon.edex.plugin.binlightning.impl.IBinLightningDecoder.*;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import com.raytheon.edex.plugin.binlightning.BinLightningDecoder;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Read from the message data source, isolate and create a decoder for the
 * current sub-message. In the event that the message decoder can not be
 * created, an instance of LightningErrorDecoder is created.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 20160116          18408 Wufeng Zhou  Remove direct dependency on bit shifting decoder
 * 
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
public class BinLightningFactory
{
    private static final IUFStatusHandler logger = UFStatus.getHandler(BinLightningDecoder.class);

    /**
     * Read from the message data source, isolate and create a decoder for the
     * current sub-message. In the event that the message decoder can not be
     * created, an instance of LightningErrorDecoder is created.
     * @param msgData The message data source.
     * @return The decoder instance.
     */
    public static IBinLightningDecoder getDecoder(IBinDataSource msgData)
    {
        IBinLightningDecoder decoder = null;
        
        int count = msgData.getU8();
        int decoderType = msgData.getU8();
        
        switch(decoderType)
        {
            case FLASH_RPT :
            {
                String className = "com.raytheon.edex.plugin.binlightning.impl.FlashLightningDecoder";
                decoder = loadDecoderInstance(className, msgData, count);
                break;
            }
            case RT_FLASH_RPT :
            {
                String className = "com.raytheon.edex.plugin.binlightning.impl.RTLightningDecoder";
                decoder = loadDecoderInstance(className, msgData, count);
                break;
            }
            case OTHER_RPT :
            {
                // The D2D decoders declare but do not define this message.
                decoder = new LightningErrorDecoder(UNIMPLEMENTED_DECODER);
                break;
            }
            case COMM_RPT :
            {
                // The D2D decoders declare but do not define this message.
                decoder = new LightningErrorDecoder(UNIMPLEMENTED_DECODER);
                break;
            }
            default :
            {
                decoder = new LightningErrorDecoder(UNKNOWN_MESSAGE_TYPE);
                break;
            }
        }
        
        return decoder;
    }
    
    private static IBinLightningDecoder loadDecoderInstance(String className, IBinDataSource msgData, int count) {
        IBinLightningDecoder decoder = null;
        try {
            Class<?> clazz = BinLightningFactory.class.getClassLoader().loadClass(className);
            Class<?>[] types = {IBinDataSource.class, Integer.TYPE};
            Constructor<?> constructor = clazz.getConstructor(types);
            Object[] parameters = {msgData, count};
            decoder = (IBinLightningDecoder)constructor.newInstance(parameters);
            logger.info("Loaded legacy binlightning decoder class " + className);
        } catch (ClassNotFoundException | InstantiationException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            logger.error("Fail to load binlightning decoder class " + className + ". FYI, this is error only if you are authorized: " + e.getMessage());
            decoder = new LightningErrorDecoder(UNIMPLEMENTED_DECODER);
        }
        return decoder;
    }
    
}
