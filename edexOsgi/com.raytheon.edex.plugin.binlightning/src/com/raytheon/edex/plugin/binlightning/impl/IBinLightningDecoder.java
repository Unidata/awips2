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

import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;

/**
 * Declare the interface for binary lightning decoding. The decoders are
 * expected to implement an Iterable interface. Data decoding will take place
 * during construction of the element.
 * 
 * <pre>
 * the recommended constructor for this interface is
 * 
 * @param data An IBinDataSource data source containing the data to be decoded.
 * @param count The number of records that this decoder should see.
 * <code>public X (IBinDataSource data, int count)</code>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 20070912            379 jkorman     Code review cleanup.
 * Jun 05, 2014 3226       bclement    LightningStikePoint refactor
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface IBinLightningDecoder extends Iterable<LightningStrikePoint>
{
    public static final int NO_ERROR = 0; 
    public static final int NO_TIME_INFO = 1;
    public static final int NOT_ENOUGH_DATA = 2;
    
    public static final int UNKNOWN_MESSAGE_TYPE = 98;
    public static final int UNIMPLEMENTED_DECODER = 99;
    
    public static final int FLASH_RPT = 0x96;
    public static final int RT_FLASH_RPT = 0x97;
    public static final int OTHER_RPT = 0xD0;
    public static final int COMM_RPT = 0xD1;
    
    public static final LtgStrikeType DEFAULT_FLASH_TYPE = LtgStrikeType.CLOUD_TO_GROUND;

    /*
     */ 
    
    /**
     * Get the last error for the instance.
     * @return The error code.
     */
    public int getError();

}
