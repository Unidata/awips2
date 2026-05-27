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

import java.util.Iterator;

import com.raytheon.uf.common.dataplugin.binlightning.impl.LightningStrikePoint;


/**
 * Mock decoder that is returned from the lightning decoder factory in the event
 * that a proper decoder could not be constructed. Instances are created by passing
 * various "permanent" error codes that will always be returned by the instance.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070810            379 jkorman     Initial Coding from prototype.
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0
 */
public class LightningErrorDecoder implements IBinLightningDecoder
{
    private final int errorCode;
    
    /**
     * Construct an instance of this decoder with a constant error code.
     * @param errorCode The constant error code.
     */
    public LightningErrorDecoder(int errorCode)
    {
        this.errorCode = errorCode;
    }
    
    /**
     * Get the error code for this decoder.
     * @return The error code.
     */
    @Override
    public int getError()
    {
        return errorCode;
    }

    /**
     * This Iterator always returns, hasNext() = false, next = null.
     * @return The "dummy" iterator for this class.
     */
    @Override
    public Iterator<LightningStrikePoint> iterator()
    {
        return new Iterator<LightningStrikePoint>()
        {

            @Override
            public boolean hasNext()
            {
                return false;
            }

            @Override
            public LightningStrikePoint next()
            {
                return null;
            }

            @Override
            public void remove()
            {
            }
        };
    }

}
