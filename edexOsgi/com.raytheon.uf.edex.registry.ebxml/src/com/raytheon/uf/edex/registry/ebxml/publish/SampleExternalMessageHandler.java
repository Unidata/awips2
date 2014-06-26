package com.raytheon.uf.edex.registry.ebxml.publish;

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

import com.raytheon.uf.common.registry.event.RegistryEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Sample External Registry Event Handler
 * 
 * This is a sample class only here to prove external delivery of registry messages.
 * When an actual handler is created this can go away along with it's supporting spring.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 25, 2014 2760      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SampleExternalMessageHandler {
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SampleExternalMessageHandler.class);
    
    // default pub constuctor
    public SampleExternalMessageHandler() {
        
    }
    
    /**
     * Notify of arrival on the JMS route specified in spring
     * @param RegistryEvent
     */
    public void notify(RegistryEvent event) {
        
        if (event != null) {
            statusHandler.info("Received Registry event externally: "+event.toString());
        }
    }

}
