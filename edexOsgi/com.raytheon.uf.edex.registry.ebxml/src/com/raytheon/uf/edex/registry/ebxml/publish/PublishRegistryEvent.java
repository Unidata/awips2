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

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.registry.event.RegistryEvent;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * 
 * Publish Registry events
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

public class PublishRegistryEvent {
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PublishRegistryEvent.class);

    /** static instance **/
    public static PublishRegistryEvent instance = null;
    
    /** allow external publish of registry events **/
    private final boolean externalPublish;
    
    /** URI to send events to, this could be another queue, topic, etc **/
    public final String externalUri;
    
    private PublishRegistryEvent() {    
        
        externalPublish = Boolean.parseBoolean(System.getProperty("registry.event.external.publish"));
        externalUri = System.getProperty("registry.event.external.publish.uri");
        
        statusHandler.info("Registry configured to publish events externally: "+externalPublish);
        
        if (externalPublish) {
            statusHandler.info("External URI: "+externalUri);
        }
    }
    
    /**
     * Static for speed
     * @return
     */
    public static PublishRegistryEvent getInstance() {
        
        if (instance == null) {
            instance = new PublishRegistryEvent();
        }
        return instance;
    }

    /**
     * Publish registry event, general method.
     * 
     * @param event
     */
    public void publish(Event event) {

        if (instance.externalPublish) {
            try {
                // filter out internal audit trail, statistics, etc
                if (event instanceof RegistryEvent) {
                    publishExternal(event);
                }
            } catch (Exception e) {
                statusHandler.error("Can't send event to external URI!: "
                        + instance.externalUri, e);
            }
        }

        // local publish as usual
        EventBus.publish(event);
    }

    /**
     * Publish registry events to an external route
     * 
     * @param event
     * @throws EdexException
     * @throws SerializationException
     */
    private void publishExternal(Event event) throws Exception {
        
        if (event != null) {
            byte[] bytes = SerializationUtil.transformToThrift(event);
            EDEXUtil.getMessageProducer().sendAsyncUri(instance.externalUri, bytes);
        }
    }
    
    
}
