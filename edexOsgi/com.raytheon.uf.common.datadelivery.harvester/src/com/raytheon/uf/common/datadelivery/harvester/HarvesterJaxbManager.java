package com.raytheon.uf.common.datadelivery.harvester;

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

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
/**
 * JAXB for Harvester
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2013 2361       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HarvesterJaxbManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HarvesterJaxbManager.class);

    private static Class<?>[] clazzess = new Class<?>[] {
            HarvesterConfig.class, Provider.class, Connection.class,
            ProviderType.class, ServiceType.class, Agent.class,
            CrawlAgent.class, OGCAgent.class, ConfigLayer.class };

    private JAXBManager jaxb = null;

    private static HarvesterJaxbManager instance = new HarvesterJaxbManager();

    /**
     * marshall and unmarshall harvester objects
     * 
     * @return
     */
    public static JAXBManager getJaxb() {

        if (instance.jaxb == null) {
            try {
                instance.jaxb = new JAXBManager(clazzess);

            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return instance.jaxb;
    }

}
