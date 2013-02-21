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
package com.raytheon.uf.common.datadelivery.registry.handlers;

import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;

/**
 * Utility class to retrieve {@link IRegistryObjectHandler} implementations for
 * data delivery.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1241      djohnson     Initial creation
 * Dec 11, 2012 1403      djohnson     Adhoc subscriptions no longer go to the registry.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public final class DataDeliveryHandlers {

    /**
     * Private constructor.
     */
    private DataDeliveryHandlers() {
    }

    /**
     * Retrieve the {@link ISubscriptionHandler}.
     * 
     * @return the handler
     */
    public static ISubscriptionHandler getSubscriptionHandler() {
        return RegistryObjectHandlers.get(ISubscriptionHandler.class);
    }

    /**
     * Retrieve the {@link IPendingSubscriptionHandler}.
     * 
     * @return the handler
     */
    public static IPendingSubscriptionHandler getPendingSubscriptionHandler() {
        return RegistryObjectHandlers.get(IPendingSubscriptionHandler.class);
    }

    /**
     * Retrieve the {@link IGroupDefinitionHandler}.
     * 
     * @return the handler
     */
    public static IGroupDefinitionHandler getGroupDefinitionHandler() {
        return RegistryObjectHandlers.get(IGroupDefinitionHandler.class);
    }

    /**
     * Retrieve the {@link IProviderHandler}.
     * 
     * @return the handler
     */
    public static IProviderHandler getProviderHandler() {
        return RegistryObjectHandlers.get(IProviderHandler.class);
    }

    /**
     * Retrieve the {@link IDataSetNameHandler}.
     * 
     * @return the handler
     */
    public static IDataSetNameHandler getDataSetNameHandler() {
        return RegistryObjectHandlers.get(IDataSetNameHandler.class);
    }

    /**
     * Retrieve the {@link IParameterHandler}.
     * 
     * @return the handler
     */
    public static IParameterHandler getParameterHandler() {
        return RegistryObjectHandlers.get(IParameterHandler.class);
    }

    /**
     * Retrieve the {@link IParameterLevelHandler}.
     * 
     * @return the handler
     */
    public static IParameterLevelHandler getParameterLevelHandler() {
        return RegistryObjectHandlers.get(IParameterLevelHandler.class);
    }

    /**
     * Retrieve the {@link IDataSetMetaDataHandler}.
     * 
     * @return the handler
     */
    public static IDataSetMetaDataHandler getDataSetMetaDataHandler() {
        return RegistryObjectHandlers.get(IDataSetMetaDataHandler.class);
    }

    /**
     * Retrieve the {@link IGriddedDataSetMetaDataHandler}.
     * 
     * @return the handler
     */
    public static IGriddedDataSetMetaDataHandler getGriddedDataSetMetaDataHandler() {
        return RegistryObjectHandlers.get(IGriddedDataSetMetaDataHandler.class);
    }

    /**
     * Retrieve the {@link IDataSetHandler}.
     * 
     * @return the handler
     */
    public static IDataSetHandler getDataSetHandler() {
        return RegistryObjectHandlers.get(IDataSetHandler.class);
    }
}
