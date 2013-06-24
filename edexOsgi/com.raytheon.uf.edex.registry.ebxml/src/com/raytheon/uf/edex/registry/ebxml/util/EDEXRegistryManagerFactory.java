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
package com.raytheon.uf.edex.registry.ebxml.util;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;

import com.raytheon.uf.common.registry.ebxml.LifecycleManagerFactory;
import com.raytheon.uf.common.registry.ebxml.QueryManagerFactory;

/**
 * Contains the implementations of {@link LifecycleManager} and
 * {@link QueryManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2013 2106       djohnson     Extracted from EdexRegistryManager.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EDEXRegistryManagerFactory implements LifecycleManagerFactory,
        QueryManagerFactory {

    private LifecycleManager lifecycleManager;

    private QueryManager queryManager;

    /**
     * Get an implementation of QueryManager that uses the internal components
     * defined by the registry itself.
     * 
     * @return An implementation of QueryManager.
     * 
     * @see QueryManagerFactory
     */
    @Override
    public QueryManager getQueryManager() {
        return queryManager;
    }

    /**
     * Get an implementation of LifeCycleManager that uses the internal
     * components defined by the registry itself.
     * 
     * @return A local implementation of LifeCycleManager.
     * 
     * @see LifecycleManagerFactory
     */
    @Override
    public LifecycleManager getLifeCycleManager() {
        return lifecycleManager;
    }

    public void setLifecycleManager(LifecycleManager lifecycleManager) {
        this.lifecycleManager = lifecycleManager;
    }

    public void setQueryManager(QueryManager queryManager) {
        this.queryManager = queryManager;
    }

}
