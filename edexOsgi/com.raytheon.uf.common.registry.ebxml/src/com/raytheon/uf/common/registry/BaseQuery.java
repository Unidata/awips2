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
package com.raytheon.uf.common.registry;

import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Consolidates result-based methods into a base query class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2012  955        djohnson     Initial creation
 * Aug 20, 2012 0743       djohnson     Finish making registry type-safe.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class BaseQuery<T> implements RegistryQuery<T> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BaseQuery.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getResults(RegistryResponse<T> response) {
        List<T> registryObjects = response.getRegistryObjects();
        return (registryObjects == null) ? Collections.<T> emptyList()
                : registryObjects;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getSingleResult(RegistryResponse<T> response) {

        List<T> results = getResults(response);

        if (!results.isEmpty()) {
            if (results.size() > 1) {
                statusHandler
                        .warn("More than one result was returned from the query, but only one result was requested?  Please set the appropriate filters to limit your result set.");
            }

            return results.get(0);
        }

        return null;
    }

}
