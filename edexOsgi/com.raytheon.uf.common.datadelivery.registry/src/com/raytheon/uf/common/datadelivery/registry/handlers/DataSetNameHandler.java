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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.DataSetName;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetNameObjectQuery;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetNameQuery;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.handler.BaseRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Registry handler for DataSetName objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1241      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DataSetNameHandler extends
        BaseRegistryObjectHandler<DataSetName, DataSetNameObjectQuery>
        implements
        IDataSetNameHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<DataSetName> getRegistryObjectClass() {
        return DataSetName.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected DataSetNameObjectQuery getQuery() {
        return new DataSetNameObjectQuery();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getByDataTypes(List<String> dataTypes)
            throws RegistryHandlerException {
        DataSetNameQuery query = new DataSetNameQuery();
        query.setDataSetTypes(dataTypes);

        RegistryQueryResponse<String> response = RegistryManager
                .getRegistyObjects(query);
        
        checkResponse(response, "getByDataTypes");

        return new HashSet<String>(response.getResults());
    }

}
