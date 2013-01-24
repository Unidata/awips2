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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetWithFiltersQuery;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * {@link IDataSetHandler} in-memory implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012 0726       djohnson     Initial creation
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class MemoryDataSetHandler extends
        BaseMemoryRegistryObjectHandler<DataSet> implements IDataSetHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public DataSet getByNameAndProvider(String name, String providerName)
            throws RegistryHandlerException {
        for (DataSet obj : getAll()) {
            if (matches(name, obj.getDataSetName())
                    && matches(providerName, obj.getProviderName())) {
                return obj;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<DataSet> getByFilters(List<String> providers,
            List<String> dataSetNames, Set<LevelType> levels,
            List<String> parameterNames, ReferencedEnvelope envelope)
            throws RegistryHandlerException {
        List<DataSet> retVal = new ArrayList<DataSet>();

        for (DataSet obj : getAll()) {
            if ((providers == null || providers.contains(obj.getProviderName()))
                    && (dataSetNames == null || dataSetNames.contains(obj
                            .getDataSetName()))) {
                if (DataSetWithFiltersQuery.satisfiesFilterCriteria(obj,
                        levels, envelope)) {
                    retVal.add(obj);
                }
            }
        }

        return retVal;
    }
}
