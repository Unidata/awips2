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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * {@link IBaseDataSetMetaDataHandler} in-memory implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012 0726       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class BaseMemoryDataSetMetaDataHandler<T extends DataSetMetaData>
        extends
        BaseMemoryRegistryObjectHandler<T> implements
        IBaseDataSetMetaDataHandler<T> {
    /**
     * {@inheritDoc}
     */
    @Override
    public Set<ImmutableDate> getDatesForDataSet(String dataSetName,
            String providerName) throws RegistryHandlerException {
        Set<ImmutableDate> dates = new HashSet<ImmutableDate>();

        for (T obj : getAll()) {
            if (matches(dataSetName, obj.getDataSetName())
                    && matches(providerName, obj.getProviderName())) {
                dates.add(obj.getDate());
            }
        }

        return dates;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByDataSet(String dataSetName,
            String providerName) throws RegistryHandlerException {
        List<T> retVal = new ArrayList<T>();

        for (T obj : getAll()) {
            if (matches(dataSetName, obj.getDataSetName())
                    && matches(providerName, obj.getProviderName())) {
                retVal.add(obj);
            }
        }

        return retVal;
    }
}
