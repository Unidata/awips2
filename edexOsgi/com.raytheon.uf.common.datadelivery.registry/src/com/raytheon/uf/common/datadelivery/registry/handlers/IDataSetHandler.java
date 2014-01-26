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

import java.util.List;
import java.util.Set;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * {@link IRegistryObjectHandler} for {@link DataSet}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1241      djohnson     Initial creation
 * Nov 19, 2012 1166      djohnson     Clean up JAXB representation of registry objects.
 * Dec 10, 2012 1259      bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Jun 04, 2013  223      mpduff       Added datatype to the filter.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IDataSetHandler extends IRegistryObjectHandler<DataSet> {

    /**
     * Get by name.
     * 
     * @param name
     *            the name
     * @param providerName
     *            the provider name
     * @throws RegistryHandlerException
     *             on unsuccessful response
     */
    DataSet getByNameAndProvider(String name, String providerName)
            throws RegistryHandlerException;

    /**
     * Retrieve datasets that match the specified filter criteria. Any of the
     * parameters may be null.
     * 
     * @param providers
     *            the provider names
     * @param dataSetNames
     *            the data set names
     * @param levels
     *            the levels
     * @param parameterNames
     *            the parameter names
     * @param dataSetTypes
     *            List of data set types
     * @param envelope
     *            the ReferencedEnvelope
     * @return
     * @throws RegistryHandlerException
     */
    List<DataSet> getByFilters(List<String> providers,
            List<String> dataSetNames, Set<LevelType> levels,
            List<String> parameterNames, List<String> dataSetTypes,
            ReferencedEnvelope envelope) throws RegistryHandlerException;
}
