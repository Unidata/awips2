package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.registry.IMultipleResultFormatter;
import com.raytheon.uf.common.registry.ebxml.encoder.IRegistryEncoder;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Query to retrieve the {@link DataLevelType}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012 736        djohnson    Initial creation
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Jun 24, 2013 2106       djohnson    Pass encoder to result formatters.
 * 
 * </pre>
 * 
 * @author djohnson
 */
@DynamicSerialize
public class DataLevelTypeQuery extends
        DataSetFilterableQuery<DataLevelType, DataSet>
        implements
        IMultipleResultFormatter<DataLevelType> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataLevelType> getResultType() {
        return DataLevelType.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataSet> getObjectType() {
        return DataSet.class;
    }

    @Override
    public Collection<DataLevelType> decodeObject(
            RegistryObjectType registryObjectType, IRegistryEncoder encoder)
            throws SerializationException {

        Object object = DataDeliveryRegistryObjectTypes.getObject(registryObjectType, encoder);

        if (object instanceof DataSet) {

            Map<String, Parameter> parameters = ((DataSet) object)
                    .getParameters();
            List<DataLevelType> levelTypes = new ArrayList<DataLevelType>(
                    parameters.size());
            for (Parameter parameter : parameters.values()) {
                levelTypes.addAll(parameter.getLevelType());
            }

            return levelTypes;
        }

        return null;
    }
}
