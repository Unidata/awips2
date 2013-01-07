package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.Transient;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.registry.IMultipleResultFormatter;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Query to retrieve the DataLevelType descriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012 736        djohnson    Initial creation
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Aug 15, 2012 0743       djohnson    Type-safe result formatters.
 * 
 * </pre>
 * 
 * @author djohnson
 */
@DynamicSerialize
public class DataLevelTypeDescriptionQuery extends
        ParameterFilterableQuery<String> implements
        IMultipleResultFormatter<String> {
    @Transient
    private transient Set<String> alreadyFound;

    @Override
    public Collection<String> decodeObject(RegistryObjectType registryObjectType)
            throws SerializationException {

        Parameter object = (Parameter) RegistryUtil
                .decodeObject(registryObjectType);

        if (alreadyFound == null) {
            alreadyFound = new HashSet<String>();
        }
        List<DataLevelType> dataLevelTypes = object.getLevelType();

        Set<String> descriptions = new HashSet<String>(dataLevelTypes.size());

        for (DataLevelType dataLevelType : dataLevelTypes) {
            String description = dataLevelType.getDescription();

            // Only add it if we haven't already found it
            if (alreadyFound.add(description)) {
                descriptions.add(description);
            }

            return descriptions;
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<String> getResultType() {
        return String.class;
    }
}
