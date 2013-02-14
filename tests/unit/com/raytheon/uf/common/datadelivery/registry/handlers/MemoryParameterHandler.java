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

import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * {@link IParameterHandler} in-memory implementation.
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
public class MemoryParameterHandler extends
        BaseMemoryRegistryObjectHandler<Parameter> implements IParameterHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getNamesByDataTypes(List<String> dataTypes)
            throws RegistryHandlerException {
        Set<String> names = new HashSet<String>();
        for (Parameter parameter : getAll()) {
            if (dataTypes == null
                    || dataTypes.contains(parameter.getDataType().toString())) {
                names.add(parameter.getName());
            }
        }
        return names;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getDataLevelTypeDescriptions(List<String> dataTypes)
            throws RegistryHandlerException {
        Set<String> descriptions = new HashSet<String>();
        for (Parameter parameter : getAll()) {
            List<DataLevelType> dataLevelTypes = parameter.getLevelType();

            for (DataLevelType dataLevelType : dataLevelTypes) {
                String description = dataLevelType.getDescription();
                descriptions.add(description);
            }
        }
        return new ArrayList<String>(descriptions);
    }
}
