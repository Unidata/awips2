package com.raytheon.uf.common.datadelivery.registry.ebxml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.DataSetName;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for DataSetName Objects that satisfy the values added with the
 * various set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2012 1241       djohnson    Initial creation
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataSetNameObjectQuery extends
        DataSetFilterableQuery<DataSetName, DataSetName> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataSetName> getResultType() {
        return DataSetName.class;
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataSetName> getObjectType() {
        return DataSetName.class;
    }
}