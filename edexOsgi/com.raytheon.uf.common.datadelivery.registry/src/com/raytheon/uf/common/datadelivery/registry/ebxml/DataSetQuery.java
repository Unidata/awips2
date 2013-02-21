package com.raytheon.uf.common.datadelivery.registry.ebxml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for {@link DataSet} objects that satisfy the values added with the
 * various set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
 * May 15, 2012 455        jspinks     Updated with parameters slot.
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataSetQuery extends DataSetFilterableQuery<DataSet, DataSet> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataSet> getObjectType() {
        return DataSet.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataSet> getResultType() {
        return DataSet.class;
    }
}