package com.raytheon.uf.common.datadelivery.registry.ebxml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
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
 * Mar 28, 2012 356        jspinks      Initial creation
 * May 15, 2012 455        jspinks      Updated with parameters slot.
 * Jun 21, 2012 736        djohnson     Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson     Add generics and results retrieval to registry queries.
 * Aug 10, 2012 1022       djohnson     Requires provider name for uniqueness.
 * Aug 15, 2012 0743       djohnson     Move filterable methods to superclass.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataSetMetaDataQuery extends
        DataSetMetaDataFilterableQuery<DataSetMetaData> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataSetMetaData> getResultType() {
        return DataSetMetaData.class;
    }
}
