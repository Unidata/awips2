package com.raytheon.uf.edex.datadelivery.retrieval;

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Provider;

/**
 * Parse MetaData Interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public interface IParseMetaData {

    List<DataSetMetaData> parseMetaData(Provider provider, LinkStore store,
            Collection collection, String dataDateFormat);

    void storeMetaData(List<DataSetMetaData> metaDatas, DataSet dataSet);
}
