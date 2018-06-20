package edu.wisc.ssec.cimss.edex.plugin.convectprob.dao;

import edu.wisc.ssec.cimss.common.dataplugin.convectprob.ConvectProbRecord;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * NOAA/CIMSS Prob Severe Model Data Acquisition Object
 *
 * Defines access to persisted data from NOAA/CIMSS Prob Severe Model
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2014 DCS 15298   jgerth      Initial Creation.
 *
 * </pre
 *
 * @author Jordan Gerth
 * @version 1.0
 *
 */

public class ConvectProbDao extends PluginDao {

    /**
     * ConvectProbDao constructor
     * @param Plugin name
     * @throws PluginException
     */
    public ConvectProbDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Copy data from a Persistable object into a given DataStore container.
     * @param dataStore DataStore instance to receive the Persistable data.
     * @param obj The Persistable object to be stored.
     * @throws Exception Any general exception thrown in this method.
     */
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        ConvectProbRecord cpRec = (ConvectProbRecord) obj;

        for (int i = 0; i < cpRec.getDataArrays().length; i++) {
            IDataRecord record = DataStoreFactory.createStorageRecord(
                    cpRec.getDataNames()[i], cpRec.getDataURI(), cpRec.getDataArrays()[i]);
            record.setCorrelationObject(cpRec);
            dataStore.addDataRecord(record);
        }

        return dataStore;
    }

}
