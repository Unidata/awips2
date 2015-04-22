package gov.noaa.nws.ncep.edex.plugin.pgen.dao;

import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * 
 * PluginDao for the pgen data plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2013            sgilbert     Initial creation
 * 
 * </pre>
 * 
 * @author sgilbert
 * @version 1.0
 */
public class PgenDao extends PluginDao {

    public PgenDao(String pluginName) throws PluginException {
        super(pluginName);
        // TODO Auto-generated constructor stub
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {

        AbstractStorageRecord storageRecord = null;
        PgenRecord record = (PgenRecord) obj;

        storageRecord = new StringDataRecord(PgenRecord.ACTIVITY_XML,
                record.getDataURI(), new String[] { record.getActivityXML() });

        StorageProperties props = new StorageProperties();

        storageRecord.setProperties(props);
        storageRecord.setCorrelationObject(record);
        dataStore.addDataRecord(storageRecord);

        return dataStore;
    }
}
