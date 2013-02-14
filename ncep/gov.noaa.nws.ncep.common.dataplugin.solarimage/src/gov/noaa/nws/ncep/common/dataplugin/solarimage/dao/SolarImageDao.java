package gov.noaa.nws.ncep.common.dataplugin.solarimage.dao;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;
/**
 * Data access object for retrieving/persisting solarimage data 
 * * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer        Description
 * ------------ ---------- --------------- ------------------------
 * 21/05/2012   865        sgurung, qzhou  Initial creation.
 * </pre>
 * 
 * @author sgurung, qzhou
 * @version 1.0
 */

public class SolarImageDao extends PluginDao {

	public SolarImageDao(String pluginName) throws PluginException {
        super(pluginName);
    }

	@Override
	protected IDataStore populateDataStore(IDataStore dataStore,
			IPersistable obj) throws Exception {
		
		    SolarImageRecord solImgRecord = (SolarImageRecord) obj;
		    AbstractStorageRecord rawDataRecord = null;

	        rawDataRecord = new ByteDataRecord(SolarImageRecord.RAW_DATA, solImgRecord.getDataURI(), solImgRecord.getRawData());

	        StorageProperties props = new StorageProperties();
	       
	        rawDataRecord.setProperties(props);
	        rawDataRecord.setCorrelationObject(solImgRecord);
	        dataStore.addDataRecord(rawDataRecord);		
		
		return dataStore;
	}

}
