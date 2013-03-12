/**
 * NtransDao
 * 
 * This java class performs the dataaccess layer functionality  to the HDF5 for ASCAT,Quikscat
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 11/2009		Uma Josyula	Initial creation	
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.common.dataplugin.ntrans.dao;

import java.util.ArrayList;
import java.util.List;


import gov.noaa.nws.ncep.common.dataplugin.ntrans.NtransRecord;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;

public class NtransDao extends PluginDao{
	public NtransDao(String pluginName) throws PluginException {
		super(pluginName);
	}
	
	@Override
	protected IDataStore populateDataStore(IDataStore dataStore,
			IPersistable record) throws StorageException {

		NtransRecord ntransRecord = (NtransRecord) record;
		int msgLength = (ntransRecord.getConvertedMessage()).length;
		long nx,ny;
		System.out.println("@@ came into NtransDao");

		nx = 1;
		ny = msgLength;

		long[] sizes = new long[] { nx, ny };
		IDataRecord storageRecord = new ByteDataRecord("Ntrans", ntransRecord.getDataURI(),
				(byte[]) ntransRecord.getConvertedMessage(), 2, sizes);
		System.out.println("@@ came into NtransDao and storageRecord set");

		storageRecord.setCorrelationObject(ntransRecord);
		dataStore.addDataRecord(storageRecord);
		System.out.println("@@ NtransDao about to return datastore");

		return dataStore;
	}



	@Override
	public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
			int tileSet) throws PluginException {
		List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();
		System.out.println("In decoderDao getHDF5Data");

		for (PluginDataObject obj : objects) {
			IDataRecord[] record = null;
			if (obj instanceof IPersistable) {
				/* connect to the data store and retrieve the data */
				try {
					record = getDataStore((IPersistable) obj).retrieve(
							obj.getDataURI());
				} catch (Exception e) {
					throw new PluginException(
							"Error retrieving NTRANS HDF5 data", e);
				}
				retVal.add(record);
			}
		}

		return retVal;
	}




}


