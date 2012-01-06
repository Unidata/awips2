package com.raytheon.uf.edex.plugin.fssobs;


import java.io.InputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataDescription;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;


public class FSSObsDAO extends PointDataPluginDao<FSSObsRecord> {
    

	public FSSObsDAO(String pluginName) throws PluginException {
		super(pluginName);
	}
	
    public FSSObsDAO() throws PluginException, SQLException {
        this("fssobs");
    }

    public FSSObsRecord queryByDataURI(String dataURI) {
        FSSObsRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (FSSObsRecord) obs.get(0);
        }
        return report;
    }
	
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.fssobs where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
    
    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        return null;
    }

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {
        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                try {
                    record = getDataStore((IPersistable) obj).retrieve(
                            obj.getDataURI());
                } catch (Exception e) {
                    throw new PluginException("Error retrieving FSSObs HDF5 data",
                            e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public FSSObsRecord newObject() {
        return new FSSObsRecord();
    }


    @Override
    public String getPointDataFileName(FSSObsRecord p) {
        return "fssobs.h5";
    }
    
    @Override
    public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
        if (hdf5DataDescription == null) {
            try {
                hdf5DataDescription = PointDataDescription.fromStream(this
                        .getClass().getResourceAsStream(
                                "/res/pointdata/fssobs.xml"));
            } catch (JAXBException e) {
                logger.error("Unable to load fssobs Point Data Description", e);
            }
        }
        return hdf5DataDescription;
    }
    
    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataDbDescription()
     */
    @Override
    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass().getResourceAsStream(
                    "/res/pointdata/fssobsdb.xml");
            if (stream != null) {
                try {
                    dbDataDescription = PointDataDbDescription
                            .fromStream(stream);
                } catch (JAXBException e) {
                    logger.error("Unable to load " + pluginName
                            + " Point Data Database Description", e);
                }
            }
        }
        return dbDataDescription;
    }
    	
}
