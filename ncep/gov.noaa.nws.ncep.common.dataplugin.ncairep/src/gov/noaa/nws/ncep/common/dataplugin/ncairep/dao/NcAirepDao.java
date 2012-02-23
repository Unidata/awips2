package gov.noaa.nws.ncep.common.dataplugin.ncairep.dao;

/**
 * This software was modified from Raytheon's pirep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
//uf.common.status.IUFStatusHandler cannot be resolved. It is indirectly 
//referenced from required .class files
import gov.noaa.nws.ncep.common.dataplugin.ncairep.NcAirepRecord;

import java.util.List;
import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Set of DAO methods for Surface Observation data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/28/2011              F. J. Yen   Initial creation from airep
 * 08/31/2011    286       qzhou       Moved this from ~edex.plugin.pirep
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class NcAirepDao extends PointDataPluginDao<NcAirepRecord> {

    private PointDataDescription pdd;

    /**
     * Creates a new NcAirepDao
     * 
     * @throws PluginException
     */
    public NcAirepDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        return null;
    }

    /**
     * Retrieves an NcAirep report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public NcAirepRecord queryByDataURI(String dataURI) {
        NcAirepRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (NcAirepRecord) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the sfcobs table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.ncairep where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

	@Override
	public String[] getKeysRequiredForFileName() {
		return new String[] { "dataTime.refTime" };
	}

	@Override
	public NcAirepRecord newObject() {
		return new NcAirepRecord();
	}

	@Override
	public String getPointDataFileName(NcAirepRecord p) {
		return "ncairep.h5";
	}
	
	/*
    @Override
    public String[] getParameters(File file) throws StorageException,
            FileNotFoundException {
    	
        try {
            // This should be faster than hitting the datastore.
            return getPointDataDescription().getParameterNames();
        } catch (Exception e) {
            // let super handle it
            return super.getParameters(file);
        }
    }
    */

    public PointDataDescription getPointDataDescription() throws JAXBException {
        if (pdd == null) {
        	
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/ncairep.xml"));
        }
        return pdd;
    }
}

