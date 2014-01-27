package gov.noaa.nws.ncep.common.dataplugin.pirep.dao;

/**
 * This software was modified from Raytheon's pirep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
//uf.common.status.IUFStatusHandler cannot be resolved. It is indirectly 
//referenced from required .class files
import gov.noaa.nws.ncep.common.dataplugin.pirep.PirepRecord;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
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
 * 04/28/2011              F. J. Yen   Initial creation from pirep
 * 08/31/2011    286       qzhou       Moved this from ~edex.plugin.pirep
 * Sep 05, 2013  2316      bsteffen    Unify pirep and ncpirep.
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class PirepDao extends PointDataPluginDao<PirepRecord> {

    private PointDataDescription pdd;

    /**
     * Creates a new PirepDao
     * 
     * @throws PluginException
     */
    public PirepDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        return null;
    }

    /**
     * Retrieves an Pirep report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public PirepRecord queryByDataURI(String dataURI) {
        PirepRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (PirepRecord) obs.get(0);
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

        String sql = "select datauri from awips.pirep where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public PirepRecord newObject() {
        return new PirepRecord();
    }

    @Override
    public String getPointDataFileName(PirepRecord p) {
        return "pirep.h5";
    }

    /*
     * @Override public String[] getParameters(File file) throws
     * StorageException, FileNotFoundException {
     * 
     * try { // This should be faster than hitting the datastore. return
     * getPointDataDescription().getParameterNames(); } catch (Exception e) { //
     * let super handle it return super.getParameters(file); } }
     */

    public PointDataDescription getPointDataDescription()
            throws SerializationException {
        if (pdd == null) {

            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/pirep.xml"));
        }
        return pdd;
    }

}
