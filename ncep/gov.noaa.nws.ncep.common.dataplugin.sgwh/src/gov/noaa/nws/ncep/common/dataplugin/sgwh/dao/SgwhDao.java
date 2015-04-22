/**
 * Set of DAO methods for SGWH data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * Aug17 2011	   		    Chin Chen	Initial Coding (Following BufrsgwhDao to refactor for 
 * 										saving data to HDF5)
 * </pre>
 * 
 * @author chin chen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.sgwh.dao;

import gov.noaa.nws.ncep.common.dataplugin.sgwh.SgwhRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepPointDataPluginDao;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class SgwhDao extends NcepPointDataPluginDao<SgwhRecord> {

    private PointDataDescription pdd;

    /**
     * Creates a new ReccoDao
     * 
     * @throws PluginException
     */
    public SgwhDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a SGWH report using the datauri .
     * 
     * @param dataURIqueryBySingleCriteria
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public SgwhRecord queryByDataURI(String dataURI) {
        SgwhRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (SgwhRecord) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the sgwh table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.sgwh where datauri='" + dataUri
                + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    public PointDataDescription getPointDataDescription()
            throws SerializationException {
        if (pdd == null) {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/sgwh.xml"));
        }
        return pdd;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(SgwhRecord p) {
        return "sgwh.h5";
    }

    @Override
    public SgwhRecord newObject() {
        return new SgwhRecord();
    }
}
