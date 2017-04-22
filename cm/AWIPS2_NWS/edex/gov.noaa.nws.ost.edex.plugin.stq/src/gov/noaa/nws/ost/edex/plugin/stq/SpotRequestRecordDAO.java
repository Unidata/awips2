package gov.noaa.nws.ost.edex.plugin.stq;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import gov.noaa.nws.ost.dataplugin.stq.SpotRequestRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * SpotRequestRecord Dao
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * Sept  4, 2015            pwang       Initial creation
 * 
 * </pre>
 * 
 * @author pwang
 * @version 1.0
 */

public class SpotRequestRecordDAO extends PointDataPluginDao<SpotRequestRecord> {

    /** The logger */
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SpotRequestRecordDAO.class);
    
    private static final String FILE_NAME_KEY = "dataTime.refTime";
    /**
     * Creates a new Spot Request Dao
     * 
     * @param pluginName
     * @throws PluginException
     */
    public SpotRequestRecordDAO(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves an spot request using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The spot request record if it exists.
     */
    public SpotRequestRecord queryByDataURI(String dataURI) {
        SpotRequestRecord req = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            logger.error("STQ DAO: query with criteria: " + dataURI + " failed ! " + e );
        }
        if ((obs != null) && (obs.size() > 0)) {
            req = (SpotRequestRecord) obs.get(0);
        }
        return req;
    }

    /**
     * Queries for to determine if a given data uri exists on the stq table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.stq where datauri='" + dataUri
                + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getKeysRequiredForFileName
     * ()
     */
    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { FILE_NAME_KEY };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataFileName
     * (com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public String getPointDataFileName(SpotRequestRecord p) {
        StringBuilder fname = new StringBuilder();
        fname.append(this.pluginName);
        fname.append(".h5");
        return fname.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.pointdata.PointDataPluginDao#newObject()
     */
    @Override
    public SpotRequestRecord newObject() {
        return new SpotRequestRecord();
    }
}
