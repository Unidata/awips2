/**
 * Set of DAO methods for BUFRSSHA data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * 01/26/11		209			F. J. Yen	Initial Coding
 * </pre>
 * 
 * @author fjyen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.bufrssha.dao;

import java.util.List;

import gov.noaa.nws.ncep.common.dataplugin.bufrssha.BufrSshaRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class BufrsshaDao extends NcepDefaultPluginDao {

    
    /**
     * Creates a new BufrsshaDao
     * @throws PluginException 
     */
    public BufrsshaDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a BUFRSSHA report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public BufrSshaRecord queryByDataURI(String dataURI) {
        BufrSshaRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (BufrSshaRecord) obs.get(0);
        }
        return report;
    }
    
    /**
     * Queries for to determine if a given data uri exists on the Bufrssha table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     * element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.bufrssha where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
}
