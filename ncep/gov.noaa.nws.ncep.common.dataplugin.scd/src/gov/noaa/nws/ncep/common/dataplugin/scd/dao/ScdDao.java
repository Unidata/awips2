/**
 * 
 * FfgDao
 * 
 * This java class performs data access for FFG.
 *  
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 05/2010		14				T. Lee		Migration to TO11DR11
 * </pre>
 *
 * @author T.Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.scd.dao;

import gov.noaa.nws.ncep.common.dataplugin.scd.ScdRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class ScdDao extends NcepDefaultPluginDao {
	
    /**
     * FfgDao constructor.
     * 
     * @throws PluginException
     */
    public ScdDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves an FFG report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public ScdRecord queryByDataURI(String dataURI) {
        ScdRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (ScdRecord) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the FFG table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.ffg where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
}
