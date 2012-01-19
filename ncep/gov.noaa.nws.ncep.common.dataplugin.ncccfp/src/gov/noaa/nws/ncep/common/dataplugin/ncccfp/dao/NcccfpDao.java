/**
 * Set of DAO methods for NCCCFP data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * 26/05/10		155		F. J. Yen	Initial Coding for to11dr11 (Following one of RTN's DAO to refactor)
 * </pre>
 * 
 * @author fjyen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.ncccfp.dao;

import java.util.List;

import gov.noaa.nws.ncep.common.dataplugin.ncccfp.NcccfpRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class NcccfpDao extends NcepDefaultPluginDao {

    
    /**
     * Creates a new ReccoDao
     * @throws PluginException 
     */
    public NcccfpDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a NCCCFP report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public NcccfpRecord queryByDataURI(String dataURI) {
        NcccfpRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (NcccfpRecord) obs.get(0);
        }
        return report;
    }
    
    /**
     * Queries for to determine if a given data uri exists on the NCCCFP table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     * element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.ncccfp where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
}
