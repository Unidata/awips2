/**
 * Set of DAO methods for PAFM data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * 27May2010		37		F. J. Yen	Initial Coding (Following one of RTN's DAO to refactor)
 * </pre>
 * 
 * @author fjyen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.pafm.dao;

import java.util.List;

import gov.noaa.nws.ncep.common.dataplugin.pafm.PafmRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class PafmDao extends NcepDefaultPluginDao {

    
    /**
     * Creates a new ReccoDao
     * @throws PluginException 
     */
    public PafmDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a pafm report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public PafmRecord queryByDataURI(String dataURI) {
        PafmRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (PafmRecord) obs.get(0);
        }
        return report;
    }
    
    /**
     * Queries for to determine if a given data uri exists on the PAFM table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     * element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.pafm where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
}
