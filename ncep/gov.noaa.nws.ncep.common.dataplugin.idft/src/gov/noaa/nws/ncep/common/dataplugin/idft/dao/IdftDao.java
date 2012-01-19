/**
 * Set of DAO methods for IDFT data.
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
package gov.noaa.nws.ncep.common.dataplugin.idft.dao;

import java.util.List;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class IdftDao extends NcepDefaultPluginDao {

    
    /**
     * Creates a new ReccoDao
     * @throws PluginException 
     */
    public IdftDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a idft report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public IdftRecord queryByDataURI(String dataURI) {
        IdftRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (IdftRecord) obs.get(0);
        }
        return report;
    }
    
    /**
     * Queries for to determine if a given data uri exists on the IDFT table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     * element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.idft where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
}
