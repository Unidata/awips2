/**
 * Set of DAO methods for BUFRSGWHV data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * 11/17/10		208			F. J. Yen	Initial Coding (based on BUFRSGWH)
 * </pre>
 * 
 * @author fjyen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.bufrsgwhv.dao;

import java.util.List;

import gov.noaa.nws.ncep.common.dataplugin.bufrsgwhv.BufrSgwhvRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class BufrsgwhvDao extends NcepDefaultPluginDao {

    
    /**
     * Creates a new ReccoDao
     * @throws PluginException 
     */
    public BufrsgwhvDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a BUFRSGWHV report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public BufrSgwhvRecord queryByDataURI(String dataURI) {
        BufrSgwhvRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (BufrSgwhvRecord) obs.get(0);
        }
        return report;
    }
    
    /**
     * Queries for to determine if a given data uri exists on the Bufrsgwhv table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     * element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.bufrsgwhv where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
}
