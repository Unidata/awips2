/**
 * Set of DAO methods for PAFM data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * 23 Sep 2011				B. Hebbard	Initial Coding (Following NcUairDao and FJY's instructions)
 * </pre>
 * 
 * @author fjyen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.ncpafm.dao;

import java.util.List;

import javax.xml.bind.JAXBException;

import gov.noaa.nws.ncep.common.dataplugin.ncpafm.NcPafmRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

public class NcPafmDao extends PointDataPluginDao<NcPafmRecord> {

	private PointDataDescription pdd;

    
    /**
     * Creates a new ReccoDao
     * @throws PluginException 
     */
    public NcPafmDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a pafm report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public NcPafmRecord queryByDataURI(String dataURI) {
        NcPafmRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (NcPafmRecord) obs.get(0);
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

        String sql = "select datauri from awips.ncpafm where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
    
    @Override
    public String[] getKeysRequiredForFileName() {  //TODO:  See if this is correct/complete
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(NcPafmRecord p) {
        return "ncpafm.h5";  //TODO:  "s"?  or no "s"?
    }

    @Override
    public NcPafmRecord newObject() {
        return new NcPafmRecord();
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
                    .getResourceAsStream("/res/pointdata/ncpafm.xml"));
        }
        return pdd;
    }
}
