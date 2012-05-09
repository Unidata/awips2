/**
 * Set of DAO methods for BUFRSSHA data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * 09/2011	   		       Chin Chen	Initial Coding (Following BufrSshaDao to refactor for 
 * 										saving data to HDF5)
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.ssha.dao;

import java.util.List;

import javax.xml.bind.JAXBException;

import gov.noaa.nws.ncep.common.dataplugin.ssha.SshaRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepPointDataPluginDao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class SshaDao extends NcepPointDataPluginDao<SshaRecord>  {
	private PointDataDescription pdd;
    
    /**
     * Creates a new BufrsshaDao
     * @throws PluginException 
     */
    public SshaDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a BUFRSSHA report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public SshaRecord queryByDataURI(String dataURI) {
        SshaRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (SshaRecord) obs.get(0);
        }
        return report;
    }
    
    /**
     * Queries for to determine if a given data uri exists on the Bufrssha table.import gov.noaa.nws.ncep.common.dataplugin.sgwh.SgwhRecord;

     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     * element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.ssha where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
    public PointDataDescription getPointDataDescription() throws JAXBException {
        if (pdd == null) {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/ssha.xml"));
        }
        return pdd;
    }
    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(SshaRecord p) {
        return "ssha.h5";
    }

    @Override
    public SshaRecord newObject() {
        return new SshaRecord();
    }
}
