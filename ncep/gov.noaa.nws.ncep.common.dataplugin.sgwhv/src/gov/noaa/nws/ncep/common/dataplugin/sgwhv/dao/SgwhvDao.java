/**
 * Set of DAO methods for SGWHV data.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------ -----------	----------- --------------------------
 * 8/23/2011	   		    Chin Chen	Initial Coding (Following BufrsgwhvDao to refactor for 
 * 										saving data to HDF5)
 * </pre>
 * 
 * @author chin chen
 * @version 1.0
 **/
package gov.noaa.nws.ncep.common.dataplugin.sgwhv.dao;

import java.util.List;

import javax.xml.bind.JAXBException;

import gov.noaa.nws.ncep.common.dataplugin.sgwhv.SgwhvRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepPointDataPluginDao;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class SgwhvDao extends NcepPointDataPluginDao<SgwhvRecord>  {
	private PointDataDescription pdd;
    
    /**
     * Creates a new ReccoDao
     * @throws PluginException 
     */
    public SgwhvDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves a BUFRSGWHV report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public SgwhvRecord queryByDataURI(String dataURI) {
        SgwhvRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if((obs != null)&&(obs.size() > 0)) {
            report = (SgwhvRecord) obs.get(0);
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

        String sql = "select datauri from awips.sgwhv where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
    public PointDataDescription getPointDataDescription() throws JAXBException {
        if (pdd == null) {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/sgwhv.xml"));
        }
        return pdd;
    }
    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(SgwhvRecord p) {
        return "sgwhv.h5";
    }

    @Override
    public SgwhvRecord newObject() {
        return new SgwhvRecord();
    }
}
