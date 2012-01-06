/**
 * H5UairDao
 * 
 * This java class defines data access object for upper air sounding 
 * data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    	Description
 * ------------ ---------- ----------- 	--------------------------
 * 4/2011				   T. Lee		Persisted to HDF5
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author T. Lee
 * @version 1.0 
 */

package gov.noaa.nws.ncep.common.dataplugin.h5uair.dao;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import javax.xml.bind.JAXBException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5UairRecord;

public class H5UairDao extends PointDataPluginDao<H5UairRecord> {

   private PointDataDescription pdd;
   
    /**
     * Creates a new ReccoDao
     * 
     * @throws PluginException
     */
    public H5UairDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves an sfcobs report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public H5UairRecord queryByDataURI(String dataURI) {
        H5UairRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (H5UairRecord) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the sfcobs table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.h5uair where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
    
    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(H5UairRecord p) {
        return "h5uairs.h5";
    }

    @Override
    public H5UairRecord newObject() {
        return new H5UairRecord();
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
                    .getResourceAsStream("/res/pointdata/h5uair.xml"));
        }
        return pdd;
    }

    
}
