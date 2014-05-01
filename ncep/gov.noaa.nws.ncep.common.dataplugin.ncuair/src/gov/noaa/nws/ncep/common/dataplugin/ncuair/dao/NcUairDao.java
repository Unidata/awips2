/**
 * NcUairDao
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
 * 09/2011      457        S. Gurung    Renamed H5 to Nc and h5 to nc
 * 09/2011                 Chin Chen    support batch decoding methods for better performance and
 * 											remove xml serialization as well * 
 * 10/2011                 S. Gurung    Added method to get dbDataDescription file ncuairdb.xml
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author T. Lee
 * @version 1.0 
 */

package gov.noaa.nws.ncep.common.dataplugin.ncuair.dao;

import gov.noaa.nws.ncep.common.dataplugin.ncuair.NcUairRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepPointDataPluginDao;

import java.io.InputStream;
import java.util.List;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;

public class NcUairDao extends NcepPointDataPluginDao<NcUairRecord> {

    private PointDataDescription pdd;

    /**
     * Creates a new ReccoDao
     * 
     * @throws PluginException
     */
    public NcUairDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves an sfcobs report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public NcUairRecord queryByDataURI(String dataURI) {
        NcUairRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (NcUairRecord) obs.get(0);
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

        String sql = "select datauri from awips.ncuair where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(NcUairRecord p) {
        return "ncuairs.h5";
    }

    @Override
    public NcUairRecord newObject() {
        return new NcUairRecord();
    }

    /*
     * @Override public String[] getParameters(File file) throws
     * StorageException, FileNotFoundException { try { // This should be faster
     * than hitting the datastore. return
     * getPointDataDescription().getParameterNames(); } catch (Exception e) { //
     * let super handle it return super.getParameters(file); } }
     */

    public PointDataDescription getPointDataDescription()
            throws SerializationException {
        if (pdd == null) {
            pdd = PointDataDescription.fromStream(this.getClass()
                    .getResourceAsStream("/res/pointdata/ncuair.xml"));
        }
        return pdd;
    }

    @Override
    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass().getResourceAsStream(
                    "/res/pointdata/ncuairdb.xml");
            if (stream != null) {
                try {
                    dbDataDescription = PointDataDbDescription
                            .fromStream(stream);
                } catch (JAXBException e) {
                    logger.error("Unable to load " + pluginName
                            + " Point Data Database Description", e);
                }
            }
        }
        return dbDataDescription;
    }

}
