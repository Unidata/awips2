package com.raytheon.uf.edex.plugin.fssobs;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.OrderField.ResultOrder;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Fog, Safeseas, Snow data accessor
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------------
 * May 21, 2019  7689     randerso  Added queryByRefTime
 * Feb 16, 2022  8608     mapeters  Remove populateDataStore override that matched super
 *
 * </pre>
 *
 * @author randerso
 */
public class FSSObsDAO extends PointDataPluginDao<FSSObsRecord> {
    private static final Logger logger = LoggerFactory
            .getLogger(FSSObsDAO.class);

    public FSSObsDAO(String pluginName) throws PluginException {
        super(pluginName);
    }

    public FSSObsDAO() throws PluginException {
        this("fssobs");
    }

    public FSSObsRecord queryByDataURI(String dataURI) {
        FSSObsRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            logger.error("Error querying FSSObs for dataURI: " + dataURI, e);
        }
        if (obs != null && !obs.isEmpty()) {
            report = (FSSObsRecord) obs.get(0);
        }
        return report;
    }

    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.fssobs where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    public List<?> queryByRefTime(Date oldestTime)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(FSSObsRecord.class);
        query.addQueryParam("dataTime.refTime", oldestTime,
                QueryParam.QueryOperand.GREATERTHAN);
        query.addOrder("dataTime.refTime", ResultOrder.ASC);
        return queryByCriteria(query);
    }

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {
        List<IDataRecord[]> retVal = new ArrayList<>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                try {
                    record = getDataStore((IPersistable) obj)
                            .retrieve(obj.getDataURI());
                } catch (Exception e) {
                    throw new PluginException(
                            "Error retrieving FSSObs HDF5 data", e);
                }
                retVal.add(record);
            }
        }
        return retVal;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public FSSObsRecord newObject() {
        return new FSSObsRecord();
    }

    @Override
    public String getPointDataFileName(FSSObsRecord p) {
        return "fssobs.h5";
    }

    @Override
    public PointDataDescription getPointDataDescription(
            Map<String, Object> obj) {
        if (hdf5DataDescription == null) {
            try {
                hdf5DataDescription = PointDataDescription
                        .fromStream(this.getClass().getResourceAsStream(
                                "/res/pointdata/fssobs.xml"));
            } catch (SerializationException e) {
                logger.error("Unable to load fssobs Point Data Description", e);
            }
        }
        return hdf5DataDescription;
    }

    @Override
    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass()
                    .getResourceAsStream("/res/pointdata/fssobsdb.xml");
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
