/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.obs;

import java.io.InputStream;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *
 * 20071217     453        jkorman     added queryDataUriColumn method.
 * Feb 27, 2013 1638       mschenke    Moved ObStationDao to edex pointdata plugin
 * Jun 22, 2022 8865       mapeters    Remove populateDataStore override and unused methods
 * </pre>
 *
 * @author jkorman
 */
public class ObsDao extends PointDataPluginDao<MetarRecord> {

    /** The station dao */
    private ObStationDao obDao = new ObStationDao();

    /**
     * Constructs a new obs dao.
     *
     * @throws PluginException
     */
    public ObsDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    public ObStationDao getObDao() {
        return obDao;
    }

    public void setObDao(ObStationDao obDao) {
        this.obDao = obDao;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { PluginDataObject.REFTIME_ID };
    }

    @Override
    public String getPointDataFileName(MetarRecord p) {
        return "metar.h5";
    }

    @Override
    public MetarRecord newObject() {
        return new MetarRecord();
    }

    @Override
    public PointDataDescription getPointDataDescription(
            Map<String, Object> obj) {
        if (hdf5DataDescription == null) {
            try {
                hdf5DataDescription = PointDataDescription
                        .fromStream(this.getClass().getResourceAsStream(
                                "/res/pointdata/metar.xml"));
            } catch (SerializationException e) {
                logger.error("Unable to load metar Point Data Description", e);
            }
        }
        return hdf5DataDescription;
    }

    @Override
    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass()
                    .getResourceAsStream("/res/pointdata/metardb.xml");
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
