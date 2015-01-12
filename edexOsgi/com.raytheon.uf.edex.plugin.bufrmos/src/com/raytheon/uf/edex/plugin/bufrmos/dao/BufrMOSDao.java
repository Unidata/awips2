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
package com.raytheon.uf.edex.plugin.bufrmos.dao;

import java.io.InputStream;
import java.util.List;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosAvnData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosEtaData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosGfsData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosHpcData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosLampData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosMrfData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosNgmData;
import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosData.MOSType;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataDbDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Set of DAO methods for BUFR Model Output Statistics.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080219            862 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BufrMOSDao extends PointDataPluginDao<BufrMosData> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BufrMOSDao.class);

    /**
     * Creates a new BufrMOSDao object.
     * 
     * @throws PluginException
     */
    public BufrMOSDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        return dataStore;
    }

    /**
     * Retrieves an MOS report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public BufrMosData queryByDataURI(String dataURI) {
        BufrMosData report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (BufrMosData) obs.get(0);
        }
        return report;
    }

    @Override
    public String getPointDataFileName(BufrMosData p) {
        return getPointDataFilePrefix(p.getType()) + ".h5";
    }

    private String getPointDataFilePrefix(MOSType t) {
        return "bufrmos-" + t;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public BufrMosData newObject() {
        BufrMosData rval = null;
        // strip off bufrmos
        switch (MOSType.valueOf(pluginName.substring(7))) {
        case AVN:
            rval = new BufrMosAvnData();
            break;
        case ETA:
            rval = new BufrMosEtaData();
            break;
        case GFS:
            rval = new BufrMosGfsData();
            break;
        case HPC:
            rval = new BufrMosHpcData();
            break;
        case LAMP:
            rval = new BufrMosLampData();
            break;
        case MRF:
            rval = new BufrMosMrfData();
            break;
        case NGM:
            rval = new BufrMosNgmData();
            break;
        }
        return rval;
    }

    @Override
    public PointDataDbDescription getPointDataDbDescription() {
        if (dbDataDescription == null) {
            InputStream stream = this.getClass().getResourceAsStream(
                    "/res/pointdata/bufrmosdb.xml");
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
