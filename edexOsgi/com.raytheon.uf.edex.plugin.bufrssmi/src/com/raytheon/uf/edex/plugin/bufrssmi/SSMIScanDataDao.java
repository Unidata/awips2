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
package com.raytheon.uf.edex.plugin.bufrssmi;

import java.util.List;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.bufrssmi.SSMIScanData;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0 
 */
public class SSMIScanDataDao extends PointDataPluginDao<SSMIScanData> {

    /**
     * Creates a new BufrMOSDao object.
     * 
     * @throws PluginException
     */
    public SSMIScanDataDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves an MOS report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public SSMIScanData queryByDataURI(String dataURI) {
        SSMIScanData report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (SSMIScanData) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the profiler
     * table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.bufrssmi where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(SSMIScanData p) {
        return "ssmi.h5";
    }

    @Override
    public SSMIScanData newObject() {
        return new SSMIScanData();
    }
}
