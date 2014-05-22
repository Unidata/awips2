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
package com.raytheon.uf.edex.plugin.svrwx;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.svrwx.SvrWxRecord;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * SvrWxRecord Dao
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------  ---------- ----------- --------------------------
 * Jan  4, 2010            jsanchez     Initial creation
 * Apr 10, 2014  2971      skorolev     Cleaned code.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class SvrWxRecordDao extends PointDataPluginDao<SvrWxRecord> {
    /**
     * Creates a new TropicalCycloneGuidance Dao
     * 
     * @param pluginName
     * @throws PluginException
     */
    public SvrWxRecordDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Retrieves an tcg report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public SvrWxRecord queryByDataURI(String dataURI) {
        SvrWxRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (SvrWxRecord) obs.get(0);
        }
        return report;
    }

    /**
     * Queries for to determine if a given data uri exists on the tcg table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.svrwx where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getKeysRequiredForFileName
     * ()
     */
    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataFileName
     * (com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public String getPointDataFileName(SvrWxRecord p) {
        return "svrwx.h5";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.pointdata.PointDataPluginDao#newObject()
     */
    @Override
    public SvrWxRecord newObject() {
        return new SvrWxRecord();
    }
}
