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

package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import com.raytheon.edex.plugin.grib.dao.GribDao;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * The FFGGribPostProcessor is a grib post processor implementation to update
 * the dataURI for FFG grids so grids are not overwritten
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/31/10      5875        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class FFGGribPostProcessor implements IDecoderPostProcessor {

    @Override
    public GribRecord[] process(GribRecord record) throws GribException {

        try {
            GribDao gribDao = (GribDao) PluginFactory.getInstance()
                    .getPluginDao("grib");
            record.constructDataURI();
            QueryResult result = (QueryResult) gribDao
                    .executeNativeSql("select max(gridVersion) from awips.grib where datauri like '"
                            + record.getDataURI().substring(0,
                                    record.getDataURI().lastIndexOf("/"))
                            + "%'");
            int resultCount = result.getResultCount();
            if (resultCount == 1 && result.getRowColumnValue(0, 0) != null) {
                int newVersion = ((Integer) result.getRowColumnValue(0, 0)) + 1;
                record.setGridVersion(newVersion);
            }
            record.setDataURI(null);
            record.constructDataURI();
        } catch (Exception e) {
            throw new GribException("Error decoding FFG grid", e);
        }
        return new GribRecord[] { record };
    }
}
