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

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.svrwx.SvrWxRecord;
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
 * Jan 19, 2016  5253      tgurney      Remove dead code
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class SvrWxRecordDao extends PointDataPluginDao<SvrWxRecord> {
    /**
     * Creates a new SvrWxRecord Dao
     * 
     * @param pluginName
     * @throws PluginException
     */
    public SvrWxRecordDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(SvrWxRecord p) {
        return "svrwx.h5";
    }

    @Override
    public SvrWxRecord newObject() {
        return new SvrWxRecord();
    }
}
