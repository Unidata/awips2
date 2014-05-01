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

package com.raytheon.edex.uengine.tasks.decode;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * FileIn task derived from original uEngine FileIn task. Reads a file in from
 * the data store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class FileIn extends ScriptTask {

    private PluginDataObject dataRecord;

    private PluginDao dao;

    /**
     * Constructor
     * 
     * @param aPlugin
     *            the plugin
     * @param aDataRecord
     *            the data record to read in
     */
    public FileIn(String aPlugin, PluginDataObject aDataRecord) {
        dataRecord = aDataRecord;
        try {
            dao = PluginFactory.getInstance().getPluginDao(
                    dataRecord.getPluginName());
        } catch (PluginException e) {
            logger.error("Unable to get " + dataRecord.getPluginName() + " dao");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() throws PluginException {
        return dao.getHDF5Data(dataRecord, -1)[0];
    }

    public Object[] retrieveGroup() throws PluginException {
        return dao.getHDF5Data(dataRecord, -1);
    }

}
