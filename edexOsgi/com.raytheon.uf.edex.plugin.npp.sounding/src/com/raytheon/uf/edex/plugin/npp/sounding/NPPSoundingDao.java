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
package com.raytheon.uf.edex.plugin.npp.sounding;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Dao for all NPP Soundings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 4, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NPPSoundingDao extends PointDataPluginDao<PluginDataObject> {

    private String[] fileNameKeys = new String[] { "dataTime.refTime" };

    private Class<? extends PluginDataObject> recordClass;

    /**
     * @param pluginName
     * @throws PluginException
     */
    public NPPSoundingDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * @param fileNameKeys
     *            the fileNameKeys to set
     */
    public void setFileNameKeys(String[] fileNameKeys) {
        this.fileNameKeys = fileNameKeys;
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
        return fileNameKeys;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.pointdata.PointDataPluginDao#newObject()
     */
    @Override
    public PluginDataObject newObject() {
        if (recordClass == null) {
            try {
                recordClass = PluginFactory.getInstance().getPluginRecordClass(
                        pluginName);
            } catch (PluginException e) {
                throw new RuntimeException(
                        "Error getting record class for plugin: " + pluginName);
            }
        }
        try {
            return recordClass.newInstance();
        } catch (Exception e) {
            throw new RuntimeException(e.getLocalizedMessage());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataFileName
     * (com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public String getPointDataFileName(PluginDataObject p) {
        return pluginName;
    }

}
