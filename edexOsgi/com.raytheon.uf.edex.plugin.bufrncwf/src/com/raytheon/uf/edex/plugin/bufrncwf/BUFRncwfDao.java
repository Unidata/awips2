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
package com.raytheon.uf.edex.plugin.bufrncwf;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.bufrncwf.BUFRncwf;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * DAO for BUFRncwf
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2009            jkorman     Initial creation
 * Feb 04, 2016 5310       tgurney     Remove dead code
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRncwfDao extends PointDataPluginDao<BUFRncwf> {

    /**
     * Creates a new BUFRncwfDao
     * 
     * @throws PluginException
     */
    public BUFRncwfDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(BUFRncwf p) {
        return "bufrncwf.h5";
    }

    @Override
    public BUFRncwf newObject() {
        return new BUFRncwf();
    }
}
