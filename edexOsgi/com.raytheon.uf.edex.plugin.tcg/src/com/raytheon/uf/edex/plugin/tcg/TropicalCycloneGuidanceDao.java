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
package com.raytheon.uf.edex.plugin.tcg;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.tcg.TropicalCycloneGuidance;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * 
 * DAO for TropicalCycloneGuidance (TCG)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * Oct 28, 2009            jsanchez     Initial creation
 * Jan 28, 2016       5286 tgurney      Remove dead code
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class TropicalCycloneGuidanceDao extends
        PointDataPluginDao<TropicalCycloneGuidance> {

    /**
     * Creates a new TropicalCycloneGuidanceDao
     * 
     * @throws PluginException
     */
    public TropicalCycloneGuidanceDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(TropicalCycloneGuidance p) {
        return "tcg.h5";
    }

    @Override
    public TropicalCycloneGuidance newObject() {
        return new TropicalCycloneGuidance();
    }

}
