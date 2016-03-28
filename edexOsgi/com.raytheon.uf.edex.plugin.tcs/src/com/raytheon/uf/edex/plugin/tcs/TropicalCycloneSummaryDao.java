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
package com.raytheon.uf.edex.plugin.tcs;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * 
 * DAO for TropicalCycloneSummary (TCS)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- -----------  --------------------------
 * Nov 12, 2009            jsanchez     Initial creation
 * Jan 27, 2016       5285 tgurney      Remove dead code
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class TropicalCycloneSummaryDao extends
        PointDataPluginDao<TropicalCycloneSummary> {
    /**
     * Creates a new TropicalCycloneSummaryDao
     * 
     * @throws PluginException
     */
    public TropicalCycloneSummaryDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(TropicalCycloneSummary p) {
        return "tcs.h5";
    }

    @Override
    public TropicalCycloneSummary newObject() {
        return new TropicalCycloneSummary();
    }
}
