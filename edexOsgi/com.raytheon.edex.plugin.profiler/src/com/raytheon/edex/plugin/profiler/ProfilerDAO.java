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
package com.raytheon.edex.plugin.profiler;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerObs;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Provide data access services against the ProfilerObs data object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  969      jkorman     Initial implementation.
 * Dec 03, 2013  2537     bsteffen    Move to edex plugin.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ProfilerDAO extends PointDataPluginDao<ProfilerObs> {

    /**
     * Creates a new BufrMOSDao object.
     * 
     * @throws PluginException
     */
    public ProfilerDAO(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(ProfilerObs p) {
        return "profiler.h5";
    }

    @Override
    public ProfilerObs newObject() {
        return new ProfilerObs();
    }

}
