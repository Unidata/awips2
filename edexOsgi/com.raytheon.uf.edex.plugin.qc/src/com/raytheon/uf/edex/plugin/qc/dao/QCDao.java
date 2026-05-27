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

package com.raytheon.uf.edex.plugin.qc.dao;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.qc.QCRecord;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.plugin.qc.internal.QCPaths;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Data access object for retrieving QC mesonet data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/04/2009   3408       bphillip    Initial creation
 * Aug 15, 2014 3530       bclement    removed commons logging
 * Apr 16, 2015 4259       njensen     Removed unreachable catch
 * Jan 04, 2018 6861       njensen     Removed python usage, removed method overrides
 *                                      so it mostly resembles a normal pointdata plugin
 * 
 * </pre>
 * 
 * @author bphillip
 */
public class QCDao extends PointDataPluginDao<QCRecord> {

    /** Map of plugin names to point data descriptions */
    private static Map<String, PointDataDescription> pdds = new HashMap<>();

    static {
        pdds = QCPaths.getPointDataDescriptions();
    }

    /**
     * Constructs a new QC data access object
     * 
     * @param pluginName
     *            "qc"
     * @throws PluginException
     *             If errors occur while constructing the data access object
     */
    public QCDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "qcType", "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(QCRecord p) {
        return "qc.h5";
    }

    @Override
    public QCRecord newObject() {
        return new QCRecord();
    }

    @Override
    public PointDataDescription getPointDataDescription(
            Map<String, Object> obj) {
        return pdds.get(obj.get("qcType").toString());
    }

}
