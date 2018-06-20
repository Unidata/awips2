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
package com.raytheon.viz.gfe.textformatter;

import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Executor object to get the user's selections from the formatter's
 * ValuesDialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2016  #5578     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class TextProductVarDictExecutor implements
        IPythonExecutor<FormatterScript, String> {

    private final String displayName;

    private final DataManager dataMgr;

    private final String issuedBy;

    private final DatabaseID dbID;

    public TextProductVarDictExecutor(String displayName, DataManager dataMgr,
            String issuedBy, String dbID) {
        this.displayName = displayName;
        this.dataMgr = dataMgr;
        this.issuedBy = issuedBy;
        this.dbID = new DatabaseID(dbID);
    }

    @Override
    public String execute(FormatterScript script) throws JepException {
        Map<String, Object> map = new HashMap<String, Object>(5, 1f);
        map.put("paths", GfePyIncludeUtil.getTextProductsIncludePath());
        map.put("dspName", displayName);
        map.put("dataMgr", dataMgr);
        map.put("ifpClient", dataMgr.getClient().getPythonClient());
        map.put("issuedBy", issuedBy);
        map.put("dataSource", dbID.getModelName());

        String varDict = (String) script.execute("getVarDict", map);
        return varDict;
    }
}
