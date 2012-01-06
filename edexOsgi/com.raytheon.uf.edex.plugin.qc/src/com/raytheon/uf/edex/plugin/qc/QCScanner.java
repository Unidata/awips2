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

package com.raytheon.uf.edex.plugin.qc;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.qc.internal.QCPaths;
import com.raytheon.uf.common.python.thread.PythonScriptManager;
import com.raytheon.uf.common.python.thread.PythonScriptManager.ScriptRequest;

public class QCScanner {
    private static PythonScriptManager psm;

    static {
        try {
            psm = new PythonScriptManager(
                    QCPaths.getPythonScriptPath("qcScanner.py"),
                    QCPaths.PYTHON_INCLUDE_PATH, 2);
        } catch (JepException e) {
            throw new RuntimeException(
                    "Failed to initialize QCScanner Python scripting.  QCScanner python calls will Fail!");
        }
    }

    private Integer maxRecordsInChunk;

    private IQCScannerTarget target;

    public QCScanner() {

    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    private PluginDataObject[] scan() throws Exception, JepException {
        // TODO: need to prevent multiple threads from accessing...
        List result = new ArrayList();

        Map<String, File> paths = QCPaths.getPaths();

        for (String key : paths.keySet()) {
            HashMap<String, Object> args = new HashMap<String, Object>();
            File qcDataFile = paths.get(key);
            args.put("directory", qcDataFile.getPath());
            args.put("qcType", key);

            psm.callScript(new ScriptRequest("init", args));
            args = null;
            if (maxRecordsInChunk != null) {
                args = new HashMap<String, Object>();
                args.put("max_records", maxRecordsInChunk);
            }
            result.addAll((List) psm
                    .callScript(new ScriptRequest("scan", args)));
        }

        return (PluginDataObject[]) result.toArray(new PluginDataObject[result
                .size()]);
    }

    public void scanInChunks() throws Exception, JepException {
        PluginDataObject[] chunk;
        while (true) {
            chunk = scan();
            if (chunk != null && chunk.length > 0)
                target.acceptRecords(chunk);
            else
                break;
        }
    }

    /**
     * @return the maxRecordsInChunk
     */
    public Integer getMaxRecordsInChunk() {
        return maxRecordsInChunk;
    }

    /**
     * @param maxRecordsInChunk
     *            the maxRecordsInChunk to set
     */
    public void setMaxRecordsInChunk(Integer maxRecordsInChunk) {
        this.maxRecordsInChunk = maxRecordsInChunk;
    }

    /**
     * @return the target
     */
    public IQCScannerTarget getTarget() {
        return target;
    }

    /**
     * @param target
     *            the target to set
     */
    public void setTarget(IQCScannerTarget target) {
        this.target = target;
    }

}
