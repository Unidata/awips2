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
package com.raytheon.viz.gfe.smarttool.script;

import com.raytheon.uf.viz.core.jobs.QueueJobRequest;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smarttool.PreviewInfo;

/**
 * Request to run a smart tool off the UI thread
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolRequest extends QueueJobRequest<Object> {

    private Parm inputParm;

    private String varDict;

    private SmartToolFinishedListener listener;

    private PreviewInfo preview;

    public Parm getInputParm() {
        return inputParm;
    }

    public void setInputParm(Parm inputParm) {
        this.inputParm = inputParm;
    }

    public String getVarDict() {
        return varDict;
    }

    public void setVarDict(String varDict) {
        this.varDict = varDict;
    }

    public SmartToolFinishedListener getListener() {
        return listener;
    }

    public void setListener(SmartToolFinishedListener listener) {
        this.listener = listener;
    }

    public PreviewInfo getPreview() {
        return preview;
    }

    public void setPreview(PreviewInfo preview) {
        this.preview = preview;
    }

}
