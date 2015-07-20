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

import java.util.Map;
import java.util.concurrent.Semaphore;

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
 * Jun 25, 2013  16065     ryu         Adding outerLevel attribute
 * Jul 17, 2015  4575      njensen     Changed varDict from String to Map
 *
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartToolRequest extends QueueJobRequest<Object> {

    private Parm inputParm;

    private Map<String, Object> varDict;

    private PreviewInfo preview;

    private Semaphore completedSemaphore;

    private Object result;

    private boolean outerLevel;

    public SmartToolRequest() {
        super();

        completedSemaphore = new Semaphore(1);
        try {
            completedSemaphore.acquire();
        } catch (InterruptedException e) {
            // don't care
        }
    }

    public Parm getInputParm() {
        return inputParm;
    }

    public void setInputParm(Parm inputParm) {
        this.inputParm = inputParm;
    }

    public Map<String, Object> getVarDict() {
        return varDict;
    }

    public void setVarDict(Map<String, Object> varDict) {
        this.varDict = varDict;
    }

    public PreviewInfo getPreview() {
        return preview;
    }

    public void setPreview(PreviewInfo preview) {
        this.preview = preview;
    }

    public boolean getOuterLevel() {
        return outerLevel;
    }

    public void setOuterLevel(boolean outerLevel) {
        this.outerLevel = outerLevel;
    }

    public void requestComplete(Object result) {
        this.result = result;
        completedSemaphore.release();
    }

    public Object getResult() {
        try {
            completedSemaphore.acquire();
            return result;
        } catch (InterruptedException e) {
            return e;
        } finally {
            completedSemaphore.release();

        }
    }
}
