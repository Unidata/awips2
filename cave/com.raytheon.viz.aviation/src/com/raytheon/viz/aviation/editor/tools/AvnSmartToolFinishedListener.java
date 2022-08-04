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
package com.raytheon.viz.aviation.editor.tools;

import java.util.concurrent.atomic.AtomicBoolean;

import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2010            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class AvnSmartToolFinishedListener implements
        IRequestCompleteListener<Object> {

    private AtomicBoolean done = new AtomicBoolean(false);

    private String result;

    @Override
    public void requestComplete(Object result) {
        if (result != null) {
            this.result = String.valueOf(result);
        } else {
            this.result = null;
        }
        done.set(true);
    }

    public boolean isDone() {
        return done.get();
    }

    public String getResult() {
        return result;
    }
}
