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

package com.raytheon.viz.mpe.ui.actions;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 05 , 2015 #14246     lbousaidi     Initial creation
 * 
 * </pre>
 * 
 * @author lbousaidi
 * @version 1.0
 */

public class TransmitBestEstimateQPE extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart editor = EditorUtil.getActiveEditor();
        IDisplayPane pane = null;
        if (editor instanceof IMultiPaneEditor) {
            IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
            if (multiPane.getNumberofPanes() > 1
                    && multiPane.displayedPaneCount() > 1) {
                pane = multiPane.getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
            } else {
                pane = ((IDisplayPaneContainer) editor).getDisplayPanes()[0];
            }
        }

        MPEDisplayManager dm = MPEDisplayManager.getInstance(pane);
        Date currentDate = dm.getCurrentEditDate();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHH");
        String transmitDate = formatter.format(currentDate);

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String scriptDir = appsDefaults.getToken("pproc_bin");

        String scriptName = "transmit_rfc_qpe";
        ProcessBuilder pb = new ProcessBuilder(scriptDir + "/" + scriptName,
                transmitDate);
        try {
            pb.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
        // TODO Auto-generated method stub
        return null;
    }

}
