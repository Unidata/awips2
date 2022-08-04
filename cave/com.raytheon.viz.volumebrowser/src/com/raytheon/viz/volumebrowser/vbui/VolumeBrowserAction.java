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
package com.raytheon.viz.volumebrowser.vbui;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.menus.vb.VbSourceList;

/**
 *
 * Action to bring up the Volume Browser Dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2009  #2161      lvenable     Initial creation
 * Dec 7, 2017  #6355      nabowle      Refresh toolbar menus when re-opened.
 *
 * </pre>
 *
 * @author lvenable
 */
public class VolumeBrowserAction extends AbstractHandler {

    /**
     * Volume Browser dialog.
     */
    private static VolumeBrowserDlg volumeBrowserDlg = null;

    public static VolumeBrowserDlg getVolumeBrowserDlg() {
        return volumeBrowserDlg;
    }

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        synchronized (this) {
            if (volumeBrowserDlg == null) {
                volumeBrowserDlg = new VolumeBrowserDlg(shell);
                volumeBrowserDlg.addListener(SWT.Dispose, new Listener() {
                    @Override
                    public void handleEvent(Event event) {
                        synchronized (VolumeBrowserAction.this) {
                            volumeBrowserDlg = null;
                        }
                    }
                });
                VolumeBrowserConfigObserver.getInstance().observePaths(
                        VbSourceList.VB_SOURCE_DIR,
                        LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE);
            } else {
                volumeBrowserDlg.updateToolbarMenus();
            }
        }

        volumeBrowserDlg.open();

        return null;
    }
}
