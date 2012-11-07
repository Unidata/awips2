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
package com.raytheon.viz.gfe.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.UIFormat;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The copy grids dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Feb 26, 2008					Eric Babin Initial Creation
 * Oct 23, 2012 1287       rferrel     Made dialog modal like AWIPS 1.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class CopyGridsDialog extends CaveJFACEDialog {

    private Composite top;

    private org.eclipse.swt.widgets.List groupList;

    private DataManager dataManager;

    private IParmManager parmMgr;

    private boolean isSelected = false;

    public CopyGridsDialog(Shell parent, DataManager dataManager,
            boolean isSelectedType) {
        super(parent);
        this.dataManager = dataManager;
        this.parmMgr = dataManager.getParmManager();
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        this.isSelected = isSelectedType;

    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(1, false);
        top.setLayout(layout);

        initializeComponents();

        return top;
    }

    private void initializeComponents() {

        Label lab = new Label(top, SWT.NONE);
        lab.setText("Source");
        groupList = new org.eclipse.swt.widgets.List(top, SWT.BORDER
                | SWT.SINGLE | SWT.V_SCROLL);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.heightHint = groupList.getItemHeight() * 12;
        groupList.setLayoutData(data);

        // Create a UIFormat to obtain the uniform ui format
        UIFormat uiFormat = new UIFormat(parmMgr, UIFormat.FilterType.NONE,
                UIFormat.FilterType.ALL);

        for (DatabaseID source : getSources()) {
            String s = uiFormat.uiDatabaseID(source);
            groupList.add(s);
            groupList.setData(s, source);
        }

    }

    private List<DatabaseID> getSources() {
        // determine sources
        boolean filter = true;
        List<DatabaseID> sources = this.parmMgr.getAvailableDbs();
        if (filter) {
            // remove all databases with different parameters
            // make list of Parmnames, levels that are displayed in
            // the mutableDb
            List<String> mutableParms = new ArrayList<String>();
            List<DatabaseID> filtSources = new ArrayList<DatabaseID>();

            Parm[] displayedParms = this.parmMgr.getDisplayedParms();
            for (Parm parm : displayedParms) {
                if (parm.isMutable()) {
                    mutableParms.add(parm.getParmID().getCompositeName());
                }
            }

            DatabaseID mutableDb = this.parmMgr.getMutableDatabase();
            for (DatabaseID db : sources) {
                if (db.equals(mutableDb)) {
                    continue;
                }

                ParmID[] dbParms = this.parmMgr.getAvailableParms(db);
                for (ParmID dbParm : dbParms) {
                    String pid = dbParm.getCompositeName();
                    if (mutableParms.contains(pid)) {
                        filtSources.add(db);
                        break;
                    }
                }
            }
            sources = filtSources;
        }
        return sources;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        if (isSelected) {
            shell.setText("Copy Selected Grids From");
        } else {
            shell.setText("Copy All Grids From");
        }
    }

    @Override
    protected void okPressed() {
        copyGrids();
        super.okPressed();
    }

    private void copyGrids() {
        String s = groupList.getSelection()[0];
        DatabaseID model = (DatabaseID) groupList.getData(s);

        long t0 = System.currentTimeMillis();
        if (isSelected) {
            dataManager.getParmOp().copySelectedFrom(model);
        } else {
            dataManager.getParmOp().copyEverythingFrom(model);
        }
        System.out.println("copyGrids took "
                + (System.currentTimeMillis() - t0) + " ms");
    }
}
