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
package com.raytheon.viz.aviation.utility;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.model.ForecastModel;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Backup dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 28 FEB 2008  938         lvenable    Initial creation.
 * 5/6/2009     1982        grichard    Permit selection of multiple products.
 * 5/11/2009    1982        grichard    Added backup/restart monitor feature.
 * 7/9/2010     5078        rferrel     Added File Not Found catch
 *                                      in initializeComponents.
 * 10/06/2010   6009        rferrel     Use product to get initial selected item.
 * 3/14/2011    8588        rferrel     Allow selection of multiple products.
 * 20121010     1229        jkorman     Added DO_NOT_BLOCK so dialog does not block on open.     
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class BackupDialog extends CaveSWTDialog {

    /**
     * List control containing backup sites.
     */
    private List backupList;

    /**
     * Current products displayed in the monitor.
     */
    private java.util.List<String> productDisplayList;

    private Composite mainComp;

    // private TafSiteConfig config;

    /**
     * Constructor.
     * 
     * @param parent
     *            - Parent shell.
     * @param product
     *            - Current product whose sites are displayed in the monitor
     */
    public BackupDialog(Shell parent, java.util.List<String> productDisplayList) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Backup");
        this.productDisplayList = productDisplayList;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        GridLayout gl = new GridLayout(1, true);
        gl.marginHeight = 3;
        gl.marginWidth = 3;
        // gl.verticalSpacing = 3;
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);

        // Initialize all of the controls and layouts
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        configMgr.setDefaultColors(mainComp);

        createListControl(configMgr);
        createBottomButtons(configMgr);
    }

    /**
     * Create the backup list control.
     */
    private void createListControl(ResourceConfigMgr configMgr) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label selectProdLbl = new Label(mainComp, SWT.CENTER);
        selectProdLbl.setText("Select Product(s)");
        selectProdLbl.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(selectProdLbl);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 250;
        gd.widthHint = 250;
        backupList = new List(mainComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        backupList.setLayoutData(gd);

        configMgr.setDefaultColors(backupList);
        configMgr.setListBoxFont(backupList);

        try {
            java.util.List<String> productList = TafSiteConfigFactory
                    .getInstance().getProductList();
            if (productList != null) {
                for (String s : productList) {
                    backupList.add(s.toString());
                }
            }
        } catch (ConfigurationException e) {
            // Leave backupList empty.
        } catch (FileNotFoundException e) {
            // Leave backupList empty.
        }

        if (backupList.getItemCount() > 0) {
            // Highlight the current display products.
            java.util.List<Integer> indicesList = new ArrayList<Integer>();
            for (String product : productDisplayList) {
                int index = backupList.indexOf(product);
                if (index >= 0) {
                    indicesList.add(index);
                }
            }
            // It's possible for the products to be deleted so it is no longer
            // on
            // the list. Just select the top of the list which should be the
            // current
            // default product.
            if (indicesList.size() == 0) {
                backupList.select(0);
            } else {
                int[] indices = new int[indicesList.size()];
                for (int i = 0; i < indices.length; ++i) {
                    indices[i] = indicesList.get(i);
                }
                backupList.select(indices);
            }

        }
    }

    /**
     * Create the OK and Close buttons.
     */
    private void createBottomButtons(ResourceConfigMgr configMgr) {
        Composite buttonArea = new Composite(mainComp, SWT.NONE);
        buttonArea.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        buttonArea.setLayout(new GridLayout(1, false));
        configMgr.setDefaultColors(buttonArea);

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));
        configMgr.setDefaultColors(buttons);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(buttons, SWT.PUSH);
        configMgr.setDefaultFontAndColors(okBtn, "Close", gd);
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (backupList.getSelectionCount() > 0) {
                    int[] selectionIndices = backupList.getSelectionIndices();
                    Arrays.sort(selectionIndices);
                    java.util.List<String> productList = new ArrayList<String>();
                    for (int i : selectionIndices) {
                        productList.add(backupList.getItem(i));
                    }

                    // int index = backupList.getSelectionIndex();
                    IBackupRestart brs = ForecastModel.getInstance()
                            .getBackupRestartUtility();

                    try {
                        brs.backupTafMonitor(productList, TafSiteConfigFactory
                                .getInstance().getAllProducts());
                    } catch (ConfigurationException e) {
                    } catch (FileNotFoundException e) {
                    }
                    setReturnValue(true);
                    shell.dispose();
                }
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttons, SWT.PUSH);
        configMgr.setDefaultFontAndColors(closeBtn, "Close", gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                shell.dispose();
            }
        });
    }
}
