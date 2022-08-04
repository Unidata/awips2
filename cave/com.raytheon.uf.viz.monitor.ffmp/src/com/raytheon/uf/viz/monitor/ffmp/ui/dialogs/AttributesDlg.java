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

package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpTableConfigData.COLUMN_NAME;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display FFMP Basin Table Attributes.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Dec 06, 2012            rferrel     Change to non-blocking dialog.
 * Oct 21, 2015 4821       dhladky     Fixed bad ffgType subString and width.
 * Mar 16, 2016 5463       dhladky     Fixed config loading and button matching.
 * Jul 30, 2018 6720       njensen     Update for changed method names
 *                                     Stopped reusing GridData
 * Nov 26, 2018 DR 11861   mfontaine  FFMP use of QPF in Basin Table
 *
 * </pre>
 *
 * @author rferrel
 */
public class AttributesDlg extends CaveSWTDialog {

    private Button qpfChk;

    /**
     * List of QPF data radio buttons.
     */
    private List<Button> qpfRdoBtns = new ArrayList<>();

    /**
     * List of FFG data radio buttons.
     */
    private List<Button> ffgChkBtns = new ArrayList<>();

    private IAttributeDisplay attributeDisplayCb;

    private FFMPResource resource;

    private boolean updateData = false;

    private AttributesDlgData attrData;

    private Shell parent = null;

    public AttributesDlg(Shell parent, FFMPResource resource,
            AttributesDlgData attrData, IAttributeDisplay attributeDisplayCb) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.parent = parent;
        this.resource = resource;
        setText("Attributes");
        this.attributeDisplayCb = attributeDisplayCb;
        this.attrData = attrData;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createAttributeControls();
        addSeparator(shell);
        createBottomButtons();
    }

    private void createAttributeControls() {
        List<Button> attributeChkBtns = new ArrayList<>();

        Composite attrComp = new Composite(shell,
                SWT.NONE | SWT.NO_RADIO_GROUP);
        attrComp.setLayout(new GridLayout(1, false));

        SelectionAdapter chkBoxAdapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateAction((Button) e.widget);
            }
        };

        Button rateChk = new Button(attrComp, SWT.CHECK);
        rateChk.setText("rate");
        rateChk.setSelection(
                attrData.isColumnVisible(COLUMN_NAME.RATE.getColumnName()));
        rateChk.setData(COLUMN_NAME.RATE.getColumnName());
        rateChk.addSelectionListener(chkBoxAdapter);
        attributeChkBtns.add(rateChk);

        Button qpeChk = new Button(attrComp, SWT.CHECK);
        qpeChk.setText("qpe");
        qpeChk.setSelection(
                attrData.isColumnVisible(COLUMN_NAME.QPE.getColumnName()));
        qpeChk.setData(COLUMN_NAME.QPE.getColumnName());
        qpeChk.addSelectionListener(chkBoxAdapter);
        attributeChkBtns.add(qpeChk);

        qpfChk = new Button(attrComp, SWT.CHECK);
        qpfChk.setText("qpf");
        qpfChk.setSelection(
                attrData.isColumnVisible(COLUMN_NAME.QPF.getColumnName()));
        qpfChk.setData(COLUMN_NAME.QPF.getColumnName());
        attributeChkBtns.add(qpfChk);
        qpfChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                for (Button btn : qpfRdoBtns) {
                    btn.setEnabled(qpfChk.getSelection());
                }
                updateAction(qpfChk);
            }
        });

        FFMPMonitor monitor = FFMPMonitor.getInstance();
        ProductXML prodXml = monitor.getProductXML(resource.getPrimarySource());

        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(resource.getSiteKey());

        FFMPRunConfigurationManager runManager = FFMPRunConfigurationManager
                .getInstance();
        ProductRunXML productRun = runManager.getProduct(resource.getSiteKey());
        List<String> qpfDisplayNames = productRun.getQpfDisplayNames(prodXml);
        String qpfType = ffmpTableCfgData.getQpfDisplayName();

        for (String qpfName : qpfDisplayNames) {
            final Button qpfBtn = new Button(attrComp, SWT.RADIO);
            qpfBtn.setText(qpfName);
            qpfBtn.setData(qpfName);
            GridData gd = new GridData();
            gd.horizontalIndent = 15;
            qpfBtn.setLayoutData(gd);
            qpfBtn.setEnabled(qpfChk.getSelection());
            qpfRdoBtns.add(qpfBtn);
            qpfBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    String qpfType = ((Button) e.getSource()).getText();
                    for (int i = 0; i < qpfRdoBtns.size(); i++) {
                        Button rdo = qpfRdoBtns.get(i);
                        if (rdo.getText().equals(qpfType)) {

                            parent.setCursor(getDisplay()
                                    .getSystemCursor(SWT.CURSOR_WAIT));
                            FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig
                                    .getInstance()
                                    .getTableConfigData(resource.getSiteKey());
                            ffmpTableCfgData.setQpfDisplayName(qpfType,
                                    resource.getSiteKey());
                            attrData.setQpfType(qpfType);
                            rdo.setSelection(true);
                        } else {
                            rdo.setSelection(false);
                        }
                    }
                    updateData = true;
                    updateAction(qpfChk);
                }
            });

            if (qpfType.startsWith(qpfName)) {
                qpfBtn.setSelection(true);
                updateAction(qpfBtn);
            }
        }
        Button guidChk = new Button(attrComp, SWT.CHECK);
        guidChk.setText("guid");
        guidChk.setSelection(
                attrData.isColumnVisible(COLUMN_NAME.GUID.getColumnName()));
        guidChk.setData(COLUMN_NAME.GUID.getColumnName());
        guidChk.addSelectionListener(chkBoxAdapter);
        attributeChkBtns.add(guidChk);

        Button ratioChk = new Button(attrComp, SWT.CHECK);
        ratioChk.setText("ratio");
        ratioChk.setSelection(
                attrData.isColumnVisible(COLUMN_NAME.RATIO.getColumnName()));
        ratioChk.setData(COLUMN_NAME.RATIO.getColumnName());
        ratioChk.addSelectionListener(chkBoxAdapter);
        attributeChkBtns.add(ratioChk);

        Button diffChk = new Button(attrComp, SWT.CHECK);
        diffChk.setText("diff");
        diffChk.setSelection(
                attrData.isColumnVisible(COLUMN_NAME.DIFF.getColumnName()));
        diffChk.setData(COLUMN_NAME.DIFF.getColumnName());
        diffChk.addSelectionListener(chkBoxAdapter);
        attributeChkBtns.add(diffChk);

        addSeparator(attrComp);

        List<String> guidDisplayNames = productRun
                .getGuidanceDisplayNames(prodXml);

        for (String name : guidDisplayNames) {
            final Button ffgBtn = new Button(attrComp, SWT.CHECK);
            ffgBtn.setText(name);
            ffgBtn.setData("GUIDSrc:" + name);
            GridData gd = new GridData();
            gd.horizontalIndent = 15;
            gd.widthHint = 180;
            ffgBtn.setLayoutData(gd);
            ffgBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    updateAction(ffgBtn);
                }
            });

            // default selection(s) based on config
            if (attrData.getGuidanceList().containsKey(name)) {
                ffgBtn.setSelection(true);
                updateAction(ffgBtn);
            }

            ffgChkBtns.add(ffgBtn);
        }
    }

    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText(" Close ");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    private void updateAction(Button chk) {
        String key = null;
        String guidSrc = null;
        String data = (String) chk.getData();


        if (data.contains(":")) {
            String[] parts = data.split(":");
            guidSrc = parts[1];
            key = parts[0];
        } else {
            key = data;
            attrData.setColumnVisible(key, chk.getSelection());
            if ("QPF".equalsIgnoreCase(key)) {
                String qpfType = "xxxxxx";
                for (Button button : qpfRdoBtns) {
                    if (button.getSelection()) {
                        qpfType = button.getText();
                        // split window requires redraw on change
                        updateData = true;
                        break;
                    }
                }
                attrData.setQpfType(qpfType);
            }
        }

        // update included guid sources if needed
        if (guidSrc != null) {
            HashMap<String, Boolean> guidMap = attrData.getGuidanceList();
            if (chk.getSelection()) {
                guidMap.put(guidSrc, chk.getSelection());
            } else {
                guidMap.remove(guidSrc);
            }

            attrData.setGuidanceMap(guidMap);
            updateData = true;
        }

        // Call the call back with the updated columns
        attributeDisplayCb.attributeDisplayAction(updateData, attrData);
    }
}
