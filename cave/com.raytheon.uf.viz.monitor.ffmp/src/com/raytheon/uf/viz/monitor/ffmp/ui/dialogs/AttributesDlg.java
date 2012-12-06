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
 * Dec 6, 2012            rferrel      Change to non-blocking dialog.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class AttributesDlg extends CaveSWTDialog {

    private Button rateChk;

    private Button qpeChk;

    private Button qpfChk;

    private Button guidChk;

    private Button ratioChk;

    private Button diffChk;

    private ArrayList<Button> attributeChkBtns;

    /**
     * List of QPF data radio buttons.
     */
    private List<Button> qpfRdoBtns = new ArrayList<Button>();

    /**
     * List of QPF data radio buttons.
     */
    private List<Button> ffgChkBtns = new ArrayList<Button>();

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
        attributeChkBtns = new ArrayList<Button>();

        Composite attrComp = new Composite(shell, SWT.NONE | SWT.NO_RADIO_GROUP);
        attrComp.setLayout(new GridLayout(1, false));

        rateChk = new Button(attrComp, SWT.CHECK);
        rateChk.setText("rate");
        rateChk.setSelection(attrData.isColumnVisible(COLUMN_NAME.RATE
                .getColumnName()));
        rateChk.setData(COLUMN_NAME.RATE.getColumnName());
        addCheckBoxListener(rateChk);
        attributeChkBtns.add(rateChk);

        qpeChk = new Button(attrComp, SWT.CHECK);
        qpeChk.setText("qpe");
        qpeChk.setSelection(attrData.isColumnVisible(COLUMN_NAME.QPE
                .getColumnName()));
        qpeChk.setData(COLUMN_NAME.QPE.getColumnName());
        addCheckBoxListener(qpeChk);
        attributeChkBtns.add(qpeChk);

        qpfChk = new Button(attrComp, SWT.CHECK);
        qpfChk.setText("qpf");
        qpfChk.setSelection(attrData.isColumnVisible(COLUMN_NAME.QPF
                .getColumnName()));
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

        GridData gd = new GridData();

        FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig.getInstance()
                .getTableConfigData(resource.getSiteKey());

        FFMPRunConfigurationManager runManager = FFMPRunConfigurationManager
                .getInstance();
        ProductRunXML productRun = runManager.getProduct(resource.getSiteKey());
        ArrayList<String> qpfTypes = productRun.getQpfTypes(prodXml);

        String columnName = ffmpTableCfgData.getTableColumnAttr(
                ffmpTableCfgData.getTableColumnKeys()[3])
                .getColumnNameWithSpace();
        String qpfType = columnName;

        for (String name : qpfTypes) {
            final Button qpfBtn = new Button(attrComp, SWT.RADIO);
            qpfBtn.setText(name);
            qpfBtn.setData(name);
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

                            parent.setCursor(getDisplay().getSystemCursor(
                                    SWT.CURSOR_WAIT));
                            FfmpTableConfigData ffmpTableCfgData = FfmpTableConfig
                                    .getInstance().getTableConfigData(
                                            resource.getSiteKey());
                            ffmpTableCfgData.setQpfType(qpfType,
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

            if (qpfType.startsWith(name)) {
                qpfBtn.setSelection(true);
            }
        }
        guidChk = new Button(attrComp, SWT.CHECK);
        guidChk.setText("guid");
        guidChk.setSelection(attrData.isColumnVisible(COLUMN_NAME.GUID
                .getColumnName()));
        guidChk.setData(COLUMN_NAME.GUID.getColumnName());
        addCheckBoxListener(guidChk);
        attributeChkBtns.add(guidChk);

        ratioChk = new Button(attrComp, SWT.CHECK);
        ratioChk.setText("ratio");
        ratioChk.setSelection(attrData.isColumnVisible(COLUMN_NAME.RATIO
                .getColumnName()));
        ratioChk.setData(COLUMN_NAME.RATIO.getColumnName());
        addCheckBoxListener(ratioChk);
        attributeChkBtns.add(ratioChk);

        diffChk = new Button(attrComp, SWT.CHECK);
        diffChk.setText("diff");
        diffChk.setSelection(attrData.isColumnVisible(COLUMN_NAME.DIFF
                .getColumnName()));
        diffChk.setData(COLUMN_NAME.DIFF.getColumnName());
        addCheckBoxListener(diffChk);
        attributeChkBtns.add(diffChk);

        addSeparator(attrComp);

        gd.horizontalIndent = 15;
        gd.widthHint = 140;

        String fcolumnName = ffmpTableCfgData.getTableColumnAttr(
                ffmpTableCfgData.getTableColumnKeys()[6]).getName();
        String ffgType = fcolumnName.substring(0, columnName.indexOf(" "));
        ArrayList<String> guidTypes = productRun.getGuidanceTypes(prodXml);

        for (String name : guidTypes) {
            final Button ffgBtn = new Button(attrComp, SWT.CHECK);
            ffgBtn.setText(name);
            ffgBtn.setData("GUIDSrc:" + name);
            gd.horizontalIndent = 15;
            ffgBtn.setLayoutData(gd);
            if (attrData.getGuidanceList().containsKey(name)) {
                ffgBtn.setSelection(attrData.getGuidanceList().get(name));
            }

            ffgBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    updateAction(ffgBtn);
                }
            });

            if (name.equals(ffgType)) {
                ffgBtn.setSelection(true);
            }
            ffgChkBtns.add(ffgBtn);
        }
    }

    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText(" Close ");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });
    }

    private void addCheckBoxListener(final Button checkBox) {
        checkBox.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateAction(checkBox);
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
            if (key.equalsIgnoreCase("QPF")) {
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
        }

        // Call the call back with the updated columns
        attributeDisplayCb.attributeDisplayAction(updateData, attrData);
        updateData = false;
    }
}
