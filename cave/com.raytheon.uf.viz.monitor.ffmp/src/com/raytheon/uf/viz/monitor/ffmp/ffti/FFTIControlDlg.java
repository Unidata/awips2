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
package com.raytheon.uf.viz.monitor.ffmp.ffti;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFTIDataManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.FFTISettingXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Flash Flood Threat Indicator display dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Dec 5, 2012  1353      rferrel      Convert to CaveSWTDialog and made non-blocking.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class FFTIControlDlg extends CaveSWTDialog {

    /**
     * THe CWAs monitor selection buttons.
     */
    private Map<String, Button> cwa_list;

    /**
     * Adds setting tab items to the FFTI tab folder.
     */
    private Button addSettingBtn;

    /**
     * Remove setting tab items from the FFTI tab folder.
     */
    private Button removeSettingBtn;

    /**
     * The FFTI tab folder.
     */
    private TabFolder fftiTabFolder;

    /**
     * Setting tab item names.
     */
    private final String[] tabTitles = new String[] { "Setting 1", "Setting 2",
            "Setting 3", "Setting 4", "Setting 5" };

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTIControlDlg.class);

    /**
     * Constructor to set up non-blocking dialog.
     * 
     * @param parent
     */
    public FFTIControlDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("Flash Flood Threat Indicator (FFTI) Control");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Set up dialog components.
     */
    private void initializeComponents() {
        try {
            createCwaControls();

            addSeparator(shell);

            createTabFolderControls();

            createSettingTabFolder();

            createBottomButtons();

            createSettingTabs();

            updateTabSettings();

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "FFTI Can not load...", e);
        }
    }

    /**
     * Set up selection buttons for the CWAs monitors.
     */
    private void createCwaControls() {
        GridLayout gl = new GridLayout(10, false);
        Composite cwaComp = new Composite(shell, SWT.NONE);
        cwaComp.setLayout(gl);

        Label cwaLbl = new Label(cwaComp, SWT.NONE);
        cwaLbl.setText("CWAs Monitored: ");
        cwa_list = new HashMap<String, Button>();

        String siteName = LocalizationManager.getInstance().getCurrentSite()
                .toUpperCase();
        // load a runner by finding the primary domain
        FFMPRunConfigurationManager frcm = FFMPRunConfigurationManager
                .getInstance();
        FFMPRunXML runner = frcm.getRunner(siteName);

        if (runner.getBackupDomains() != null) {
            for (DomainXML domain : runner.getBackupDomains()) {
                Button cwa_bt = new Button(cwaComp, SWT.CHECK);
                cwa_bt.setText(domain.getCwa());
                cwa_list.put(domain.getCwa(), cwa_bt);
            }
        }

        Button cwa_bt = new Button(cwaComp, SWT.CHECK);
        cwa_bt.setText(runner.getPrimaryDomain().getCwa());
        cwa_list.put(runner.getPrimaryDomain().getCwa(), cwa_bt);

        cwa_list.get(runner.getPrimaryDomain().getCwa()).setSelection(true);

        FFTIDataManager fftiDataMgr = FFTIDataManager.getInstance();

        // initialize the CWAs
        ArrayList<String> cwaList = fftiDataMgr.getCwaList();
        if ((cwaList != null) && (cwaList.size() > 0)) {
            for (String cwa : cwaList) {
                if (cwa_list.get(cwa) != null) {
                    cwa_list.get(cwa).setSelection(true);
                }
            }
        } else {
            // cwa_list.get(CWAs.get(0)).setSelection(true);
        }
    }

    /**
     * Buttons to add remove tab items from the FFTI tab.
     */
    private void createTabFolderControls() {
        Composite tabControlComp = new Composite(shell, SWT.NONE);
        tabControlComp.setLayout(new GridLayout(2, false));

        int buttonWidth = 120;

        GridData gd = new GridData(buttonWidth, SWT.DEFAULT);
        addSettingBtn = new Button(tabControlComp, SWT.PUSH);
        addSettingBtn.setText("Add Setting");
        addSettingBtn.setLayoutData(gd);
        addSettingBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                addSettingTab();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        removeSettingBtn = new Button(tabControlComp, SWT.PUSH);
        removeSettingBtn.setText("Remove Setting");
        removeSettingBtn.setLayoutData(gd);
        removeSettingBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                removeSettingTab();
            }
        });
    }

    /**
     * Layout the FFTI tab folder.
     */
    private void createSettingTabFolder() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        fftiTabFolder = new TabFolder(shell, SWT.NONE);
        fftiTabFolder.setLayoutData(gd);

        fftiTabFolder.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateTabSettings();
            }
        });
    }

    /**
     * The bottom save and close buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 140;

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button saveSettingsBtn = new Button(buttonComp, SWT.PUSH);
        saveSettingsBtn.setText("Save All Settings");
        saveSettingsBtn.setLayoutData(gd);
        saveSettingsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveAllSettings();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Get the duplicated settings.
     * 
     * @return duplicate list
     */
    private Set<Integer> getDuplicates() {
        Set<Integer> duplicateLst = new HashSet<Integer>();
        for (int i = 0; i < fftiTabFolder.getItemCount(); i++) {
            SettingComp thisItem = (SettingComp) fftiTabFolder.getItem(i)
                    .getControl();
            for (int j = (i + 1); j < fftiTabFolder.getItemCount(); j++) {
                SettingComp nextItem = (SettingComp) fftiTabFolder.getItem(j)
                        .getControl();
                if (thisItem.getSelectedAttribType().endsWith(
                        nextItem.getSelectedAttribType())
                        && (thisItem.getYellowThreshold() == nextItem
                                .getYellowThreshold())
                        && (thisItem.getRedThreshold() == nextItem
                                .getRedThreshold())
                        && (thisItem.getQpeDurHr() == nextItem.getQpeDurHr())
                        && (thisItem.getGuidDurHr() == nextItem.getGuidDurHr())
                        && (thisItem.getQpfDurHr() == nextItem.getQpfDurHr())
                        && (thisItem.getTotalDurHr() == nextItem
                                .getTotalDurHr())
                        && (thisItem.getQpeSrc().length > 0
                                && nextItem.getQpeSrc().length > 0 && thisItem
                                .getQpeSrc()[0].equals(nextItem.getQpeSrc()[0]))
                        && (thisItem.getQpfSrc().length > 0
                                && nextItem.getQpfSrc().length > 0 && thisItem
                                .getQpfSrc()[0].equals(nextItem.getQpfSrc()[0]))
                        && (thisItem.getGuidSrc().length > 0
                                && nextItem.getGuidSrc().length > 0 && thisItem
                                .getGuidSrc()[0]
                                .equals(nextItem.getGuidSrc()[0]))) {

                    duplicateLst.add(i + 1);
                    duplicateLst.add(j + 1);
                }
            }
        }

        return duplicateLst;
    }

    /**
     * Save the current settings to the FFTIDataManger.
     */
    protected void saveAllSettings() {
        // Find duplicates
        Set<Integer> duplicates = getDuplicates();
        if (duplicates.size() > 0) {
            StringBuilder message = new StringBuilder("Sets ");
            String separator = "";
            for (int setIndex : duplicates) {
                message.append(separator).append(setIndex);
                separator = "/";
            }
            message.append(" are duplicated!\nModify the configuration before saving.");
            MessageBox messageBox = new MessageBox(shell, SWT.OK);
            messageBox.setText("Warning: Duplicate Setting(s)!");
            messageBox.setMessage(message.toString());
            messageBox.open();
            return;
        }

        FFTIDataManager fdm = FFTIDataManager.getInstance();

        // Clear out old settings
        fdm.clear();

        // get the selected CWA list
        List<String> selectedCwas = getCWASelectionIDs();

        fdm.setCwaList((ArrayList<String>) selectedCwas);

        // get attribute, and sources for each tab
        TabItem[] tabItemArray = fftiTabFolder.getItems();
        List<FFTISettingXML> settings = new ArrayList<FFTISettingXML>();

        for (TabItem ti : tabItemArray) {
            SettingComp sc = (SettingComp) ti.getControl();
            FFTISettingXML aSetting = new FFTISettingXML();

            // get attributes: name yellow threshold, red threshold
            aSetting.getAttribute()
                    .setAttributeName(sc.getSelectedAttribType());
            aSetting.getAttribute().setYellowThrshld(sc.getYellowThreshold());
            aSetting.getAttribute().setRedThrshld(sc.getRedThreshold());

            // get precipitation source + value
            if (sc.getqpeToggle().getToggleState()) {
                for (String src : sc.getQpeSrc()) {
                    aSetting.getQpeSource().addDisplayName(src);
                }
                aSetting.getQpeSource().setDurationHour(sc.getQpeDurHr());
            }

            if (sc.getGuidToggle().getToggleState()) {
                for (String src : sc.getGuidSrc()) {
                    aSetting.getGuidSource().addDisplayName(src);
                }
                aSetting.getGuidSource().setDurationHour(sc.getGuidDurHr());
            }

            if (sc.getQpfToggle().getToggleState()) {
                for (String src : sc.getQpfSrc()) {
                    aSetting.getQpfSource().addDisplayName(src);
                }
                aSetting.getQpfSource().setDurationHour(sc.getQpfDurHr());
            }

            settings.add(aSetting);
        }

        fdm.setSettings((ArrayList<FFTISettingXML>) settings);
        fdm.saveConfigXml();
    }

    /**
     * Add a new setting tab in the FFTI tab folder.
     */
    private void createSettingTabs() {
        FFTIDataManager fftiDataMgr = FFTIDataManager.getInstance();

        if (fftiDataMgr.getSettingList().size() == 0) {
            TabItem settingTab = new TabItem(fftiTabFolder, SWT.NONE);
            settingTab.setText(tabTitles[0]);
            settingTab.setControl(new SettingComp(fftiTabFolder));
        } else {
            for (int i = 0; i < fftiDataMgr.getSettingList().size(); i++) {
                FFTISettingXML fftiSetting = fftiDataMgr.getSettingList()
                        .get(i);
                TabItem settingTab = new TabItem(fftiTabFolder, SWT.NONE);
                settingTab.setText(tabTitles[i]);
                SettingComp sc = new SettingComp(fftiTabFolder, fftiSetting);
                settingTab.setControl(sc);
            }
        }
    }

    /**
     * Place horizontal separator in the component.
     * 
     * @param parentComp
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Add a new settings tab.
     */
    private void addSettingTab() {
        // Add tab
        TabItem settingTab = new TabItem(fftiTabFolder, SWT.NONE);
        settingTab.setText(tabTitles[fftiTabFolder.getItemCount() - 1]);
        settingTab.setControl(new SettingComp(fftiTabFolder));

        updateTabSettings();

        fftiTabFolder.setSelection(settingTab);
        removeSettingBtn.setEnabled(true);
    }

    /**
     * Dispose of the control in the selected tab and then dispose of the tab.
     */
    private void removeSettingTab() {
        int selectedTab = fftiTabFolder.getSelectionIndex();
        fftiTabFolder.getItem(selectedTab).getControl().dispose();
        fftiTabFolder.getItem(selectedTab).dispose();

        renameTabs();
        updateTabSettings();

        fftiTabFolder.setSelection(0);
        removeSettingBtn.setEnabled(false);
    }

    /**
     * Update the enable status of tab control buttons.
     */
    private void updateTabSettings() {
        if (fftiTabFolder.getItemCount() > 1) {
            if (fftiTabFolder.getSelectionIndex() == 0) {
                removeSettingBtn.setEnabled(false);
            } else {
                removeSettingBtn.setEnabled(true);
            }
        } else {
            removeSettingBtn.setEnabled(false);
        }

        if (fftiTabFolder.getItemCount() < 5) {
            addSettingBtn.setEnabled(true);
        } else {
            addSettingBtn.setEnabled(false);
        }
    }

    /**
     * Adjust the names of the FFTI tab folder items.
     */
    private void renameTabs() {
        TabItem[] tabItemArray = fftiTabFolder.getItems();

        for (int i = 0; i < tabItemArray.length; i++) {
            tabItemArray[i].setText(tabTitles[i]);
        }
    }

    /**
     * Get CWA names whose buttons are selected.
     * 
     * @return cwaIds
     */
    public List<String> getCWASelectionIDs() {
        List<String> cwaIds = new ArrayList<String>();
        for (Object id : cwa_list.keySet().toArray()) {
            Button cwa_bt = cwa_list.get(id);
            if (cwa_bt.getSelection()) {
                cwaIds.add(id.toString());
            }
        }

        return cwaIds;
    }
}
