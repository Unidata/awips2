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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

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
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 *
 * Dialog to change the time limits for CELL, DMD, MESO, and/or TVS.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Nov 22, 2010           lvenable  Initial creation
 * Jul 24, 2013  2143     skorolev  Changes for non-blocking dialogs.
 * Aug 15, 2013  2143     mpduff    Remove resize.
 * Oct 17, 2013  2361     njensen   Use JAXBManager for XML
 * Jul 30, 2018  6685     randerso  Code cleanup.
 * Jan 31, 2019  7655     tgurney   Support radar-specific configuration
 *
 * </pre>
 *
 * @author lvenable
 */
public class SCANAlarmTimeLimitDlg extends CaveSWTDialog
        implements ICommonDialogAction {

    private static final SingleTypeJAXBManager<ScanAlarmXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(ScanAlarmXML.class);

    private static final String xmlFilePath = LocalizationUtil.join("scan",
            "ScanAlarms.xml");

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private Map<ScanTables, Spinner> spinners = new HashMap<>();

    private ScanAlarmXML dataXml;

    private String scanSite;

    private boolean isDmd;

    public SCANAlarmTimeLimitDlg(Shell parentShell, ScanTables scanTable,
            String site) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK);
        setText("Alarm Time Limit for: " + site);
        this.scanSite = site.toLowerCase();
        this.isDmd = scanTable == ScanTables.DMD;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 10;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {

        readXmlFile();

        // Initialize all of the controls and layouts
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.marginHeight = 20;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, false, false));

        if (isDmd) {
            createControlRow(controlComp, ScanTables.DMD);
        } else {
            createControlRow(controlComp, ScanTables.CELL);
            createControlRow(controlComp, ScanTables.MESO);
            createControlRow(controlComp, ScanTables.TVS);
        }

        for (ScanTables tableType : ScanTables.values()) {
            setSpinnerValueFromXml(tableType);
        }

        createBottomButtons();
    }

    private void setSpinnerValueFromXml(ScanTables tableType) {
        Spinner spinner = spinners.get(tableType);
        if (spinner != null) {
            if (spinner.isEnabled()) {
                spinner.setSelection(dataXml.getAlarmTime(tableType, scanSite));
            } else {
                spinner.setSelection(dataXml.getDefaultAlarmTime(tableType));
            }
        }
    }

    /** Create spinner, check box and labels for a single table type */
    private void createControlRow(Composite controlComp, ScanTables tableType) {
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label lbl = new Label(controlComp, SWT.RIGHT);
        lbl.setText(tableType + ":");
        lbl.setLayoutData(gd);

        gd = new GridData();
        Spinner spinner = new Spinner(controlComp, SWT.BORDER);
        spinner.setMinimum(0);
        spinner.setMaximum(ScanAlarmXML.MAX_ALARM_TIME);
        spinner.setLayoutData(gd);
        spinners.put(tableType, spinner);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label minLbl = new Label(controlComp, SWT.NONE);
        minLbl.setText("min   ");
        minLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        Button defaultChk = new Button(controlComp, SWT.CHECK);
        defaultChk.setText("Use Default");
        defaultChk.setLayoutData(gd);
        defaultChk.setSelection(
                !dataXml.hasSiteSpecificValue(tableType, scanSite));
        spinner.setEnabled(!defaultChk.getSelection());
        defaultChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                spinner.setEnabled(!spinner.isEnabled());
                setSpinnerValueFromXml(tableType);
            }
        });

        setSpinnerValueFromXml(tableType);
    }

    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(
                new GridData(SWT.CENTER, SWT.DEFAULT, true, false));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveXmlFile();
                close();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getSize());
    }

    @Override
    public void closeDialog() {
        close();
    }

    private void readXmlFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        ILocalizationFile lf = pm.getStaticLocalizationFile(
                LocalizationType.COMMON_STATIC, xmlFilePath);

        try (InputStream is = lf.openInputStream()) {
            dataXml = jaxb.unmarshalFromInputStream(is);
        } catch (LocalizationException | IOException
                | SerializationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading Scan Alarms from " + lf, e);
            close();
        }
    }

    private void saveXmlFile() {
        for (Entry<ScanTables, Spinner> e : spinners.entrySet()) {
            ScanTables tableType = e.getKey();
            Spinner s = e.getValue();
            if (s.isEnabled()) {
                dataXml.setAlarmTime(tableType, scanSite, s.getSelection());
            } else {
                /*
                 * Delete the value from config so SCAN will fall back to the
                 * default alarm time for this table type
                 */
                dataXml.setAlarmTime(tableType, scanSite, null);
            }
        }

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        ILocalizationFile lf = pm.getLocalizationFile(context, xmlFilePath);

        try (SaveableOutputStream out = lf.openOutputStream()) {
            jaxb.marshalToStream(dataXml, out);
            out.save();
        } catch (IOException | LocalizationException
                | SerializationException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error saving Scan Alarms to " + lf, e);
        }
    }

}
