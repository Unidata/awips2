package com.raytheon.viz.hydrobase.dialogs;

import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.texteditor.TextEditorDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
/**
 * This class contains ASCII specific functionality to display the Areal Definitions dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Mar 11, 2020 19533      mgamazaychikov Initial creation
 *                                        Extracted ASCII specific functionality from ArealDefinitionsDlg
 * </pre>
 *
 * @author mgamazaychikov
 *
 */
public class ArealDefinitionsDlgASCII extends AbstractArealDefinitionsDlg {

    private final IUFStatusHandler statusHandler = UFStatus.getHandler(ArealDefinitionsDlgASCII.class);

    private static final String AREAL_IMPORT_TYPE = "ASCII";

    private static final String[] GEOAREA_ASCII_FILENAMES = { "zones.dat", "counties.dat", "basins.dat", "resvrs.dat" };

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     */
    public ArealDefinitionsDlgASCII(Shell parent) {
        super(parent);
        setText("Areal Definitions for ASCII file source");
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents() {
        createListComboControl();
        createAreaListControl();
        createImportGroup();
        createOkButton();

        try {
            loadAreaList(ArealTypeSelection.ZONES);
            loadAreaImport(ArealTypeSelection.ZONES);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error updating Geo Area List", e);
        }
    }

    /**
     * Create the List combo box control.
     */
    protected void createListComboControl() {
        Composite comboComp = new Composite(shell, SWT.NONE);
        comboComp.setLayout(new GridLayout(2, false));

        Label listLbl = new Label(comboComp, SWT.NONE);
        listLbl.setText("List:");

        listCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (ArealTypeSelection type : ArealTypeSelection.values()) {
            String s = type.toDisplayString();
            listCbo.add(type.toDisplayString());
            listCbo.setData(s, type);
        }
        selectedType = ArealTypeSelection.ZONES;
        listCbo.select(selectedType.ordinal());

        listCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                super.widgetSelected(e);
                handleTypeSelection();
            }

        });
    }



    /**
     * Create the Import group and controls.
     */
    protected void createImportGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group importGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 25;
        importGroup.setLayout(gl);
        importGroup.setLayoutData(gd);
        importGroup.setText(" Import Operations ");

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label sourceDataLbl = new Label(importGroup, SWT.NONE);
        sourceDataLbl.setText("Source Data File:");
        sourceDataLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        sourceDataFileTF = new Text(importGroup, SWT.BORDER);
        sourceDataFileTF.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        Button importBtn = new Button(importGroup, SWT.PUSH);
        importBtn.setText("Import to Database");
        importBtn.setLayoutData(gd);
        importBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleImport();
            }
        });

        gd = new GridData(180, SWT.DEFAULT);
        Button reviewLogBtn = new Button(importGroup, SWT.PUSH);
        reviewLogBtn.setText("Review Log");
        reviewLogBtn.setLayoutData(gd);
        reviewLogBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadGeoAreaLog();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button editFileBtn = new Button(importGroup, SWT.PUSH);
        editFileBtn.setText("Edit File");
        editFileBtn.setLayoutData(gd);
        editFileBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String fileName = getAreaFilename();
                if (fileName != null) {
                    File f = new File(fileName);
                    TextEditorDlg teDlg = textEditorDlgMap.get(f);
                    if (teDlg == null || teDlg.isDisposed()) {
                        teDlg = new TextEditorDlg(shell, false, f);
                        teDlg.addCloseCallback(new ICloseCallback() {

                            @Override
                            public void dialogClosed(Object returnValue) {
                                if (returnValue instanceof File) {
                                    File f = (File) returnValue;
                                    textEditorDlgMap.remove(f);
                                }
                            }
                        });
                        teDlg.open();
                        textEditorDlgMap.put(f, teDlg);
                    } else {
                        teDlg.bringToTop();
                    }
                }
            }
        });
    }

    /**
     * Load the import filename for the GeoArea info.
     *
     * @param type
     *            The ArealTypeSelection
     */
    @Override
    protected void loadAreaImport(ArealTypeSelection type) {
        sourceDataFileTF.setText(GEOAREA_ASCII_FILENAMES[type.ordinal()]);
    }

    @Override
    protected boolean isDefaultMatch(String filename) {
        /* check if the name is the default for this data set */
        if (filename.equals(selectedType.getDataName())) {
            return true;
        }
        return false;
    }

    @Override
    protected IUFStatusHandler getStatusHandler() {
        return statusHandler;
    }

    @Override
    protected String getArealImportType() {
        return AREAL_IMPORT_TYPE;
    }

    @Override
    protected void handleImportOK(String importFile) {
        try {
            importGeoArea();
            loadAreaList(selectedType);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error importing Geo Data for file:  " + importFile, e);
        }
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        // TODO Auto-generated method stub

    }

    @Override
    protected void addObserver() {
        // TODO Auto-generated method stub

    }

    @Override
    protected void removeObserver() {
        // TODO Auto-generated method stub

    }
}
