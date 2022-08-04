package com.raytheon.viz.hydrobase.dialogs;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.jface.dialogs.MessageDialog;
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

import com.raytheon.uf.common.hydro.areal.ArealDataImportNotification;
import com.raytheon.uf.common.hydro.areal.ArealShapefile;
import com.raytheon.uf.common.hydro.areal.ArealShapefileXML;
import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;

/**
 * This class contains shapefile specific functionality to display the Areal Definitions dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Mar 11, 2020 19533      mgamazaychikov Initial creation
 * </pre>
 *
 * @author mgamazaychikov
 *
 */
public class ArealDefinitionsDlgSHP extends AbstractArealDefinitionsDlg {

    private final IUFStatusHandler statusHandler = UFStatus.getHandler(ArealDefinitionsDlgSHP.class);

    private static int FIRST = 0;

    private static final String AREAL_IMPORT_TYPE = "SHP";

    private static final String AREAL_SHAPEFILE_SELECTION_FILE = "hydro" + IPathManager.SEPARATOR
            + "arealShapefileSelection.xml";

    private ArealShapefileXML arealShapefileSelections;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     */
    public ArealDefinitionsDlgSHP(Shell parent) {
        super(parent);
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
        loadArealShapefileSelectionList();
        createListComboControl();
        createAreaListControl();
        createImportGroup();
        createOkButton();
        try {
            ArealTypeSelection first = getArealTypeSelection(FIRST);
            loadAreaList(first);
            loadAreaImport(first);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error updating Geo Area List", e);
        }
    }

    private ArealTypeSelection getArealTypeSelection(int idx) {
        ArealTypeSelection retType = null;
        for (ArealShapefile ashp : arealShapefileSelections.getArealShapefileList()) {
            for (ArealTypeSelection type : ArealTypeSelection.values()) {
                if (ashp.getName().equals(type.name())) {
                    return type;
                }
            }
        }
        return retType;
    }

    protected void createListComboControl() {
        Composite comboComp = new Composite(shell, SWT.NONE);
        comboComp.setLayout(new GridLayout(2, false));

        Label listLbl = new Label(comboComp, SWT.NONE);
        listLbl.setText("List:");

        listCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);

        for (ArealShapefile ashp : arealShapefileSelections.getArealShapefileList()) {
            for (ArealTypeSelection type : ArealTypeSelection.values()) {
                if (ashp.getName().equals(type.name())) {
                    String s = type.toDisplayString();
                    listCbo.add(type.toDisplayString());
                    listCbo.setData(s, type);
                }
            }
        }
        selectedType = ArealTypeSelection.fromOrdinal(FIRST);
        listCbo.select(selectedType.ordinal());

        listCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                super.widgetSelected(e);
                handleTypeSelection();
            }

        });
    }
    private void loadArealShapefileSelectionList() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File file = pm.getFile(lc, AREAL_SHAPEFILE_SELECTION_FILE);
        lc = pm.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        file = pm.getFile(lc, AREAL_SHAPEFILE_SELECTION_FILE);
        if (file == null || !file.exists()) {
            lc = pm.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            file = pm.getFile(lc, AREAL_SHAPEFILE_SELECTION_FILE);
        }
        if (file != null) {
            try {
                statusHandler.info("Loaded Areal Shapefile Selection file [" + AREAL_SHAPEFILE_SELECTION_FILE + "]");
                JAXBContext context = JAXBContext.newInstance(ArealShapefileXML.class);
                Unmarshaller un = context.createUnmarshaller();
                arealShapefileSelections = (ArealShapefileXML) un.unmarshal(file);
            } catch (JAXBException e1) {
                statusHandler.info(
                        "Problem reading Areal Shapefile Selection file [" + AREAL_SHAPEFILE_SELECTION_FILE + "]");
            }
        } else {
            MessageDialog.openError(getShell(), "File Not Found", AREAL_SHAPEFILE_SELECTION_FILE + " file not found.");
        }
    }

    /**
     * Create the Import group and controls.
     */
    @Override
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
    }

    private String getSHPFilename(ArealTypeSelection type) {
        for (ArealShapefile sel : arealShapefileSelections.getArealShapefileList()) {
            if (sel.getName().equalsIgnoreCase(type.toDisplayString())) {
                return sel.getFilename() + "." + AREAL_IMPORT_TYPE.toLowerCase();
            }
        }
        return null;
    }

    /**
     * Load the import filename for the GeoArea info.
     *
     * @param type
     *            The ArealTypeSelection
     */
    @Override
    protected void loadAreaImport(ArealTypeSelection type) {
        sourceDataFileTF.setText(getSHPFilename(type));
    }

    @Override
    protected boolean isDefaultMatch(String filename) {
        String selectedFilename = null;
        for (ArealShapefile sel : arealShapefileSelections.getArealShapefileList()) {
            if (sel.getName().equalsIgnoreCase(selectedType.toDisplayString())) {
                selectedFilename = sel.getFilename();
            }
        }
        if (filename.equals(selectedFilename)) {
            return true;
        }
        return false;
    }

    @Override
    protected IUFStatusHandler getStatusHandler() {
        return UFStatus.getHandler(ArealDefinitionsDlgSHP.class);
    }

    @Override
    protected String getArealImportType() {
        return AREAL_IMPORT_TYPE;
    }

    @Override
    protected void handleImportOK(String importFile) {
        areaList.removeAll();
        importGeoArea();
        addObserver();
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage message : messages) {

            try {
                Object payload = message.getMessagePayload();
                if (payload instanceof ArealDataImportNotification) {
                    System.out.println("ArealDataImportNotification received");
                    ArealDataImportNotification notification = (ArealDataImportNotification) payload;
                    removeObserver();
                    if (notification.isSuccess()) {
                        getDisplay().asyncExec(new Runnable() {
                            public void run() {
                                try {
                                    if (!shell.isDisposed()) {
                                        loadAreaList(selectedType);
                                    }
                                } catch (VizException e) {
                                    statusHandler.handle(Priority.PROBLEM, "Could not load AreaList", e);
                                }
                            }
                        });
                    }
                } else {
                    statusHandler.handle(Priority.EVENTA,
                            "Areal Data Import request completed unsuccessfully, check the log file.");
                }
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM, "Could not parse message from server", e);
            }
        }
    }

    @Override
    protected void addObserver() {
        NotificationManagerJob.addObserver(IMPORT_NOTIFICATION_URI, this);
    }

    @Override
    protected void removeObserver() {
        NotificationManagerJob.removeObserver(IMPORT_NOTIFICATION_URI, this);
    }

}
