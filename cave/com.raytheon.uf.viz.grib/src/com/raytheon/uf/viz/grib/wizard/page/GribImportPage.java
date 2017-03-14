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
package com.raytheon.uf.viz.grib.wizard.page;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBException;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.grib.GribCoverageStore;
import com.raytheon.uf.common.grib.GridModel;
import com.raytheon.uf.common.grib.GridModelSet;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.grib.wizard.GribWizardData;
import com.raytheon.viz.volumebrowser.xml.VbSourceList;

/**
 * 
 * Wizard page allowing the user to select data to import into the wizard.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 26, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribImportPage extends WizardPage {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribImportPage.class);

    private final Pattern GRIB_MODEL_PATTERN = Pattern
            .compile("^GribModel:(\\d{1,5}):(\\d{1,5}):(\\d{1,3})$");

    private final GribWizardData data;

    private Button importIngestedButton;

    private Combo importIngestedCombo;

    private Button importFileRadio;

    private Text importLocationText;

    private Button importLocationButton;

    /**
     * Map of model names to the coverage. If a modelname is used for more than
     * one coverage the value will be null. This map is populated only with
     * model names like GribModel%
     */
    private final Map<String, GridCoverage> ingestedModels;

    private final GribCoverageStore coverageStore;

    public GribImportPage(GribCoverageStore coverageStore, GribWizardData data) {
        super("Save Settings");
        setTitle(getName());
        setDescription("Choose how to start the new grib model.");
        this.coverageStore = coverageStore;
        this.data = data;
        this.ingestedModels = new HashMap<>();

        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(GridInfoRecord.class);
        request.addFields(new String[] { GridInfoConstants.DATASET_ID,
                GridInfoConstants.LOCATION });
        request.addConstraint(GridInfoConstants.DATASET_ID,
                new RequestConstraint("GribModel:%", ConstraintType.LIKE));
        request.setDistinct(true);
        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            String[] datasetIds = response.getFieldObjects(
                    GridInfoConstants.DATASET_ID, String.class);
            GridCoverage[] coverages = response.getFieldObjects(
                    GridInfoConstants.LOCATION, GridCoverage.class);
            for (int i = 0; i < datasetIds.length; i += 1) {
                if (ingestedModels.containsKey(datasetIds[i])) {
                    /*
                     * null indicates multiple coverages which is assumed to be
                     * a moving model
                     */
                    ingestedModels.put(datasetIds[i], null);
                } else {
                    ingestedModels.put(datasetIds[i], coverages[i]);
                }
            }
        } catch (VizException e) {
            statusHandler.error("Unable to query ingested models", e);
        }
    }

    @Override
    public void createControl(Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(1, false));
        Button manualRadio = new Button(container, SWT.RADIO);
        manualRadio.setText("Manually input settings.");
        manualRadio.setSelection(true);

        importIngestedButton = new Button(container, SWT.RADIO);
        Point radioSize = importIngestedButton.computeSize(SWT.DEFAULT,
                SWT.DEFAULT);
        importIngestedButton.setText("Create settings for an ingested model");
        importIngestedCombo = new Combo(container, SWT.READ_ONLY);
        GridData gridData = new GridData();
        gridData.horizontalIndent = radioSize.x;
        importIngestedCombo.setLayoutData(gridData);
        String[] models = ingestedModels.keySet().toArray(new String[0]);
        Arrays.sort(models);
        importIngestedCombo.setItems(models);

        importFileRadio = new Button(container, SWT.RADIO);
        importFileRadio.setText("Import settings from an archive file.");
        importFileRadio
                .setToolTipText("Archive files can be created through this wizard.");

        Composite exportLocationComposite = new Composite(container, SWT.NONE);
        exportLocationComposite.setLayout(new GridLayout(2, false));
        gridData = new GridData();
        gridData.horizontalIndent = radioSize.x;
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        exportLocationComposite.setLayoutData(gridData);

        importLocationText = new Text(exportLocationComposite, SWT.BORDER
                | SWT.SINGLE);
        importLocationText.setToolTipText(importFileRadio.getToolTipText());
        gridData = new GridData();
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        importLocationText.setLayoutData(gridData);

        importLocationButton = new Button(exportLocationComposite, SWT.PUSH);
        importLocationButton.setText("Browse ...");
        importLocationButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                selectDestinationFile();
            }
        });

        SelectionListener selectionListener = new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updatePageComplete();
            }
        };

        ModifyListener modifyListener = new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                updatePageComplete();
            }

        };

        manualRadio.addSelectionListener(selectionListener);
        importIngestedButton.addSelectionListener(selectionListener);
        importIngestedCombo.addSelectionListener(selectionListener);
        importFileRadio.addSelectionListener(selectionListener);
        importLocationText.addModifyListener(modifyListener);

        setControl(container);

        updatePageComplete();
    }

    protected void selectDestinationFile() {
        FileDialog fileDialog = new FileDialog(getShell(), SWT.SAVE);
        File file = new File(importLocationText.getText());
        fileDialog.setFileName(file.getName());
        if (file.getParentFile() != null && file.getParentFile().isDirectory()) {
            fileDialog.setFilterPath(file.getParent());
        }
        fileDialog.setFilterExtensions(new String[] { "*.zip" });
        fileDialog.open();

        String filterPath = fileDialog.getFilterPath();
        String selectedFile = fileDialog.getFileName();
        if (selectedFile.equalsIgnoreCase("")) {
            return;
        }

        if (!filterPath.endsWith("/")) {
            filterPath += "/";
        }
        String destinationFile = filterPath + selectedFile;
        importLocationText.setText(destinationFile);
    }

    private void updatePageComplete() {
        setErrorMessage(null);

        if (importFileRadio.getSelection()) {
            importIngestedCombo.setEnabled(false);
            importLocationText.setEnabled(true);
            importLocationButton.setEnabled(true);
            String location = importLocationText.getText();
            if (location == null || location.isEmpty()) {
                setPageComplete(false);
                setErrorMessage("Please select an import file.");
                return;
            }
            Path path = Paths.get(location);
            if (!Files.exists(path)) {
                setPageComplete(false);
                setErrorMessage("Import File does not exist");
                return;
            }
            try (ZipFile zf = new ZipFile(path.toFile())) {
                Enumeration<? extends ZipEntry> entries = zf.entries();
                while (entries.hasMoreElements()) {
                    ZipEntry entry = entries.nextElement();
                    if (entry.getName().startsWith("grib/models/")) {
                        GridModelSet modelSet = JAXB.unmarshal(
                                zf.getInputStream(entry), GridModelSet.class);
                        data.setModel(modelSet.getModels().get(0));
                    } else if (entry.getName().startsWith(
                            "volumebrowser/VbSources/")) {
                        VbSourceList vblist = JAXB.unmarshal(
                                zf.getInputStream(entry), VbSourceList.class);
                        data.setVbSource(vblist.getEntries().get(0));
                    } else if (entry.getName().startsWith("grib/grids/")) {
                        SingleTypeJAXBManager<GridCoverage> gridCovJaxb = new SingleTypeJAXBManager<>(
                                GridCoverage.class);
                        GridCoverage coverage = gridCovJaxb
                                .unmarshalFromInputStream(GridCoverage.class,
                                        zf.getInputStream(entry));
                        data.setCoverage(coverage);
                    }
                }
            } catch (IOException | JAXBException | SerializationException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
                setPageComplete(false);
                setErrorMessage("Error opening archive file.");
                return;
            }

            data.setExportPath(path);
        } else if (importIngestedButton.getSelection()) {
            importLocationText.setEnabled(false);
            importLocationButton.setEnabled(false);
            if (importIngestedCombo.getItemCount() == 0) {
                setPageComplete(false);
                setErrorMessage("No unrecognized grib data in the database.");
                return;
            }
            importIngestedCombo.setEnabled(true);
            if (importIngestedCombo.getSelectionIndex() < 0) {
                setPageComplete(false);
                setErrorMessage("Select a model to configure.");
                return;
            }
            GridModel model = new GridModel();
            model.setName(importIngestedCombo.getText());
            Matcher matcher = GRIB_MODEL_PATTERN.matcher(model.getName());
            if (matcher.find()) {
                model.setCenter(Integer.parseInt(matcher.group(1)));
                model.setSubCenter(matcher.group(2));
                model.setProcess(Arrays.asList(Integer.parseInt(matcher
                        .group(3))));
            } else {
                setMessage(
                        "Cannot determine center/subcenter/process from the selected model.",
                        WizardPage.WARNING);
            }
            GridCoverage coverage = ingestedModels.get(model.getName());
            if (coverage == null) {
                /*
                 * Multiple coverages are defined, default to using same model
                 * for all coverages.
                 */
                data.setCoverage(null);
                model.setGrids(null);
            } else {
                Set<String> names = coverageStore
                        .getGribCoverageNames(coverage);
                if (names.isEmpty()) {
                    /*
                     * Coverage is not defined in coverage files, set the
                     * coverage which will prompt the user for a name and write
                     * a new coverage file.
                     */
                    data.setCoverage(coverage);
                } else {
                    /*
                     * Coverage is already defined, just set the name in he
                     * model and the area page will default to using the already
                     * defined definition.
                     */
                    data.setCoverage(null);
                    model.setGrids(Arrays.asList(names.iterator().next()));
                }
            }
            data.setModel(model);
            data.setVbSource(null);
            data.setExportPath(null);
        } else {
            importIngestedCombo.setEnabled(false);
            importLocationText.setEnabled(false);
            importLocationButton.setEnabled(false);
            GridModel model = new GridModel();
            /*
             * Setting an empty grid causes the area page to default to
             * selecting a predefined area but it will not allow the user to
             * continue until a real name is entered.
             */
            model.setGrids(Arrays.asList(""));
            data.setModel(model);
            data.setCoverage(null);
            data.setVbSource(null);
            data.setExportPath(null);
        }

        for (IWizardPage page : getWizard().getPages()) {
            if (page.getControl() == null) {
                /* During startup. */
                continue;
            }
            if (page instanceof AbstractGribWizardPage) {
                ((AbstractGribWizardPage) page).populateFromData();
            }
        }
        setPageComplete(true);
    }

}
