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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.grib.GribCoverageStore;
import com.raytheon.uf.common.grib.GribModelLookup;
import com.raytheon.uf.common.grib.GridModel;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.viz.grib.wizard.GribWizardData;

/**
 * 
 * Wizard page which allows the user to select or define a grid definition for a
 * {@link GridModel}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GribModelWizardPage extends AbstractGribWizardPage {

    private final Set<String> existingNames;

    private Text nameText;

    private Text centerText;

    private Text subcenterText;

    private Button removeProcessButton;

    private Button addProcessButton;

    private List processList;

    private Text processText;

    public GribModelWizardPage(GribModelLookup modelLookup,
            GribCoverageStore coverageStore, GribWizardData data)
            throws Exception {
        super("Grib Model", data);
        setDescription("Define a new Grib Model");
        existingNames = loadExistingNames(modelLookup, coverageStore);
    }

    private static Set<String> loadExistingNames(GribModelLookup modelLookup,
            GribCoverageStore coverageStore) throws Exception {
        Set<String> existingNames = new HashSet<>();
        for (GridModel model : modelLookup.getAllModels().getModels()) {
            String name = model.getName();
            existingNames.add(name);
            if (name.contains("$")) {
                for (String grid : model.getAllGrids()) {
                    GridCoverage coverage = coverageStore
                            .getCoverageByName(grid);
                    if (coverage != null) {
                        try {
                            name = model.getTransformedName(coverage, "",
                                    coverageStore);
                            existingNames.add(name);
                        } catch (GridCoverageException e) {
                            /* It is not possible to generate a name so ignore. */
                        }
                    }
                }
            }
        }
        return existingNames;
    }

    @Override
    public void createControl(Composite parent) {
        Composite container = new Composite(parent, SWT.NONE);
        container.setLayout(new GridLayout(2, false));

        GC gc = new GC(container);
        int octetWidth = gc.textExtent("999").x;
        int twoOctetWidth = gc.textExtent("99999").x;
        gc.dispose();

        Label label = new Label(container, SWT.NONE);
        label.setText("Name");
        nameText = new Text(container, SWT.BORDER | SWT.SINGLE);
        GridData gridData = new GridData();
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        nameText.setLayoutData(gridData);

        label = new Label(container, SWT.NONE);
        label.setText("Center");
        centerText = new Text(container, SWT.BORDER | SWT.SINGLE);
        gridData = new GridData();
        gridData.widthHint = twoOctetWidth;
        centerText.setLayoutData(gridData);

        label = new Label(container, SWT.NONE);
        label.setText("Subcenter");
        subcenterText = new Text(container, SWT.BORDER | SWT.SINGLE);
        gridData = new GridData();
        gridData.widthHint = twoOctetWidth;
        subcenterText.setLayoutData(gridData);

        label = new Label(container, SWT.NONE);
        label.setText("Process");
        Group processGroup = new Group(container, SWT.NONE);
        processGroup.setLayout(new GridLayout(2, false));
        processList = new List(processGroup, SWT.BORDER);
        gridData = new GridData();
        gridData.widthHint = octetWidth;
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        processList.setLayoutData(gridData);
        removeProcessButton = new Button(processGroup, SWT.PUSH);
        removeProcessButton.setText("Remove");
        removeProcessButton.setEnabled(false);
        processText = new Text(processGroup, SWT.BORDER | SWT.SINGLE);
        gridData = new GridData();
        gridData.widthHint = octetWidth;
        processText.setLayoutData(gridData);
        addProcessButton = new Button(processGroup, SWT.PUSH);
        addProcessButton.setText("Add");
        gridData = new GridData();
        gridData.grabExcessHorizontalSpace = true;
        gridData.horizontalAlignment = SWT.FILL;
        addProcessButton.setLayoutData(gridData);

        ModifyListener listener = new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                updatePageComplete();
            }
        };

        nameText.addModifyListener(listener);
        centerText.addModifyListener(listener);
        subcenterText.addModifyListener(listener);
        processText.addModifyListener(listener);

        addProcessButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                processList.add(processText.getText());
                processText.setText("");
                addProcessButton.setEnabled(false);
            }

        });

        removeProcessButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                processList.remove(processList.getSelectionIndices());
                removeProcessButton.setEnabled(false);
                updatePageComplete();
            }

        });

        processList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                removeProcessButton.setEnabled(processList.getSelection().length > 0);
            }

        });

        setControl(container);

        populateFromData();
    }

    @Override
    public void populateFromData() {
        GridModel model = data.getModel();
        if (model == null) {
            nameText.setText("");
            centerText.setText("");
            subcenterText.setText("");
            processList.removeAll();
            return;
        }
        String name = model.getName();
        if (name != null) {
            nameText.setText(name);
        }else{
            nameText.setText("");
        }
        Integer center = model.getCenter();
        if (center != null) {
            centerText.setText(center.toString());
        }else{
            centerText.setText("");
        }
        String subcenter = model.getSubCenter();
        if (subcenter != null) {
            subcenterText.setText(subcenter);
        }else{
            subcenterText.setText("");
        }
        java.util.List<Integer> processes = model.getProcess();
        processList.removeAll();
        if (processes != null) {
            for (Integer process : processes) {
                processList.add(process.toString());
            }
        }

        updatePageComplete();
    }

    /**
     * Copy the information from the data into the UI.
     * 
     * @param force
     *            this should be false when called from listeners so that
     *            prgramatic changes to the UI
     */
    private void updatePageComplete() {
        GridModel model = new GridModel();

        setErrorMessage(null);
        setPageComplete(true);

        /* Check the process first since it needs to enable/disable buttons. */
        if (processText.getText().isEmpty()) {
            addProcessButton.setEnabled(false);
        } else {
            Integer process = parseGribValue(processText, 1);
            if (process == null) {
                addProcessButton.setEnabled(false);
                setErrorMessage("New Process must be a number between 0 and 255.");
                setPageComplete(false);
            } else if (inProcessList(process)) {
                addProcessButton.setEnabled(false);
                setErrorMessage(process + " is already in use.");
                setPageComplete(false);
            } else {
                addProcessButton.setEnabled(true);
                setErrorMessage("Must add process to proceed.");
                setPageComplete(false);
            }
        }

        if (processList.getItemCount() == 0) {
            setErrorMessage("Must add at least one process");
            setPageComplete(false);
        } else {
            ArrayList<Integer> process = new ArrayList<>(
                    processList.getItemCount());
            for (String processString : processList.getItems()) {
                process.add(parseGribValue(processString, 1));
            }
            model.setProcess(process);
        }

        if (nameText.getText().isEmpty()) {
            setErrorMessage("Name must not be blank");
            setPageComplete(false);
            return;
        } else if (existingNames.contains(nameText.getText())) {
            setErrorMessage("There is already another model named "
                    + nameText.getText());
            setPageComplete(false);
            return;
        } else {
            model.setName(nameText.getText());
        }

        Integer center = parseGribValue(centerText, 2);
        if (center == null) {
            setErrorMessage("Center must be a number between 0 and 65535.");
            setPageComplete(false);
            return;
        } else {
            model.setCenter(center.intValue());
        }

        Integer subcenter = parseGribValue(subcenterText, 2);
        if (subcenter == null) {
            setErrorMessage("Subcenter must be a number between 0 and 65535.");
            setPageComplete(false);
            return;
        } else {
            model.setSubCenter(subcenter.toString());
        }

        GridModel oldModel = data.getModel();
        /*
         * grids are set on the area page so make sure we don't lose changes by
         * modifying this page.
         */
        if (oldModel != null) {
            model.setGrids(oldModel.getGrids());
        }

        data.setModel(model);
    }

    private boolean inProcessList(Integer process) {
        String processString = process.toString();
        for (String listedProcess : processList.getItems()) {
            if (listedProcess.equals(processString)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @see #parseGribValue(String, int)
     */
    private Integer parseGribValue(Text text, int count) {
        return parseGribValue(text.getText(), count);
    }


    /**
     * Parse a value that consists of count octets. In grib bytes are called
     * octets and values are assumed unsigned so a count of 1 will limit value
     * to an unsigned byte and a count of 2 is an unsigned short. Null will be
     * returned if the number in str is too large or contains an unparseable
     * number.
     */
    private Integer parseGribValue(String str, int count) {

        Integer value = null;
        try {
            value = Integer.parseInt(str);
            if (value < 0 || value > Math.pow(255, count)) {
                value = null;
            }
        } catch (NumberFormatException ex) {
            value = null;
        }
        return value;
    }

}