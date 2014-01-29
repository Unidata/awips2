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
package com.raytheon.uf.viz.kml.export;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.kml.export.KmlExportOptions.KmlExportTimeMode;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Allow user to select options for export and starts the KmlExportJob.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 05, 2012           bsteffen    Initial creation
 * Jan 23, 2014  2703     bsteffen    Enable subclasses to add custom frame
 *                                    selection options.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlExportDialog extends CaveSWTDialog {

    protected final KmlExportOptions options;

    private Text locationText;

    private Tree productTree;

    private Button exportHiddenButton;

    private Button exportMapsButton;

    private Button shadeEarthButton;

    private Button setTimesButton;

    private Button timeSpanButton;

    private Button timeStampButton;

    private Button fillPlotsButton;

    private Button selectedFramesButton;

    private Button currentFramesButton;

    private Button allFramesButton;

    private Text framesFromText;

    private Text framesToText;

    public KmlExportDialog(Shell shell, KmlExportOptions options) {
        super(shell, SWT.RESIZE | SWT.DIALOG_TRIM);
        this.setText("Export KML");
        this.options = options;
    }

    @Override
    protected void initializeComponents(Shell shell) {

        Composite leftComposite = new Composite(shell, SWT.NONE);
        leftComposite.setLayoutData(new GridData(SWT.NONE, SWT.FILL, false,
                true));
        RowLayout layout = new RowLayout(SWT.VERTICAL);
        layout.fill = true;
        leftComposite.setLayout(layout);

        Group locationGroup = new Group(leftComposite, SWT.NONE);
        initializeLocationGroup(locationGroup);

        Group framesGroup = new Group(leftComposite, SWT.NONE);
        initializeFramesGroup(framesGroup);

        Group optionsGroup = new Group(leftComposite, SWT.NONE);
        initializeOptionsGroup(optionsGroup);

        // Group timeOptionsGroup = new Group(leftComposite, SWT.NONE);
        // initializeTimeOptionsGroup(timeOptionsGroup);

        Group productsGroup = new Group(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 250;
        productsGroup.setLayoutData(gridData);
        initializeProductsGroup(productsGroup);

        Composite buttonComposite = new Composite(shell, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.NONE, true, false, 2, 1);
        gridData.horizontalAlignment = SWT.CENTER;
        buttonComposite.setLayoutData(gridData);
        initializeButtons(buttonComposite);

        populateProductTree();
        if (productTree.getItemCount() == 0) {
            // for the intial load change this option so there is something to
            // export.
            exportMapsButton.setSelection(true);
            populateProductTree();
        }

        shell.pack();
        shell.setMinimumSize(shell.getSize());
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(2, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
    }

    protected void initializeLocationGroup(Group group) {
        group.setLayout(new GridLayout(2, false));
        group.setText("Export Location");
        locationText = new Text(group, SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 250;
        locationText.setLayoutData(gridData);
        locationText.setText(options.getKmzFileLocation().getAbsolutePath());
        Button button = new Button(group, SWT.PUSH);
        button.setText("Browse ...");
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                selectDestinationFile();
            }
        });
    }

    protected void initializeFramesGroup(Group group) {
        group.setLayout(new GridLayout(5, false));
        group.setText("Export Location");

        allFramesButton = new Button(group, SWT.RADIO);
        allFramesButton.setText("All Frames");
        GridData gridData = new GridData();
        gridData.horizontalSpan = 5;
        allFramesButton.setLayoutData(gridData);
        allFramesButton.setSelection(true);

        currentFramesButton = new Button(group, SWT.RADIO);
        currentFramesButton.setText("Current Frame");
        gridData = new GridData();
        gridData.horizontalSpan = 5;
        currentFramesButton.setLayoutData(gridData);

        selectedFramesButton = new Button(group, SWT.RADIO);
        selectedFramesButton.setText("Frames");

        new Label(group, SWT.NONE).setText("from:");
        framesFromText = new Text(group, SWT.BORDER);
        gridData = new GridData();
        gridData.widthHint = 24;
        framesFromText.setLayoutData(gridData);
        framesFromText.setEnabled(false);
        framesFromText.setText("1");
        new Label(group, SWT.NONE).setText("to:");
        framesToText = new Text(group, SWT.BORDER);
        gridData = new GridData();
        gridData.widthHint = 24;
        framesToText.setLayoutData(gridData);
        framesToText.setEnabled(false);
        int numFrames = 1;
        for (KmlPane pane : options.getPanes()) {
            int frames = pane.getDisplay().getDescriptor().getFramesInfo()
                    .getFrameCount();
            numFrames = Math.max(frames, numFrames);
        }
        framesToText.setText(Integer.toString(numFrames));
        selectedFramesButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                framesToText.setEnabled(selectedFramesButton.getSelection());
                framesFromText.setEnabled(selectedFramesButton.getSelection());
            }

        });
    }

    protected void initializeOptionsGroup(Group group) {
        group.setText("Other Options");
        group.setLayout(new RowLayout(SWT.VERTICAL));
        exportHiddenButton = new Button(group, SWT.CHECK);
        exportHiddenButton.setText("Export Hidden");
        exportHiddenButton.setSelection(true);
        exportHiddenButton
                .setToolTipText("Include hidden products in the selection of products to export.");
        exportHiddenButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                populateProductTree();
            }
        });
        exportMapsButton = new Button(group, SWT.CHECK);
        exportMapsButton.setText("Export Maps");
        exportMapsButton.setSelection(false);
        exportMapsButton
                .setToolTipText("Include maps in the selection of products to export.");
        exportMapsButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                populateProductTree();
            }
        });
        shadeEarthButton = new Button(group, SWT.CHECK);
        shadeEarthButton.setText("Shade Earth");
        shadeEarthButton.setSelection(options.isShadeEarth());
        shadeEarthButton
                .setToolTipText("Hides the Google Earth surface imagery.");
        fillPlotsButton = new Button(group, SWT.CHECK);
        fillPlotsButton.setText("Show Background Tiles");
        fillPlotsButton.setSelection(options.isFillPlotBackground());
        fillPlotsButton
                .setToolTipText("Displays an opaque background tile behind point observations");
    }

    protected void initializeTimeOptionsGroup(Group group) {
        group.setText("Time Options");
        group.setLayout(new RowLayout(SWT.VERTICAL));
        setTimesButton = new Button(group, SWT.CHECK);
        setTimesButton.setText("Set KML Time");
        setTimesButton
                .setSelection(options.getTimeMode() != KmlExportTimeMode.NONE);
        setTimesButton
                .setToolTipText("Causes Google Earth to display a time slider.");
        Composite timeComposite = new Composite(group, SWT.NONE);
        RowLayout layout = new RowLayout(SWT.VERTICAL);
        layout.marginLeft = 20;
        timeComposite.setLayout(layout);
        timeSpanButton = new Button(timeComposite, SWT.RADIO);
        timeSpanButton.setText("Time Span");
        timeSpanButton
                .setSelection(options.getTimeMode() != KmlExportTimeMode.TIME_STAMP);
        timeSpanButton.setEnabled(setTimesButton.getSelection());
        timeSpanButton
                .setToolTipText("Allow products to be visible over a time range.");
        timeStampButton = new Button(timeComposite, SWT.RADIO);
        timeStampButton.setText("Time Stamp");
        timeStampButton
                .setSelection(options.getTimeMode() == KmlExportTimeMode.TIME_STAMP);
        timeStampButton.setEnabled(setTimesButton.getSelection());
        timeStampButton
                .setToolTipText("Makes each product visible only at its exact valid time.");
        setTimesButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                timeSpanButton.setEnabled(setTimesButton.getSelection());
                timeStampButton.setEnabled(setTimesButton.getSelection());
            }

        });
    }

    protected void initializeProductsGroup(Group group) {
        group.setText("Products");
        group.setLayout(new FillLayout());
        productTree = new Tree(group, SWT.CHECK);
        productTree.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (e.detail == SWT.CHECK) {
                    treeItemChecked((TreeItem) e.item);
                }
                super.widgetSelected(e);
            }

        });
    }

    protected void initializeButtons(Composite comp) {
        comp.setLayout(new RowLayout(SWT.HORIZONTAL));
        Button okButton = new Button(comp, SWT.PUSH);
        okButton.setText("OK");
        okButton.setLayoutData(new RowData(100, SWT.DEFAULT));
        okButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                okPressed();
            }

        });

        Button cancelButton = new Button(comp, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(new RowData(100, SWT.DEFAULT));
        cancelButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });
    }

    protected void populateProductTree() {
        boolean exportMaps = exportMapsButton.getSelection();
        boolean exportHidden = exportHiddenButton.getSelection();
        if (options.isSinglePane()) {
            List<ResourcePair> rscList = options.getSinglPane().getResources(
                    exportMaps, exportHidden);
            populateProductSubTree(rscList, null);
        } else {
            int index = 0;
            for (KmlPane pane : options.getPanes()) {
                List<ResourcePair> rscList = pane.getResources(exportMaps,
                        exportHidden);
                TreeItem item = null;
                if (index < productTree.getItemCount()) {
                    item = productTree.getItem(index);
                }
                if (!rscList.isEmpty()) {
                    if (item == null || item.getData() != pane) {
                        item = new TreeItem(productTree, SWT.NONE, index);
                        item.setText("Pane " + (index + 1));
                        item.setData(pane);
                        populateProductSubTree(rscList, item);
                        item.setExpanded(true);
                    } else {
                        populateProductSubTree(rscList, item);
                    }
                    index += 1;
                } else {
                    if (item != null && item.getData() == pane) {
                        item.dispose();
                    }
                }
            }
        }
    }

    private void populateProductSubTree(List<ResourcePair> rscList,
            final TreeItem parent) {
        TreeItem[] items = parent != null ? parent.getItems() : productTree
                .getItems();
        int itemIndex = 0;
        int rscIndex = 0;
        while (itemIndex < items.length || rscIndex < rscList.size()) {
            TreeItem item = null;
            if (itemIndex < items.length) {
                item = items[itemIndex];
            }
            ResourcePair pair = null;
            if (rscIndex < rscList.size()) {
                pair = rscList.get(rscIndex);
            }
            if (item != null && item.getData() == pair) {
                itemIndex += 1;
                rscIndex += 1;
            } else if (item != null && !rscList.contains(item.getData())) {
                item.dispose();
                itemIndex += 1;
            } else {
                if (parent != null) {
                    item = new TreeItem(parent, SWT.NONE, rscIndex);
                } else {
                    item = new TreeItem(productTree, SWT.NONE, rscIndex);
                }
                String name = pair.getResource().getName();
                if (name == null) {
                    name = pair.getResource().getClass().getSimpleName();
                }
                item.setText(name);
                item.setData(pair);
                item.setChecked(true);
                treeItemChecked(item);
                rscIndex += 1;
            }
        }
    }

    private void treeItemChecked(TreeItem item) {
        for (TreeItem ti : item.getItems()) {
            ti.setChecked(item.getChecked());
        }
        TreeItem parent = item.getParentItem();
        while (parent != null) {
            parent.setChecked(true);
            for (TreeItem ti : parent.getItems()) {
                if (!ti.getChecked()) {
                    parent.setChecked(false);
                    break;
                }
            }
            parent = parent.getParentItem();
        }
    }

    protected void selectDestinationFile() {
        FileDialog fileDialog = new FileDialog(this.shell, SWT.SAVE);
        File file = new File(locationText.getText());
        fileDialog.setFileName(file.getName());
        if (file.getParentFile() != null && file.getParentFile().isDirectory()) {
            fileDialog.setFilterPath(file.getParent());
        }
        fileDialog.setFilterExtensions(new String[] { ".kmz" });
        fileDialog.open();

        String filterPath = fileDialog.getFilterPath();
        String selectedFile = fileDialog.getFileName();
        /*
         * Ensure that the user has entered a name for the file.
         */
        if (selectedFile.equalsIgnoreCase("")) {
            return;
        }

        if (!filterPath.endsWith("/")) {
            filterPath += "/";
        }
        String destinationFile = filterPath + selectedFile;
        this.locationText.setText(destinationFile);
        this.locationText.setToolTipText(destinationFile);
    }

    protected void okPressed() {
        if (allFramesButton.getSelection()) {
            options.setFirstFrameIndex(Integer.MIN_VALUE);
            options.setLastFrameIndex(Integer.MAX_VALUE);
        } else if (currentFramesButton.getSelection()) {
            for (KmlPane pane : options.getPanes()) {
                int frame = pane.getDisplay().getDescriptor().getFramesInfo()
                        .getFrameIndex();
                frame = Math.max(0, frame);
                options.setFirstFrameIndex(frame);
                options.setLastFrameIndex(frame + 1);
            }
        } else if (selectedFramesButton.getSelection()) {
            try {
                int from = Integer.parseInt(framesFromText.getText()) - 1;
                options.setFirstFrameIndex(from);
            } catch (NumberFormatException e) {
                MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR
                        | SWT.OK);
                mb.setText("Invalid Number");
                mb.setMessage(framesFromText.getText()
                        + " is not a valid number, please enter a valid number for the starting frame.");
                mb.open();
                return;
            }
            try {
                int to = Integer.parseInt(framesToText.getText());
                options.setLastFrameIndex(to);
            } catch (NumberFormatException e) {
                MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR
                        | SWT.OK);
                mb.setText("Invalid Number");
                mb.setMessage(framesFromText.getText()
                        + " is not a valid number, please enter a valid number for the ending frame.");
                mb.open();
                return;
            }
        }
        options.setKmzFileLocation(new File(locationText.getText()));
        options.setShadeEarth(shadeEarthButton.getSelection());
        // if (!setTimesButton.getSelection()) {
        // options.setTimeMode(KmlExportTimeMode.NONE);
        // } else if (timeSpanButton.getSelection()) {
        // options.setTimeMode(KmlExportTimeMode.TIME_SPAN);
        // } else if (timeStampButton.getSelection()) {
        // options.setTimeMode(KmlExportTimeMode.TIME_STAMP);
        //
        // }
        options.setFillPlotBackground(fillPlotsButton.getSelection());
        if (options.isSinglePane()) {
            List<ResourcePair> resourcesToExport = new ArrayList<ResourcePair>();
            for (TreeItem ti : productTree.getItems()) {
                if (ti.getChecked()) {
                    resourcesToExport.add((ResourcePair) ti.getData());
                }
            }
            options.getSinglPane().setResourcesToExport(resourcesToExport);
        } else {
            for (TreeItem paneitem : productTree.getItems()) {
                KmlPane pane = (KmlPane) paneitem.getData();
                List<ResourcePair> resourcesToExport = new ArrayList<ResourcePair>();
                for (TreeItem ti : paneitem.getItems()) {
                    if (ti.getChecked()) {
                        resourcesToExport.add((ResourcePair) ti.getData());
                    }
                }
                pane.setResourcesToExport(resourcesToExport);
            }
        }
        if (!validate()) {
            // clear the current selection
            for (KmlPane pane : options.getPanes()) {
                pane.setResourcesToExport(null);
            }
            return;
        }
        new KmlExportJob(options).schedule();
        close();
    }

    protected boolean validate() {
        boolean products = false;
        for (KmlPane pane : options.getPanes()) {
            if (pane.getResourcesToExport() != null
                    && !pane.getResourcesToExport().isEmpty()) {
                products = true;
                break;
            }
        }
        if (!products) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("No Products");
            mb.setMessage("No products are selected.");
            mb.open();
            return false;
        }
        if (options.getFirstFrameIndex() > options.getLastFrameIndex()
                || options.getLastFrameIndex() < 0) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Range");
            mb.setMessage("The frame range you entered is invalid, please enter a valid range");
            mb.open();
            return false;
        }
        File file = options.getKmzFileLocation();
        if (!file.getParentFile().exists()) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_QUESTION
                    | SWT.YES | SWT.NO);
            mb.setText("Create Directory");
            mb.setMessage("The directory " + file.getParent()
                    + " does not exist, would you like to create it.");
            int result = mb.open();
            if (result == SWT.YES) {
                if (!file.getParentFile().mkdirs()) {
                    mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Error Creating Directory");
                    mb.setMessage("An unspecified error has occured creating the directory, please select a new file location.");
                    mb.open();
                    return false;
                }
            } else {
                return false;
            }
        }

        if (file.exists()) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.YES | SWT.NO);
            mb.setText("Overwrite file");
            mb.setMessage("The specified file already exist. Would you like to overwrite it?");
            int result = mb.open();
            if (result == SWT.NO) {
                return false;
            }
        }
        return true;
    }

}
