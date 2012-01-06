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

package com.raytheon.viz.volumebrowser.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.volumebrowser.Activator;
import com.raytheon.viz.volumebrowser.catalog.GridDataCatalog;
import com.raytheon.viz.volumebrowser.catalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.catalog.ObservationDataCatalog;
import com.raytheon.viz.volumebrowser.catalog.RadarDataCatalog;
import com.raytheon.viz.volumebrowser.catalog.RedbookDataCatalog;
import com.raytheon.viz.volumebrowser.catalog.SatelliteDataCatalog;

/**
 * Volume browser dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 11/22/06                 brockwoo    Initial Creation.
 * 12/08/06     103         brockwoo    Removed Spinner control and replaced with text boxes
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
public class VolumeBrowserDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(VolumeBrowserDialog.class);

    private Shell parentShell = null;

    private String title;

    private Composite top = null;

    private Composite dataSourceSelector = null;

    private Composite parameters = null;

    private Label parameter1 = null;

    private Label parameter2 = null;

    private Label parameter3 = null;

    private ListViewer lParameter1 = null;

    private ListViewer lParameter2 = null;

    private ListViewer lParameter3 = null;

    private Button addButton = null;

    private List currentList = null;

    private List currentTimes = null;

    private Combo currentProductType = null;

    private Combo currentProductColormap = null;

    private Text lowerValue = null;

    private Text upperValue = null;

    private Text interval = null;

    private ArrayList<AbstractRequestableResourceData> productEntries;

    private String currentProduct;

    private Map<String, String> editorConfig = new HashMap<String, String>();

    private IDataCatalog currentDataCatalog = null;

    private Label wxSelectorLabel = null;

    // private String colormap = "";

    public VolumeBrowserDialog(Shell parShell, String dialogTitle)
            throws VizException {
        super(parShell);
        parentShell = parShell;
        this.title = dialogTitle;
        productEntries = new ArrayList<AbstractRequestableResourceData>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.OK_ID) {

        }
        super.buttonPressed(buttonId);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        createDataTypeSelector();
        createParameterFields();
        createAddButton();
        createCatalogArea();
        createProductDefinition();
        return top;
    }

    /**
     * Returns the defined layers to be displayed by CAVE.
     * 
     * @return An array list of layer properties which can be used to create the
     *         proper EDEX scripts
     */
    public ArrayList<AbstractRequestableResourceData> getLayers() {
        return productEntries;
    }

    private void createDataTypeSelector() {
        dataSourceSelector = new Composite(top, SWT.NONE);
        dataSourceSelector.setLayout(new GridLayout(2, false));
        wxSelectorLabel = new Label(dataSourceSelector, SWT.NONE);
        wxSelectorLabel.setBounds(new Rectangle(10, 10, 200, 16));
        wxSelectorLabel.setText("Data Source:");

        ComboViewer testCombo = new ComboViewer(dataSourceSelector,
                SWT.READ_ONLY);

        testCombo.setContentProvider(new IStructuredContentProvider() {
            public void dispose() {
            }

            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }

            public Object[] getElements(Object inputElement) {
                return (IDataCatalog[]) inputElement;
            }
        });
        testCombo.setLabelProvider(new LabelProvider() {
            @Override
            public Image getImage(Object element) {
                return null;
            }

            @Override
            public String getText(Object element) {
                return ((IDataCatalog) element).getDataType();
            }
        });
        testCombo.addSelectionChangedListener(new ISelectionChangedListener() {
            public void selectionChanged(SelectionChangedEvent event) {
                StructuredSelection test = (StructuredSelection) event
                        .getSelection();
                IDataCatalog info = (IDataCatalog) test.getFirstElement();
                if (currentDataCatalog != null) {
                    currentDataCatalog.dispose();
                }
                currentProductType.select(0);
                currentProduct = info.getDataType();
                currentDataCatalog = info;
                currentDataCatalog.initDataCatalog(parameter1, parameter2,
                        parameter3, lParameter1, lParameter2, lParameter3,
                        addButton);
                parameters.layout();
            }
        });
        IDataCatalog[] testRadar = new IDataCatalog[5];
        testRadar[0] = new GridDataCatalog();
        testRadar[1] = new ObservationDataCatalog();
        testRadar[2] = new RadarDataCatalog();
        testRadar[3] = new SatelliteDataCatalog();
        testRadar[4] = new RedbookDataCatalog();
        testCombo.setInput(testRadar);
        testCombo.refresh();
    }

    private void createParameterFields() {
        parameters = new Composite(top, SWT.NONE);
        parameters.setLayout(new GridLayout(3, true));
        parameter1 = new Label(parameters, SWT.NONE);
        parameter1.setBounds(new Rectangle(10, 10, 200, 16));
        parameter1.setText("Data Provider 1");
        parameter2 = new Label(parameters, SWT.NONE);
        parameter2.setBounds(new Rectangle(10, 10, 200, 16));
        parameter2.setText("Data Provider 2");
        parameter3 = new Label(parameters, SWT.NONE);
        parameter3.setBounds(new Rectangle(10, 10, 200, 16));
        parameter3.setText("Data Provider 3");

        lParameter1 = new ListViewer(parameters, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);
        lParameter1.getList().setLayoutData(
                new GridData(IDataCatalog.THREEPANEWIDTH,
                        IDataCatalog.DEFAULTPANEHEIGHT));

        lParameter2 = new ListViewer(parameters, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);
        lParameter2.getList().setLayoutData(
                new GridData(IDataCatalog.THREEPANEWIDTH,
                        IDataCatalog.DEFAULTPANEHEIGHT));

        lParameter3 = new ListViewer(parameters, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);
        lParameter3.getList().setLayoutData(
                new GridData(IDataCatalog.THREEPANEWIDTH,
                        IDataCatalog.DEFAULTPANEHEIGHT));
    }

    private void createAddButton() {
        Composite addButtonComposite = new Composite(top, SWT.NONE);
        GridLayout positionRight = new GridLayout(1, true);
        positionRight.marginLeft = 675;
        addButtonComposite.setLayout(positionRight);
        addButton = new Button(addButtonComposite, SWT.PUSH);
        addButton.setText("Add Product");
        addButton.setEnabled(false);
        addButton.addMouseListener(new MouseListener() {

            public void mouseDoubleClick(MouseEvent e) {
            }

            public void mouseDown(MouseEvent e) {
            }

            public void mouseUp(MouseEvent e) {
                // productEntries = new ArrayList<LayerProperty>();
                // currentList.removeAll();
                int selected = currentList.getItemCount();
                AbstractRequestableResourceData rd = currentDataCatalog
                        .getResourceData();

                productEntries.add(selected, rd);
                // productEntries.get(selected).setColorMap(colormap);
                StringBuffer displayName = new StringBuffer();
                displayName.append(currentProduct + " - ");
                Set<String> parameterKeys = rd.getMetadataMap().keySet();
                for (String key : parameterKeys) {
                    displayName.append(rd.getMetadataMap().get(key)
                            .getConstraintValue()
                            + " ");
                }
                currentList.add(displayName.toString());

                // Set the editor type for the specified data
                setEditorType(currentDataCatalog.getEditorConfig());
            }
        });
    }

    private void createCatalogArea() {
        Composite catalogArea = new Composite(top, SWT.NONE);
        catalogArea.setLayout(new GridLayout(2, false));

        Label catalogLabel = new Label(catalogArea, SWT.NONE);
        catalogLabel.setBounds(new Rectangle(10, 10, 200, 16));
        catalogLabel.setText("Selected Product");
        Label timeLabel = new Label(catalogArea, SWT.NONE);
        timeLabel.setBounds(new Rectangle(10, 10, 200, 16));
        timeLabel.setText("Product Times");

        currentList = new List(catalogArea, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);
        currentList.setLayoutData(new GridData(500, 150));
        currentList.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub

            }

            public void widgetSelected(SelectionEvent e) {
                AbstractRequestableResourceData thisEntry = productEntries
                        .get(currentList.getSelectionIndex());
                currentTimes.removeAll();
                DataTime[] hours;
                try {
                    hours = thisEntry.getAvailableTimes();
                } catch (VizException e1) {
                    statusHandler.handle(Priority.SIGNIFICANT,
                            "Error retrieving data times", e1);
                    return;
                }
                int counter = 0;
                int selectedIndex = 0;
                for (DataTime hour : hours) {
                    // if (thisEntry.getSelectedEntryTime()[0].equals(hour))
                    // selectedIndex = counter;
                    currentTimes.add(hour.toString());
                    counter++;
                }
                currentTimes.select(selectedIndex);
                // TODO
                String productType = ""; // thisEntry.getEntryDataType();
                currentProductType.removeAll();
                if ("Grid".matches(productType)
                        || "Satellite".matches(productType)
                        || "Radar".matches(productType)) {
                    currentProductType.add("Image");
                }
                if ("Grid".matches(productType)) {
                    currentProductType.add("Contour");
                }
                if ("Observations".matches(productType)) {
                    currentProductType.add("Plot");
                }
                // TODO: fix
                // String entryDisplayType = thisEntry.getDesiredProduct()
                // .toString();
                // for (int i = 0; i < currentProductType.getItems().length;
                // i++) {
                // if (currentProductType.getItems()[i]
                // .matches(entryDisplayType)) {
                // currentProductType.select(i);
                // break;
                // }
                // }

                String entryColormap = ""; // thisEntry.getColorMap();
                for (int i = 0; i < currentProductColormap.getItems().length; i++) {
                    if (currentProductColormap.getItems()[i]
                            .matches(entryColormap)) {
                        currentProductColormap.select(i);
                        break;
                    }
                }

            }

        });
        currentTimes = new List(catalogArea, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL);
        currentTimes.setLayoutData(new GridData(200, 150));
        currentTimes.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub

            }

            public void widgetSelected(SelectionEvent e) {
                // TODO: ?

                // DataTime[] hours = ((AbstractRequestableResourceData)
                // productEntries
                // .get(currentList.getSelectionIndex()))
                // .getAvailableTimes();
                // String selectedHour = hours[currentTimes.getSelectionIndex()]
                // .toString();

            }

        });

    }

    private void createProductDefinition() {
        Composite definitionArea = new Composite(top, SWT.NONE);
        definitionArea.setLayout(new GridLayout(4, false));
        Label typeDef = new Label(definitionArea, SWT.NONE);
        typeDef.setText("Product Type: ");
        currentProductType = new Combo(definitionArea, SWT.READ_ONLY
                | SWT.BORDER);
        ResourceType[] vals = ResourceType.values();
        for (ResourceType t : vals) {
            currentProductType.add(t.toString());
        }
        // currentProductType.add("Contour");
        // currentProductType.add("Image");
        // currentProductType.add("Plot");
        currentProductType.select(0);
        currentProductType.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                // if (currentList.getSelectionIndex() > -1) {
                // String desiredProduct = currentProductType
                // .getItem(currentProductType.getSelectionIndex());
                // productEntries.get(currentList.getSelectionIndex())
                // .setDesiredProduct(
                // ResourceType.valueOf(desiredProduct));
                // }
            }

        });
        Label colorDef = new Label(definitionArea, SWT.NONE);
        colorDef.setText("    Colormap: ");
        currentProductColormap = new Combo(definitionArea, SWT.READ_ONLY
                | SWT.BORDER);
        String[] colormaps = getColormaps();
        // colormap = colormaps.length > 0 ? colormaps[0] : " ";
        for (String colormap : colormaps) {
            currentProductColormap.add(colormap);
        }
        currentProductColormap.select(0);
        currentProductColormap.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                if (currentList.getSelectionIndex() > -1) {
                    // String colorMap = currentProductColormap
                    // .getItem(currentProductColormap.getSelectionIndex());
                    // colormap = colorMap;
                }
            }
        });
        Composite contourArea = new Composite(top, SWT.NONE);
        contourArea.setLayout(new GridLayout(6, false));
        Label lowerDef = new Label(contourArea, SWT.NONE);
        lowerDef.setText("Bottom Contour: ");
        lowerValue = new Text(contourArea, SWT.SINGLE | SWT.BORDER);
        lowerValue.setLayoutData(new GridData(100, 20));
        lowerValue.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                if (currentList.getSelectionIndex() > -1) {
                    String inputValue = lowerValue.getText();
                    if (inputValue.length() > 0
                            && !inputValue.matches("[-\\d]\\d*\\.*\\d*")) {
                        MessageDialog
                                .openError(parentShell,
                                        "Invalid Number Specified",
                                        "The input for the lower contour value is not a number.  Please correct.");
                        upperValue.setFocus();
                    } else {
                        if (inputValue.length() > 0) {
                            if (!(inputValue.length() == 1 && inputValue
                                    .startsWith("-"))) {
                                // double upperNumber = Double
                                // .parseDouble(inputValue);
                            }

                        }
                    }
                }

            }
        });
        Label higherDef = new Label(contourArea, SWT.NONE);
        higherDef.setText("Top Contour: ");
        upperValue = new Text(contourArea, SWT.SINGLE | SWT.BORDER);
        upperValue.setLayoutData(new GridData(100, 20));
        upperValue.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                if (currentList.getSelectionIndex() > -1) {
                    String inputValue = upperValue.getText();
                    if (inputValue.length() > 0
                            && !inputValue.matches("[-\\d]\\d*\\.*\\d*")) {
                        MessageDialog
                                .openError(parentShell,
                                        "Invalid Number Specified",
                                        "The input for the upper contour value is not a number.  Please correct.");
                        upperValue.setFocus();
                    } else {
                        if (inputValue.length() > 0) {
                            if (!(inputValue.length() == 1 && inputValue
                                    .startsWith("-"))) {
                                // double upperNumber = Double
                                // .parseDouble(inputValue);
                            }

                        }
                    }
                }

            }
        });
        Label intervalText = new Label(contourArea, SWT.NONE);
        intervalText.setText("Interval: ");
        interval = new Text(contourArea, SWT.SINGLE | SWT.BORDER);
        interval.setLayoutData(new GridData(100, 20));
        interval.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                if (currentList.getSelectionIndex() > -1) {
                    String inputValue = interval.getText();
                    if (inputValue.length() > 0
                            && !inputValue.matches("[-\\d]\\d*\\.*\\d*")) {
                        MessageDialog
                                .openError(parentShell,
                                        "Invalid Number Specified",
                                        "The input for the contour interval is not a number.  Please correct.");
                        upperValue.setFocus();
                    } else {
                        if (inputValue.length() > 0) {
                            if (!(inputValue.length() == 1 && inputValue
                                    .startsWith("-"))) {
                                // double upperNumber = Double
                                // .parseDouble(inputValue);
                            }

                        }
                    }
                }

            }
        });
    }

    /**
     * Get a list of colormaps in the colormap directory
     * 
     * @return the script names
     */
    private String[] getColormaps() {

        return ColorMapLoader
                .listColorMaps(LocalizationContext.LocalizationLevel.UNKNOWN);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected Point getInitialSize() {
        return new Point(800, 700);
    }

    /**
     * @return the editorType so
     */
    public Map<String, String> getEditorConfig() {
        return editorConfig;
    }

    /**
     * @param editorType
     *            the editorType to set
     */
    public void setEditorType(Map<String, String> editorConfig) {
        this.editorConfig = editorConfig;
    }
} // @jve:decl-index=0:visual-constraint="49,107,731,599"
