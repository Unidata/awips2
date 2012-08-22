/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.uf.viz.points.ui.dialog;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.points.PointUtilities;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointSize;
import com.raytheon.uf.viz.points.ui.layer.PointsToolLayer;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Dialog for getting values for a point.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  October-2010              epolster    Initial Creation.
 *  Jul 31, 2012 #875         rferrel    Implements groups, hidden and
 *                                        assign color.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PointEditDialog extends CaveJFACEDialog {

    private enum EditOptions {
        CREATE_FROM_SCRATCH, EDIT, CREATE_AT_LOCATION;
    }

    public static Point MOST_RECENTLY_MODIFIED_POINT = null;

    static private final int VALIDATE_FIRST_CURSOR_ID = IDialogConstants.CLIENT_ID + 4;

    final int MAX_LATITUDE = 90;

    final int MAX_LONGITUDE = 180;

    final double MINUTE_PER_DEGREE = 60;

    final double SECOND_PER_MINUTE = 60;

    static final public int PREFERRED_FONT_SIZE_MEDIUM = 9;

    static final public int PREFERRED_FONT_SIZE_SMALL = 8;

    static protected String NOT_ENABLED = "---";

    static private final String COLOR_CHOOSER_TITLE = "Change Point Color";

    final private Font DIALOG_FONT_MEDIUM = FontManager.getFont("Tahoma",
            PREFERRED_FONT_SIZE_MEDIUM, SWT.BOLD);

    final private Font DIALOG_FONT_SMALL = FontManager.getFont("Tahoma",
            PREFERRED_FONT_SIZE_SMALL, SWT.NORMAL);

    private Composite rootContainer;

    private Text pointNameText;

    private Button pointColorChooserButton;

    private Button pointMovableButton;

    private Button pointHiddenButton;

    private Button pointAssignColorButton;

    private Combo pointFontSizeChooser;

    private Combo pointGroupChooser;

    protected CoordinateInputPanel coordinateInput;

    private Coordinate createAtLocation = null;

    private EditOptions dialogFlavor;

    private Point currPoint;

    private Button okButton;

    private Button cancelButton;

    // allow user to use cursor to define location
    private Button useCursorButton;

    private Color currColor;

    private PointsToolLayer toolLayer;

    static public Point createNewPointViaDialog(PointsToolLayer layer, Point m) {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        PointEditDialog dlg = new PointEditDialog(shell, layer,
                EditOptions.CREATE_FROM_SCRATCH, m);
        int ret = dlg.open();
        if (ret != Window.OK) {
            MOST_RECENTLY_MODIFIED_POINT = null;
        }
        return MOST_RECENTLY_MODIFIED_POINT;
    }

    static public Point createPointAtPositionViaDialog(PointsToolLayer layer,
            Coordinate c) {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        PointEditDialog dlg = new PointEditDialog(shell, layer,
                EditOptions.CREATE_AT_LOCATION, c);
        int ret = dlg.open();
        if (ret != Window.OK) {
            MOST_RECENTLY_MODIFIED_POINT = null;
        }
        return MOST_RECENTLY_MODIFIED_POINT;
    }

    static public Point editPointViaDialog(PointsToolLayer layer, Point m) {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        PointEditDialog dlg = new PointEditDialog(shell, layer,
                EditOptions.EDIT, m);
        int ret = dlg.open();
        if (ret != Window.OK) {
            MOST_RECENTLY_MODIFIED_POINT = null;
        }
        return MOST_RECENTLY_MODIFIED_POINT;
    }

    private PointEditDialog(Shell parentShell, PointsToolLayer layer,
            EditOptions meo) {
        super(parentShell);
        toolLayer = layer;
        dialogFlavor = meo;
        currPoint = null;
    }

    private PointEditDialog(Shell parentShell, PointsToolLayer layer,
            EditOptions meo, Point cm) {
        super(parentShell);
        toolLayer = layer;
        dialogFlavor = meo;
        currPoint = cm;
    }

    private PointEditDialog(Shell parentShell, PointsToolLayer layer,
            EditOptions meo, Coordinate atLocation) {
        super(parentShell);
        toolLayer = layer;
        createAtLocation = atLocation;
        dialogFlavor = meo;
        currPoint = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        if (dialogFlavor == EditOptions.CREATE_AT_LOCATION) {
            newShell.setText("Create Point At Location");
        } else if (dialogFlavor == EditOptions.CREATE_FROM_SCRATCH) {
            newShell.setText("Create Point");
        } else if (dialogFlavor == EditOptions.EDIT) {
            newShell.setText("Edit Point");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */

    @Override
    protected Control createDialogArea(Composite parent) {
        rootContainer = (Composite) super.createDialogArea(parent);
        rootContainer.setLayout(new GridLayout(1, true));

        // create group components
        createPointNameGroup();
        createLocationGroup();
        createPropertiesGroup();

        if (dialogFlavor == EditOptions.CREATE_AT_LOCATION) {
            initForCreateAtPosition(true);
        } else if (dialogFlavor == EditOptions.CREATE_FROM_SCRATCH) {
            initForCreateFromScratch();
        } else if (dialogFlavor == EditOptions.EDIT) {
            initForEdit();
        }

        pointNameText.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(FocusEvent e) {
                pointNameText.selectAll();
            }

            @Override
            public void focusLost(FocusEvent e) {
                String pointName = pointNameText.getText();
                if ((pointName == null) || (pointName.length() == 0)) {
                    return;
                } else {
                    pointName = PointUtilities.trimAll(pointName);
                    pointNameText.setText(pointName);
                }
            }
        });

        applyDialogFont(rootContainer);
        return rootContainer;
    }

    private void createPointNameGroup() {
        Group group = new Group(rootContainer, SWT.NONE);
        GridData gd = null;
        group.setLayout(new GridLayout(2, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group.setLayoutData(gd);
        Label label = new Label(group, SWT.CENTER);
        label.setFont(DIALOG_FONT_MEDIUM);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        label.setLayoutData(gd);
        label.setText("Point Name:");

        pointNameText = new Text(group, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pointNameText.setLayoutData(gd);
    }

    private void createLocationGroup() {
        Group group = new Group(rootContainer, SWT.NONE);
        coordinateInput = new CoordinateInputPanel(this, group);
    }

    private void createPropertiesGroup() {
        Group group = new Group(rootContainer, SWT.NO_RADIO_GROUP);
        Label vs = null;
        GridData gd = null;

        group.setLayout(new GridLayout(5, false));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group.setLayoutData(gd);

        pointAssignColorButton = new Button(group, SWT.CHECK);
        pointAssignColorButton.setFont(DIALOG_FONT_SMALL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pointAssignColorButton.setLayoutData(gd);
        // activeColorButton.setBounds(9, 10, 120, 17);
        pointAssignColorButton.setText("Assign Color");
        pointAssignColorButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                pointColorChooserButton.setEnabled(pointAssignColorButton
                        .getSelection());
            }
        });

        vs = new Label(group, SWT.SEPARATOR | SWT.VERTICAL);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.verticalSpan = 2;
        vs.setLayoutData(gd);

        pointGroupChooser = new Combo(group, SWT.SINGLE | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pointGroupChooser.setLayoutData(gd);
        pointGroupChooser.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_MEDIUM, SWT.NORMAL));
        String[] groups = PointsDataManager.getInstance().getGroupList();
        pointGroupChooser.add("<No Group>");
        for (String grp : groups) {
            pointGroupChooser.add(grp);
        }

        vs = new Label(group, SWT.SEPARATOR | SWT.VERTICAL);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.verticalSpan = 2;
        vs.setLayoutData(gd);

        pointHiddenButton = new Button(group, SWT.CHECK);
        pointHiddenButton.setFont(DIALOG_FONT_SMALL);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        pointHiddenButton.setLayoutData(gd);
        pointHiddenButton.setText("Hidden");
        pointColorChooserButton = new Button(group, SWT.NONE);
        pointColorChooserButton.setFont(DIALOG_FONT_SMALL);
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        pointColorChooserButton.setLayoutData(gd);
        pointColorChooserButton.setText("Change Color");

        RGB color = null;
        if (currPoint != null && currPoint.isColorActive()) {
            color = currPoint.getColor();
        }
        setCurrentColor(color);

        pointColorChooserButton.setBackground(currColor);
        pointColorChooserButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                ColorDialog dlg = new ColorDialog(getShell());
                dlg.setRGB(currColor.getRGB());
                dlg.setText(COLOR_CHOOSER_TITLE);
                RGB result = dlg.open();
                if (result != null) {
                    setCurrentColor(result);
                    pointColorChooserButton.setBackground(currColor);
                }
            }
        });

        pointFontSizeChooser = new Combo(group, SWT.SINGLE | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pointFontSizeChooser.setLayoutData(gd);
        pointFontSizeChooser.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_MEDIUM, SWT.NORMAL));

        for (PointSize ps : PointSize.values()) {
            pointFontSizeChooser.add(ps.getReadableName());
        }
        if (currPoint != null) {
            pointFontSizeChooser.select(currPoint.getFontSize().ordinal());
        } else {
            pointFontSizeChooser.select(0);
        }

        pointMovableButton = new Button(group, SWT.CHECK);
        pointMovableButton.setFont(DIALOG_FONT_SMALL);
        pointMovableButton.setText("Movable");
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
    }

    protected void resetFocus() {
        pointNameText.setFocus();
    }

    /**
     * Create contents of the button bar.
     * 
     * @param parent
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        final PointEditDialog dialog = this;

        okButton = createButton(parent, VALIDATE_FIRST_CURSOR_ID,
                IDialogConstants.OK_LABEL, true);
        okButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (dialog.acceptChanges()) {
                    dialog.close();
                } else {
                    resetFocus();
                }
            }
        });

        cancelButton = createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    protected boolean acceptChanges() {

        String name = pointNameText.getText();
        if ((name == null) || (name.trim().length() == 0)) {
            MessageBox d = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            d.setMessage("You must provide a name for the point.");
            d.open();
            return false;
        }

        if (!PointUtilities.isValidFileName(name)) {
            MessageBox d = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            d.setMessage("Point names can contain only letters, numbers, underscores, and spaces.");
            d.open();
            return false;
        }

        double lat = coordinateInput.getLatAsDegreesOnly();
        if ((lat < (PointUtilities.MAX_LATITUDE * -1))
                || (lat > PointUtilities.MAX_LATITUDE)) {
            MessageBox d = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            d.setMessage("Please enter a valid latitude.");
            d.open();
            return false;
        }

        double lon = coordinateInput.getLonAsDegreesOnly();
        if ((lon < (PointUtilities.MAX_LONGITUDE * -1))
                || (lon > PointUtilities.MAX_LONGITUDE)) {
            MessageBox d = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            d.setMessage("Please enter a valid longitude.");
            d.open();
            return false;
        }

        MOST_RECENTLY_MODIFIED_POINT = createPointFromInput();
        boolean alreadyExists = PointsDataManager.getInstance().exists(
                MOST_RECENTLY_MODIFIED_POINT.getName());

        // if we are creating a new point then it can't be a duplicate
        if (dialogFlavor != EditOptions.EDIT && alreadyExists) {
            MessageBox d = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            d.setMessage("Duplicate point name not allowed.");
            d.open();
            return false;
        } else if (alreadyExists
                && !currPoint.getName().equals(
                        MOST_RECENTLY_MODIFIED_POINT.getName())) {
            MessageBox d = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            d.setMessage("Point's new name is a duplicate; and not allowed.");
            d.open();
            return false;
        }

        return true;
    }

    public void initForEdit() {

        pointNameText.setText(currPoint.getName());
        coordinateInput.recalculateCoordinateFields(currPoint);
        pointFontSizeChooser.select(currPoint.getFontSize().ordinal());
        pointMovableButton.setSelection(currPoint.isMovable());
        pointHiddenButton.setSelection(currPoint.isHidden());
        RGB color = currPoint.getColor();
        setCurrentColor(color);
        pointAssignColorButton.setSelection(currPoint.isColorActive());
        pointColorChooserButton.setEnabled(currPoint.isColorActive());
        pointColorChooserButton.setBackground(currColor);
        int index = pointGroupChooser.indexOf(currPoint.getGroup().replace(
                PointUtilities.DELIM_CHAR, ' '));
        if (index < 0) {
            index = 0;
        }
        pointGroupChooser.select(index);
        resetFocus();

    }

    public void initForCreateAtPosition(boolean firstTimeThru) {

        pointNameText.setEditable(true);
        pointNameText.setEnabled(true);
        if (firstTimeThru) {
            pointNameText.setText("");
        }
        coordinateInput.recalculateCoordinateFields(createAtLocation);

        resetFocus();

        pointFontSizeChooser.select(PointSize.DEFAULT.ordinal());
        pointMovableButton.setSelection(false);
        pointHiddenButton.setSelection(false);
    }

    protected void initForCreateFromScratch() {

        if (currPoint != null) {
            initForEdit();
        } else {
            pointNameText.setEditable(true);
            pointNameText.setEnabled(true);
            coordinateInput.setLatDegrees(0);
            coordinateInput.setLatMinutes(0);
            coordinateInput.setLatSeconds(0.0f);

            coordinateInput.setLonDegrees(0);
            coordinateInput.setLonMinutes(0);
            coordinateInput.setLonSeconds(0.0f);
            resetFocus();

            coordinateInput.setNorth(true);
            coordinateInput.setWest(true);
            pointFontSizeChooser.select(PointSize.DEFAULT.ordinal());
            pointMovableButton.setSelection(true);
            pointHiddenButton.setSelection(false);
            pointAssignColorButton.setSelection(false);
            pointColorChooserButton.setEnabled(false);
            pointGroupChooser.select(0);
        }
        pointNameText.setText("");
    }

    /**
     * Generate a point based on the user information provided by the dialog.
     * This assumes validation of of the input has already been performed.
     * 
     * @return point
     */
    private Point createPointFromInput() {

        String name = pointNameText.getText();
        boolean colorActive = pointAssignColorButton.getSelection();
        double latDeg = coordinateInput.getLatAsDegreesOnly();
        double lonDeg = coordinateInput.getLonAsDegreesOnly();
        int ordinal = pointFontSizeChooser.getSelectionIndex();
        boolean movable = pointMovableButton.getSelection();
        String group = "";
        int selIndex = pointGroupChooser.getSelectionIndex();
        if (selIndex > 0) {
            group = pointGroupChooser.getItem(selIndex);
        }
        boolean hidden = pointHiddenButton.getSelection();
        Point point = new Point(name, latDeg, lonDeg, movable, colorActive,
                currColor.getRGB(), PointSize.getPointSize(ordinal), group);
        point.setHidden(hidden);
        return point;
    }

    private void setCurrentColor(RGB rgb) {
        if (rgb == null) {
            rgb = toolLayer.getCapability(ColorableCapability.class).getColor();
        }

        if (currColor != null) {
            if (currColor.getRed() == rgb.red
                    && currColor.getGreen() == rgb.green
                    && currColor.getBlue() == rgb.blue) {
                return;
            }
            currColor.dispose();
        }
        Display display = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell().getDisplay();
        currColor = new Color(display, rgb);
    }

    @Override
    public boolean close() {
        if (currColor != null) {
            currColor.dispose();
            currColor = null;
        }
        return super.close();
    }
}
