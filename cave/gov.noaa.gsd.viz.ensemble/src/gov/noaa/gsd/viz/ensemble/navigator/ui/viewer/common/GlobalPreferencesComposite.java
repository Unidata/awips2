package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common;

import gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.EnsembleToolViewer;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.core.VizApp;

/***
 * This composite contains the global preferences controls for the Ensemble
 * Tool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2015   12565     polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class GlobalPreferencesComposite extends Composite {

    public final static String PREFERENCES_NAME = "Preferences";

    private Composite thickenOnSelectionComposite = null;

    private Composite smallFlagsComposite = null;

    private Button thickenOnSelectionBtn = null;

    private Button useResourceColorRdo = null;

    private Button chooseColorRdo = null;

    private Label colorChooserLbl = null;

    private Label thicknessChooserLbl = null;

    private Spinner thicknessChooserSpinner = null;

    private Button editableOnRestoreBtn = null;

    private Button editableOnSwapInBtn = null;

    private Button uneditableOnMinimizeBtn = null;

    private Button minimizeOnForeignToolBtn = null;

    private Button minimizeOnToggleUneditableBtn = null;

    private Button createToolLayerOnNewEditorBtn = null;

    /**
     * Ensemble Tool Preferences
     */
    private static boolean editableOnRestore = false;

    private static boolean editableOnSwapIn = false;

    private static boolean uneditableOnMinimize = false;

    private static boolean minimizeOnForeignToolLoad = false;

    private static boolean minimizeOnToggleUneditable = false;

    private static boolean createToolLayerOnNewEditor = false;

    private static boolean thickenOnSelection = false;

    private static boolean useResourceColorOnThicken = false;

    private static Color thickenOnSelectionColor = null;

    private static int thickenWidth = 4;

    public GlobalPreferencesComposite(Composite parent, int style) {
        super(parent, style);

        createToolLayerOnNewEditor = true;
        useResourceColorOnThicken = true;
        minimizeOnForeignToolLoad = true;
        editableOnRestore = true;
        thickenOnSelection = true;

        createRootArea();

    }

    public void createRootArea() {

        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

        setLayout(new GridLayout(2, false));

        setBackground(GlobalColor.get(GlobalColor.MEDIUM_GRAY));

        final int numCols = 6;
        thickenOnSelectionComposite = new Composite(this, SWT.SHADOW_ETCHED_IN);
        thickenOnSelectionComposite.setLayout(new GridLayout(numCols, false));

        GridData thickenOnSelectionComposite_gd = new GridData(SWT.FILL,
                SWT.FILL, true, true, 1, 10);
        thickenOnSelectionComposite
                .setLayoutData(thickenOnSelectionComposite_gd);

        GridData separatorLbl_gd = new GridData(SWT.FILL, SWT.CENTER, true,
                false, numCols, 1);

        Label separatorLbl_0 = new Label(thickenOnSelectionComposite,
                SWT.SEPARATOR | SWT.HORIZONTAL);
        separatorLbl_0.setLayoutData(separatorLbl_gd);
        separatorLbl_0.setVisible(false);

        thickenOnSelectionBtn = new Button(thickenOnSelectionComposite,
                SWT.CHECK);
        thickenOnSelectionBtn.setSelection(true);
        setThickenOnSelectionPreference(true);
        thickenOnSelectionBtn.setText("  Thicken On Selection");
        thickenOnSelectionBtn.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 4, 1));
        thickenOnSelectionBtn.setFont(EnsembleToolViewer.getViewFontNormal());

        Label separatorLbl_1 = new Label(thickenOnSelectionComposite,
                SWT.SEPARATOR | SWT.HORIZONTAL);
        separatorLbl_1.setLayoutData(separatorLbl_gd);

        Composite useResourceColorComposite = new Composite(
                thickenOnSelectionComposite, SWT.NONE);
        GridData useResourceColorComposite_gd = new GridData(SWT.LEFT,
                SWT.CENTER, true, false, 6, 1);
        useResourceColorComposite.setLayoutData(useResourceColorComposite_gd);
        useResourceColorComposite.setLayout(new GridLayout(1, false));

        useResourceColorRdo = new Button(useResourceColorComposite, SWT.RADIO);
        useResourceColorRdo.setSelection(true);
        useResourceColorRdo.setText("Use Product Color");
        useResourceColorRdo.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                true, false, 1, 1));
        useResourceColorRdo.setFont(EnsembleToolViewer.getViewFontSmall());

        Composite chooseColorComposite = new Composite(
                thickenOnSelectionComposite, SWT.NONE);
        GridData chooseColorComposite_gd = new GridData(SWT.FILL, SWT.CENTER,
                true, false, 6, 1);
        chooseColorComposite.setLayoutData(chooseColorComposite_gd);
        chooseColorComposite.setLayout(new GridLayout(9, false));

        chooseColorRdo = new Button(chooseColorComposite, SWT.RADIO);
        GridData chooseColorBtn_gd = new GridData(SWT.FILL, SWT.CENTER, false,
                false, 6, 1);
        chooseColorRdo.setLayoutData(chooseColorBtn_gd);
        chooseColorRdo.setFont(EnsembleToolViewer.getViewFontSmall());
        chooseColorRdo.setText("Use Color");
        chooseColorRdo.setSelection(false);

        colorChooserLbl = new Label(chooseColorComposite, SWT.BORDER);
        colorChooserLbl.setBackground(getThickenOnSelectionColorPreference());
        colorChooserLbl.setFont(SWTResourceManager.getFont("Dialog", 12,
                SWT.NONE));
        colorChooserLbl.setAlignment(SWT.CENTER);
        GridData colorChooserLbl_gd = new GridData(SWT.FILL, SWT.CENTER, true,
                false, 3, 1);
        colorChooserLbl.setLayoutData(colorChooserLbl_gd);
        colorChooserLbl.setEnabled(false);
        colorChooserLbl.setBackground(GlobalColor.get(GlobalColor.LIGHT_GRAY));
        colorChooserLbl.setText("X");

        Composite thicknessWidgetComposite = new Composite(
                thickenOnSelectionComposite, SWT.NONE);
        GridData thicknessWidgetComposite_gd = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 6, 1);
        thicknessWidgetComposite.setLayoutData(thicknessWidgetComposite_gd);
        thicknessWidgetComposite.setLayout(new GridLayout(9, false));

        Label spacerLbl = new Label(thicknessWidgetComposite, SWT.None);
        GridData spacerLbl_gd = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 1, 1);
        spacerLbl.setLayoutData(spacerLbl_gd);
        thicknessChooserLbl = new Label(thicknessWidgetComposite, SWT.BORDER
                | SWT.CENTER);

        GridData thicknessChooserLbl_gd = new GridData(SWT.FILL, SWT.CENTER,
                true, false, 5, 1);
        thicknessChooserLbl.setLayoutData(thicknessChooserLbl_gd);
        thicknessChooserLbl.setText("  Thickness:  ");
        thicknessChooserLbl.setFont(EnsembleToolViewer.getViewFontSmall());
        thicknessChooserLbl.setAlignment(SWT.CENTER);

        thicknessChooserSpinner = new Spinner(thicknessWidgetComposite,
                SWT.BORDER);
        thicknessChooserSpinner.setValues(getThickenWidthPreference(), 2, 7, 0,
                1, 1);
        GridData thicknessChooser_gd = new GridData(SWT.FILL, SWT.CENTER, true,
                false, 3, 1);
        thicknessChooserSpinner.setLayoutData(thicknessChooser_gd);
        thicknessChooserSpinner.setFont(EnsembleToolViewer.getViewFontSmall());
        thicknessChooserSpinner.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int thickenWidth = ((Spinner) e.getSource()).getSelection();
                setThickenWidthPreference(thickenWidth);
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int thickenWidth = ((Spinner) e.getSource()).getSelection();
                setThickenWidthPreference(thickenWidth);
            }

        });

        colorChooserLbl.addMouseListener(new MouseAdapter() {

            public void mouseUp(MouseEvent e) {
                if (!isUseResourceColorOnThickenPreference()) {
                    ColorDialog cd = new ColorDialog(EnsembleToolViewer
                            .getShell());
                    cd.setRGB(getThickenOnSelectionColorPreference().getRGB());
                    cd.setText("Choose Selection Color");
                    RGB result = cd.open();
                    if (result != null) {
                        Color c = SWTResourceManager.getColor(result);
                        setThickenOnSelectionColorPreference(c);
                        colorChooserLbl.setBackground(c);
                    }
                }
            }
        });

        thickenOnSelectionBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean isResourceColorBeingUsed = useResourceColorRdo
                        .getSelection();
                boolean isChecked = thickenOnSelectionBtn.getSelection();
                if (isChecked) {
                    useResourceColorRdo.setEnabled(true);
                    chooseColorRdo.setEnabled(true);
                    colorChooserLbl.setEnabled(true);
                    if (isResourceColorBeingUsed) {
                        colorChooserLbl.setBackground(GlobalColor
                                .get(GlobalColor.LIGHT_GRAY));
                        colorChooserLbl.setText("X");
                    } else {
                        colorChooserLbl
                                .setBackground(getThickenOnSelectionColorPreference());
                        colorChooserLbl.setText("");
                    }
                    thicknessChooserLbl.setEnabled(true);
                    thicknessChooserSpinner.setEnabled(true);
                    setThickenOnSelectionPreference(true);
                } else {
                    useResourceColorRdo.setEnabled(false);
                    chooseColorRdo.setEnabled(false);
                    colorChooserLbl.setEnabled(false);
                    colorChooserLbl.setBackground(GlobalColor
                            .get(GlobalColor.LIGHT_GRAY));
                    colorChooserLbl.setText("X");
                    thicknessChooserLbl.setEnabled(false);
                    thicknessChooserSpinner.setEnabled(false);
                    setThickenOnSelectionPreference(false);
                }
            }
        });

        useResourceColorRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean isSelected = ((Button) e.getSource()).getSelection();
                if (isSelected) {
                    chooseColorRdo.setSelection(false);
                    colorChooserLbl.setBackground(GlobalColor
                            .get(GlobalColor.LIGHT_GRAY));
                    colorChooserLbl.setText("X");
                    colorChooserLbl.setEnabled(false);
                    setUseResourceColorOnThickenPreference(true);
                }
            }

        });

        chooseColorRdo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean isSelected = ((Button) e.getSource()).getSelection();
                if (isSelected) {
                    useResourceColorRdo.setSelection(false);
                    colorChooserLbl
                            .setBackground(getThickenOnSelectionColorPreference());
                    colorChooserLbl.setText("");
                    colorChooserLbl.setEnabled(true);
                    setUseResourceColorOnThickenPreference(false);
                } else {
                    colorChooserLbl.setEnabled(false);
                    colorChooserLbl.setBackground(GlobalColor
                            .get(GlobalColor.LIGHT_GRAY));
                    colorChooserLbl.setText("X");
                    colorChooserLbl.setImage(null);
                }
            }

        });

        /* Additional preferences */

        smallFlagsComposite = new Composite(this, SWT.SHADOW_ETCHED_IN);
        smallFlagsComposite.setLayout(new GridLayout(1, false));
        GridData smallFlagsComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 1, 10);
        smallFlagsComposite.setLayoutData(smallFlagsComposite_gd);

        /*
         * Allow the user to control editability of the ensemble tool layer when
         * this view (ViewPart) is restored.
         */
        editableOnRestoreBtn = new Button(smallFlagsComposite, SWT.CHECK);
        editableOnRestoreBtn.setSelection(isEditableOnRestorePreference());
        editableOnRestoreBtn.setText("Make editable on restore");
        editableOnRestoreBtn.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                true, false, 1, 1));
        editableOnRestoreBtn.setFont(EnsembleToolViewer.getViewFontSmall());
        editableOnRestoreBtn.setEnabled(true);
        editableOnRestoreBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean editableOnRestore = ((Button) e.getSource())
                        .getSelection();
                setEditableOnRestorePreference(editableOnRestore);
            }

        });

        /*
         * Allow the user to control whether this view (ViewPart) is minimized
         * when the user loads another tool (e.g. Points, Baselines, etc).
         */
        minimizeOnForeignToolBtn = new Button(smallFlagsComposite, SWT.CHECK);
        minimizeOnForeignToolBtn
                .setSelection(isMinimizeOnForeignToolLoadPreference());
        minimizeOnForeignToolBtn.setText("Minimize on foreign tool");
        minimizeOnForeignToolBtn.setLayoutData(new GridData(SWT.LEFT,
                SWT.CENTER, true, false, 1, 1));
        minimizeOnForeignToolBtn.setFont(EnsembleToolViewer.getViewFontSmall());
        minimizeOnForeignToolBtn.setEnabled(true);
        minimizeOnForeignToolBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean minimizeOnForeignToolLoad = ((Button) e.getSource())
                        .getSelection();
                setMinimizeOnForeignToolLoadPreference(minimizeOnForeignToolLoad);
            }

        });

        /*
         * Allow the user to control whether the ensemble tool view is minimized
         * when the active tool layer is toggled to uneditable.
         */
        minimizeOnToggleUneditableBtn = new Button(smallFlagsComposite,
                SWT.CHECK);
        minimizeOnToggleUneditableBtn
                .setSelection(isMinimizeOnToggleUneditablePreference());
        minimizeOnToggleUneditableBtn.setText("Minimize on toggle uneditable");
        minimizeOnToggleUneditableBtn.setLayoutData(new GridData(SWT.LEFT,
                SWT.CENTER, true, false, 1, 1));
        minimizeOnToggleUneditableBtn.setFont(EnsembleToolViewer
                .getViewFontSmall());
        minimizeOnToggleUneditableBtn.setEnabled(true);
        minimizeOnToggleUneditableBtn
                .addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        boolean minimizeOnToggleUneditable = ((Button) e
                                .getSource()).getSelection();
                        setMinimizeOnToggleUneditablePreference(minimizeOnToggleUneditable);
                    }

                });

        /*
         * Allow the user to control the way swapping-in a pane containing an
         * ensemble tool layer effects the editability of the tool layer.
         */
        editableOnSwapInBtn = new Button(smallFlagsComposite, SWT.CHECK);
        setEditableOnSwapInPreference(false);
        editableOnSwapInBtn.setSelection(isEditableOnSwapInPreference());
        editableOnSwapInBtn.setText("Make editable on swap-in");
        editableOnSwapInBtn.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER,
                true, false, 1, 1));
        editableOnSwapInBtn.setFont(EnsembleToolViewer.getViewFontSmall());
        // btnEditableOnSwapIn.setEnabled(true);
        editableOnSwapInBtn.setEnabled(false);
        editableOnSwapInBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean editableOnSwapIn = ((Button) e.getSource())
                        .getSelection();

                setEditableOnSwapInPreference(editableOnSwapIn);
            }

        });

        /*
         * Allow the user to control whether the active ensemble tool layer
         * should be made uneditable when this view (ViewPart) is minimized.
         */
        uneditableOnMinimizeBtn = new Button(smallFlagsComposite, SWT.CHECK);
        setUneditableOnMinimizePreference(false);
        uneditableOnMinimizeBtn
                .setSelection(isUneditableOnMinimizePreference());
        uneditableOnMinimizeBtn.setText("Make uneditable on minimize");
        uneditableOnMinimizeBtn.setLayoutData(new GridData(SWT.LEFT,
                SWT.CENTER, true, false, 1, 1));
        uneditableOnMinimizeBtn.setFont(EnsembleToolViewer.getViewFontSmall());
        // btnUneditableOnMinimize.setEnabled(true);
        uneditableOnMinimizeBtn.setEnabled(false);
        uneditableOnMinimizeBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean uneditableOnMinimize = ((Button) e.getSource())
                        .getSelection();

                setUneditableOnMinimizePreference(uneditableOnMinimize);
            }

        });

        /*
         * Allow the user to control, when a new editor is opened, whether a new
         * ensemble tool layer is created and made editable.
         */

        createToolLayerOnNewEditorBtn = new Button(smallFlagsComposite,
                SWT.CHECK);
        setCreateToolLayerOnNewEditorPreference(true);
        createToolLayerOnNewEditorBtn
                .setSelection(isCreateToolLayerOnNewEditorPreference());
        createToolLayerOnNewEditorBtn
                .setSelection(isCreateToolLayerOnNewEditorPreference());
        createToolLayerOnNewEditorBtn.setText("New tool layer on new editor");
        createToolLayerOnNewEditorBtn.setLayoutData(new GridData(SWT.LEFT,
                SWT.CENTER, true, false, 1, 1));
        createToolLayerOnNewEditorBtn.setFont(EnsembleToolViewer
                .getViewFontSmall()); //
        createToolLayerOnNewEditorBtn.setEnabled(false);
        createToolLayerOnNewEditorBtn
                .addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        boolean createToolLayerOnNewEditor = ((Button) e
                                .getSource()).getSelection();

                        setCreateToolLayerOnNewEditorPreference(createToolLayerOnNewEditor);
                    }

                });

    }

    public void setViewEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                thickenOnSelectionComposite.setEnabled(enabled);
                smallFlagsComposite.setEnabled(enabled);
                thickenOnSelectionBtn.setEnabled(enabled);
                useResourceColorRdo.setEnabled(enabled);
                chooseColorRdo.setEnabled(enabled);
                colorChooserLbl.setEnabled(enabled);
                thicknessChooserLbl.setEnabled(enabled);
                thicknessChooserSpinner.setEnabled(enabled);

                editableOnRestoreBtn.setEnabled(enabled);
                minimizeOnForeignToolBtn.setEnabled(enabled);
                minimizeOnToggleUneditableBtn.setEnabled(enabled);

                /* TODO: Remaining preferences */
                // btnEditableOnSwapIn.setEnabled(viewEditable);
                // btnUneditableOnMinimize.setEnabled(viewEditable);
                // btnCreateToolLayerOnNewEditor.setEnabled(viewEditable);
                createToolLayerOnNewEditorBtn.setEnabled(false);
                editableOnSwapInBtn.setEnabled(false);
                uneditableOnMinimizeBtn.setEnabled(false);

            }
        });

    }

    /*
     * Static Preferences section
     */
    public static boolean isEditableOnRestorePreference() {
        return editableOnRestore;
    }

    public static void setEditableOnRestorePreference(boolean eor) {
        editableOnRestore = eor;
    }

    public static boolean isEditableOnSwapInPreference() {
        return editableOnSwapIn;
    }

    public static void setEditableOnSwapInPreference(boolean e) {
        editableOnSwapIn = e;
    }

    public static boolean isMinimizeOnForeignToolLoadPreference() {
        return minimizeOnForeignToolLoad;
    }

    public static void setMinimizeOnForeignToolLoadPreference(boolean m) {
        minimizeOnForeignToolLoad = m;
    }

    public static boolean isMinimizeOnToggleUneditablePreference() {
        return minimizeOnToggleUneditable;
    }

    public static void setMinimizeOnToggleUneditablePreference(boolean m) {
        minimizeOnToggleUneditable = m;
    }

    public static boolean isCreateToolLayerOnNewEditorPreference() {
        return createToolLayerOnNewEditor;
    }

    public static void setCreateToolLayerOnNewEditorPreference(boolean c) {
        createToolLayerOnNewEditor = c;
    }

    public static boolean isThickenOnSelectionPreference() {
        return thickenOnSelection;
    }

    public static void setThickenOnSelectionPreference(boolean tos) {
        thickenOnSelection = tos;
    }

    public static boolean isUseResourceColorOnThickenPreference() {
        return useResourceColorOnThicken;
    }

    public static void setUseResourceColorOnThickenPreference(boolean tos) {
        useResourceColorOnThicken = tos;
    }

    public static boolean isUneditableOnMinimizePreference() {
        return uneditableOnMinimize;
    }

    public static void setUneditableOnMinimizePreference(boolean u) {
        uneditableOnMinimize = u;
    }

    public static int getThickenWidthPreference() {
        return thickenWidth;
    }

    public static void setThickenWidthPreference(int w) {
        thickenWidth = w;
    }

    public static Color getThickenOnSelectionColorPreference() {

        Color tosColor = null;
        if (thickenOnSelectionColor != null
                && !thickenOnSelectionColor.isDisposed()) {
            tosColor = thickenOnSelectionColor;
        } else {
            tosColor = GlobalColor.get(GlobalColor.PASTEL_LIGHT_BLUE);
        }
        return tosColor;
    }

    public static void setThickenOnSelectionColorPreference(Color c) {
        thickenOnSelectionColor = c;
    }

}
