package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.legend;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.Range;
import gov.noaa.gsd.viz.ensemble.display.calculate.RangeType;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

/**
 * A dialog which allows the user to choose create an ensemble relative
 * frequency product from different probability ranges.
 * 
 * @author polster
 * @author jing
 * 
 *         <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2015   6863      polster     Initial creation
 * Feb 17,2017   19325     jing        Added ERF image capability
 * 
 *         </pre>
 * 
 * @version 1.0
 */

public class ERFProductDialog extends CaveJFACEDialog {

    public static enum ProbabilityRanges {
        INNER_RANGE, OUTER_RANGE, ABOVE, BELOW
    }

    private Composite mainPanelComposite = null;

    private Text probabilityOfXInsideRangeLeftEntryTxt = null;

    private Text probabilityOfXInsideRangeRightEntryTxt = null;

    private Text probabilityOfXOutsideRangeLeftEntryTxt = null;

    private Text probabilityOfXOutsideRangeRightEntryTxt = null;

    private Text probabilityOfXAboveRangeEntryTxt = null;

    private Text probabilityOfXBelowRangeEntryTxt = null;

    private Button chooserRangeRdo_1 = null;

    private Button chooserRangeRdo_2 = null;

    private Button chooserRangeRdo_3 = null;

    private Button chooserRangeRdo_4 = null;

    private Label ensembleProductNameLbl = null;

    private String ensembleProductName = null;

    private ERFProductDialog.ProbabilityRanges probabilityRange = ERFProductDialog.ProbabilityRanges.INNER_RANGE;

    private double setProbabilityLowerValue = 0;

    private double setProbabilityUpperValue = 0;

    final private String probabilityOfX = "P(x):  ";

    private boolean isImage = false;

    private ERFProductDialog(Shell parentShell) {
        super(parentShell);
    }

    public ERFProductDialog(Shell parentShell, String rscName, boolean image) {
        this(parentShell);
        ensembleProductName = rscName;
        isImage = image;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#isResizable()
     */
    @Override
    protected boolean isResizable() {
        return true;
    }

    /**
     * Create contents of the dialog.
     * 
     * @param parent
     */
    @Override
    protected Control createDialogArea(Composite parent) {

        setBlockOnOpen(true);

        createRootArea(parent);

        createTitleWidget();

        createVerticalSeparator();

        createWithinRangeWidget();

        createOutsideOfRangeWidget();

        createAboveThresholdWidget();

        createBelowThresholdWidget();

        Label dummySpacerLbl_1 = new Label(mainPanelComposite, SWT.NONE);
        GridData dummySpacerLbl_gd_1 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 2, 1);
        dummySpacerLbl_1.setLayoutData(dummySpacerLbl_gd_1);

        // only one range-filter row-widget is enabled at a time
        probabilityOfXInsideRangeLeftEntryTxt.setEnabled(true);
        probabilityOfXInsideRangeRightEntryTxt.setEnabled(true);

        probabilityOfXOutsideRangeLeftEntryTxt.setEnabled(false);
        probabilityOfXOutsideRangeRightEntryTxt.setEnabled(false);

        probabilityOfXAboveRangeEntryTxt.setEnabled(false);
        probabilityOfXBelowRangeEntryTxt.setEnabled(false);

        addWidgetSelectionBehavior();

        addThresholdValueModifyBehavior();

        parent.layout();

        return parent;

    }

    private void createVerticalSeparator() {
        new Label(mainPanelComposite, SWT.NONE).setLayoutData(
                new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 2));

    }

    private void createRootArea(Composite parent) {
        mainPanelComposite = new Composite(parent, SWT.BORDER);

        GridData rootCalculatorPanel_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        mainPanelComposite.setLayoutData(rootCalculatorPanel_gd);
        mainPanelComposite.setLayout(new GridLayout(1, false));
    }

    private void createTitleWidget() {

        Composite titleContainerComposite = new Composite(mainPanelComposite,
                SWT.BORDER);
        titleContainerComposite.setLayoutData(
                new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
        titleContainerComposite.setLayout(new GridLayout(1, true));

        ensembleProductNameLbl = new Label(titleContainerComposite,
                SWT.CENTER | SWT.BORDER);
        ensembleProductNameLbl
                .setFont(SWTResourceManager.getFont("Dialog", 10, SWT.NORMAL));
        ensembleProductNameLbl
                .setForeground(GlobalColor.get(GlobalColor.BLACK));
        ensembleProductNameLbl
                .setBackground(GlobalColor.get(GlobalColor.PALE_LIGHT_AZURE));
        GridData frameTimeUsingBasisLbl_gd = new GridData(SWT.FILL, SWT.CENTER,
                true, true, 1, 1);
        ensembleProductNameLbl.setLayoutData(frameTimeUsingBasisLbl_gd);

    }

    private void createBelowThresholdWidget() {

        /*
         * How many columns do we have to position components inside the root
         * composite.
         */
        final int numColumns = 15;

        // This is the beginning of the BELOW A THRESHOLD row-widget.
        // Select this widget row by choosing the radio button which
        // precedes it.
        Composite rangeToolRootComposite_4 = new Composite(mainPanelComposite,
                SWT.BORDER);
        GridData rangeToolRootComposite_gd_4 = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 1, 1);

        rangeToolRootComposite_4.setLayoutData(rangeToolRootComposite_gd_4);
        rangeToolRootComposite_4.setLayout(new GridLayout(numColumns, false));
        rangeToolRootComposite_4
                .setToolTipText("ERF probability P(x) is below (%)");

        // Are you choosing the BELOW A THRESHOLD row-widget?
        chooserRangeRdo_4 = new Button(rangeToolRootComposite_4, SWT.RADIO);
        chooserRangeRdo_4.setSelection(false);
        GridData chooserRangeRdo_gd_4 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        chooserRangeRdo_4.setLayoutData(chooserRangeRdo_gd_4);

        // Put the "probability of x" label ...
        Label probabilityOfX_lbl_4 = new Label(rangeToolRootComposite_4,
                SWT.NONE);
        GridData probabilityOfX_lbl_gd_4 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 3, 1);
        probabilityOfX_lbl_4.setLayoutData(probabilityOfX_lbl_gd_4);
        probabilityOfX_lbl_4.setText(probabilityOfX);
        probabilityOfX_lbl_4.setFont(
                SWTResourceManager.getFont("Serif", 11, SWT.BOLD | SWT.ITALIC));
        probabilityOfX_lbl_4.setToolTipText("Probability P(x) is below");

        // There's a lower bound text entry to this BELOW A THRESHOLD row-
        // widget.
        Label valueOfX_lbl_4 = new Label(rangeToolRootComposite_4, SWT.NONE);
        valueOfX_lbl_4.setAlignment(SWT.CENTER);
        GridData valueOfX_lbl_gd_4 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 3, 1);
        valueOfX_lbl_4.setLayoutData(valueOfX_lbl_gd_4);
        valueOfX_lbl_4.setFont(
                SWTResourceManager.getFont("Serif", 12, SWT.BOLD | SWT.ITALIC));
        valueOfX_lbl_4.setText("x   < ");
        valueOfX_lbl_4.setToolTipText("ERF probability P(x) is below (%)");

        probabilityOfXBelowRangeEntryTxt = new Text(rangeToolRootComposite_4,
                SWT.BORDER | SWT.CENTER);
        GridData lowerRangeEntryTxt_gd_4 = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 5, 1);
        probabilityOfXBelowRangeEntryTxt.setLayoutData(lowerRangeEntryTxt_gd_4);
        probabilityOfXBelowRangeEntryTxt
                .setToolTipText("The threshold that 'x' is below");

        new Label(rangeToolRootComposite_4, SWT.NONE).setLayoutData(
                new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

    }

    private void createAboveThresholdWidget() {

        /*
         * How many columns do we have to position components inside the root
         * composite.
         */
        final int numColumns = 15;

        // This is the beginning of the ABOVE A THRESHOLD row-widget.
        // Select this widget row by choosing the radio button which
        // precedes it.

        Composite rangeToolRootComposite_3 = new Composite(mainPanelComposite,
                SWT.BORDER);
        GridData rangeToolRootComposite_gd_3 = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 1, 1);
        rangeToolRootComposite_3.setLayoutData(rangeToolRootComposite_gd_3);

        rangeToolRootComposite_3.setLayout(new GridLayout(numColumns, false));
        rangeToolRootComposite_3
                .setToolTipText("ERF probability P(x) is above (%)");

        // Are you choosing the ABOVE A THRESHOLD widget row?
        chooserRangeRdo_3 = new Button(rangeToolRootComposite_3, SWT.RADIO);
        chooserRangeRdo_3.setSelection(false);
        GridData chooserRangeRdo_gd_3 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        chooserRangeRdo_3.setLayoutData(chooserRangeRdo_gd_3);

        // Put the "probability of x" label ...
        Label probabilityOfX_lbl_3 = new Label(rangeToolRootComposite_3,
                SWT.NONE);
        GridData probabilityOfX_lbl_gd_3 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 3, 1);
        probabilityOfX_lbl_3.setLayoutData(probabilityOfX_lbl_gd_3);
        probabilityOfX_lbl_3.setText(probabilityOfX);
        probabilityOfX_lbl_3.setFont(
                SWTResourceManager.getFont("Serif", 11, SWT.BOLD | SWT.ITALIC));

        // There's an upper bound text entry to this ABOVE A THRESHOLD row-
        // widget.
        Label valueOfX_lbl_3 = new Label(rangeToolRootComposite_3, SWT.NONE);
        valueOfX_lbl_3.setAlignment(SWT.CENTER);
        GridData valueOfX_lbl_gd_3 = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 3, 1);
        valueOfX_lbl_3.setLayoutData(valueOfX_lbl_gd_3);
        valueOfX_lbl_3.setFont(
                SWTResourceManager.getFont("Serif", 12, SWT.BOLD | SWT.ITALIC));
        valueOfX_lbl_3.setText("x   > ");
        valueOfX_lbl_3.setToolTipText("ERF probability P(x) is above (%)");

        probabilityOfXAboveRangeEntryTxt = new Text(rangeToolRootComposite_3,
                SWT.BORDER | SWT.CENTER);
        GridData lowerRangeEntryTxt_gd_3 = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 5, 1);
        probabilityOfXAboveRangeEntryTxt.setLayoutData(lowerRangeEntryTxt_gd_3);
        probabilityOfXAboveRangeEntryTxt
                .setToolTipText("The threshold that 'x' is above");

        new Label(rangeToolRootComposite_3, SWT.NONE).setLayoutData(
                new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
    }

    private void createOutsideOfRangeWidget() {

        /*
         * How many columns do we have to position components inside the root
         * composite.
         */
        final int numColumns = 15;

        // This is the beginning of the OUTSIDE A RANGE widget row.
        // Select this widget row by choosing the radio button which
        // precedes it.
        Composite rangeToolRootComposite_2 = new Composite(mainPanelComposite,
                SWT.BORDER);
        GridData rangeToolRootComposite_gd_2 = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 1, 1);
        rangeToolRootComposite_2.setLayoutData(rangeToolRootComposite_gd_2);

        rangeToolRootComposite_2.setLayout(new GridLayout(numColumns, false));
        rangeToolRootComposite_2
                .setToolTipText("ERF probability P(x) is outside a range (%)");

        // Are you choosing the OUTSIDE A RANGE row-widget?
        chooserRangeRdo_2 = new Button(rangeToolRootComposite_2, SWT.RADIO);
        chooserRangeRdo_2.setSelection(false);
        GridData chooserRangeRdo_gd_2 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        chooserRangeRdo_2.setLayoutData(chooserRangeRdo_gd_2);

        // Put the "probability of x" label ...
        Label probabilityOfX_lbl_2 = new Label(rangeToolRootComposite_2,
                SWT.NONE);
        GridData probabilityOfX_lbl_gd_2 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        probabilityOfX_lbl_2.setLayoutData(probabilityOfX_lbl_gd_2);
        probabilityOfX_lbl_2.setText(probabilityOfX);
        probabilityOfX_lbl_2.setFont(
                SWTResourceManager.getFont("Serif", 11, SWT.BOLD | SWT.ITALIC));
        probabilityOfX_lbl_2
                .setToolTipText("ERF probability P(x) is outside a range (%)");

        // There's a lower bound text entry to this OUTSIDE A RANGE row-widget.
        probabilityOfXOutsideRangeLeftEntryTxt = new Text(
                rangeToolRootComposite_2, SWT.BORDER);
        GridData lowerRangeEntryTxt_gd_2 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 4, 1);
        probabilityOfXOutsideRangeLeftEntryTxt
                .setLayoutData(lowerRangeEntryTxt_gd_2);
        probabilityOfXOutsideRangeLeftEntryTxt
                .setToolTipText("This must be the minimum value for 'x'");

        Label lowerConditionalLbl_2 = new Label(rangeToolRootComposite_2,
                SWT.NONE);
        GridData lowerConditionalLbl_gd_2 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 3, 1);
        lowerConditionalLbl_2
                .setFont(SWTResourceManager.getFont("Serif", 11, SWT.BOLD));
        lowerConditionalLbl_2.setLayoutData(lowerConditionalLbl_gd_2);
        lowerConditionalLbl_2.setText("  >  x  > ");
        lowerConditionalLbl_2
                .setToolTipText("ERF probability P(x) is outside a range (%)");

        // There's an upper bound text entry to this OUTSIDE A RANGE row-widget.
        probabilityOfXOutsideRangeRightEntryTxt = new Text(
                rangeToolRootComposite_2, SWT.BORDER | SWT.CENTER);
        GridData upperRangeEntryTxt_gd_2 = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 4, 1);
        probabilityOfXOutsideRangeRightEntryTxt
                .setLayoutData(upperRangeEntryTxt_gd_2);
        probabilityOfXOutsideRangeRightEntryTxt
                .setToolTipText("This must be the maximum value for 'x'");

    }

    private void createWithinRangeWidget() {

        /*
         * How many columns do we have to position components inside the root
         * composite.
         */
        final int numColumns = 15;

        //
        // This is the beginning of the WITHIN A RANGE row-widget
        // Select this widget row by choosing the radio button which
        // precedes it.
        Composite rangeToolRootComposite_1 = new Composite(mainPanelComposite,
                SWT.BORDER);
        GridData rangeToolRootComposite_gd_1 = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 1, 1);
        rangeToolRootComposite_1.setLayoutData(rangeToolRootComposite_gd_1);

        rangeToolRootComposite_1.setLayout(new GridLayout(numColumns, false));

        rangeToolRootComposite_1
                .setToolTipText("ERF probability P(x) is within a range (%)");

        // Are you choosing the WITHIN A RANGE row-widget?
        chooserRangeRdo_1 = new Button(rangeToolRootComposite_1, SWT.RADIO);
        chooserRangeRdo_1.setSelection(true);
        GridData chooserRangeRdo_gd_1 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        chooserRangeRdo_1.setLayoutData(chooserRangeRdo_gd_1);

        // Put the "probability of x" label ...
        Label probabilityOfX_lbl_1 = new Label(rangeToolRootComposite_1,
                SWT.NONE);
        GridData probabilityOfX_lbl_gd_1 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        probabilityOfX_lbl_1.setLayoutData(probabilityOfX_lbl_gd_1);
        probabilityOfX_lbl_1.setFont(
                SWTResourceManager.getFont("Serif", 11, SWT.BOLD | SWT.ITALIC));
        probabilityOfX_lbl_1.setText(probabilityOfX);
        probabilityOfX_lbl_1
                .setToolTipText("ERF probability P(x) is within a range (%)");

        // There's a lower bound text entry to this WITHIN A RANGE row-widget.
        probabilityOfXInsideRangeLeftEntryTxt = new Text(
                rangeToolRootComposite_1, SWT.BORDER);
        GridData lowerRangeEntryTxt_gd_1 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 4, 1);
        probabilityOfXInsideRangeLeftEntryTxt
                .setLayoutData(lowerRangeEntryTxt_gd_1);
        probabilityOfXInsideRangeLeftEntryTxt
                .setToolTipText("This must be the minimum value for 'x'");

        Label lowerConditionalLbl_1 = new Label(rangeToolRootComposite_1,
                SWT.NONE);
        GridData lowerConditionalLbl_gd_1 = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 3, 1);
        lowerConditionalLbl_1
                .setFont(SWTResourceManager.getFont("Serif", 11, SWT.BOLD));
        lowerConditionalLbl_1.setLayoutData(lowerConditionalLbl_gd_1);
        lowerConditionalLbl_1.setText("  <  x  < ");
        lowerConditionalLbl_1
                .setToolTipText("ERF probability P(x) is within a range (%)");

        // There's an upper bound text entry to this WITHIN A RANGE row-widget.
        probabilityOfXInsideRangeRightEntryTxt = new Text(
                rangeToolRootComposite_1, SWT.BORDER);
        GridData upperRangeEntryTxt_gd_1 = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 4, 1);
        probabilityOfXInsideRangeRightEntryTxt
                .setLayoutData(upperRangeEntryTxt_gd_1);
        probabilityOfXInsideRangeRightEntryTxt
                .setToolTipText("This must be the maximum value for 'x'");

    }

    private void addWidgetSelectionBehavior() {

        // select the WITHIN A RANGE row-widget and deselect the others.
        chooserRangeRdo_1.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                chooserRangeRdo_1.setSelection(true);
                chooserRangeRdo_2.setSelection(false);
                chooserRangeRdo_3.setSelection(false);
                chooserRangeRdo_4.setSelection(false);

                clearProbabilityFields();

                probabilityOfXInsideRangeLeftEntryTxt.setEnabled(true);
                probabilityOfXOutsideRangeLeftEntryTxt.setEnabled(false);
                probabilityOfXAboveRangeEntryTxt.setEnabled(false);
                probabilityOfXBelowRangeEntryTxt.setEnabled(false);
                probabilityOfXInsideRangeRightEntryTxt.setEnabled(true);
                probabilityOfXOutsideRangeRightEntryTxt.setEnabled(false);

                probabilityOfXInsideRangeLeftEntryTxt.forceFocus();
                probabilityRange = ProbabilityRanges.INNER_RANGE;
            }
        });

        // select the OUTSIDE A RANGE row-widget and deselect the others.
        chooserRangeRdo_2.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                chooserRangeRdo_1.setSelection(false);
                chooserRangeRdo_2.setSelection(true);
                chooserRangeRdo_3.setSelection(false);
                chooserRangeRdo_4.setSelection(false);

                clearProbabilityFields();

                probabilityOfXInsideRangeLeftEntryTxt.setEnabled(false);
                probabilityOfXOutsideRangeLeftEntryTxt.setEnabled(true);
                probabilityOfXAboveRangeEntryTxt.setEnabled(false);
                probabilityOfXBelowRangeEntryTxt.setEnabled(false);
                probabilityOfXInsideRangeRightEntryTxt.setEnabled(false);
                probabilityOfXOutsideRangeRightEntryTxt.setEnabled(true);

                probabilityOfXOutsideRangeLeftEntryTxt.forceFocus();
                probabilityRange = ProbabilityRanges.OUTER_RANGE;

            }
        });

        // select the ABOVE A THRESHOLD and deselect the others.
        chooserRangeRdo_3.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                chooserRangeRdo_1.setSelection(false);
                chooserRangeRdo_2.setSelection(false);
                chooserRangeRdo_3.setSelection(true);
                chooserRangeRdo_4.setSelection(false);

                clearProbabilityFields();

                probabilityOfXInsideRangeLeftEntryTxt.setEnabled(false);
                probabilityOfXOutsideRangeLeftEntryTxt.setEnabled(false);
                probabilityOfXAboveRangeEntryTxt.setEnabled(true);
                probabilityOfXBelowRangeEntryTxt.setEnabled(false);
                probabilityOfXInsideRangeRightEntryTxt.setEnabled(false);
                probabilityOfXOutsideRangeRightEntryTxt.setEnabled(false);

                probabilityOfXAboveRangeEntryTxt.forceFocus();
                probabilityRange = ProbabilityRanges.ABOVE;

            }
        });

        // select the BELOW A THRESHOLD and deselect the others.
        chooserRangeRdo_4.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                chooserRangeRdo_1.setSelection(false);
                chooserRangeRdo_2.setSelection(false);
                chooserRangeRdo_3.setSelection(false);
                chooserRangeRdo_4.setSelection(true);

                clearProbabilityFields();

                probabilityOfXInsideRangeLeftEntryTxt.setEnabled(false);
                probabilityOfXOutsideRangeLeftEntryTxt.setEnabled(false);
                probabilityOfXAboveRangeEntryTxt.setEnabled(false);
                probabilityOfXBelowRangeEntryTxt.setEnabled(true);
                probabilityOfXInsideRangeRightEntryTxt.setEnabled(false);
                probabilityOfXOutsideRangeRightEntryTxt.setEnabled(false);

                probabilityOfXBelowRangeEntryTxt.forceFocus();
                probabilityRange = ProbabilityRanges.BELOW;
            }
        });

    }

    private void addThresholdValueModifyBehavior() {

        probabilityOfXInsideRangeLeftEntryTxt
                .addModifyListener(new ModifyListener() {

                    @Override
                    public void modifyText(ModifyEvent e) {

                        Text text = (Text) e.widget;
                        setProbabilityLowerValue = handleTextEntry(
                                text.getText());
                    }

                });

        probabilityOfXOutsideRangeLeftEntryTxt
                .addModifyListener(new ModifyListener() {

                    @Override
                    public void modifyText(ModifyEvent e) {

                        Text text = (Text) e.widget;
                        setProbabilityLowerValue = handleTextEntry(
                                text.getText());
                    }
                });

        probabilityOfXAboveRangeEntryTxt
                .addModifyListener(new ModifyListener() {

                    @Override
                    public void modifyText(ModifyEvent e) {

                        Text text = (Text) e.widget;
                        setProbabilityLowerValue = handleTextEntry(
                                text.getText());
                    }
                });

        probabilityOfXBelowRangeEntryTxt
                .addModifyListener(new ModifyListener() {

                    @Override
                    public void modifyText(ModifyEvent e) {

                        Text text = (Text) e.widget;
                        setProbabilityLowerValue = handleTextEntry(
                                text.getText());
                    }
                });

        probabilityOfXInsideRangeRightEntryTxt
                .addModifyListener(new ModifyListener() {

                    @Override
                    public void modifyText(ModifyEvent e) {

                        Text text = (Text) e.widget;
                        setProbabilityUpperValue = handleTextEntry(
                                text.getText());
                    }
                });

        probabilityOfXOutsideRangeRightEntryTxt
                .addModifyListener(new ModifyListener() {

                    @Override
                    public void modifyText(ModifyEvent e) {

                        Text text = (Text) e.widget;
                        setProbabilityUpperValue = handleTextEntry(
                                text.getText());
                    }
                });

    }

    protected double handleTextEntry(String entry) {

        if ((entry == null) || (entry.length() == 0)) {
            return 0;
        }
        double value = 0;
        try {
            value = Double.parseDouble(entry);
        } catch (NumberFormatException nfe) {
            MessageDialog.openError(getParentShell().getShell(),
                    "Invalid Number", "Entry must be a valid number.");
            return 0;
        }
        return value;
    }

    // enable the ERF widgets default state
    protected void enableDefaultERFTabWidgetState() {

        clearProbabilityFields();
        ensembleProductNameLbl.setText(ensembleProductName);

        chooserRangeRdo_1.setEnabled(true);
        chooserRangeRdo_2.setEnabled(true);
        chooserRangeRdo_3.setEnabled(true);
        chooserRangeRdo_4.setEnabled(true);

        probabilityOfXInsideRangeLeftEntryTxt.setEnabled(true);
        probabilityOfXOutsideRangeLeftEntryTxt.setEnabled(false);
        probabilityOfXAboveRangeEntryTxt.setEnabled(false);
        probabilityOfXBelowRangeEntryTxt.setEnabled(false);
        probabilityOfXInsideRangeRightEntryTxt.setEnabled(true);
        probabilityOfXOutsideRangeRightEntryTxt.setEnabled(false);

        chooserRangeRdo_1.setSelection(true);
        chooserRangeRdo_2.setSelection(false);
        chooserRangeRdo_3.setSelection(false);
        chooserRangeRdo_4.setSelection(false);

        probabilityOfXInsideRangeLeftEntryTxt.insert("");
        probabilityOfXInsideRangeLeftEntryTxt.forceFocus();

    }

    // clear all user-entered probability field ranges.
    private void clearProbabilityFields() {
        probabilityOfXInsideRangeLeftEntryTxt.setText("");
        probabilityOfXOutsideRangeLeftEntryTxt.setText("");
        probabilityOfXAboveRangeEntryTxt.setText("");
        probabilityOfXBelowRangeEntryTxt.setText("");
        probabilityOfXInsideRangeRightEntryTxt.setText("");
        probabilityOfXOutsideRangeRightEntryTxt.setText("");
    }

    // extract and validate the values from the chosen ERF range

    protected void computeERF() {

        Calculation cal = Calculation.ENSEMBLE_RELATIVE_FREQUENCY;
        if (isImage) {
            cal = Calculation.ENSEMBLE_RELATIVE_FREQUENCY_IMAGE;
        }
        Range range = null;
        switch (probabilityRange) {
        case ABOVE:
            range = new Range(RangeType.ABOVE_THRESHOLD);
            range.setThreshold(setProbabilityLowerValue);
            EnsembleTool.getInstance().calculate(cal, range);
            break;
        case BELOW:
            range = new Range(RangeType.BELOW_THRESHOLD);
            range.setThreshold(setProbabilityLowerValue);
            EnsembleTool.getInstance().calculate(cal, range);
            break;
        case INNER_RANGE:
            range = new Range(RangeType.INNER_RANGE);
            range.setRange(setProbabilityLowerValue, setProbabilityUpperValue);

            EnsembleTool.getInstance().calculate(cal, range);
            break;
        case OUTER_RANGE:
            range = new Range(RangeType.OUTER_RANGE);
            range.setRange(setProbabilityLowerValue, setProbabilityUpperValue);
            EnsembleTool.getInstance().calculate(cal, range);
            break;
        }
    }

    /**
     * Create contents of the button bar.
     * 
     * @param parent
     */

    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        Button computeButton = createButton(parent, IDialogConstants.OK_ID,
                "Compute ERF", true);

        computeButton.addSelectionListener(new SelectionAdapter() {

            public void widgetSelected(SelectionEvent e) {
                computeERF();
            }

        });

        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("ERF Product Constraints");
    }

    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);
        enableDefaultERFTabWidgetState();
        return contents;
    }

}
