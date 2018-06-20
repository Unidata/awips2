package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;

/***
 * 
 * A control which allows the user to load additional frames of resources. This
 * widget allows the user to control the number of frames to load and displays
 * the anticipated memory usage after the load occurs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2016  13211      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class LoadControlStatusComposite extends Composite {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LoadControlStatusComposite.class);

    private static final String TOOLTIP_MEMORY_PER_LOAD = "Approx memory usage per load";

    /**
     * This tooltip (and more importantly, the text widget, needs to display the
     * total memory used my the matrix navigator.
     * 
     * private static final String TOOLTIP_TOTAL_MEMORY_USED =
     * "Total memory used by Matrix Navigator";
     * 
     */
    private static final String TOOLTIP_TOTAL_MEMORY_USED = "Total memory available";

    private Composite innerRootComposite = null;

    private Label maxFramesToLoadLbl = null;

    private Spinner maxFramesToLoadChooser = null;

    private String maxFramesToTooltip = null;

    private Label memoryPerFrameLbl = null;

    private Text memoryPerFrameTxt = null;

    private Label totalMatrixMemoryLbl = null;

    private Text totalMatrixMemoryTxt = null;

    private IMatrixEditorFocusProvider focusProvider = null;

    public LoadControlStatusComposite(Composite parent,
            IMatrixEditorFocusProvider mefp) {
        super(parent, SWT.BORDER);
        createContents();
        focusProvider = mefp;
    }

    /**
     * Creates the contents of this loader control.
     */
    private void createContents() {

        configureRootArea();

        createLoaderControls();

        pack();
    }

    /**
     * Configures the layout and layout data for the control's root area.
     */
    private void configureRootArea() {

        setLayoutData(new GridData(SWT.FILL, SWT.BOTTOM, true, false, 1, 1));
        GridLayout thisComposite_gl = new GridLayout(1, false);
        thisComposite_gl.marginWidth = 2;
        thisComposite_gl.marginHeight = 2;
        setLayout(thisComposite_gl);

        innerRootComposite = new Composite(this, SWT.NONE);
        innerRootComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true,
                false, 1, 1));
        GridLayout innerRootComposite_gl = new GridLayout(7, false);
        innerRootComposite_gl.marginWidth = 1;
        innerRootComposite_gl.marginHeight = 1;
        innerRootComposite.setLayout(innerRootComposite_gl);

    }

    /**
     * Creates the working contents of this control widget
     */
    private void createLoaderControls() {

        final Font labelFont = SWTResourceManager.getFont("Arial", 8, SWT.NONE);
        final Font valueFont = SWTResourceManager.getFont("Courier", 10,
                SWT.BOLD);
        /**
         * Container for the maximum frames to load widget.
         */
        Composite framesToLoadComposite = new Composite(innerRootComposite,
                SWT.BORDER);
        framesToLoadComposite.setLayoutData(new GridData(SWT.LEFT, SWT.TOP,
                false, false, 2, 1));
        GridLayout framesToLoadComposite_gl = new GridLayout(3, false);
        framesToLoadComposite_gl.marginLeft = 4;
        framesToLoadComposite_gl.marginHeight = 4;
        framesToLoadComposite.setLayout(framesToLoadComposite_gl);

        maxFramesToLoadLbl = new Label(framesToLoadComposite, SWT.NONE);
        maxFramesToLoadLbl.setFont(labelFont);
        maxFramesToLoadLbl.setText("Max Frame:");
        GridData framesToLoadLbl_gd = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 2, 1);
        maxFramesToLoadLbl.setLayoutData(framesToLoadLbl_gd);

        final int digitPrecision = 0;
        maxFramesToLoadChooser = new Spinner(framesToLoadComposite, SWT.BORDER);
        maxFramesToLoadChooser.setValues(
                MatrixNavigatorComposite.defaultMaxFrameCount,
                MatrixNavigatorComposite.defaultMaxFrameCount,
                MatrixNavigatorComposite.maxFrameCount, digitPrecision, 1, 8);
        GridData frameCountChooser_gd = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 1, 1);
        frameCountChooser_gd.widthHint = 20;
        maxFramesToLoadChooser.setLayoutData(frameCountChooser_gd);
        maxFramesToLoadChooser.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {

                setLoadFramesToolTip();
                focusProvider.giveEditorFocus();

            }

        });

        /**
         * Container for the memory statistics
         */
        Composite memoryStatsComposite = new Composite(innerRootComposite,
                SWT.BORDER);
        memoryStatsComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP,
                true, false, 5, 1));
        GridLayout memoryStatsComposite_gl = new GridLayout(9, false);
        memoryStatsComposite_gl.marginLeft = 4;
        memoryStatsComposite_gl.marginHeight = 4;
        memoryStatsComposite.setLayout(memoryStatsComposite_gl);

        memoryPerFrameLbl = new Label(memoryStatsComposite, SWT.NONE);
        memoryPerFrameLbl.setFont(labelFont);
        memoryPerFrameLbl.setText("Per Frame:");
        memoryPerFrameLbl.setToolTipText(TOOLTIP_MEMORY_PER_LOAD);
        GridData memoryPerFrameLbl_gd = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        memoryPerFrameLbl.setLayoutData(memoryPerFrameLbl_gd);

        memoryPerFrameTxt = new Text(memoryStatsComposite, SWT.BORDER
                | SWT.CENTER);
        memoryPerFrameTxt.setEditable(false);
        memoryPerFrameTxt.setFont(valueFont);
        memoryPerFrameTxt.setBackground(GlobalColor
                .get(GlobalColor.PALE_WEAK_BLUE));
        memoryPerFrameTxt.setToolTipText(TOOLTIP_MEMORY_PER_LOAD);
        GridData memoryPerFrameTxt_gd = new GridData(SWT.FILL, SWT.CENTER,
                true, false, 2, 1);
        memoryPerFrameTxt_gd.widthHint = 55;
        memoryPerFrameTxt.setLayoutData(memoryPerFrameTxt_gd);

        /* horizontal separator */
        Label separator_1 = new Label(memoryStatsComposite, SWT.None);
        GridData separator1_gd = new GridData(SWT.FILL, SWT.CENTER, false,
                false, 1, 1);
        separator1_gd.widthHint = 2;
        separator_1.setLayoutData(separator1_gd);

        totalMatrixMemoryLbl = new Label(memoryStatsComposite, SWT.NONE);
        totalMatrixMemoryLbl.setFont(labelFont);
        totalMatrixMemoryLbl.setText("Total:");
        totalMatrixMemoryLbl.setToolTipText(TOOLTIP_TOTAL_MEMORY_USED);
        GridData totalMatrixMemoryLbl_gd = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 2, 1);
        totalMatrixMemoryLbl.setLayoutData(totalMatrixMemoryLbl_gd);

        totalMatrixMemoryTxt = new Text(memoryStatsComposite, SWT.BORDER
                | SWT.CENTER);
        totalMatrixMemoryTxt.setEditable(false);
        totalMatrixMemoryTxt.setFont(valueFont);
        totalMatrixMemoryTxt.setBackground(GlobalColor
                .get(GlobalColor.PALE_WEAK_BLUE));
        totalMatrixMemoryTxt.setToolTipText(TOOLTIP_TOTAL_MEMORY_USED);
        GridData totalMatrixMemoryTxt_gd = new GridData(SWT.FILL, SWT.CENTER,
                true, false, 2, 1);
        totalMatrixMemoryTxt_gd.widthHint = 55;
        totalMatrixMemoryTxt.setLayoutData(totalMatrixMemoryTxt_gd);

    }

    public void setEnabled(boolean isEnabled) {

        maxFramesToLoadChooser.setEnabled(isEnabled);
        maxFramesToLoadLbl.setEnabled(isEnabled);
        memoryPerFrameLbl.setEnabled(isEnabled);
        memoryPerFrameTxt.setEnabled(isEnabled);
        totalMatrixMemoryLbl.setEnabled(isEnabled);
        totalMatrixMemoryTxt.setEnabled(isEnabled);

        setLoadFramesToolTip();
    }

    public void setMemoryUsage(long bytes) {

        if (bytes <= 0) {
            return;
        }

        final String memoryUsagePerFrameInHumanReadableForm = Utilities
                .bytesIntoHumanReadable(bytes);

        System.gc();

        final String maxMemoryInHumanReadableForm = Utilities
                .bytesIntoHumanReadable(Runtime.getRuntime().totalMemory());

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                memoryPerFrameTxt
                        .setText(memoryUsagePerFrameInHumanReadableForm);
                totalMatrixMemoryTxt.setText(maxMemoryInHumanReadableForm);
            }
        });
    }

    private void setLoadFramesToolTip() {

        maxFramesToTooltip = "Maximum of "
                + maxFramesToLoadChooser.getSelection() + " frames to load.";
        maxFramesToLoadLbl.setToolTipText(maxFramesToTooltip);
        maxFramesToLoadChooser.setToolTipText(maxFramesToTooltip);

    }

    public int getMaxFrameCount() {
        int maxFrameCount = 8;
        if (maxFramesToLoadChooser != null) {
            maxFrameCount = maxFramesToLoadChooser.getSelection();
        }
        return maxFrameCount;
    }

    public void clearAll() {
        memoryPerFrameTxt.setText("");
        totalMatrixMemoryTxt.setText("");
    }

}
