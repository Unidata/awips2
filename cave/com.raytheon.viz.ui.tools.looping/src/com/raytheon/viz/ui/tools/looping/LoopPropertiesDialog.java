/**
 * dialogInstance software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * dialogInstance software product contains export-restricted data whose
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

package com.raytheon.viz.ui.tools.looping;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IVizEditorChangedListener;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.datastructure.LoopProperties.LoopMode;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.tools.LoopSpeedManager;

/**
 * Loop properties dialog box
 *
 * <pre>
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 1, 2006              chammack    Initial Creation.
 * Jan 12, 2016 5238        lvenable    Increased label width so the scale labels do not get hidden.
 * Mar 13, 2018 6586        tgurney     Add minimize button
 * Jun 05, 2019 64620       tjensen     Update to line up with performance frame times
 *
 * </pre>
 *
 * @author chammack
 */
public class LoopPropertiesDialog extends CaveJFACEDialog implements
        IRenderableDisplayChangedListener, IVizEditorChangedListener {

    private static LoopPropertiesDialog dialogInstance = null;

    private boolean closed = true;

    private boolean needsUpdate = false;

    private IDisplayPaneContainer container = null;

    public static void openDialog(Shell shell,
            IDisplayPaneContainer container) {
        if (dialogInstance == null) {
            createDialog(shell, container);
        }

        if (dialogInstance.isClosed()) {
            dialogInstance.open();
        }
    }

    public static synchronized void createDialog(Shell shell,
            IDisplayPaneContainer container) {
        if (dialogInstance == null) {
            dialogInstance = new LoopPropertiesDialog(shell, "Loop Properties",
                    container);
            dialogInstance.setBlockOnOpen(false);
        }

        if (container != null) {
            container.addRenderableDisplayChangedListener(dialogInstance);
            VizWorkbenchManager.getInstance().addListener(dialogInstance);
        }
    }

    public static void setLooping(boolean newVal) {
        if (dialogInstance != null) {
            dialogInstance.loopProperties.setLooping(newVal);
            dialogInstance.updateInUIThread();
        }
    }

    private class PageUpDownListener implements KeyListener {
        @Override
        public void keyPressed(KeyEvent e) {
            if (e.keyCode == SWT.PAGE_DOWN || e.keyCode == SWT.PAGE_UP) {
                e.doit = false;
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {
            if (e.keyCode == SWT.PAGE_DOWN) {
                LoopSpeedManager.handleSpeedDown(container);
                update();
            } else if (e.keyCode == SWT.PAGE_UP) {
                LoopSpeedManager.handleSpeedUp(container);
                update();
            }
        }
    }

    private static final int SCALE_WIDTH = 130;

    private static final int LABEL_WIDTH = 60;

    private final String title;

    private Scale fwdLoopSpeedScale;

    private Label fwdLoopSpeedLabel;

    private Scale revLoopSpeedScale;

    private Label revLoopSpeedLabel;

    private Scale firstFrameDwellScale;

    private Label firstFrameDwellLabel;

    private Scale lastFrameDwellScale;

    private Label lastFrameDwellLabel;

    // private IDisplayPaneContainer editor;

    private Button isLoopingButton;

    private LoopProperties loopProperties;

    private final PageUpDownListener keyListener = new PageUpDownListener();

    private LoopPropertiesDialog(Shell parentShell, String dialogTitle,
            IDisplayPaneContainer editor) {
        super(parentShell);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS | SWT.MIN);
        this.title = dialogTitle;
        this.container = editor;
    }

    @Override
    protected void buttonPressed(int buttonId) {
        super.buttonPressed(buttonId);
    }

    private int calculateFrameTime(Scale scale) {
        int value = scale.getSelection();
        if (value == 0) {
            value = LoopProperties.NOT_LOOPING;
        } else {
            value = LoopProperties.MAX_FRAME_TIME
                    - ((value - 1) * LoopProperties.FRAME_STEP);
        }
        return value;
    }

    private int calculateDwellTime(Scale scale) {
        return LoopProperties.MAX_DWELL_TIME
                - (LoopProperties.FRAME_STEP * (scale.getSelection()));
    }

    private int calculateFrameScaleValue(int frameTime) {
        int value = 0;
        if (frameTime != LoopProperties.NOT_LOOPING) {
            value = (LoopProperties.MAX_FRAME_TIME + LoopProperties.FRAME_STEP
                    - frameTime) / LoopProperties.FRAME_STEP;
        }
        return value;
    }

    private int calculateDwellScaleValue(int dwellTime) {
        int value = (LoopProperties.MAX_DWELL_TIME - dwellTime)
                / LoopProperties.FRAME_STEP;
        return value;
    }

    private String formatFrameTime(int value) {
        String rval = "0";
        if (value != LoopProperties.NOT_LOOPING) {
            rval = (double) value / TimeUtil.MILLIS_PER_SECOND + " sec";
        }
        return rval;
    }

    private String formatDwellTime(int val) {
        return (double) val / TimeUtil.MILLIS_PER_SECOND + " sec";
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        return null;
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        composite.setLayout(new GridLayout(3, false));

        Label label = new Label(composite, SWT.BOLD);
        label.setText("Forward Speed: ");

        fwdLoopSpeedScale = new Scale(composite, SWT.HORIZONTAL);
        fwdLoopSpeedScale.setLayoutData(new GridData(SCALE_WIDTH, SWT.DEFAULT));
        fwdLoopSpeedScale.setMinimum(0);
        fwdLoopSpeedScale.setMaximum(
                calculateFrameScaleValue(LoopProperties.FRAME_STEP));
        fwdLoopSpeedScale.setIncrement(1);
        fwdLoopSpeedScale.setPageIncrement(1);

        fwdLoopSpeedLabel = new Label(composite, SWT.NONE);
        fwdLoopSpeedLabel.setLayoutData(new GridData(LABEL_WIDTH, SWT.DEFAULT));
        fwdLoopSpeedScale.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                loopProperties
                        .setFwdFrameTime(calculateFrameTime(fwdLoopSpeedScale));
                update();
            }
        });
        fwdLoopSpeedScale.addKeyListener(keyListener);

        label = new Label(composite, SWT.BOLD);
        label.setText("Backward Speed: ");

        revLoopSpeedScale = new Scale(composite, SWT.NONE);
        revLoopSpeedScale.setLayoutData(new GridData(SCALE_WIDTH, SWT.DEFAULT));
        revLoopSpeedScale.setMinimum(0);
        revLoopSpeedScale.setMaximum(
                calculateFrameScaleValue(LoopProperties.FRAME_STEP));
        revLoopSpeedScale.setIncrement(1);
        revLoopSpeedScale.setPageIncrement(1);

        revLoopSpeedLabel = new Label(composite, SWT.NONE);
        revLoopSpeedLabel.setLayoutData(new GridData(LABEL_WIDTH, SWT.DEFAULT));
        revLoopSpeedScale.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                loopProperties
                        .setRevFrameTime(calculateFrameTime(revLoopSpeedScale));
                update();
            }
        });
        revLoopSpeedScale.addKeyListener(keyListener);

        label = new Label(composite, SWT.BOLD);
        label.setText("First Frame Dwell: ");

        firstFrameDwellScale = new Scale(composite, SWT.NONE);
        firstFrameDwellScale
                .setLayoutData(new GridData(SCALE_WIDTH, SWT.DEFAULT));
        firstFrameDwellScale.setMinimum(
                calculateDwellScaleValue(LoopProperties.MAX_DWELL_TIME));
        firstFrameDwellScale.setMaximum(
                calculateDwellScaleValue(LoopProperties.FRAME_STEP));
        firstFrameDwellScale.setIncrement(1);
        firstFrameDwellScale.setPageIncrement(1);

        firstFrameDwellLabel = new Label(composite, SWT.NONE);
        firstFrameDwellLabel
                .setLayoutData(new GridData(LABEL_WIDTH, SWT.DEFAULT));
        firstFrameDwellScale.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                loopProperties.setFirstFrameDwell(
                        calculateDwellTime(firstFrameDwellScale));
                update();
            }
        });
        firstFrameDwellScale.addKeyListener(keyListener);

        label = new Label(composite, SWT.BOLD);
        label.setText("Last Frame Dwell: ");

        lastFrameDwellScale = new Scale(composite, SWT.NONE);
        lastFrameDwellScale
                .setLayoutData(new GridData(SCALE_WIDTH, SWT.DEFAULT));
        lastFrameDwellScale.setMinimum(
                calculateDwellScaleValue(LoopProperties.MAX_DWELL_TIME));
        lastFrameDwellScale.setMaximum(
                calculateDwellScaleValue(LoopProperties.FRAME_STEP));
        lastFrameDwellScale.setIncrement(1);
        lastFrameDwellScale.setPageIncrement(1);

        lastFrameDwellLabel = new Label(composite, SWT.NONE);
        lastFrameDwellLabel
                .setLayoutData(new GridData(LABEL_WIDTH, SWT.DEFAULT));
        lastFrameDwellScale.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                loopProperties.setLastFrameDwell(
                        calculateDwellTime(lastFrameDwellScale));
                update();
            }
        });
        lastFrameDwellScale.addKeyListener(keyListener);

        isLoopingButton = new Button(composite, SWT.CHECK);
        isLoopingButton.setText("Looping");
        isLoopingButton.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                loopProperties.setLooping(isLoopingButton.getSelection());
                if ((loopProperties.getFwdFrameTime() == 0)
                        && (loopProperties.getRevFrameTime() == 0)) {
                    loopProperties
                            .setFwdFrameTime(LoopProperties.DEFAULT_FRAME_TIME);
                }
                update();
            }
        });
        isLoopingButton.addKeyListener(keyListener);

        composite.addKeyListener(keyListener);

        initDialogControls();

        return composite;
    }

    private void initDialogControls() {

        loopProperties = container.getLoopProperties();

        update();
    }

    private void update() {

        updateFromLoopProps();

        // update the editor

        container.setLoopProperties(loopProperties);
    }

    private void updateFromLoopProps() {
        dialogInstance.needsUpdate = false;

        int fwdFrameTime = loopProperties.getFwdFrameTime();
        fwdLoopSpeedScale.setSelection(calculateFrameScaleValue(fwdFrameTime));
        fwdLoopSpeedLabel.setText(formatFrameTime(fwdFrameTime));

        int revFrameTime = loopProperties.getRevFrameTime();
        revLoopSpeedScale.setSelection(calculateFrameScaleValue(revFrameTime));
        revLoopSpeedLabel.setText(formatFrameTime(revFrameTime));

        int firstFrameDwellTime = loopProperties.getFirstFrameDwell();
        firstFrameDwellScale
                .setSelection(calculateDwellScaleValue(firstFrameDwellTime));
        firstFrameDwellLabel.setText(formatDwellTime(firstFrameDwellTime));

        int lastFrameDwellTime = loopProperties.getLastFrameDwell();
        lastFrameDwellScale
                .setSelection(calculateDwellScaleValue(lastFrameDwellTime));
        lastFrameDwellLabel.setText(formatDwellTime(lastFrameDwellTime));

        // both frame times are 0 turn looping off
        if ((fwdFrameTime == LoopProperties.NOT_LOOPING)
                && (revFrameTime == LoopProperties.NOT_LOOPING)) {
            loopProperties.setLooping(false);

        } else {
            // update the mode based on the frame times
            if ((fwdFrameTime != LoopProperties.NOT_LOOPING)
                    && (revFrameTime == LoopProperties.NOT_LOOPING)) {
                loopProperties.setMode(LoopMode.Forward);
            } else if ((revFrameTime != LoopProperties.NOT_LOOPING)
                    && (fwdFrameTime == LoopProperties.NOT_LOOPING)) {
                loopProperties.setMode(LoopMode.Backward);
            } else {
                loopProperties.setMode(LoopMode.Cycle);
            }
        }

        isLoopingButton.setSelection(loopProperties.isLooping());
    }

    @Override
    public boolean close() {
        dialogInstance.closed = true;
        return super.close();
    }

    @Override
    public int open() {
        dialogInstance.closed = false;
        int rval = super.open();

        if (dialogInstance.needsUpdate) {
            dialogInstance.needsUpdate = false;
            updateInUIThread();
        }
        return rval;
    }

    public boolean isClosed() {
        return dialogInstance.closed;
    }

    private void updateInUIThread() {
        updateInUIThread(false);
    }

    private void updateInUIThread(boolean shouldOpen) {
        if (dialogInstance.isClosed()) {
            dialogInstance.needsUpdate = true;
        } else {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    updateFromLoopProps();
                    // update();
                }
            });
        }
        if (shouldOpen) {
            dialogInstance.open();
        }
    }

    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {

        if (DisplayChangeType.ADD == type) {
            dialogInstance.loopProperties = dialogInstance.container
                    .getLoopProperties();
            updateInUIThread();
        }
    }

    @Override
    public void editorChanged(IDisplayPaneContainer container) {

        dialogInstance.container
                .removeRenderableDisplayChangedListener(dialogInstance);

        if (container == null) {
            // close dialog
            VizWorkbenchManager.getInstance().removeListener(dialogInstance);
            if (!dialogInstance.isClosed()) {
                dialogInstance.close();
            }
            synchronized (LoopProperties.class) {
                dialogInstance = null;
            }
        } else {
            // refresh dialog
            container.addRenderableDisplayChangedListener(dialogInstance);
            dialogInstance.container = container;
            dialogInstance.loopProperties = dialogInstance.container
                    .getLoopProperties();
            updateInUIThread();
        }
    }
}
