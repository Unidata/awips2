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
package com.raytheon.uf.viz.d2d.ui.dialogs;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.d2d.core.map.MapScales;
import com.raytheon.uf.viz.d2d.core.map.MapScales.MapScale;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.ui.DensityPopulator;
import com.raytheon.uf.viz.d2d.ui.MagnificationPopulator;
import com.raytheon.uf.viz.d2d.ui.actions.DensityHandler;
import com.raytheon.uf.viz.d2d.ui.actions.LoadModeHandler;
import com.raytheon.uf.viz.d2d.ui.actions.MagHandler;
import com.raytheon.uf.viz.d2d.ui.map.actions.ScaleHandler;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.FramesHandler;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog for changing display properties.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            bgonzale     Initial creation
 * Oct 16, 2012 1229       rferrel     Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class DisplayPropertiesDialog extends CaveSWTDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DisplayPropertiesDialog.class);

    private Combo scale;

    private ScaleHandler sHandler;

    private Combo loadMode;

    private LoadModeHandler lmHandler;

    private Combo frames;

    private FramesHandler framesHandler;

    private Combo magnification;

    private MagHandler magHandler;

    private Combo density;

    private DensityHandler densityHandler;

    private Combo lineWidth;

    private Combo lineStyle;

    private Map<String, LineStyle> styleMap = new HashMap<String, LineStyle>();

    private IGlobalChangedListener densityListener = new IGlobalChangedListener() {
        @Override
        public void updateValue(IWorkbenchWindow changedWindow, Object value) {
            Double d = (Double) value;
            if (d != null) {
                density.setText(DensityPopulator.getLabelFor(d));
            }
        }
    };

    private IGlobalChangedListener magListener = new IGlobalChangedListener() {
        @Override
        public void updateValue(IWorkbenchWindow changedWindow, Object value) {
            Double m = (Double) value;
            if (m != null) {
                magnification.setText(MagnificationPopulator.getLabelFor(m));
            }
        }
    };

    private IGlobalChangedListener loadModeListener = new IGlobalChangedListener() {
        @Override
        public void updateValue(IWorkbenchWindow changedWindow, Object value) {
            LoadMode lm = (LoadMode) value;
            if (lm != null) {
                loadMode.setText(lm.getLabel());
            }
        }
    };

    private IGlobalChangedListener scaleListener = new IGlobalChangedListener() {
        @Override
        public void updateValue(IWorkbenchWindow changedWindow, Object value) {
            String scale = String.valueOf(value);
            if (scale != null) {
                DisplayPropertiesDialog.this.scale.setText(scale);
            }
        }
    };

    private IGlobalChangedListener framesListener = new IGlobalChangedListener() {
        @Override
        public void updateValue(IWorkbenchWindow changedWindow, Object value) {
            Integer frames = (Integer) value;
            if (frames != null) {
                DisplayPropertiesDialog.this.frames.setText(frames.toString());
            }
        }
    };

    /**
     * 
     * @param parentShell
     * @param title
     * @param editor
     */
    public DisplayPropertiesDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);
        setText("Display Properties");

        this.sHandler = new ScaleHandler();
        this.lmHandler = new LoadModeHandler();
        this.framesHandler = new FramesHandler();
        this.magHandler = new MagHandler();
        this.densityHandler = new DensityHandler();

        // Add listeners
        VizGlobalsManager.addListener(VizConstants.SCALE_ID, scaleListener);
        VizGlobalsManager.addListener(VizConstants.LOADMODE_ID,
                loadModeListener);
        VizGlobalsManager.addListener(VizConstants.FRAMES_ID, framesListener);
        VizGlobalsManager.addListener(VizConstants.DENSITY_ID, densityListener);
        VizGlobalsManager.addListener(VizConstants.MAGNIFICATION_ID,
                magListener);
    }

    @Override
    protected void disposed() {
        // remove listeners
        VizGlobalsManager.removeListener(VizConstants.SCALE_ID, scaleListener);
        VizGlobalsManager.removeListener(VizConstants.LOADMODE_ID,
                loadModeListener);
        VizGlobalsManager
                .removeListener(VizConstants.FRAMES_ID, framesListener);
        VizGlobalsManager.removeListener(VizConstants.DENSITY_ID,
                densityListener);
        VizGlobalsManager.removeListener(VizConstants.MAGNIFICATION_ID,
                magListener);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite composite = new Composite(shell, SWT.NONE);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 5;
        gl.marginWidth = 5;
        composite.setLayout(gl);
        setupScaleWidget(composite);
        setupLoadModeWidget(composite);
        setupFramesWidget(composite);
        setupMagnificationWidget(composite);
        setupDensityWidget(composite);
        setupLineWidthWidget(composite);
        setupLineStyleWidget(composite);
    }

    private Object getCurrentValue(String key) {
        return VizGlobalsManager.getCurrentInstance().getPropery(key);
    }

    /**
     * Executes the handler with the given key/pair combo
     * 
     * @param handler
     * @param key
     * @param value
     */
    private void executeHandler(AbstractHandler handler, String key,
            String value) {
        Map<String, String> params = new HashMap<String, String>();
        params.put(key, value);
        ExecutionEvent eEvent = new ExecutionEvent(null, params, null, null);
        try {
            handler.execute(eEvent);
        } catch (ExecutionException e) {
            statusHandler.handle(Priority.PROBLEM,
                    String.format("Error executing %s handler", key), e);
        }
    }

    private void setupScaleWidget(Composite parent) {
        Listener listener = new Listener() {
            public void handleEvent(Event event) {
                executeHandler(sHandler, "scale", scale.getText());
            }
        };
        int maxSize = 0;
        MapScale[] mScales = MapScales.getInstance().getScales();
        String[] scales = new String[mScales.length];
        for (int i = 0; i < scales.length; ++i) {
            scales[i] = mScales[i].getDisplayName();
            if (scales[i].length() > maxSize) {
                maxSize = scales[i].length();
            }
        }

        scale = createComboBox(parent, "Scale:", scales,
                String.valueOf(getCurrentValue(VizConstants.SCALE_ID)),
                listener);
    }

    private void setupLoadModeWidget(Composite parent) {
        Listener listener = new Listener() {
            public void handleEvent(Event event) {
                executeHandler(lmHandler, "loadMode",
                        LoadMode.valueOfLabel(loadMode.getText()).toString());
            }
        };

        LoadMode curMode = (LoadMode) getCurrentValue(VizConstants.LOADMODE_ID);
        if (curMode == null) {
            curMode = LoadMode.VALID_TIME_SEQ;
        }
        loadMode = createComboBox(parent, "Load Mode:", LoadMode.labels(),
                curMode.getLabel(), listener);
    }

    private void setupFramesWidget(Composite parent) {
        Listener listener = new Listener() {
            public void handleEvent(Event event) {
                executeHandler(framesHandler, "frameCount", frames.getText());
            }
        };
        // TODO this is not how the current systems frame count selection menu
        // looks
        String[] frameArr = new String[64];
        for (int i = 1; i < 65; ++i) {
            frameArr[i - 1] = Integer.toString(i);
        }
        frames = createComboBox(parent, "Frames:", frameArr,
                String.valueOf(getCurrentValue(VizConstants.FRAMES_ID)),
                listener);
    }

    private void setupMagnificationWidget(Composite parent) {
        Listener listener = new Listener() {
            public void handleEvent(Event event) {
                executeHandler(magHandler, "magnification",
                        magnification.getText());
            }
        };
        magnification = createComboBox(
                parent,
                "Mag:",
                MagnificationPopulator.getMagnifications(),
                MagnificationPopulator
                        .getLabelFor((Double) getCurrentValue(VizConstants.MAGNIFICATION_ID)),
                listener);
    }

    private void setupDensityWidget(Composite parent) {
        Listener listener = new Listener() {
            public void handleEvent(Event event) {
                executeHandler(densityHandler, "density",
                        String.valueOf(DensityPopulator.getValueFor(density
                                .getText())));
            }
        };

        density = createComboBox(
                parent,
                "Density:",
                DensityPopulator.getDensityLabels(),
                DensityPopulator
                        .getLabelFor((Double) getCurrentValue(VizConstants.DENSITY_ID)),
                listener);
    }

    private void setupLineWidthWidget(Composite parent) {
        Listener listener = new Listener() {
            public void handleEvent(Event event) {
                String text = lineWidth.getText();
                int width = 1;
                try {
                    width = Integer.parseInt(text);
                } catch (Throwable t) {

                }
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    for (IDisplayPane pane : container.getDisplayPanes()) {
                        IDescriptor desc = pane.getDescriptor();
                        if (desc != null) {
                            List<OutlineCapability> capabilities = getCapabilities(
                                    desc.getResourceList(),
                                    OutlineCapability.class);
                            for (OutlineCapability cap : capabilities) {
                                cap.setOutlineWidth(width);
                            }
                        }
                        pane.refresh();
                    }
                }
            }
        };

        lineWidth = createComboBox(parent, "Line Width:", new String[] { "1",
                "2", "3", "4" }, "1", listener);
    }

    private void setupLineStyleWidget(Composite parent) {
        Listener listener = new Listener() {
            public void handleEvent(Event event) {
                LineStyle style = styleMap.get(lineStyle.getText());
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    for (IDisplayPane pane : container.getDisplayPanes()) {
                        IDescriptor desc = pane.getDescriptor();
                        if (desc != null) {
                            List<OutlineCapability> capabilities = getCapabilities(
                                    desc.getResourceList(),
                                    OutlineCapability.class);
                            for (OutlineCapability cap : capabilities) {
                                cap.setLineStyle(style);
                            }
                        }
                        pane.refresh();
                    }
                }
            }
        };
        LineStyle[] styles = LineStyle.values();
        String[] values = new String[styles.length];
        for (int i = 0; i < values.length; ++i) {
            String label = styles[i].toString().toLowerCase();
            String[] parts = label.split("[_]");
            label = "";
            for (String part : parts) {
                char[] chars = part.toCharArray();
                chars[0] = Character.toUpperCase(chars[0]);
                label += " " + new String(chars);
            }
            values[i] = label.trim();
            styleMap.put(values[i], styles[i]);
        }

        lineStyle = createComboBox(parent, "Line Style:", values, values[0],
                listener);
    }

    private Combo createComboBox(Composite parent, String title,
            String[] items, String selected, Listener listener) {
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        Label label = new Label(parent, SWT.BOLD);
        label.setText(title);

        int maxSize = 0;
        for (String item : items) {
            if (item.length() > maxSize) {
                maxSize = item.length();
            }
        }

        gd.widthHint = maxSize * 8 + 38;

        Combo combo = new Combo(parent, SWT.READ_ONLY);
        combo.setLayoutData(gd);
        combo.setItems(items);
        combo.setText(selected);
        combo.addListener(SWT.Selection, listener);
        return combo;
    }

    /**
     * Get the list of capabilities to change given the class and descriptor
     * 
     * @param descriptor
     * @param capability
     * @return
     */
    private <T extends AbstractCapability> List<T> getCapabilities(
            ResourceList list, Class<T> capability) {
        List<T> capabilities = new ArrayList<T>();
        for (ResourcePair rp : list) {
            if (rp.getResource() != null) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc.hasCapability(capability)) {
                    capabilities.add(rsc.getCapability(capability));
                }
                if (rsc instanceof IResourceGroup) {
                    capabilities.addAll(getCapabilities(
                            ((IResourceGroup) rsc).getResourceList(),
                            capability));
                }
            }
        }
        return capabilities;
    }
}
