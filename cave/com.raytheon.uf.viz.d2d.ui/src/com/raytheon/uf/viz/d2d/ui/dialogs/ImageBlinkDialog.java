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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.IPaintListener;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.d2d.core.map.D2DMapRenderableDisplay;
import com.raytheon.uf.viz.d2d.ui.actions.BlinkToggleAction;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.colordialog.ColorBar;
import com.raytheon.viz.ui.dialogs.colordialog.ColorData;
import com.raytheon.viz.ui.dialogs.colordialog.IColorBarAction;

/**
 * This is a dialog to determine what range of a rendered display should blink.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2010            mschenke     Initial creation
 * Oct 16, 2012 1229       rferrel     Updated to use bringToTop to
 *                                      activate existing dialog.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ImageBlinkDialog extends CaveSWTDialog implements
        IDisposeListener, IPaintListener, IColorBarAction {

    private ColorBar colorBar;

    private float[] blinkRates;

    private float rate;

    private ColorMapParameters cmapParams;

    private AbstractVizResource<?, ?> resource;

    private D2DMapRenderableDisplay[] displays = null;

    private byte[] originalAlphaMask;

    private boolean originalBlinkSetting;

    private ResourceProperties rscProps;

    private boolean lastUseMaskState;

    private static Map<AbstractVizResource<?, ?>, ImageBlinkDialog> dialogMap = new HashMap<AbstractVizResource<?, ?>, ImageBlinkDialog>();

    /**
     * If needed creates an instance of this dialog and associates it with the
     * resource and opens the dialog or activates the dialog if one already
     * exists for the resource.
     * 
     * @param rates
     * @param currRate
     * @param resource
     * @param rscProps
     * @param displays
     */
    public static synchronized void openDialog(float[] rates, float currRate,
            AbstractVizResource<?, ?> resource, ResourceProperties rscProps,
            D2DMapRenderableDisplay[] displays) {
        ImageBlinkDialog dlg = dialogMap.get(resource);
        if (dlg == null || dlg.getShell() == null || dlg.isDisposed()) {
            dlg = new ImageBlinkDialog(new Shell(Display.getCurrent()), rates,
                    currRate, resource, rscProps, displays);
            dialogMap.put(resource, dlg);
            dlg.open();
        } else {
            dlg.bringToTop();
        }
    }

    /**
     * Removes the dialog associated with the resource.
     * 
     * @param resource
     */
    public static synchronized void removeDialog(
            AbstractVizResource<?, ?> resource) {
        dialogMap.remove(resource);
    }

    /**
     * @param parentShell
     */
    private ImageBlinkDialog(Shell parentShell, float[] rates, float currRate,
            AbstractVizResource<?, ?> resource, ResourceProperties rscProps,
            D2DMapRenderableDisplay[] displays) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        this.blinkRates = rates;
        this.cmapParams = resource.getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        this.lastUseMaskState = cmapParams.isUseMask();
        this.rate = currRate;
        this.displays = displays;
        this.resource = resource;
        this.rscProps = rscProps;
        this.originalBlinkSetting = rscProps.isBlinking();
        this.originalAlphaMask = Arrays.copyOf(cmapParams.getAlphaMask(),
                cmapParams.getAlphaMask().length);

        resource.registerListener((IDisposeListener) this);
        resource.registerListener((IPaintListener) this);
        setText(resource.getName());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialog#initializeComponents(org.eclipse
     * .swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                removeDialog(resource);
                resource.unregisterListener((IDisposeListener) ImageBlinkDialog.this);
                resource.unregisterListener((IPaintListener) ImageBlinkDialog.this);
            }
        });
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        // gd.widthHint = 550;
        mainComp.setLayoutData(gd);

        createBlinkSelection(mainComp);
        createColorbar(mainComp);
        createBottomButtons(mainComp);
    }

    /**
     * @param mainComp
     */
    private void createBlinkSelection(Composite mainComp) {
        Composite comp = new Composite(mainComp, SWT.NONE);
        comp.setLayout(new GridLayout(2, true));
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        comp.setLayoutData(gd);

        Label label = new Label(comp, SWT.RIGHT);
        label.setText("Blink Rate (seconds):");

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        gd.widthHint = 150;
        label.setLayoutData(gd);

        final Combo cbo = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (float f : blinkRates) {
            cbo.add("" + f);
        }

        cbo.add(BlinkToggleAction.NO_BLINK);

        cbo.setText(rate == BlinkToggleAction.NO_BLINK_VALUE ? BlinkToggleAction.NO_BLINK
                : "" + rate);

        cbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int idx = cbo.getSelectionIndex();
                if (idx < blinkRates.length) {
                    setRate(blinkRates[cbo.getSelectionIndex()]);
                } else {
                    setRate(BlinkToggleAction.NO_BLINK_VALUE);
                    rscProps.setBlinking(false);
                    cmapParams.setUseMask(false);
                }
            }
        });

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = 80;
        cbo.setLayoutData(gd);
    }

    /**
     * @param mainComp
     */
    private void createColorbar(Composite mainComp) {
        Composite cbComp = new Composite(mainComp, SWT.NONE);
        cbComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        cbComp.setLayoutData(gd);

        colorBar = new ColorBar(cbComp, this, cmapParams, true);
    }

    /**
     * @param mainComp
     */
    private void createBottomButtons(Composite mainComp) {
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        GridLayout layout = new GridLayout(5, false);
        layout.horizontalSpacing = 10;
        buttonComp.setLayout(layout);
        GridData gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        buttonComp.setLayoutData(gd);

        Button b = new Button(buttonComp, SWT.PUSH);
        b.setText("Enable Range");
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                enableRange();
            }
        });
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 105;
        b.setLayoutData(gd);

        b = new Button(buttonComp, SWT.PUSH);
        b.setText("Disable Range");
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                disableRange();
            }
        });
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 105;
        b.setLayoutData(gd);

        b = new Button(buttonComp, SWT.PUSH);
        b.setText("Reset");
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                reset();
            }
        });
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 65;
        b.setLayoutData(gd);

        b = new Button(buttonComp, SWT.PUSH);
        b.setText("OK");
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ok();
            }
        });
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 65;
        b.setLayoutData(gd);

        b = new Button(buttonComp, SWT.PUSH);
        b.setText("Cancel");
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                cancel();
            }
        });
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 65;
        b.setLayoutData(gd);
    }

    private void setRate(float rate) {
        this.rate = rate;
        for (D2DMapRenderableDisplay display : displays) {
            display.setBlinkInterval((long) (rate * 1000));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IDisposeListener#diposed(com.raytheon.uf
     * .viz.core.rsc.AbstractVizResource)
     */
    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {
        close();
    }

    private void enableRange() {
        Point range = colorBar.getSliderRange();
        byte[] mask = cmapParams.getAlphaMask();
        for (int i = range.x; i <= range.y; ++i) {
            mask[i] = 1;
        }
        rscProps.setBlinking(true);
        cmapParams.setAlphaMask(mask);
        cmapParams.setUseMask(true);
        colorBar.repaintColorbars();
        resource.issueRefresh();
    }

    private void disableRange() {
        Point range = colorBar.getSliderRange();
        byte[] mask = cmapParams.getAlphaMask();
        for (int i = range.x; i <= range.y; ++i) {
            mask[i] = 0;
        }
        boolean blinking = false;
        for (int i = 0; i < mask.length; ++i) {
            if (mask[i] != 0) {
                blinking = true;
                break;
            }
        }
        rscProps.setBlinking(blinking);
        cmapParams.setUseMask(blinking);
        cmapParams.setAlphaMask(mask);
        colorBar.repaintColorbars();
        resource.issueRefresh();
    }

    private void reset() {
        cmapParams.setAlphaMask(new byte[cmapParams.getColorMap().getSize()]);
        rscProps.setBlinking(false);
        cmapParams.setUseMask(false);
        colorBar.repaintColorbars();
        resource.issueRefresh();
    }

    private void ok() {
        close();
        resource.issueRefresh();
    }

    private void cancel() {
        cmapParams.setAlphaMask(Arrays.copyOf(originalAlphaMask,
                originalAlphaMask.length));
        rscProps.setBlinking(originalBlinkSetting);
        cmapParams.setUseMask(originalBlinkSetting);
        close();
        resource.issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.colordialog.IColorBarAction#updateColor(com
     * .raytheon.viz.ui.dialogs.colordialog.ColorData, boolean)
     */
    @Override
    public void updateColor(ColorData colorData, boolean upperFlag) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IPaintListener#painted(com.raytheon.uf.viz
     * .core.rsc.AbstractVizResource)
     */
    @Override
    public void painted(AbstractVizResource<?, ?> resource) {
        if (cmapParams.isUseMask() != lastUseMaskState) {
            lastUseMaskState = cmapParams.isUseMask();
            colorBar.repaint();
        }
    }
}
