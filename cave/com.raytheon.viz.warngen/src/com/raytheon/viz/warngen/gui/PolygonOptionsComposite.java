package com.raytheon.viz.warngen.gui;

import javax.measure.converter.UnitConverter;

import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.value.WritableValue;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.warngen.gui.WarngenLayer.ExtensionAreaOptions;

/**
 * GUI for advanced WarnGen options.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * 12/21/2015   DCS 17942  D. Friedman  Initial revision
 * 03/10/2016   DCS 18509  D. Friedman  Make extension area display a separate map background.
 * 03/22/2016   DCS 18719  D. Friedman  Add dynamic extension area option.
 * </pre>
 *
 */
public class PolygonOptionsComposite extends Composite {
    private WarngenLayer warngenLayer;

    private Button allowExtendedPolygonButton;
    private Text extensionDistanceText;
    private Text extensionSimplificationToleranceText;
    private Button visualizeExtensionButton;
    private Button dynamicExtensionButton;

    private WritableValue observableOptions;
    private boolean ignoreControls;
    private boolean ignoreOptions;

    public PolygonOptionsComposite(Composite parent, WarngenLayer warngenLayer) {
        super(parent, SWT.NONE);
        this.warngenLayer = warngenLayer;
        observableOptions = warngenLayer.getObservableExtensionAreaOptions();
        createControls();
    }

    private void createControls() {
        GridLayout gl = new GridLayout();
        gl.numColumns = 2;
        setLayout(gl);

        Label label;

        GridData textGD = new GridData();
        textGD.horizontalAlignment = GridData.FILL;
        textGD.grabExcessHorizontalSpace = true;

        GridData fillGD = new GridData();
        fillGD.horizontalAlignment = GridData.FILL;
        fillGD.grabExcessHorizontalSpace = true;
        fillGD.horizontalSpan = 2;

        label = new Label(this, SWT.CENTER);
        label.setText("Extension Area Options");
        label.setLayoutData(fillGD);

        allowExtendedPolygonButton = new Button(this, SWT.CHECK);
        allowExtendedPolygonButton.setText("Allow polygon to extend past valid hatching area");
        allowExtendedPolygonButton.setLayoutData(fillGD);
        allowExtendedPolygonButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (ignoreControls) {
                    return;
                }

                ExtensionAreaOptions options = getExtensionAreaOptions().clone();
                options.setEnabled(allowExtendedPolygonButton.getSelection());
                setOptions(options);
            }
        });

        visualizeExtensionButton = new Button(this, SWT.CHECK);
        visualizeExtensionButton.setText("Show extension area");
        visualizeExtensionButton.setLayoutData(fillGD);
        visualizeExtensionButton.setSelection(warngenLayer.isExtensionAreaVisible());
        visualizeExtensionButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (ignoreControls) {
                    return;
                }

                warngenLayer.setExtensionAreaVisualized(visualizeExtensionButton.getSelection());
            }
        });
        warngenLayer.getObservableExtensionAreaVisible().addChangeListener(new IChangeListener() {
            @Override
            public void handleChange(ChangeEvent event) {
                if (!visualizeExtensionButton.isDisposed()) {
                    visualizeExtensionButton.setSelection(warngenLayer.isExtensionAreaVisible());
                }
            }
        });

        dynamicExtensionButton = new Button(this, SWT.CHECK);
        dynamicExtensionButton.setText("Dynamic extension area");
        dynamicExtensionButton.setLayoutData(fillGD);
        dynamicExtensionButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (ignoreControls) {
                    return;
                }

                ExtensionAreaOptions options = getExtensionAreaOptions().clone();
                options.setDynamicArea(dynamicExtensionButton.getSelection());
                setOptions(options);
            }
        });

        label = new Label(this, SWT.LEFT);
        label.setText("Extension distance (mi)");
        extensionDistanceText = new Text(this, SWT.LEFT | SWT.SINGLE | SWT.BORDER);
        extensionDistanceText.setLayoutData(textGD);
        new DistanceController() {
            @Override
            void setValue(double value) {
                if (ignoreControls) {
                    return;
                }

                ExtensionAreaOptions options = getExtensionAreaOptions().clone();
                options.setDistance(value);
                setOptions(options);
            }
        }.setControl(extensionDistanceText);

        label = new Label(this, SWT.LEFT);
        label.setText("Simplification tolerance (mi)");
        extensionSimplificationToleranceText = new Text(this, SWT.LEFT | SWT.SINGLE | SWT.BORDER);
        extensionSimplificationToleranceText.setLayoutData(textGD);
        new DistanceController() {
            @Override
            void setValue(double value) {
                if (ignoreControls) {
                    return;
                }

                ExtensionAreaOptions options = getExtensionAreaOptions().clone();
                options.setSimplificationTolerance(value);
                setOptions(options);
            }
            @Override
            public boolean validate(double value) {
                return value >= WarngenLayer.ExtensionAreaOptions.MINIMUM_SIMPLIFICATION_TOLERANCE;
            }
        }.setControl(extensionSimplificationToleranceText);

        realizeExtensionAreaOptions();

        observableOptions.addChangeListener(new IChangeListener() {
            @Override
            public void handleChange(ChangeEvent event) {
                if (! ignoreOptions) {
                    realizeExtensionAreaOptions();
                }
            }
        });
    }

    private void setOptions(ExtensionAreaOptions options) {
        ignoreOptions = true;
        try {
            observableOptions.setValue(options);
        } finally {
            ignoreOptions = false;
        }
    }

    private void realizeExtensionAreaOptions() {
        UnitConverter metersToMile = WarngenLayer.MILES_TO_METER.inverse();
        ExtensionAreaOptions options = getExtensionAreaOptions();

        ignoreControls = true;
        try {
            allowExtendedPolygonButton.setSelection(options.isEnabled());
            dynamicExtensionButton.setSelection(options.isDynamicArea());
            extensionDistanceText.setText(Double.toString(
                    metersToMile.convert(options.getDistance())));
            extensionSimplificationToleranceText.setText(Double.toString(
                    metersToMile.convert(options.getSimplificationTolerance())));
        } finally {
            ignoreControls = false;
        }
    }

    private ExtensionAreaOptions getExtensionAreaOptions() {
        return (ExtensionAreaOptions) observableOptions.getValue();
    }

    private static abstract class DistanceController implements ModifyListener {
        Text text;
        public DistanceController() {
        }
        void setControl(Text text) {
            text.setTextLimit(10);
            this.text = text;
            text.addModifyListener(this);
        }
        @Override
        public void modifyText(ModifyEvent event) {
            boolean ok = false;
            double newValue = Double.NaN;
            String s = text.getText();
            s = s.trim();
            if (s.length() > 0) {
                try {
                    newValue = WarngenLayer.MILES_TO_METER.convert(
                            Double.parseDouble(s));
                } catch (RuntimeException e) {
                    // ignore
                }
                ok = validate(newValue);
                text.setBackground(text.getDisplay().getSystemColor(
                        ok ? SWT.COLOR_LIST_BACKGROUND : SWT.COLOR_RED));
                if (ok) {
                    setValue(newValue);
                }
            }
        }
        abstract void setValue(double value);
        public boolean validate(double value) { return ! Double.isNaN(value); }
    }
}
