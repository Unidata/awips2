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
package com.raytheon.uf.viz.gisdatastore.ui;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * Abstract base class for field editors with images.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public abstract class AbstractFieldEditor extends FieldEditor {

    protected Button fButton;

    protected Point fExtent;

    protected Image fImage;

    public AbstractFieldEditor() {
        super();
    }

    public AbstractFieldEditor(String name, String labelText, Composite parent) {
        super(name, labelText, parent);
    }

    @Override
    protected void adjustForNumColumns(int numColumns) {
        ((GridData) getLabelControl().getLayoutData()).horizontalSpan = numColumns - 1;
    }

    protected Point computeImageSize(Control window) {
        GC gc = new GC(window);
        Font f = JFaceResources.getFontRegistry().get(
                JFaceResources.DIALOG_FONT);
        gc.setFont(f);
        int height = gc.getFontMetrics().getHeight();
        gc.dispose();
        Point p = new Point(64, height);
        return p;
    }

    @Override
    public int getNumberOfControls() {
        return 2;
    }

    @Override
    protected void doFillIntoGrid(Composite parent, int numColumns) {
        Control control = getLabelControl(parent);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        gd.horizontalSpan = numColumns - 1;
        control.setLayoutData(gd);

        fButton = new Button(parent, SWT.PUSH);
        fButton.setLayoutData(new GridData());
        fExtent = computeImageSize(parent);
        updateImage();
        fButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                open();
            }
        });
        fButton.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent event) {
                if (fImage != null) {
                    fImage.dispose();
                    fImage = null;
                }
            }
        });
    }

    /**
     * Base method just removes old image. Subclasses should call super to
     * dispose old image before updating the image.
     */
    protected void updateImage() {
        if (fImage != null) {
            fImage.dispose();
            fImage = null;
        }
    }

    protected abstract void open();

    protected abstract void setValueFromString(String value);

    protected abstract String getStringFromValue();

    @Override
    protected void doLoad() {
        String value = getPreferenceStore().getString(getPreferenceName());
        setValueFromString(value);
        updateImage();
    }

    @Override
    protected void doLoadDefault() {
        String value = getPreferenceStore().getDefaultString(
                getPreferenceName());
        setValueFromString(value);
        updateImage();
    }

    @Override
    protected void doStore() {
        getPreferenceStore()
                .setValue(getPreferenceName(), getStringFromValue());
    }

}