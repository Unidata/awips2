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
package com.raytheon.uf.viz.python.swt.widgets;

import java.io.File;
import java.io.FileNotFoundException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PushButtonWidget extends Widget {

    private Button button;

    private String text;

    private File image;

    private Image imageObj;

    private Runnable callback;

    protected PushButtonWidget() {
    }

    public static PushButtonWidget withText(String text) {
        PushButtonWidget pbw = new PushButtonWidget();
        pbw.text = text;
        return pbw;
    }

    public static PushButtonWidget withImage(String image)
            throws FileNotFoundException {
        PushButtonWidget pbw = new PushButtonWidget();
        pbw.image = PathManagerFactory.getPathManager().getStaticFile(image);
        if (!pbw.image.exists()) {
            throw new FileNotFoundException(image);
        }

        return pbw;
    }

    public void setCallback(final Runnable runnable) {
        this.callback = runnable;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.python.swt.widgets.Widget#buildComposite(org.eclipse
     * .swt.widgets.Composite, int)
     */
    @Override
    public Composite buildComposite(Composite parent, int style) {
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout());
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        button = new Button(composite, SWT.PUSH);
        button.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        if (this.text != null) {
            button.setText(makeGuiLabel(this.text));
        }

        if (this.image != null) {
            ImageData id = new ImageData(image.getAbsolutePath());
            imageObj = new Image(parent.getDisplay(), id);
            button.setImage(imageObj);

            button.addDisposeListener(new DisposeListener() {

                @Override
                public void widgetDisposed(DisposeEvent e) {
                    if (imageObj != null && !imageObj.isDisposed()) {
                        imageObj.dispose();
                    }
                }

            });
        }

        if (this.callback != null) {
            button.addSelectionListener(new SelectionListener() {

                @Override
                public void widgetDefaultSelected(SelectionEvent e) {

                }

                @Override
                public void widgetSelected(SelectionEvent e) {
                    callback.run();
                }

            });
        }

        return composite;
    }

}
