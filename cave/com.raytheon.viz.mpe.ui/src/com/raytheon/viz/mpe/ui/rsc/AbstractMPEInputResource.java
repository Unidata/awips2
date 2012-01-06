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
package com.raytheon.viz.mpe.ui.rsc;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * Abstract resource for mpe that have input handlers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractMPEInputResource extends
        AbstractVizResource<GenericResourceData, IDescriptor> implements
        IResourceDataChanged {

    public static final String PROPERTY = "checked";

    protected static final Map<String, Object> map = new HashMap<String, Object>();

    static {
        map.put(PROPERTY, false);
    }

    private static final Cursor CURSOR_HAND = new Cursor(Display.getDefault(),
            SWT.CURSOR_HAND);

    /** Input handler for polygon creation and dialog opening */
    private IInputHandler inputHandler = new InputAdapter() {

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (AbstractMPEInputResource.this.handleMouseUp(x, y, mouseButton)) {
                shell.setCursor(oldCursor);
            }
            return false;
        }

    };

    protected Shell shell;

    private Cursor oldCursor;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractMPEInputResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        // Add as data update listener
        resourceData.addChangeListener(this);
    }

    /**
     * @param x
     * @param y
     * @param mouseButton
     * @return
     */
    protected abstract boolean handleMouseUp(int x, int y, int mouseButton);

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                shell = VizWorkbenchManager.getInstance().getCurrentWindow()
                        .getShell();
                oldCursor = shell.getCursor();
            }
        });
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // Nothing to paint by default
    }

    @Override
    protected void disposeInternal() {
        // Nothing to dispose by default
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof EditableCapability) {
                IDisplayPaneContainer container = getResourceContainer();
                EditableCapability cap = (EditableCapability) object;
                if (cap.isEditable()) {
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            shell.setCursor(CURSOR_HAND);
                        }
                    });
                    if (container != null) {
                        container.registerMouseHandler(inputHandler);
                    }
                } else {
                    if (container != null) {
                        container.unregisterMouseHandler(inputHandler);
                    }
                }
            }
        }
    }
}
