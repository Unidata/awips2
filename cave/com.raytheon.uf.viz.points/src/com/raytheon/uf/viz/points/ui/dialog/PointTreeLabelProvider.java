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
package com.raytheon.uf.viz.points.ui.dialog;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.points.Activator;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.ui.dialog.TriStateCellEditor.STATE;

/**
 * Provides the label for a point node.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012  875       rferrel     Initial creation
 * Apr 18, 2017  6237      njensen     Use gif icons for checkboxes
 * 
 * </pre>
 * 
 * @author rferrel
 */
public class PointTreeLabelProvider implements ITableLabelProvider,
        ILabelProvider, IFontProvider {

    private static final int TREE_COL_INDEX = 0;

    private static final int MOVABLE_COL_INDEX = 1;

    private static final int HIDDEN_COL_INDEX = 2;

    private static final String GROUP = "group";

    private static final String POINT = "point";

    private List<ILabelProviderListener> listeners;

    private Font boldFont;

    private Color imageBackground;

    IPointNode prevPoint = null;

    private ImageRegistry imageReg;

    Shell shell;

    public PointTreeLabelProvider() {
        listeners = new ArrayList<>();
        shell = Display.getCurrent().getActiveShell();

        // The FontRegistry will dispose of the font.
        boldFont = JFaceResources.getFontRegistry().getBold(
                JFaceResources.getDefaultFont().toString());
        imageReg = new ImageRegistry();
    }

    @Override
    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    @Override
    public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    @Override
    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    @Override
    public Image getColumnImage(Object element, int columnIndex) {
        if (Activator.getDefault() == null) {
            return null;
        }
        Image image = null;
        Object key = null;
        IPointNode node = (IPointNode) element;

        switch (columnIndex) {
        case TREE_COL_INDEX:
            if (node.isGroup()) {
                key = GROUP;
            } else {
                key = POINT;
            }
            break;
        case MOVABLE_COL_INDEX:
            switch (node.getMovable()) {
            case TRUE:
                key = STATE.SELECTED;
                break;
            case FALSE:
                key = STATE.UNSELECTED;
                break;
            case UNKNOWN:
                key = STATE.GRAYED;
                break;
            default:
                Assert.isTrue(false);
            }
            break;
        case HIDDEN_COL_INDEX:
            switch (node.getHidden()) {
            case TRUE:
                key = STATE.SELECTED;
                break;
            case FALSE:
                key = STATE.UNSELECTED;
                break;
            case UNKNOWN:
                key = STATE.GRAYED;
                break;
            default:
                Assert.isTrue(false);
            }
            break;
        }

        if (key != null) {
            if (imageReg.getDescriptor(key.toString()) != null) {
                image = imageReg.get(key.toString());
            } else {
                image = createImage(key);
                if (image != null) {
                    imageReg.put(key.toString(), image);
                }
            }
        }
        return image;
    }

    /*
     * TODO: The current implementation is using images in the plugin to provide
     * the checkboxes on the table. These images were captured from a Linux
     * theme and reused, therefore the PointsMgrDialog is not truly cross
     * platform. It will not have the native look of other operating systems or
     * themes. It should be possible to rewrite the PointsMgrDialog to use a
     * TableEditor to create the same appearance of the dialog while using SWT
     * Button widgets to make tri-state checkboxes and achieve native rendering.
     */
    private Image createImage(Object key) {
        return PointUtils.getImage(key.toString());
    }

    public void setImageBackground(Color color) {
        imageBackground = color;
    }

    @Override
    public String getColumnText(Object element, int columnIndex) {
        IPointNode node = (IPointNode) element;
        String text = null;
        if (columnIndex == TREE_COL_INDEX) {
            text = node.getName();
        }
        return text;
    }

    /**
     * @param element
     * @param columnIndex
     * @return
     */
    public Color getForeground(Object element, int columnIndex) {
        return null;
    }

    /**
     * @param element
     * @param columnIndex
     * @return
     */
    public Color getBackground(Object element, int columnIndex) {
        return null;
    }

    /**
     * @param element
     * @param columnIndex
     * @return
     */
    public Font getFont(Object element, int columnIndex) {
        IPointNode node = (IPointNode) element;
        if (node.isGroup()) {
            return boldFont;
        }
        return null;
    }

    @Override
    public Font getFont(Object element) {
        IPointNode node = (IPointNode) element;
        if (node.isGroup()) {
            return boldFont;
        }
        return null;
    }

    @Override
    public Image getImage(Object element) {
        return getColumnImage(element, 0);
    }

    @Override
    public String getText(Object element) {
        return getColumnText(element, 0);
    }

    @Override
    public void dispose() {
        imageReg.dispose();
    }
}
