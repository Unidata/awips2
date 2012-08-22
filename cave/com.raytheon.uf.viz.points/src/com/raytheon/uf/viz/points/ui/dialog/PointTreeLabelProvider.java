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

import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.points.Activator;
import com.raytheon.uf.viz.points.data.IPointNode;

/**
 * Provides the label for a point node.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012 #875       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PointTreeLabelProvider implements ITableLabelProvider,
        ILabelProvider, IFontProvider {

    private static final int TREE_COL_INDEX = 0;

    private static final int MOVABLE_COL_INDEX = 1;

    private static final int HIDDEN_COL_INDEX = 2;

    private static final String CHECKED = "checked";

    private static final String UNCHECKED = "unchecked";

    private static final String GROUP = "group";

    private static final String POINT = "point";

    private List<ILabelProviderListener> listeners;

    private Font boldFont;

    private Color imageBackground;

    IPointNode prevPoint = null;

    private ImageRegistry imageReg;

    public PointTreeLabelProvider() {
        listeners = new ArrayList<ILabelProviderListener>();

        // The FontRegistry will dispose of the font.
        boldFont = JFaceResources.getFontRegistry().getBold(
                JFaceResources.getDefaultFont().toString());
        imageReg = new ImageRegistry();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.
     * jface.viewers.ILabelProviderListener)
     */
    @Override
    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang
     * .Object, java.lang.String)
     */
    @Override
    public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse
     * .jface.viewers.ILabelProviderListener)
     */
    @Override
    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang
     * .Object, int)
     */
    public Image getColumnImage(Object element, int columnIndex) {
        if (Activator.getDefault() == null) {
            return null;
        }
        Image image = null;
        String key = null;
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
            if (node.isMovable()) {
                key = CHECKED;
            } else {
                key = UNCHECKED;
            }
            break;
        case HIDDEN_COL_INDEX:
            if (node.isHidden()) {
                key = CHECKED;
            } else {
                key = UNCHECKED;
            }
            break;
        }

        if (key != null) {
            if (imageReg.getDescriptor(key) != null) {
                image = imageReg.get(key);
            } else {
                image = getImage(key);
                if (image != null) {
                    imageReg.put(key, image);
                }
            }
        }
        return image;
    }

    private Image getImage(String key) {
        Image image = null;
        if (key != CHECKED && key != UNCHECKED) {
            image = PointUtils.getImage(key);
        } else {
            image = makeImage(CHECKED.equals(key));
        }
        return image;
    }

    private Image makeImage(boolean checked) {
        Shell shell = Display.getCurrent().getActiveShell();
        Shell s = new Shell(shell, SWT.NO_TRIM);
        Button b = new Button(s, SWT.CHECK);
        b.setSelection(checked);
        Point bsize = b.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        b.setSize(bsize);
        b.setLocation(0, 0);
        s.setSize(bsize);
        b.setBackground(imageBackground);
        s.open();

        GC gc = new GC(b);
        Image image = new Image(shell.getDisplay(), bsize.x, bsize.y);
        gc.copyArea(image, 0, 0);
        gc.dispose();
        s.close();
        return image;
    }

    public void setImageBackground(Color color) {
        imageBackground = color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang
     * .Object, int)
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
     */
    @Override
    public Image getImage(Object element) {
        return getColumnImage(element, 0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
     */
    @Override
    public String getText(Object element) {
        return getColumnText(element, 0);
    }

    @Override
    public void dispose() {
        imageReg.dispose();
    }
}
