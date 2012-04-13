package com.raytheon.uf.viz.collaboration.ui;

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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.data.CollaborationGroup;
import com.raytheon.uf.viz.collaboration.data.CollaborationNode;
import com.raytheon.uf.viz.collaboration.data.LoginUser;
import com.raytheon.uf.viz.collaboration.data.SessionGroup;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class UsersTreeLabelProvider implements ITableLabelProvider,
        ITableFontProvider, ITableColorProvider {
    private List<ILabelProviderListener> listeners;

    private Map<String, Image> imageMap;

    private Font boldFont = null;

    public UsersTreeLabelProvider() {
        listeners = new ArrayList<ILabelProviderListener>();
        imageMap = new HashMap<String, Image>();
    }

    @Override
    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    @Override
    public void dispose() {
        for (String key : imageMap.keySet()) {
            imageMap.get(key).dispose();
        }
    }

    @Override
    public boolean isLabelProperty(Object element, String property) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }

    @Override
    public Image getColumnImage(Object element, int index) {
        if (Activator.getDefault() == null) {
            return null;
        }
        Image image = null;
        if (element instanceof CollaborationNode) {
            CollaborationNode node = (CollaborationNode) element;
            String key = node.getImageKey();
            if (key != null) {
                image = imageMap.get(key);
                if (image == null) {
                    image = CollaborationUtils.getNodeImage(node);
                    if (image != null) {
                        imageMap.put(key, image);
                    }
                }
            }
        }
        return image;
    }

    @Override
    public String getColumnText(Object element, int index) {
        CollaborationNode elem = (CollaborationNode) element;
        if (elem.getText() == null) {
            return elem.getId();
        }
        return elem.getText();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang
     * .Object, int)
     */
    @Override
    public Color getForeground(Object element, int columnIndex) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang
     * .Object, int)
     */
    @Override
    public Color getBackground(Object element, int columnIndex) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITableFontProvider#getFont(java.lang.Object,
     * int)
     */
    @Override
    public Font getFont(Object element, int columnIndex) {
        if (element instanceof CollaborationGroup
                || element instanceof LoginUser) {
            if (element instanceof SessionGroup
                    && !((SessionGroup) element).isSessionRoot()) {
                // for this case do nothing, as it is not the top level of
                // session groups
            } else {
                if (boldFont == null) {
                    Font currFont = Display.getCurrent().getSystemFont();
                    boldFont = new Font(Display.getCurrent(),
                            currFont.toString(),
                            currFont.getFontData()[0].getHeight(), SWT.BOLD);
                }
                return boldFont;
            }
        }
        return null;
    }
}
