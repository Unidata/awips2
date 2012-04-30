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

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Type;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.SessionGroupContainer;

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
        if (boldFont != null && !boldFont.isDisposed()) {
            boldFont.dispose();
        }
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
    public Image getColumnImage(Object element, int index) {
        if (Activator.getDefault() == null) {
            return null;
        }
        String key = "";
        if (element instanceof UserId) {
            String mode = CollaborationDataManager.getInstance()
                    .getCollaborationConnection().getPresence().getMode()
                    .toString();
            key = mode;
        } else if (element instanceof IRosterEntry) {
            IRosterEntry entry = (IRosterEntry) element;
            if (entry.getPresence().getType() == Type.AVAILABLE) {
                key = entry.getPresence().getMode().toString();
            } else {
                key = "contact_disabled";
            }
        } else if (element instanceof IRosterGroup) {
            key = "group";
        } else if (element instanceof IVenueSession) {
            // key = "session_group";
        } else if (element instanceof SessionGroupContainer) {
            key = "session_group";
        }

        if (imageMap.get(key) == null && !key.equals("")) {
            imageMap.put(key, CollaborationUtils.getNodeImage(key));
        }

        return imageMap.get(key);
    }

    @Override
    public String getColumnText(Object element, int index) {
        if (element instanceof IRosterEntry) {
            IRosterEntry entry = (IRosterEntry) element;
            if (entry.getUser().getAlias() == null
                    || entry.getUser().getAlias().isEmpty()) {
                return entry.getUser().getName();
            } else {
                return entry.getUser().getAlias();
            }
        } else if (element instanceof IRosterGroup) {
            return ((IRosterGroup) element).getName();
        } else if (element instanceof SessionGroupContainer) {
            return "Active Sessions";
        } else if (element instanceof UserId) {
            return ((UserId) element).getName() + " - "
                    + ((UserId) element).getHost();
        } else if (element instanceof IVenueSession) {
            if (((IVenueSession) element).getVenue() == null) {
                return null;
            }
            return ((IVenueSession) element).getVenue().getInfo()
                    .getVenueDescription();
        }
        return null;
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
        if (element instanceof IRosterGroup
                || element instanceof SessionGroupContainer) {
            // for this case do nothing, as it is not the top level of
            // session groups
            if (boldFont == null) {
                Font currFont = Display.getCurrent().getSystemFont();
                boldFont = new Font(Display.getCurrent(), currFont.toString(),
                        currFont.getFontData()[0].getHeight(), SWT.BOLD);
            }
            return boldFont;
        }
        return null;
    }
}
