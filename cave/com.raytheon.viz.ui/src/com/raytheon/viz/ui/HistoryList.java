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
package com.raytheon.viz.ui;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;

/**
 * HistoryList
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Sep 12, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class HistoryList {

    /** Maximum size of the history list: from D2D */
    private static final int MAXIMUM_HISTORY_SIZE = 100;

    private static HistoryList instance;

    private List<HistoryEntry> backingList;

    private List<IHistoryListener> historyListeners;

    public static synchronized HistoryList getInstance() {
        if (instance == null) {
            instance = new HistoryList();
        }

        return instance;
    }

    private HistoryList() {
        this.backingList = new LinkedList<HistoryEntry>();
        this.historyListeners = new ArrayList<IHistoryListener>();
    }

    public void addHistoryListener(IHistoryListener hl) {
        this.historyListeners.add(hl);
    }

    public void removeHistoryListener(IHistoryListener hl) {
        this.historyListeners.remove(hl);
    }

    public void addBundle() throws VizException {
        Bundle bundle = prepareHistoryEntry();
        if (bundle != null) {
            addBundle(bundle);
        }
    }

    public void addBundle(Bundle b) throws VizException {

        HistoryEntry he = buildEntry(b);

        this.backingList.add(0, he);
        List<HistoryEntry> toRemove = new ArrayList<HistoryEntry>(
                this.backingList.size());

        // Check for any blank entries that are not entry 0
        for (int i = 1; i < backingList.size(); i++) {
            HistoryEntry e = backingList.get(i);
            if (e.name.trim().equals("")) {
                toRemove.add(e);
            } else if (e.bundle != null) {
                e.xml = e.bundle.toXML();
                e.bundle = null;
            }
        }

        backingList.removeAll(toRemove);

        // Trim to max size
        while (backingList.size() > MAXIMUM_HISTORY_SIZE) {
            backingList.remove(backingList.size() - 1);
        }

        for (IHistoryListener hl : historyListeners) {
            hl.historyListUpdated();
        }

    }

    public void refreshLatestBundle() throws VizException {
        Bundle bundle = prepareHistoryEntry();
        if (bundle != null) {
            refreshLatestBundle(bundle);
        }
    }

    public void refreshLatestBundle(Bundle b) throws VizException {
        if (backingList.size() > 0) {
            backingList.remove(0);
        }

        addBundle(b);
    }

    private HistoryEntry buildEntry(Bundle b) throws VizException {
        HistoryEntry he = new HistoryEntry();
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        Set<String> names = new HashSet<String>();
        for (AbstractRenderableDisplay display : b.getDisplays()) {
            if (!first) {
                sb.append(", ");
            } else {
                first = false;
            }
            sb.append(recursiveBuildName(display.getDescriptor()
                    .getResourceList(), names));
        }
        he.name = sb.toString();
        he.bundle = b;
        return he;
    }

    private String recursiveBuildName(ResourceList list, Set<String> names) {
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (ResourcePair rp : list) {
            if (rp.getProperties().isMapLayer()
                    || rp.getProperties().isSystemResource()
                    || rp.getResource() == null) {
                continue;
            }

            String name = "";
            boolean wasFirst = first;
            if (!first) {
                name += ", ";
            } else {
                first = false;
            }

            if (rp.getResource() instanceof IResourceGroup) {
                sb.append(recursiveBuildName(
                        ((IResourceGroup) rp.getResource()).getResourceList(),
                        names));
            } else {
                name += rp.getResource().getName();
                if (names.contains(name) == false) {
                    names.add(name);
                    sb.append(name);
                } else {
                    if (wasFirst) {
                        first = true;
                    }
                }
            }
        }
        return sb.toString();
    }

    public Bundle getBundle(int idx, boolean moveToTop) throws VizException {
        if (idx < 0 || idx >= backingList.size()) {
            return null;
        }
        String xml = null;
        try {
            xml = getBundleAsString(idx, moveToTop);
            Bundle bundle = Bundle.unmarshalBundle(xml, null);
            return bundle;
        } catch (Exception e) {
            System.out.println("Bad bundle: " + xml);
            e.printStackTrace();
            throw new VizException("Error loading bundle", e);
        }
    }

    public String getBundleAsString(int idx, boolean moveToTop)
            throws VizException {
        if (idx < 0 || idx >= backingList.size()) {
            return null;
        }

        String xml = null;
        try {
            HistoryEntry he = backingList.get(idx);
            if (he.bundle != null) {
                xml = he.bundle.toXML();
            } else {
                xml = he.xml;
            }

            if (moveToTop) {
                backingList.remove(idx);
                backingList.add(0, he);
            }
            return xml;
        } catch (Exception e) {
            e.printStackTrace();
            throw new VizException("Error loading bundle", e);
        }
    }

    public String[] getLabels() {
        String[] strings = new String[backingList.size()];
        for (int i = 0; i < strings.length; i++) {
            strings[i] = backingList.get(i).name;
        }
        return strings;
    }

    private static class HistoryEntry {
        public String xml;

        public String name;

        public Bundle bundle;
    }

    public static interface IHistoryListener {
        public void historyListUpdated();
    }

    public static Bundle prepareHistoryEntry() {
        return prepareHistoryEntry(EditorUtil.getActiveVizContainer());
    }

    public static Bundle prepareHistoryEntry(IDisplayPaneContainer cont) {
        if (cont != null) {
            Bundle b = new Bundle();
            com.raytheon.uf.viz.core.IDisplayPane[] panes = cont
                    .getDisplayPanes();
            List<AbstractRenderableDisplay> rds = new ArrayList<AbstractRenderableDisplay>();
            for (IDisplayPane p : panes) {
                IRenderableDisplay rd = p.getRenderableDisplay();
                if (rd instanceof AbstractRenderableDisplay) {
                    AbstractRenderableDisplay newDisplay = (AbstractRenderableDisplay) rd
                            .createNewDisplay();
                    for (ResourcePair rp : rd.getDescriptor().getResourceList()) {
                        ResourcePair newPair = new ResourcePair();
                        newPair.setLoadProperties(rp.getLoadProperties());
                        newPair.setProperties(rp.getProperties());
                        newPair.setResourceData(rp.getResourceData());
                        newDisplay.getDescriptor().getResourceList()
                                .add(newPair);
                        // Add resource to pair AFTER adding to resource list to
                        // avoid firing listeners
                        newPair.setResource(rp.getResource());
                    }
                    rds.add(newDisplay);
                }
            }
            b.setDisplays(rds.toArray(new AbstractRenderableDisplay[rds.size()]));
            return b;
        }
        return null;
    }

}
