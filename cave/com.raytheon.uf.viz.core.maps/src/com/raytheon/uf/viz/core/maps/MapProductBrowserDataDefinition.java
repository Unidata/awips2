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
package com.raytheon.uf.viz.core.maps;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.MapStore.MapNode;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Product browser plugin for the maps
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class MapProductBrowserDataDefinition extends
        AbstractProductBrowserDataDefinition<AbstractRequestableResourceData> {

    /**
     * 
     */
    public MapProductBrowserDataDefinition() {
        displayName = "Maps";
        loadProperties = new LoadProperties();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #getResourceData()
     */
    @Override
    public AbstractRequestableResourceData getResourceData() {
        // nothing to return here
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #populateData(java.lang.String[])
     */
    @Override
    public List<ProductBrowserLabel> populateData(String[] selection) {
        MapNode root = null;
        if (selection.length == 1) {
            root = MapStore.getMapTree();
        } else {
            root = MapStore.getMapTree();
            for (MapNode node : root.getSubTree()) {
                if (node.getName().equals(selection[selection.length - 1])) {
                    root = node;
                    break;
                }
            }
        }
        List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
        int count = 0;
        for (MapNode node : root.getSubTree()) {
            labels.add(getSubTree(node));
            count++;
        }
        return labels;
    }

    private ProductBrowserLabel getSubTree(MapNode node) {
        ProductBrowserLabel label = new ProductBrowserLabel();
        label.setName(node.getName());
        label.setData(node.getName());
        if (node.getSubTree() == null) {
            label.setProduct(true);
        } else {
            label.setProduct(false);
        }
        return label;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #constructResource(java.lang.String[],
     * com.raytheon.uf.viz.core.rsc.ResourceType)
     */
    @Override
    public void constructResource(String[] selection, ResourceType type) {
        IDescriptor currDesc = null;
        if (EditorUtil.getActiveEditor() != null) {
            if (EditorUtil.getActiveEditor() instanceof IMultiPaneEditor) {
                IMultiPaneEditor editor = (IMultiPaneEditor) EditorUtil
                        .getActiveEditor();
                if (editor.getSelectedPane(IMultiPaneEditor.LOAD_ACTION) == null) {
                    currDesc = editor.getActiveDisplayPane().getDescriptor();
                } else {
                    currDesc = editor.getSelectedPane(
                            IMultiPaneEditor.LOAD_ACTION).getDescriptor();
                }
            } else {
                currDesc = ((AbstractEditor) EditorUtil.getActiveEditor())
                        .getActiveDisplayPane().getDescriptor();
            }
            MapManager manager = MapManager
                    .getInstance((MapDescriptor) currDesc);

            manager.loadMapByName(selection[selection.length - 1]);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #buildHistoryList(java.util.List)
     */
    @Override
    public List<String> buildProductList(List<String> historyList) {
        // TODO Auto-generated method stub
        return historyList;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.xml.IProductBrowserPreferences#
     * configurePreferences()
     */
    @Override
    public List<ProductBrowserPreference> configurePreferences() {
        return super.configurePreferences();
    }
}
