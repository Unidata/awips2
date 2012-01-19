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

package com.raytheon.viz.volumebrowser.catalog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.satellite.rsc.SatResourceData;

/**
 * 
 * An interface to the satellite data catalog made available for the various
 * satellite parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/29/2006   #7         brockwoo    Initial creation
 * 12/07/2006   #111       brockwoo    Put in a filter to remove visible imagery
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class SatelliteDataCatalog implements IDataCatalog {

    private ListViewer list1;

    private ListViewer list2;

    private ListViewer list3;

    private Button addDataButton;

    private String source;

    private String region;

    private String type;

    private SourceSelectionListener sourceListener;

    private RegionSelectionListener regionListener;

    private TypeSelectionListener typeListener;

    private static final String PLUGIN = "satellite";

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#getDataType()
     */
    public String getDataType() {
        return "Satellite";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#dispose()
     */
    public void dispose() {
        addDataButton.setEnabled(false);
        this.list1.setInput(EMPTYLIST);
        this.list2.setInput(EMPTYLIST);
        this.list3.setInput(EMPTYLIST);
        this.list1.refresh();
        this.list2.refresh();
        this.list3.refresh();
        this.list1.removeSelectionChangedListener(sourceListener);
        this.list2.removeSelectionChangedListener(regionListener);
        this.list3.removeSelectionChangedListener(typeListener);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.catalog.IDataCatalog#initDataCatalog(org.eclipse
     * .swt.widgets.Label, org.eclipse.swt.widgets.Label,
     * org.eclipse.swt.widgets.Label, org.eclipse.jface.viewers.ListViewer,
     * org.eclipse.jface.viewers.ListViewer,
     * org.eclipse.jface.viewers.ListViewer, org.eclipse.swt.widgets.Button)
     */
    public void initDataCatalog(Label label1, Label label2, Label label3,
            ListViewer list1, ListViewer list2, ListViewer list3,
            Button addButton) {
        label1.setText("Source");
        label2.setText("Region");
        label3.setText("Type");
        this.list1 = list1;
        this.list2 = list2;
        this.list3 = list3;
        this.list1.getList().setVisible(true);
        this.list2.getList().setVisible(true);
        this.list3.getList().setVisible(true);
        this.list1.getList().setLayoutData(
                new GridData(IDataCatalog.THREEPANEWIDTH,
                        IDataCatalog.DEFAULTPANEHEIGHT));
        this.list2.getList().setLayoutData(
                new GridData(IDataCatalog.THREEPANEWIDTH,
                        IDataCatalog.DEFAULTPANEHEIGHT));
        this.list3.getList().setLayoutData(
                new GridData(IDataCatalog.THREEPANEWIDTH,
                        IDataCatalog.DEFAULTPANEHEIGHT));
        this.list1.setContentProvider(new SatelliteContentProvider());
        this.list1.setLabelProvider(new SatelliteLabelProvider());
        this.list2.setContentProvider(new SatelliteContentProvider());
        this.list2.setLabelProvider(new SatelliteLabelProvider());
        this.list3.setContentProvider(new SatelliteContentProvider());
        this.list3.setLabelProvider(new SatelliteLabelProvider());
        sourceListener = new SourceSelectionListener();
        this.list1.addSelectionChangedListener(sourceListener);
        regionListener = new RegionSelectionListener();
        this.list2.addSelectionChangedListener(regionListener);
        typeListener = new TypeSelectionListener();
        this.list3.addSelectionChangedListener(typeListener);
        addDataButton = addButton;
        source = "";
        region = "";
        type = "";
        populateSourceParameters();
    }

    private void populateSourceParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        try {
            String[] parameters = CatalogQuery.performQuery("creatingEntity",
                    queryList);
            if (parameters != null && parameters.length > 0) {
                Arrays.sort(parameters);
                list1.setInput(parameters);
            } else {
                list1.setInput(NODATA);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private void populateRegionParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        queryList.put("creatingEntity", new RequestConstraint(source));
        try {
            String[] parameters = CatalogQuery.performQuery("sectorID",
                    queryList);
            Arrays.sort(parameters);
            list2.setInput(parameters);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private void populateTypeParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        queryList.put("creatingEntity", new RequestConstraint(source));
        queryList.put("sectorID", new RequestConstraint(region));
        try {
            String[] parameters = CatalogQuery.performQuery("physicalElement",
                    queryList);
            Arrays.sort(parameters);
            ArrayList<String> goodParameters = new ArrayList<String>();
            for (int i = 0; i < parameters.length; i++) {
                goodParameters.add(parameters[i]);
            }
            list3.setInput(goodParameters.toArray(new String[] {}));
        } catch (VizException e) {
            // TODO Put in a popup to alert the user that the query failed
            e.printStackTrace();
        }
    }

    class SatelliteContentProvider implements IStructuredContentProvider {
        public Object[] getElements(Object inputElement) {
            return (String[]) inputElement;
        }

        public void dispose() {
        }

        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        }
    }

    class SatelliteLabelProvider extends LabelProvider {
        public Image getImage(Object element) {
            return null;
        }

        public String getText(Object element) {
            return (String) element;
        }
    }

    class SourceSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedModel = (StructuredSelection) event
                    .getSelection();
            String sourceInfo = (String) selectedModel.getFirstElement();
            if (sourceInfo != null && !sourceInfo.matches(source)
                    && !sourceInfo.matches(NODATA[0])) {
                addDataButton.setEnabled(false);
                source = sourceInfo;
                populateRegionParameters();
            }
        }
    }

    class RegionSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedParameter = (StructuredSelection) event
                    .getSelection();
            String selectedRegion = (String) selectedParameter
                    .getFirstElement();
            if (selectedRegion != null && !selectedRegion.matches(region)) {
                type = "";
                addDataButton.setEnabled(false);
                region = selectedRegion;
                populateTypeParameters();
            }
        }
    }

    class TypeSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedParameter = (StructuredSelection) event
                    .getSelection();
            String selectedType = (String) selectedParameter.getFirstElement();
            if (selectedType != null && !selectedType.matches(type)) {
                addDataButton.setEnabled(true);
                type = selectedType;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#getProductParameters()
     */
    public HashMap<String, RequestConstraint> getProductParameters() {
        HashMap<String, RequestConstraint> parameters = new HashMap<String, RequestConstraint>();
        parameters.put("pluginName", new RequestConstraint(PLUGIN));
        parameters.put("creatingEntity", new RequestConstraint(source));
        parameters.put("sectorID", new RequestConstraint(region));
        parameters.put("physicalElement", new RequestConstraint(type));
        return parameters;
    }

    @Override
    public AbstractRequestableResourceData getResourceData() {
        SatResourceData srd = new SatResourceData();
        srd.setMetadataMap(getProductParameters());
        return srd;
    }

    @Override
    public Map<String, String> getEditorConfig() {
        Map<String, String> config = new HashMap<String, String>();

        config.put("editor", VizMapEditor.EDITOR_ID);
        config.put("editorInput",
                "com.raytheon.uf.viz.d2d.core.map.D2DMapRenderableDisplay");

        return config;
    }
}
