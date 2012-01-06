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

import java.io.File;
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

import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.xy.VizXyEditor;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.raytheon.viz.radar.ui.xy.RadarGraphDisplay;
import com.raytheon.viz.radar.ui.xy.RadarXYDisplay;

/**
 * 
 * An interface to the radar data catalog made available for the various radar
 * parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/29/2006   #7         brockwoo    Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class RadarDataCatalog implements IDataCatalog {

    private ListViewer list1;

    private ListViewer list2;

    private ListViewer list3;

    private Button addDataButton;

    private TypeSelectionListener typeListener;

    private ElevationSelectionListener elevationListener;

    private LocationSelectionListener locationListener;

    // private static HashMap<String, String> radarTypes;

    private CatalogQuery radarQuery;

    private String type;

    private String elevation;

    private String location;

    private Map<String, String> editorConfig = new HashMap<String, String>();

    private final RadarInfoDict infoDict;

    private static final String PLUGIN = "radar";

    public RadarDataCatalog() {
        File file = PathManagerFactory.getPathManager().getStaticFile(
                "radarInfo.txt");
        infoDict = RadarInfoDict.getInstance(file.getParent());

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#getDataType()
     */
    public String getDataType() {
        return "Radar";
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
        this.list1.removeSelectionChangedListener(typeListener);
        this.list2.removeSelectionChangedListener(elevationListener);
        this.list3.removeSelectionChangedListener(locationListener);

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
        label1.setText("Data Type");
        label2.setText("Elevation");
        label3.setText("Location");
        radarQuery = new CatalogQuery();
        this.list1 = list1;
        this.list2 = list2;
        this.list3 = list3;
        this.list1.getList().setVisible(true);
        this.list2.getList().setVisible(true);
        this.list3.getList().setVisible(true);
        this.list1.getList().setLayoutData(
                new GridData(THREEPANEWIDTH, DEFAULTPANEHEIGHT));
        this.list2.getList().setLayoutData(
                new GridData(THREEPANEWIDTH, DEFAULTPANEHEIGHT));
        this.list3.getList().setLayoutData(
                new GridData(THREEPANEWIDTH, DEFAULTPANEHEIGHT));
        // typeListener = new TypeSelectionListener();
        // this.list2.addSelectionChangedListener(typeListener);
        this.list1.setContentProvider(new RadarContentProvider());
        this.list1.setLabelProvider(new RadarTypeLabelProvider());
        typeListener = new TypeSelectionListener();
        this.list1.addSelectionChangedListener(typeListener);

        this.list2.setContentProvider(new RadarContentProvider());
        this.list2.setLabelProvider(new RadarElevationLabelProvider());
        elevationListener = new ElevationSelectionListener();
        this.list2.addSelectionChangedListener(elevationListener);

        this.list3.setContentProvider(new RadarContentProvider());
        this.list3.setLabelProvider(new RadarLabelProvider());
        locationListener = new LocationSelectionListener();
        this.list3.addSelectionChangedListener(locationListener);
        addDataButton = addButton;
        type = " ";
        elevation = " ";
        location = " ";
        populateTypeParameters();

    }

    private void populateTypeParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        try {
            String[] parameters = CatalogQuery.performQuery("productCode",
                    queryList);
            if (parameters != null && parameters.length > 0) {
                Arrays.sort(parameters);
                list1.setInput(parameters);
            } else {
                list1.setInput(NODATA);
            }
        } catch (VizException e) {
            // TODO Put a pop up dialog box here to notify user the query failed
            e.printStackTrace();
        }
    }

    private void populateElevationParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        queryList.put("productCode", new RequestConstraint(type));
        try {
            String[] parameters = CatalogQuery.performQuery(
                    "primaryElevationAngle", queryList);
            Arrays.sort(parameters);
            list2.setInput(parameters);
        } catch (VizException e) {
            // TODO Put a pop up dialog box here to notify user the query failed
            e.printStackTrace();
        }
    }

    private void populateLocationParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        queryList.put("productCode", new RequestConstraint(type));
        queryList
                .put("primaryElevationAngle", new RequestConstraint(elevation));
        try {
            String[] parameters = CatalogQuery.performQuery("icao", queryList);
            Arrays.sort(parameters);
            list3.setInput(parameters);
        } catch (VizException e) {
            // TODO Put a pop up dialog box here to notify user the query failed
            e.printStackTrace();
        }
    }

    class TypeSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedModel = (StructuredSelection) event
                    .getSelection();
            String typeInfo = (String) selectedModel.getFirstElement();
            if (typeInfo != null && !typeInfo.matches(type)
                    && !typeInfo.matches(NODATA[0])) {
                addDataButton.setEnabled(false);
                type = typeInfo;
                populateElevationParameters();
            }
        }
    }

    class ElevationSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedElevation = (StructuredSelection) event
                    .getSelection();
            String elevationInfo = (String) selectedElevation.getFirstElement();
            if (elevationInfo != null && !elevationInfo.matches(elevation)) {
                addDataButton.setEnabled(false);
                elevation = elevationInfo;
                populateLocationParameters();
            }
        }
    }

    class LocationSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedParameter = (StructuredSelection) event
                    .getSelection();
            String selectedLocation = (String) selectedParameter
                    .getFirstElement();
            if (selectedLocation != null && !selectedLocation.matches(location)) {
                addDataButton.setEnabled(true);
                location = selectedLocation;
            }
        }
    }

    class RadarContentProvider implements IStructuredContentProvider {
        public Object[] getElements(Object inputElement) {
            return (String[]) inputElement;
        }

        public void dispose() {
        }

        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        }
    }

    class RadarLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            return (String) element;
        }
    }

    class RadarTypeLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            return getNameFromDictionary(element);
        }
    }

    protected String getNameFromDictionary(Object element) {
        try {
            int productCode = Integer.parseInt((String) element);
            RadarInfo info = infoDict.getInfo(productCode);
            if (info != null) {
                String name = info.getDescription();
                if (name == null) {
                    name = "Product Code " + type;
                }
                return name;
            }
        } catch (NumberFormatException e) {
        }
        return (String) element;
    }

    class RadarElevationLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            String type = (String) element;
            return type + "°";
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
        parameters.put("productCode", new RequestConstraint(type));
        parameters.put("primaryElevationAngle",
                new RequestConstraint(elevation));
        parameters.put("icao", new RequestConstraint(location));
        return parameters;
    }

    @Override
    public AbstractRequestableResourceData getResourceData() {
        RadarResourceData rrd = new RadarResourceData();
        rrd.setMetadataMap(getProductParameters());

        // Need to set the type of editor
        RadarInfo info = infoDict.getInfo(Integer.parseInt(type));
        String format = null;
        if (info != null) {
            format = info.getFormat();
        }
        if ("Graph".equals(format)) {
            // Handles radar products that are displayed on a nonMap display
            // with a graph

            // Radar XY plot editor
            setEditorConfig(VizXyEditor.class.getName(),
                    RadarGraphDisplay.class.getName());
        } else if ("XY".equals(format)) {
            // Handles radar products that are displayed on a nonMap display
            // without a graph

            // Radar XY plot editor
            setEditorConfig(VizXyEditor.class.getName(),
                    RadarXYDisplay.class.getName());
        } else {
            // Standard GL Map editor
            setEditorConfig(VizMapEditor.EDITOR_ID, VizMapEditor.EDITOR_ID);
        }

        // Set the product name
        // rrd.setProductName(getNameFromDictionary(type));

        return rrd;
    }

    private void setEditorConfig(String editor, String editorInput) {
        editorConfig.put("editor", editor);
        editorConfig.put("editorInput", editorInput);
    }

    @Override
    public Map<String, String> getEditorConfig() {
        return editorConfig;
    }

}
