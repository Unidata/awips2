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
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;

/**
 * 
 * An interface to the surface data catalog made available for the various
 * surface parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/29/2006   #7         brockwoo    Initial creation
 * 05/11/2007   #273       brockwoo    Changed the plugin name from 'metar'
 *                                     to 'obs' for JavaScript support
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class ObservationDataCatalog implements IDataCatalog {

    private ListViewer list1;

    private Button addDataButton;

    private CatalogQuery obsQuery;

    private TypeSelectionListener typeListener;

    private static final String PLUGIN = "obs";

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#getDataType()
     */
    public String getDataType() {
        return "Observations";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#dispose()
     */
    public void dispose() {
        addDataButton.setEnabled(false);
        this.list1.setInput(EMPTYLIST);
        this.list1.refresh();
        this.list1.removeSelectionChangedListener(typeListener);
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
        label2.setText(" ");
        label3.setText(" ");
        obsQuery = new CatalogQuery();
        this.list1 = list1;
        this.list1.getList().setLayoutData(
                new GridData(ONEPANEWIDTH, DEFAULTPANEHEIGHT));
        typeListener = new TypeSelectionListener();
        this.list1.addSelectionChangedListener(typeListener);
        this.list1.setContentProvider(new ObservationContentProvider());
        this.list1.setLabelProvider(new ObservationLabelProvider());
        this.list1.getList().setEnabled(true);
        list2.getList().setVisible(false);
        list3.getList().setVisible(false);
        addDataButton = addButton;
        populateTypeParameters();
    }

    private void populateTypeParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        queryList.put("reportType", new RequestConstraint("METAR"));
        try {
            String[] parameters = CatalogQuery.performQuery("dataTime",
                    queryList);
            if (parameters != null && parameters.length > 0) {
                String[] obsList = { "Observations (METARS)" };
                list1.setInput(obsList);
            } else {
                list1.setInput(NODATA);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    class ObservationContentProvider implements IStructuredContentProvider {
        public Object[] getElements(Object inputElement) {
            return (String[]) inputElement;
        }

        public void dispose() {
        }

        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        }
    }

    class ObservationLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            return (String) element;
        }
    }

    class TypeSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedParameter = (StructuredSelection) event
                    .getSelection();
            String selectedType = (String) selectedParameter.getFirstElement();
            if (selectedType != null && !selectedType.matches(NODATA[0])) {
                addDataButton.setEnabled(true);
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
        parameters.put("reportType", new RequestConstraint("METAR"));
        return parameters;
    }

    @Override
    public AbstractRequestableResourceData getResourceData() {
        PlotResourceData prd = new PlotResourceData();
        prd.setRetrieveData(false);
        prd.setUpdatingOnMetadataOnly(true);
        prd.setRequeryNecessaryOnTimeMatch(true);
        BinOffset binOffset = new BinOffset(1800, 1800);
        prd.setBinOffset(binOffset);
        prd.setPlotSource("METAR Plot");
        prd.setMetadataMap(getProductParameters());
        return prd;
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
