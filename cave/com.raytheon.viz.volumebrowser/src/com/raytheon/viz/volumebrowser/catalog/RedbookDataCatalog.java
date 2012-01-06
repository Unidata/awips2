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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.redbook.Activator;
import com.raytheon.viz.redbook.RedbookWMOMap;
import com.raytheon.viz.redbook.rsc.RedbookResourceData;

/**
 * 
 * An interface to the redbook data catalog made available for the various
 * redbook product ids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/29/2008              brockwoo    Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class RedbookDataCatalog implements IDataCatalog {

    private ListViewer list1;

    private Button addDataButton;

    private TypeSelectionListener typeListener;

    private String wmoHeader;

    private static final String PLUGIN = "redbook";

    private RedbookWMOMap mapping;

    public RedbookDataCatalog() {
        try {
            mapping = RedbookWMOMap.load();
        } catch (Exception e) {
            VizApp.logAndAlert(Status.ERROR, e,
                    "Error loading redbook mapping",
                    "Redbook mapping could not be loaded",
                    Activator.getDefault(), Activator.PLUGIN_ID);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#getDataType()
     */
    public String getDataType() {
        return "Redbook";
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
        label1.setText("Redbook Product ID");
        label2.setText(" ");
        label3.setText(" ");
        this.list1 = list1;
        this.list1.getList().setLayoutData(
                new GridData(ONEPANEWIDTH, DEFAULTPANEHEIGHT));
        typeListener = new TypeSelectionListener();
        this.list1.addSelectionChangedListener(typeListener);
        this.list1.setContentProvider(new ObservationContentProvider());
        this.list1.setLabelProvider(new RedbookLabelProvider());
        this.list1.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                return super.compare(viewer, descriptiveName((String) e1),
                        descriptiveName((String) e2));
            }

        });
        this.list1.getList().setEnabled(true);
        list2.getList().setVisible(false);
        list3.getList().setVisible(false);
        addDataButton = addButton;
        this.wmoHeader = " ";
        populateTypeParameters();
    }

    private String descriptiveName(String in) {
        RedbookWMOMap.Info info = mapping.mapping.get(in);
        if (info != null)
            return info.name + " (" + in + ")";

        return in;

    }

    private void populateTypeParameters() {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put("pluginName", new RequestConstraint(PLUGIN));
        try {
            String[] parameters = CatalogQuery.performQuery("wmoTTAAii",
                    queryList);
            Arrays.sort(parameters);
            if (parameters != null && parameters.length > 0) {
                list1.setInput(parameters);
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

    class RedbookLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            return descriptiveName((String) element);
        }
    }

    class TypeSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedParameter = (StructuredSelection) event
                    .getSelection();
            String selectedType = (String) selectedParameter.getFirstElement();
            wmoHeader = selectedType;
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
        parameters.put("wmoTTAAii", new RequestConstraint(this.wmoHeader));
        return parameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.catalog.IDataCatalog#getResourceData()
     */
    @Override
    public AbstractRequestableResourceData getResourceData() {
        RedbookResourceData lrd = new RedbookResourceData();
        lrd.setMetadataMap(getProductParameters());
        return lrd;
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
