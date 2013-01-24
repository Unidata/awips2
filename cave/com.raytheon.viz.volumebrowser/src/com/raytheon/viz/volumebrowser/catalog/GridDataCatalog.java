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
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

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

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.grid.inv.GridInventory;
import com.raytheon.viz.grid.rsc.GridResourceData;

/**
 * 
 * An interface to the grid datacatalog made available for the various grid
 * parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/29/2006   #7         brockwoo    Initial creation
 * 12/05/2006   #98        brockwoo    Fix for the grid parameter list not
 *                                     updating when only the model time
 *                                     changes
 * Aug 27, 2008 1502       dglazesk    Updated to use JAXB marshalling
 *                                     Switched to DatasetInfo from plugin-grib
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class GridDataCatalog implements IDataCatalog {

    private ListViewer list1;

    private ListViewer list2;

    private ListViewer list3;

    private Button addDataButton;

    private String modelName;

    private GridInventory inventory;

    private String parameter;

    private String level;

    private ModelSelectionListener modelListener;

    private ParameterSelectionListener parameterListener;

    private LevelSelectionListener levelListener;

    private static final String PLUGIN = GridConstants.GRID;

    public GridDataCatalog() {
    }

    private HashMap<String, Level> levelList;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.catalog.IDataCatalog#getDataType()
     */
    public String getDataType() {
        return "Grid";
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
        inventory = (GridInventory) DataCubeContainer
                .getInventory(GridConstants.GRID);

        label1.setText("Model");
        label2.setText("Parameter");
        label3.setText("Level");

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

        this.list1.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                return super.compare(viewer, e1, e2);
            }

        });

        this.list1.setContentProvider(new ModelContentProvider());
        this.list1.setLabelProvider(new ModelLabelProvider());

        this.list2.setContentProvider(new ParamContentProvider());
        this.list2.setLabelProvider(new ParamLabelProvider());

        this.list3.setContentProvider(new LevelContentProvider());
        this.list3.setLabelProvider(new LevelLabelProvider());

        modelListener = new ModelSelectionListener();
        this.list1.addSelectionChangedListener(modelListener);

        parameterListener = new ParameterSelectionListener();
        this.list2.addSelectionChangedListener(parameterListener);

        levelListener = new LevelSelectionListener();
        this.list3.addSelectionChangedListener(levelListener);

        addDataButton = addButton;
        modelName = "";
        parameter = "";
        level = "";

        populateModelList();
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
        this.list1.removeSelectionChangedListener(modelListener);
        this.list2.removeSelectionChangedListener(parameterListener);
        this.list3.removeSelectionChangedListener(levelListener);
    }

    private void populateModelList() {
        ArrayList<DatasetInfo> modelTimes = new ArrayList<DatasetInfo>();
        if (inventory == null) {
            DatasetInfo[] empty = new DatasetInfo[1];
            empty[0] = new DatasetInfo();
            list1.setInput(empty);
        } else {
            DatasetInfoLookup modelLookup = DatasetInfoLookup.getInstance();

            BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
            try {
                inventory.checkSources(null, null, null, returnQueue);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            for (String modelName : returnQueue) {
                DatasetInfo theGribData = modelLookup.getInfo(modelName);

                if (theGribData != null) {
                    modelTimes.add(theGribData);
                } else {
                    // TODO: generate false entry
                    theGribData = new DatasetInfo();
                    theGribData.setDatasetId(modelName);
                }
            }

            if (modelTimes.size() > 0) {
                this.list1.setInput(modelTimes.toArray(new DatasetInfo[] {}));
            }
        }
    }

    private void populateModelParameters() {
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        try {
            inventory.checkParameters(Arrays.asList(modelName), null, null,
                    false, returnQueue);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        String[] parameters = returnQueue.toArray(new String[0]);
        // this should be sorted by the parameter name
        // tree.getParameterNode(modelName, param).getParameterName();
        Arrays.sort(parameters);
        list2.setInput(parameters);
    }

    private void populateModelLevel() {
        this.levelList = new HashMap<String, Level>();

        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        try {
            inventory.checkLevels(Arrays.asList(modelName),
                    Arrays.asList(parameter), null, returnQueue);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        String[] levelIds = returnQueue.toArray(new String[0]);
        String[] levelNames = new String[levelIds.length];
        LevelFactory lf = LevelFactory.getInstance();

        for (int i = 0; i < levelIds.length; i++) {
            try {
                Level level = lf.getLevel(levelIds[i]);
                levelNames[i] = level.getMasterLevel().getName()
                        + level.getLevelInfo();
                this.levelList.put(levelNames[i], level);
            } catch (CommunicationException e) {
                e.printStackTrace();
            }
        }

        Arrays.sort(levelNames);
        list3.getList().removeAll();
        list3.add(levelNames);
    }

    class ModelContentProvider implements IStructuredContentProvider {
        public Object[] getElements(Object inputElement) {
            DatasetInfo[] models = (DatasetInfo[]) inputElement;
            String[] modelNames = new String[models.length];
            for (int i = 0; i < models.length; i++) {
                modelNames[i] = models[i].getTitle();
            }
            return models;
        }

        public void dispose() {
        }

        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        }
    }

    class ParamContentProvider implements IStructuredContentProvider {
        public Object[] getElements(Object inputElement) {
            String[] params = (String[]) inputElement;
            return params;
        }

        public void dispose() {
        }

        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        }
    }

    class LevelContentProvider implements IStructuredContentProvider {
        public Object[] getElements(Object inputElement) {
            String[] params = (String[]) inputElement;
            return params;
        }

        public void dispose() {
        }

        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        }
    }

    class ModelLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            DatasetInfo model = (DatasetInfo) element;
            // return (String) element;
            return model.getTitle();
        }
    }

    class ParamLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            String param = (String) element;
            return inventory.getParameterName(modelName, param);
        }
    }

    class LevelLabelProvider extends LabelProvider {
        @Override
        public Image getImage(Object element) {
            return null;
        }

        @Override
        public String getText(Object element) {
            Level level = levelList.get((String) element);
            String returnValue = level.getMasterLevel().getDescription() + " "
                    + level.getLevelInfo();
            return returnValue;
        }
    }

    class ModelSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {

            StructuredSelection selectedModel = (StructuredSelection) event
                    .getSelection();
            DatasetInfo modelInfo = (DatasetInfo) selectedModel
                    .getFirstElement();
            if (modelInfo != null && modelInfo.getDatasetId() != null
                    && !modelInfo.getDatasetId().equals(modelName)) {
                modelName = modelInfo.getDatasetId();
                addDataButton.setEnabled(false);
                populateModelParameters();
            }
        }
    }

    class ParameterSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedParameter = (StructuredSelection) event
                    .getSelection();
            String selParameter = (String) selectedParameter.getFirstElement();
            if (selParameter != null && !selParameter.matches(parameter)) {
                parameter = selParameter;
                addDataButton.setEnabled(false);
                populateModelLevel();
            }
        }
    }

    class LevelSelectionListener implements ISelectionChangedListener {
        public void selectionChanged(SelectionChangedEvent event) {
            StructuredSelection selectedParameter = (StructuredSelection) event
                    .getSelection();
            String selectedLevel = (String) selectedParameter.getFirstElement();
            if (selectedLevel != null && !selectedLevel.matches(level)) {
                addDataButton.setEnabled(true);
                level = (String) selectedParameter.getFirstElement();
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
        parameters
                .put(GridConstants.PLUGIN_NAME, new RequestConstraint(PLUGIN));
        parameters.put(GridConstants.DATASET_ID, new RequestConstraint(
                modelName));
        parameters.put(GridConstants.PARAMETER_ABBREVIATION,
                new RequestConstraint(parameter));

        Level level = this.levelList.get(this.level);
        parameters.put(GridConstants.MASTER_LEVEL_NAME, new RequestConstraint(
                level.getMasterLevel().getName()));
        parameters
                .put(GridConstants.LEVEL_ONE,
                        new RequestConstraint(Double.toString(level
                                .getLevelonevalue())));
        parameters
                .put(GridConstants.LEVEL_TWO,
                        new RequestConstraint(Double.toString(level
                                .getLeveltwovalue())));

        return parameters;
    }

    @Override
    public AbstractRequestableResourceData getResourceData() {
        GridResourceData grd = new GridResourceData();
        grd.setMetadataMap(getProductParameters());
        return grd;
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
