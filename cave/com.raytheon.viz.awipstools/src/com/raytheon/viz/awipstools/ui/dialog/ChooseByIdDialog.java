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
package com.raytheon.viz.awipstools.ui.dialog;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.ui.layer.PointsToolLayer;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.ui.layer.HomeToolLayer;
import com.raytheon.viz.awipstools.ui.layer.InteractiveBaselinesLayer;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Dialog for selecting points
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07Dec2007    #576        Eric Babin Initial Creation.
 * 23Jul2010    #5948      bkowal      Added the ability to move
 *                                     a point to the location of a
 *                                     &quot;mesocyclone&quot;.
 * 31Jul2012    #875       rferrel     Let preopen initialize components.
 * 05Nov2012    #1304      rferrel     Added Point Change Listener.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ChooseByIdDialog extends CaveSWTDialog implements
        IPointChangedListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ChooseByIdDialog.class);

    public static final String DIALOG_TITLE = "Choose By Id";

    private final String HOME_POINT = "Home";

    private Button pointsRdo, baselinesRdo, homeRdo;

    private HomeToolLayer homeToolLayer;

    private PointsToolLayer pointsToolLayer;

    private InteractiveBaselinesLayer baselinesToolLayer;

    private final List<IResourceDataChanged> changeListeners = new ArrayList<IResourceDataChanged>();

    private final List<Text> pointStationIdTextFields;

    private final List<Text> baselineStationIdTextFields;

    private IDescriptor descriptor;

    private final PointsDataManager pointsDataManager = PointsDataManager
            .getInstance();

    private final ToolsDataManager toolsDataManager = ToolsDataManager
            .getInstance();

    private ScrolledComposite pointsScroll;

    private final int NAME_INDEX = 0;

    private final int POINT_TEXT_INDEX = 1;

    private class ChooseByIdSelectionListener implements SelectionListener {

        private final List<Text> texts;

        /**
         * @param texts
         */
        public ChooseByIdSelectionListener(List<Text> texts) {
            this.texts = texts;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org
         * .eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetDefaultSelected(SelectionEvent e) {
            return;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse
         * .swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected(SelectionEvent e) {
            Button checkBoxButton = (Button) e.getSource();
            if (checkBoxButton.getSelection()) {

                Event pressEnterEvent = new Event();
                pressEnterEvent.keyCode = SWT.CR;

                for (Text textField : texts) {
                    pressEnterEvent.widget = textField;
                    textField.notifyListeners(SWT.KeyUp, pressEnterEvent);
                }
            }
        }

    }

    private class ChooseByIdKeyListener implements KeyListener,
            IResourceDataChanged {

        private final String idName;

        private boolean isBaseline;

        private Text textWidget;

        private ArrayList<Coordinate> stationCoordinates;

        private String clearedStationIDs;

        private boolean isHome() {
            return (idName.equals(HOME_POINT));
        }

        private boolean isBaseline() {
            return (isBaseline && toolsDataManager.getBaseline(idName) != null);
        }

        private boolean isPoint() {
            return (!isBaseline && pointsDataManager.getCoordinate(idName) != null);
        }

        /**
         * 
         * @param idName
         */
        public ChooseByIdKeyListener(String idName) {
            this.idName = idName;
            setBaseline(false);
            clearedStationIDs = "";
        }

        @Override
        public void keyPressed(KeyEvent e) {
            switch (e.keyCode) {
            case '=':
                e.doit = false;
                break;
            default:
                break;
            }
        }

        @Override
        public void keyReleased(KeyEvent e) {

            stationCoordinates = new ArrayList<Coordinate>();
            textWidget = ((org.eclipse.swt.widgets.Text) e.getSource());

            switch (e.keyCode) {
            case SWT.CR:
                String stationIDs = getFilteredStationIDs(textWidget.getText());

                if (stationIDs.equals("") && clearedStationIDs.length() > 0) {
                    stationIDs = clearedStationIDs;
                }

                if (stationIDs.length() == 0) {
                    return;
                }

                if (isBaseline && stationIDs.split(" ").length < 2) {
                    String errorString = getErrorString(stationIDs) + " ";
                    textWidget.setText(errorString);
                    textWidget.setSelection(errorString.length());
                    return;
                }

                textWidget.setText(stationIDs);

                for (String stationID : stationIDs.split(" ")) {

                    Coordinate c = null;
                    /*
                     * If an Obs station is not found, see if the user has
                     * entered a MD or DMD id.
                     */
                    ObStation obStation = getObStation(stationID);
                    if (obStation != null) {
                        c = obStation.getGeometry().getCoordinate();
                    } else {
                        c = this.getDmdInformation(stationID,
                                descriptor.getNumberOfFrames());
                    }

                    if (c == null) {
                        String errorString = getErrorString(stationIDs);
                        textWidget.setText(errorString);
                        int badStationIndex = errorString.indexOf(stationID);
                        textWidget.setSelection(badStationIndex,
                                badStationIndex + stationID.length());
                        stationCoordinates = null;
                        return;
                    }

                    this.stationCoordinates.add(c);
                }
                updatePosition(stationCoordinates);
                break;
            case SWT.ESC:
            case '=':
                clearedStationIDs = getFilteredStationIDs(textWidget.getText());
                textWidget.setText("");
                stationCoordinates = null;
            default:
                break;
            }
        }

        /**
         * @param text
         * @return
         */
        private String getFilteredStationIDs(String text) {
            text = text.toUpperCase().replace("> ", "");
            return text.replace("* ", "");
        }

        /**
         * @param stationIDs
         * @return
         */
        private String getErrorString(String stationIDs) {
            return "> " + stationIDs;
        }

        /**
         * @param stationCoordinates
         */
        private void updatePosition(List<Coordinate> stationCoordinates) {

            if (isPoint()) {
                pointsDataManager.setCoordinate(idName,
                        stationCoordinates.get(0));
                refreshToolLayer(pointsToolLayer);

            } else if (isBaseline()) {
                toolsDataManager.setBaseline(idName, (new GeometryFactory())
                        .createLineString(stationCoordinates
                                .toArray(new Coordinate[] {})));
                refreshToolLayer(baselinesToolLayer);

            } else if (isHome()) {
                pointsDataManager.setHome(stationCoordinates.get(0));
                refreshToolLayer(homeToolLayer);
            }
        }

        /**
         * @param toolLayer
         */
        private void refreshToolLayer(AbstractVizResource<?, ?> toolLayer) {
            if (toolLayer != null) {
                toolLayer.getResourceData().fireChangeListeners(
                        ChangeType.DATA_UPDATE, null);
                toolLayer.getResourceData().addChangeListener(this);
                changeListeners.add(this);
            }
        }

        /**
         * 
         * @param mesoId
         * @param frameCount
         * @return
         */
        private Coordinate getDmdInformation(String mesoId, int frameCount) {
            Coordinate c = null;
            RadarRecord rr = null;

            LayerProperty property = new LayerProperty();
            HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>();
            RequestConstraint requestConstraint = null;

            Object[] resp = null;
            DataTime[] availableDataTimes = null;
            DataTime[] selectedDataTimes = null;

            requestConstraint = new RequestConstraint();
            requestConstraint.setConstraintType(ConstraintType.EQUALS);
            requestConstraint.setConstraintValue("149");
            metadataMap.put("productCode", requestConstraint);

            requestConstraint = new RequestConstraint();
            requestConstraint.setConstraintType(ConstraintType.EQUALS);
            requestConstraint.setConstraintValue("radar");
            metadataMap.put("pluginName", requestConstraint);

            requestConstraint = new RequestConstraint();
            requestConstraint.setConstraintType(ConstraintType.EQUALS);
            requestConstraint.setConstraintValue("Graphic");
            metadataMap.put("format", requestConstraint);

            try {
                availableDataTimes = DataCubeContainer.performTimeQuery(
                        metadataMap, false);
            } catch (Exception e) {
                Status s = new Status(Status.INFO, UiPlugin.PLUGIN_ID,
                        "The query for the entered Mesocyclone ID has failed.");
                ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                        "Error Finding Mesocyclone ID",
                        "Error Finding Mesocyclone ID", s);
                return null;
            }

            if (availableDataTimes != null && availableDataTimes.length > 0) {
                Arrays.sort(availableDataTimes);
                ArrayUtils.reverse(availableDataTimes);

                selectedDataTimes = new DataTime[frameCount];
                int count = 0;
                int numElements = availableDataTimes.length;
                int indx = 0;
                while (count < frameCount && indx < numElements) {
                    boolean found = false;
                    DataTime dt = availableDataTimes[indx];
                    if (dt == null) {
                        ++indx;
                        continue;
                    }

                    for (DataTime t : selectedDataTimes) {
                        if (dt.equals(t)) {
                            found = true;
                        }
                    }

                    if (!found) {
                        selectedDataTimes[count] = dt;
                        ++count;
                    }
                    ++indx;
                }
            }

            property.setDesiredProduct(ResourceType.PLAN_VIEW);
            property.setNumberOfImages(9999);
            property.setSelectedEntryTimes(selectedDataTimes);

            try {
                property.setEntryQueryParameters(metadataMap, false);
                resp = DataCubeContainer.getData(property, 60000).toArray(
                        new Object[] {});
            } catch (Exception e) {
                Status s = new Status(Status.INFO, UiPlugin.PLUGIN_ID,
                        "The query for the entered Mesocyclone ID has failed.");
                ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                        "Error Finding Mesocyclone ID",
                        "Error Finding Mesocyclone ID", s);
                return null;
            }

            for (int i = 0; i < resp.length; i++) {
                rr = (RadarRecord) resp[i];
                File loc = HDF5Util.findHDF5Location(rr);

                IDataStore dataStore = DataStoreFactory.getDataStore(loc);

                try {
                    RadarDataRetriever.populateRadarRecord(dataStore, rr);
                } catch (Exception e) {
                    e.printStackTrace();
                }

                List<String> ids = RadarRecordUtil.getDMDFeatureIDs(rr);
                if (!ids.contains(mesoId)) {
                    continue;
                }
                c = RadarRecordUtil.getDMDLonLatFromFeatureID(rr, mesoId);
                break;
            }

            return c;
        }

        /**
         * 
         * @param stationID
         * @return the ObStation with the given stationID
         */
        private ObStation getObStation(String stationID) {

            HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
            query.put("pluginName", new RequestConstraint("table"));
            query.put("databasename", new RequestConstraint("metadata"));
            query.put("classname",
                    new RequestConstraint(ObStation.class.getCanonicalName()));
            String gid = ObStation
                    .createGID(ObStation.CAT_TYPE_ICAO, stationID);
            query.put("gid", new RequestConstraint(gid));

            LayerProperty lpParm = new LayerProperty();

            List<Object> list = null;
            try {
                lpParm.setEntryQueryParameters(query, false);
                String tableScript = ScriptCreator.createScript(lpParm);
                list = Loader.loadData(tableScript, 10000);
            } catch (VizException e) {
                statusHandler.handle(Priority.WARN,
                        "Error querying for points", e);
            }

            ObStation obStation = null;
            if (list != null && list.size() != 0) {
                obStation = ((ObStation) (list.get(0)));
            }

            return obStation;
        }

        public void setBaseline(boolean isBaseline) {
            this.isBaseline = isBaseline;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged
         * (com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
         * java.lang.Object)
         */
        @Override
        public void resourceChanged(ChangeType type, Object object) {

            boolean stationLocationHasChanged = false;

            if (stationCoordinates == null || stationCoordinates.size() == 0) {
                return;
            }

            if (isHome()) {
                Coordinate homeCoordinate = pointsDataManager.getHome();

                stationLocationHasChanged = (!homeCoordinate
                        .equals(stationCoordinates.get(0)));

            } else if (isPoint()) {
                Coordinate pointCoordinate = pointsDataManager
                        .getCoordinate(idName);

                stationLocationHasChanged = (!pointCoordinate
                        .equals(stationCoordinates.get(0)));
            } else if (isBaseline()) {
                Coordinate[] baselineCoordinates = toolsDataManager
                        .getBaseline(idName).getCoordinates();

                for (Coordinate baselineCoordinate : baselineCoordinates) {
                    if (!stationCoordinates.contains(baselineCoordinate)) {
                        stationLocationHasChanged = true;
                    }
                }
            }

            if (stationLocationHasChanged) {
                String coordinateChanged = "* "
                        + getFilteredStationIDs(textWidget.getText());
                textWidget.setText(coordinateChanged);

                if ((homeRdo.getSelection() && isHome())
                        || (pointsRdo.getSelection() && isPoint())
                        || (baselinesRdo.getSelection() && isBaseline)) {

                    Event pressEnterEvent = new Event();
                    pressEnterEvent.keyCode = SWT.CR;

                    pressEnterEvent.widget = textWidget;
                    textWidget.notifyListeners(SWT.KeyUp, pressEnterEvent);
                }

            }
        }

    }

    public ChooseByIdDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText(DIALOG_TITLE);

        pointStationIdTextFields = new ArrayList<Text>();
        baselineStationIdTextFields = new ArrayList<Text>();
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout gridLayout = new GridLayout(1, true);
        gridLayout.verticalSpacing = 2;
        return gridLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // prevent escape from closing the dialog
        shell.addListener(SWT.Traverse, new Listener() {
            @Override
            public void handleEvent(Event event) {
                if (event.detail == SWT.TRAVERSE_ESCAPE) {
                    event.doit = false;
                }
            }
        });

        // remove registered listeners when closing the dialog
        shell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                unregisterListeners();
            }
        });

        shell.pack();
        shell.setMinimumSize(new Point(340, 570));
    }

    /**
     * 
     */
    private void unregisterListeners() {
        for (IResourceDataChanged changeListener : changeListeners) {
            homeToolLayer.getResourceData()
                    .removeChangeListener(changeListener);
            pointsToolLayer.getResourceData().removeChangeListener(
                    changeListener);
            baselinesToolLayer.getResourceData().removeChangeListener(
                    changeListener);
        }
    }

    /**
     * Initializes the components.
     */
    private void initializeComponents() {
        createTopBar();
        pointsScroll = new ScrolledComposite(shell, SWT.V_SCROLL);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        pointsScroll.setLayoutData(gd);
        createIdBoxes();
        createBottomBar();
    }

    private void createIdBoxes() {
        Composite pointsComposite = (Composite) pointsScroll.getContent();
        boolean doPack = false;

        // Assume base lines do not change.
        if (pointsComposite == null) {
            String[] names = toolsDataManager.getBaselineNames().toArray(
                    new String[0]);
            Arrays.sort(names);

            pointsComposite = new Composite(pointsScroll, SWT.NONE);
            GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            pointsComposite.setLayout(new GridLayout(1, false));
            pointsComposite.setLayoutData(gd);
            pointsScroll.setContent(pointsComposite);
            for (String name : names) {
                createGroup(pointsComposite, name, true);
            }
        }

        String[] pointNames = pointsDataManager.getPointNames().toArray(
                new String[] {});
        Arrays.sort(pointNames);
        Control[] oldGroups = pointsComposite.getChildren();
        int index = 0;
        for (String point : pointNames) {
            boolean doMoveAbove = false;
            boolean createGroup = true;
            Control moveAbove = null;
            while (index < oldGroups.length) {
                Group group = (Group) oldGroups[index];
                String oldPoint = ((Label) group.getChildren()[NAME_INDEX])
                        .getText();
                boolean isBaseline = (Boolean) group.getData();
                Text pointText = (Text) group.getChildren()[POINT_TEXT_INDEX];
                int cmp = point.compareTo(oldPoint);
                if (cmp == 0) {
                    createGroup = false;
                    if (pointText.isEnabled() == false) {
                        pointStationIdTextFields.add(pointText);
                        pointText.setEnabled(true);
                    }
                    ++index;
                    break;
                } else if (cmp > 0) {
                    pointStationIdTextFields.remove(pointText);

                    if (isBaseline) {
                        pointText.setEnabled(false);
                        pointText.setText("");
                    } else {
                        group.dispose();
                        doPack = true;
                    }
                    ++index;
                } else {
                    moveAbove = group;
                    doMoveAbove = true;
                    break;
                }
            }

            if (createGroup) {
                doPack = true;
                Group group = createGroup(pointsComposite, point, false);
                if (doMoveAbove) {
                    group.moveAbove(moveAbove);
                }
            }
        }

        while (index < oldGroups.length) {
            Group group = (Group) oldGroups[index];
            boolean isBaseline = (Boolean) group.getData();
            Text pointText = (Text) group.getChildren()[POINT_TEXT_INDEX];
            pointStationIdTextFields.remove(pointText);
            if (isBaseline) {
                pointText.setEnabled(false);
            } else {
                oldGroups[index].dispose();
                doPack = true;
            }
            ++index;
        }

        pointsScroll.setMinSize(pointsComposite.computeSize(SWT.DEFAULT,
                SWT.DEFAULT));
        pointsScroll.setExpandHorizontal(true);
        pointsScroll.setExpandVertical(true);
        if (doPack) {
            pointsComposite.pack();
        }
    }

    private Group createGroup(Composite pointsComposite, String name,
            boolean isBaseLine) {
        Group group = new Group(pointsComposite, SWT.NONE);
        group.setData(isBaseLine);

        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.grabExcessHorizontalSpace = true;
        group.setLayoutData(gridData);
        group.setLayout(new GridLayout(3, false));

        Label label = new Label(group, SWT.NONE);
        label.setText(name);
        Text pointText = new Text(group, SWT.BORDER);
        pointText.setText("");
        pointText.addKeyListener(new ChooseByIdKeyListener(name));

        if (isBaseLine) {
            pointText.setEnabled(false);
        } else {
            pointStationIdTextFields.add(pointText);
        }

        Text baselineText = new Text(group, SWT.BORDER);
        baselineText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));

        ChooseByIdKeyListener chooseByIdKeyListener = new ChooseByIdKeyListener(
                name);
        chooseByIdKeyListener.setBaseline(true);
        baselineText.addKeyListener(chooseByIdKeyListener);

        if (isBaseLine) {
            baselineStationIdTextFields.add(baselineText);
        } else {
            baselineText.setEnabled(false);
        }
        return group;
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        initializeComponents();
        pointsDataManager.addPointsChangedListener(this);
    }

    private void createBottomBar() {
        Group g = new Group(shell, SWT.NONE);

        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.CENTER;
        gridData.grabExcessHorizontalSpace = true;
        g.setLayoutData(gridData);
        g.setLayout(new GridLayout(3, false));

        Label label = new Label(g, SWT.NONE);
        label.setText(HOME_POINT);
        Text text1 = new Text(g, SWT.BORDER);
        text1.setText("");

        text1.addKeyListener(new ChooseByIdKeyListener(HOME_POINT));

        homeRdo = new Button(g, SWT.CHECK);
        homeRdo.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        homeRdo.setText("lock");

        List<Text> homeStationIdText = new ArrayList<Text>();
        homeStationIdText.add(text1);

        homeRdo.addSelectionListener(new ChooseByIdSelectionListener(
                homeStationIdText));

    }

    private void createTopBar() {
        Group comp = new Group(shell, SWT.NONE | SWT.FILL);

        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.grabExcessHorizontalSpace = true;
        comp.setLayoutData(gridData);

        comp.setLayout(new GridLayout(4, true));
        Label pointsLabel = new Label(comp, SWT.NONE);
        pointsLabel.setText("Points");
        pointsLabel.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));

        pointsRdo = new Button(comp, SWT.CHECK);
        pointsRdo.setText("lock");
        pointsRdo
                .setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING
                        | GridData.GRAB_HORIZONTAL));

        pointsRdo.addSelectionListener(new ChooseByIdSelectionListener(
                pointStationIdTextFields));

        Label baselinesLabel = new Label(comp, SWT.NONE);
        baselinesLabel.setText("Baselines");
        baselinesLabel
                .setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));
        baselinesRdo = new Button(comp, SWT.CHECK);
        baselinesRdo.setText("lock");
        baselinesRdo
                .setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING
                        | GridData.GRAB_HORIZONTAL));
        baselinesRdo.addSelectionListener(new ChooseByIdSelectionListener(
                baselineStationIdTextFields));
    }

    /**
     * @param containsResource
     */
    public void setHomeResource(AbstractVizResource<?, ?> containsResource) {
        this.homeToolLayer = (HomeToolLayer) containsResource;
    }

    /**
     * @param containsResource
     */
    public void setPointsResource(AbstractVizResource<?, ?> containsResource) {
        this.pointsToolLayer = (PointsToolLayer) containsResource;
    }

    /**
     * @param containsResource
     */
    public void setBaslinesResource(AbstractVizResource<?, ?> containsResource) {
        this.baselinesToolLayer = (InteractiveBaselinesLayer) containsResource;
    }

    public void setDescriptor(IDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    @Override
    protected void disposed() {
        pointsDataManager.removePointsChangedListener(this);
    }

    @Override
    public void pointChanged() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                createIdBoxes();
            }
        });
    }

}
