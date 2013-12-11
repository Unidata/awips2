/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.volumebrowser;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.procedures.AlterBundleChangeEvent;
import com.raytheon.uf.viz.core.procedures.AlterBundleContributorAdapter;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.IAlterBundleContributor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IBaseLinesContainer;
import com.raytheon.uf.viz.core.rsc.IPointsToolContainer;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointUtilities;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.volumebrowser.datacatalog.AbstractDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogManager;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * This class generates the alter bundle's contributions for points and lines.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer       Description
 * ------------- -------- -------------- --------------------------------------
 * Aug 08, 2012  875      rferrel        Generate menu entries for points.
 * Oct 03, 2012  1248     rferrel        Added listener for when points change.
 * May 03, 2013  14824    mgamazaychikov Added alterResource method
 * Dec 11, 2013  2602     bsteffen       Fix compiler warnings and format.
 * 
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ToolsAlterBundleContributor extends AlterBundleContributorAdapter {
    private static final String POINTS_PREFIX = "Point-";

    private static final String LINES_PREFIX = "Line-";

    private static final String POINTS_KEY = "point";

    private static final String LINES_KEY = "line";

    private static final String PLUGIN_KEY = "pluginName";

    private static final String REPORTYPE_KEY = "reportType";

    private IPointChangedListener pointChangedListener;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.procedures.IAlterBundleContributor#getAlterables
     * ()
     */
    @Override
    public Map<String, String[]> getAlterables() {
        Map<String, String[]> alterables = new HashMap<String, String[]>();
        String[] linesValues = createLineArray();
        String[] pointsValues = createPointArray();

        alterables.put(LINES_KEY, linesValues);
        alterables.put(POINTS_KEY, pointsValues);

        return alterables;
    }

    private final static Pattern pat = Pattern.compile(File.separator + "+");

    private List<String> createChildrenList(PointsDataManager pdm,
            IPointNode parent) {
        List<String> childrenList = new ArrayList<String>();
        for (IPointNode node : pdm.getChildren(parent)) {
            if (node.isGroup()) {
                String value = node.getGroup().replace(
                        PointUtilities.DELIM_CHAR, ' ')
                        + File.separator;
                value = pat.matcher(value).replaceAll(
                        IAlterBundleContributor.MENU_SEPARATOR);
                childrenList.add(value);
                childrenList.addAll(createChildrenList(pdm, node));
            } else {
                String value = (node.getGroup() + File.separator
                        + POINTS_PREFIX + node.getName()).replace(
                        PointUtilities.DELIM_CHAR, ' ');
                value = pat.matcher(value).replaceAll(
                        IAlterBundleContributor.MENU_SEPARATOR);
                childrenList.add(value);
            }
        }
        return childrenList;
    }

    private String[] createLineArray() {
        ToolsDataManager tdm = ToolsDataManager.getInstance();
        Collection<String> blNames = tdm.getBaselineNames();
        String[] lines = new String[blNames.size()];
        int i = 0;
        for (String line : blNames) {
            lines[i] = LINES_PREFIX + line;
            ++i;
        }

        Arrays.sort(lines);
        String[] linesValues = new String[lines.length + 2];
        linesValues[0] = ProcedureDlg.ORIGINAL;
        linesValues[1] = ProcedureDlg.CURRENT;
        System.arraycopy(lines, 0, linesValues, 2, lines.length);
        return linesValues;
    }

    /**
     * @return pointsValues
     */
    private String[] createPointArray() {
        PointsDataManager pdm = PointsDataManager.getInstance();

        List<String> pointsList = new ArrayList<String>();
        pointsList.add(ProcedureDlg.ORIGINAL);
        pointsList.add(ProcedureDlg.CURRENT);
        pointsList.add(IAlterBundleContributor.MI_SEPARATOR);
        pointsList.addAll(createChildrenList(pdm, null));
        String[] pointsValues = pointsList.toArray(new String[0]);
        return pointsValues;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.procedures.IAlterBundleContributor#alterBundle
     * (com.raytheon.uf.viz.core.procedures.Bundle, java.lang.String,
     * java.lang.String)
     */
    @Override
    public void alterBundle(Bundle bundleToAlter, String alterKey,
            String alterValue) {
        if (alterValue != null) {
            if (POINTS_KEY.equals(alterKey)) {
                if (ProcedureDlg.CURRENT.equals(alterValue)) {
                    for (AbstractRenderableDisplay display : bundleToAlter
                            .getDisplays()) {
                        replaceWithCurrentPoints(display.getDescriptor()
                                .getResourceList());
                    }
                } else if (ProcedureDlg.ORIGINAL.equals(alterValue) == false) {
                    String point = alterValue.replace(POINTS_PREFIX, "");
                    alterPoints(bundleToAlter, point);
                }
            } else if (LINES_KEY.equals(alterKey)) {
                if (ProcedureDlg.CURRENT.equals(alterValue)) {
                    replaceWithCurrentLines(bundleToAlter);
                } else if (ProcedureDlg.ORIGINAL.equals(alterValue) == false) {
                    String line = alterValue.replace(LINES_PREFIX, "");
                    alterLines(bundleToAlter, line);
                }
            }
        }
    }

    /**
     * @param list
     */
    private void replaceWithCurrentPoints(ResourceList list) {
        for (ResourcePair rp : list) {
            AbstractResourceData rData = rp.getResourceData();
            if (rData instanceof IPointsToolContainer) {
                alterContainer((IPointsToolContainer) rData,
                        ((IPointsToolContainer) rData).getPointLetter());
            } else if (rData instanceof IResourceGroup) {
                replaceWithCurrentPoints(((IResourceGroup) rData)
                        .getResourceList());
            }
        }
    }

    /**
     * @param b
     */
    private void replaceWithCurrentLines(Bundle b) {
        for (AbstractRenderableDisplay display : b.getDisplays()) {
            IDescriptor desc = display.getDescriptor();
            if (desc instanceof IBaseLinesContainer) {
                String line = ((IBaseLinesContainer) desc).getBaseLine();
                if (line != null && line.startsWith("Line")) {
                    alterContainer((IBaseLinesContainer) desc,
                            line.replace("Line", ""));
                }
            }
        }
    }

    /**
     * @param bundleToAlter
     * @param point
     */
    private void alterPoints(Bundle bundleToAlter, String point) {
        for (AbstractRenderableDisplay display : bundleToAlter.getDisplays()) {
            alterResourceList(display.getDescriptor().getResourceList(), point);
        }
    }

    /**
     * @param list
     * @param selectedString
     */
    private void alterResourceList(ResourceList list, String selectedString) {
        for (ResourcePair rp : list) {
            AbstractResourceData rData = rp.getResourceData();
            if (rData instanceof AbstractRequestableResourceData) {
                alterResource((AbstractRequestableResourceData) rData,
                        POINTS_KEY, selectedString);
                alterContainer((IPointsToolContainer) rData, selectedString);
            } else if (rData instanceof IResourceGroup) {
                alterResourceList(((IResourceGroup) rData).getResourceList(),
                        selectedString);
            }
        }
    }

    private void alterResource(AbstractRequestableResourceData data,
            String selectedKey, String selectedString) {
        if (selectedString == null) {
            return;
        }
        HashMap<String, RequestConstraint> metadataMap = data.getMetadataMap();
        HashMap<String, RequestConstraint> metadataMapOld = new HashMap<String, RequestConstraint>();
        // make a copy of original matedata map
        metadataMapOld.putAll(metadataMap);
        // obtain data catalog
        AbstractDataCatalog ac = getDataCatalog(metadataMap);
        if (ac == null) {
            return;
        }
        // change the parameters in the original metadata map
        ac.alterProductParameters(selectedKey, selectedString, metadataMap);

        // in addition to metadata map resource data needs to be modified
        // for CrossSectionResourceData
        if (data instanceof CrossSectionResourceData) {
            String theKeyToChange = null;
            Iterator<Map.Entry<String, RequestConstraint>> it = metadataMap
                    .entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry<String, RequestConstraint> pairs = it.next();
                String currentKey = pairs.getKey();
                String currentValue = metadataMap.get(currentKey)
                        .getConstraintValue();
                if (metadataMapOld.containsKey(currentKey)) {
                    String oldValue = metadataMapOld.get(currentKey)
                            .getConstraintValue();
                    if (!oldValue.equalsIgnoreCase(currentValue)) {
                        theKeyToChange = currentKey;
                    }
                }
            }
            String stationID = metadataMap.get(theKeyToChange)
                    .getConstraintValue();
            List<String> stationIDs = Arrays.asList(stationID
                    .split("\\s*,\\s*"));
            ((CrossSectionResourceData) data).setStationIDs(stationIDs);
        }
    }

    private AbstractDataCatalog getDataCatalog(
            HashMap<String, RequestConstraint> metadataMap) {
        String pluginName = null;
        String reportType = null;
        if (metadataMap.containsKey(PLUGIN_KEY)) {
            pluginName = metadataMap.get(PLUGIN_KEY).getConstraintValue();
        } else {
            return null;
        }
        if (metadataMap.containsKey(REPORTYPE_KEY)) {
            reportType = metadataMap.get(REPORTYPE_KEY).getConstraintValue();
        } else {
            return null;
        }
        String sourcesKey = pluginName + reportType;

        SelectedData sd = new SelectedData(null, sourcesKey, null, null, null,
                null, null);
        return (AbstractDataCatalog) DataCatalogManager.getDataCatalogManager()
                .getDataCatalog(sd);
    }

    /**
     * @param rData
     * @param selectedString
     */
    private void alterContainer(IPointsToolContainer container,
            String selectedString) {
        Coordinate point = PointsDataManager.getInstance().getCoordinate(
                selectedString);
        container.setPointCoordinate(point);
        container.setPointLetter(selectedString);
    }

    /**
     * @param bundleToAlter
     * @param line
     */
    private void alterLines(Bundle bundleToAlter, String line) {
        for (AbstractRenderableDisplay display : bundleToAlter.getDisplays()) {
            if (display.getDescriptor() instanceof IBaseLinesContainer) {
                alterContainer((IBaseLinesContainer) display.getDescriptor(),
                        line);
                ResourceList rl = display.getDescriptor().getResourceList();
                for (ResourcePair rp : rl) {
                    AbstractResourceData rData = rp.getResourceData();
                    if (rData instanceof AbstractRequestableResourceData) {
                        alterResource((AbstractRequestableResourceData) rData,
                                LINES_KEY, line);
                    }
                }
            }
        }
    }

    /**
     * @param container
     * @param selectedString
     */
    private void alterContainer(IBaseLinesContainer container,
            String selectedString) {
        LineString line = ToolsDataManager.getInstance().getBaseline(
                selectedString);
        container.setBaseLine(selectedString);
        container.setBaseLineString(line);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.procedures.AlterBundleContributorAdapter#
     * getAlterables(java.lang.String)
     */
    @Override
    public String[] getAlterables(String key) {
        if (key == POINTS_KEY) {
            return createPointArray();
        } else if (key == LINES_KEY) {
            return createLineArray();
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.procedures.AlterBundleContributorAdapter#
     * listenerSetup()
     */
    @Override
    public void listenerSetup() {
        if (pointChangedListener == null) {
            pointChangedListener = new IPointChangedListener() {

                @Override
                public void pointChanged() {
                    notifyBundleListeners();
                }
            };
            PointsDataManager.getInstance().addPointsChangedListener(
                    pointChangedListener);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.procedures.AlterBundleContributorAdapter#
     * listenerShutdown()
     */
    @Override
    public void listenerShutdown() {
        if (pointChangedListener != null) {
            PointsDataManager.getInstance().removePointsChangedListener(
                    pointChangedListener);
            pointChangedListener = null;
        }
    }

    /**
     * Received notification of changes to points notify anyone interested in
     * the change.
     */
    private void notifyBundleListeners() {
        AlterBundleChangeEvent event = new AlterBundleChangeEvent(
                new String[] { POINTS_KEY });
        fireAlterBundleChangeListener(event);
    }
}
