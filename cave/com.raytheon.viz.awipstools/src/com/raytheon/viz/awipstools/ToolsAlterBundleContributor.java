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
package com.raytheon.viz.awipstools;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.IAlterBundleContributor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IBaseLinesContainer;
import com.raytheon.uf.viz.core.rsc.IPointsToolContainer;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg;
import com.raytheon.uf.viz.points.PointUtilities;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * This class generates the alternate bundle's contributions for points and
 * lines.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Aug 08, 2012 #875       rferrel     Generate menu entries for points.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ToolsAlterBundleContributor implements IAlterBundleContributor {

    private static final String POINTS_PREFIX = "Point-";

    private static final String LINES_PREFIX = "Line-";

    private static final String POINTS_KEY = "point";

    private static final String LINES_KEY = "line";

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
        ToolsDataManager tdm = ToolsDataManager.getInstance();
        PointsDataManager pdm = PointsDataManager.getInstance();
        Collection<String> blNames = tdm.getBaselineNames();
        String[] lines = new String[blNames.size()];
        int i = 0;
        for (String line : blNames) {
            lines[i] = LINES_PREFIX + line;
            ++i;
        }
        Arrays.sort(lines);

        Collection<String> pNames = pdm.getPointNames();
        String[] points = new String[pNames.size()];
        i = 0;
        for (String point : pNames) {
            points[i] = POINTS_PREFIX + point;
            ++i;
        }
        Arrays.sort(points);

        List<String> pointsList = new ArrayList<String>();
        pointsList.add(ProcedureDlg.ORIGINAL);
        pointsList.add(ProcedureDlg.CURRENT);
        pointsList.add(IAlterBundleContributor.MI_SEPARATOR);
        pointsList.addAll(createChildrenList(pdm, null));
        String[] pointsValues = pointsList.toArray(new String[0]);

        String[] linesValues = new String[lines.length + 2];
        linesValues[0] = ProcedureDlg.ORIGINAL;
        linesValues[1] = ProcedureDlg.CURRENT;
        System.arraycopy(lines, 0, linesValues, 2, lines.length);

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

    private void alterResourceList(ResourceList list, String selectedString) {
        for (ResourcePair rp : list) {
            AbstractResourceData rData = rp.getResourceData();
            if (rData instanceof IPointsToolContainer) {
                alterContainer((IPointsToolContainer) rData, selectedString);
            } else if (rData instanceof IResourceGroup) {
                alterResourceList(((IResourceGroup) rData).getResourceList(),
                        selectedString);
            }
        }
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
            }
        }
    }

    private void alterContainer(IBaseLinesContainer container,
            String selectedString) {
        LineString line = ToolsDataManager.getInstance().getBaseline(
                selectedString);
        container.setBaseLine(selectedString);
        container.setBaseLineString(line);
    }

}
