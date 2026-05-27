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
package com.raytheon.uf.viz.grid.radar;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.derivparam.tree.AbstractCubeLevelNode;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.data.CubeRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.CubeLevel;

/**
 *
 * Implementation of {@link AbstractCubeLevelNode} that is intended specifically
 * for cubes of data from a radar source.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------
 * Jan 19, 2010  4126     bsteffen  Initial creation
 * Mar 22, 2016  5439     bsteffen  Remove unneccessary constructor arg.
 * Aug 15, 2017  6332     bsteffen  Move to viz.grid.radar plugin
 * Jul 15, 2024  2037624  mapeters  Override methods to support virtual volumes
 * Oct 14, 2024  2037939  mapeters  Override getAvailability()
 *
 * </pre>
 *
 * @author bsteffen
 */
public class RadarCubeLevelNode extends AbstractCubeLevelNode {

    public RadarCubeLevelNode(AbstractCubeLevelNode that) {
        super(that);
    }

    public RadarCubeLevelNode(
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> levels,
            String modelName) {
        super(levels, modelName);
    }

    @Override
    protected List<AbstractRequestableData> getParamData(
            Map<TimeAndSpace, List<AbstractRequestableData>> paramMap,
            TimeAndSpace timeAndSpace) {
        /*
         * Overridden to use matches() rather than a normal map lookup. For
         * virtual volumes, there will be a normal TimeAndSpace entry for the
         * latest volume scan's data, along with a VirtualTimeAndSpace entry for
         * data from the previous scan that should be blended into the current
         * scan. This combines those 2 entries.
         */
        return paramMap.entrySet().stream()
                .filter(entry -> timeAndSpace.matches(entry.getKey()))
                .map(Entry::getValue).flatMap(List::stream)
                .collect(Collectors.toList());
    }

    @Override
    protected CubeRequestableData createCubeRequestableData(
            AbstractRequestableData dataToCopy) {
        return new RadarCubeRequestableData(dataToCopy);
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvailability)
            throws DataCubeException {
        Set<Pair<TimeAndSpace, Level>> paramAvailability = getParamAvailability(
                dependencyAvailability);
        Set<TimeAndSpace> multiLevelAvailability = getMultiLevelAvailability(
                paramAvailability);
        RadarVirtualDerivedTimeAndSpace virtualDerivedAvail = getMergedVirtualAvailability(
                multiLevelAvailability, paramAvailability);
        if (virtualDerivedAvail != null) {
            // Replace entries that were merged with the merge result
            multiLevelAvailability.removeIf(virtualDerivedAvail::matches);
            multiLevelAvailability.add(virtualDerivedAvail);
        }
        return multiLevelAvailability;
    }

    protected Set<Pair<TimeAndSpace, Level>> getParamAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> dependencyAvailability) {
        Set<Pair<TimeAndSpace, Level>> rval = new HashSet<>();
        for (CubeLevel<AbstractRequestableNode, AbstractRequestableNode> level : levels) {
            for (TimeAndSpace tas : dependencyAvailability
                    .get(level.getParam())) {
                rval.add(ImmutablePair.of(tas, level.getParam().getLevel()));
            }
        }
        return rval;
    }

    /**
     * @param paramAvailability
     *            availability of param dependency
     * @return TimeAndSpace values that are available for multiple levels
     */
    protected static Set<TimeAndSpace> getMultiLevelAvailability(
            Set<Pair<TimeAndSpace, Level>> paramAvailability) {
        /*
         * This is copied from super.getAvailability() and modified to use
         * matches() to handle RadarVirtual*TimeAndSpace values
         */
        // things in one are available for one level
        Set<TimeAndSpace> one = new HashSet<>();
        // things in two are available for two or more levels.
        Set<TimeAndSpace> two = new HashSet<>();

        for (Pair<TimeAndSpace, Level> paramAvail : paramAvailability) {
            TimeAndSpace paramTas = paramAvail.getLeft();
            Optional<TimeAndSpace> matchInOne = one.stream()
                    .filter(paramTas::matches).findAny();
            if (matchInOne.isPresent()) {
                two.add(matchInOne.get());
                two.add(paramTas);
            } else {
                one.add(paramTas);
            }

        }

        return two;
    }

    /**
     * When a virtual volume is used for the current scan, there will be normal
     * TimeAndSpace entries for tilts up to the current scan tilt, and
     * RadarVirtualTimeAndSpace entries for higher tilts that actually reference
     * data from the previous scan.
     *
     * Check for this scenario and merge those time/space entries into a single
     * RadarVirtualDerivedTimeAndSpace that indicates the current scan tilt
     * where the data switches from the current scan to the previous scan.
     *
     * This is necessary because this availability is passed down to
     * dependencies and this lets the dependencies determine which scan time to
     * return data for, depending on which tilt each dependency is for.
     *
     * @param multiLevelAvailability
     * @param paramAvailability
     * @return merged RadarVirtualDerivedTimeAndSpace if there was virtual
     *         availability for the current scan, otherwise null
     */
    protected static RadarVirtualDerivedTimeAndSpace getMergedVirtualAvailability(
            Set<TimeAndSpace> multiLevelAvailability,
            Set<Pair<TimeAndSpace, Level>> paramAvailability) {
        RadarVirtualTimeAndSpace virtTas = null;
        for (TimeAndSpace tas : multiLevelAvailability) {
            if (tas instanceof RadarVirtualTimeAndSpace) {
                virtTas = (RadarVirtualTimeAndSpace) tas;
                break;
            }
        }
        if (virtTas == null) {
            return null;
        }
        double currScanTilt = Double.MIN_VALUE;
        for (Pair<TimeAndSpace, Level> paramAvail : paramAvailability) {
            TimeAndSpace paramTas = paramAvail.getLeft();
            if (!paramTas.isVirtual() && paramTas.matches(virtTas)) {
                double tilt = paramAvail.getRight().getLevelonevalue();
                currScanTilt = Math.max(currScanTilt, tilt);
            }
        }
        return new RadarVirtualDerivedTimeAndSpace(virtTas.getTime(),
                virtTas.getSpace(), virtTas.getPrevScanTime(), currScanTilt);
    }
}
