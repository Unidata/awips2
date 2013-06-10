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
package com.raytheon.uf.viz.core.level;

import java.text.ParsePosition;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.CompareType;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;

/**
 * Level utilities
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/21/2009    #3576     rjpeter     Initial version
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class LevelUtilities {
    private static Unit<?> hPa;

    static {
        try {
            hPa = UnitFormat.getUCUMInstance().parseProductUnit("hPa",
                    new ParsePosition(0));
        } catch (Exception e) {
            // this is bad
        }
    }

    public static boolean isPressureLevel(long levelId)
            throws VizCommunicationException {
        try {
            return isPressureLevel(LevelFactory.getInstance().getLevel(levelId)
                    .getMasterLevel());
        } catch (CommunicationException e) {
            throw new VizCommunicationException(e);
        }
    }

    public static boolean isPressureLevel(String masterLevelName)
            throws VizCommunicationException {
        try {
            return isPressureLevel(LevelFactory.getInstance().getMasterLevel(
                    masterLevelName));
        } catch (CommunicationException e) {
            throw new VizCommunicationException(e);
        }
    }

    public static boolean isPressureLevel(Level level) {
        return isPressureLevel(level.getMasterLevel());
    }

    public static boolean isPressureLevel(MasterLevel ml) {
        Unit<?> unit = ml.getUnit();
        return unit != null && unit.isCompatible(hPa);
    }

    private static Map<String, NavigableSet<Level>> masterLevelToOrderedSet = null;

    /**
     * Get all single levels for a given masterlevel, ordered from lowest to
     * highest.
     * 
     * @param masterLevelName
     * @return
     * @throws CommunicationException
     */
    public synchronized static NavigableSet<Level> getOrderedSetOfStandardLevels(
            String masterLevelName) throws VizCommunicationException {
        if (masterLevelToOrderedSet == null) {
            Comparator<Level> levelComparator = new Comparator<Level>() {

                @Override
                public int compare(Level l1, Level l2) {
                    CompareType rel = l1.compare(l2);
                    if (rel == CompareType.ABOVE) {
                        return 1;
                    } else if (rel == CompareType.BELOW) {
                        return -1;
                    } else {
                        return 0;
                    }
                }

            };
            Map<String, NavigableSet<Level>> masterLevelToOrderedSet = new HashMap<String, NavigableSet<Level>>();
            Set<Level> allLevels;
            try {
                allLevels = LevelMappingFactory.getInstance(
                        LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                        .getAllLevels();
            } catch (CommunicationException e) {
                throw new VizCommunicationException(e);
            }
            for (Level level : allLevels) {
                NavigableSet<Level> levels = masterLevelToOrderedSet.get(level
                        .getMasterLevel().getName());
                if (levels == null) {
                    levels = new TreeSet<Level>(levelComparator);
                    masterLevelToOrderedSet.put(level.getMasterLevel()
                            .getName(), levels);
                }
                if (level.isRangeLevel()) {

                    try {
                        levels.add(level.getUpperLevel());
                        levels.add(level.getLowerLevel());
                    } catch (CommunicationException e) {
                        throw new VizCommunicationException(e);
                    }
                } else {
                    levels.add(level);
                }
            }
            LevelUtilities.masterLevelToOrderedSet = masterLevelToOrderedSet;
        }
        return masterLevelToOrderedSet.get(masterLevelName);

    }
}
