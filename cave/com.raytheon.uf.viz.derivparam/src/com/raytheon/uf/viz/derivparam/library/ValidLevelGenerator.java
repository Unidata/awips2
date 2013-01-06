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
package com.raytheon.uf.viz.derivparam.library;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.CompareType;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.util.mapping.Mapper;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.level.LevelMapping;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;

/**
 * Generates the valid levels for a derived parameter script based on it
 * validLevels parameter.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/21/2009    #3576     rjpeter     Initial version
 * 
 * &#064;author rjpeter
 * @version 1.0
 */
public class ValidLevelGenerator {
    private enum Type {
        LevelMapping, MasterLevel, Group, Unknown
    };

    private LevelMappingFactory lmf;

    private LevelFactory lf;

    private LevelMapper lm;

    private Map<MasterLevel, Set<Level>> masterLevels;

    private Set<Level> validLevels;

    private Set<MasterLevel> masterLevelsHandled;

    public ValidLevelGenerator() {
        lmf = LevelMappingFactory.getInstance();
        lf = LevelFactory.getInstance();
        lm = LevelMapper.getInstance();
    }

    public Set<Level> generateLevels(String validLevelsString)
            throws IllegalArgumentException, VizCommunicationException {
        masterLevels = new HashMap<MasterLevel, Set<Level>>();
        validLevels = new HashSet<Level>();
        masterLevelsHandled = new HashSet<MasterLevel>();
        if (validLevelsString != null && validLevelsString.length() > 0) {
            String[] levelTokenArray = validLevelsString.split(",");
            List<String> tokensToProcess = new ArrayList<String>(
                    levelTokenArray.length);

            for (String token : levelTokenArray) {
                tokensToProcess.add(token);
            }

            // generate initial list
            Iterator<String> iter = tokensToProcess.iterator();

            while (iter.hasNext()) {
                String token = iter.next();

                if (token.charAt(0) == '!') {
                    token = token.substring(1);
                }
                // See if this is a group.
                Map<MasterLevel, Set<Level>> tmp = lmf
                        .getLevelMapForGroup(token);

                if (tmp != null) {
                    masterLevels = tmp;
                    iter.remove();
                    break;
                }
            }

            if (masterLevels.size() == 0 && tokensToProcess.size() > 0) {
                for (Level l : lmf.getAllLevels()) {
                    MasterLevel ml = l.getMasterLevel();
                    Set<Level> levels = masterLevels.get(ml);
                    if (levels == null) {
                        levels = new HashSet<Level>();
                        masterLevels.put(ml, levels);
                    }

                    levels.add(l);
                }
            }

            if (tokensToProcess.size() > 0) {
                for (String token : tokensToProcess) {
                    try {
                        processLevelToken(token);
                    } catch (CommunicationException e) {
                        throw new VizCommunicationException(e);
                    }
                }
            } else {
                for (Set<Level> levels : masterLevels.values()) {
                    validLevels.addAll(levels);
                }
            }
        }

        return validLevels;
    }

    private void processLevelToken(String token)
            throws VizCommunicationException, CommunicationException {
        boolean negate = token.charAt(0) == '!';
        int rangeIndex = token.indexOf('>');

        // strip ! if is negation
        if (negate) {
            token = token.substring(1);
        }

        // handle range
        if (rangeIndex >= 0) {
            Set<Level> initialDataSet = new HashSet<Level>();

            Level bottomLevel = null;
            Level topLevel = null;

            if (rangeIndex > 0) {
                // may need to check for - and manually build a composite level
                String bottomToken = token.substring(0, rangeIndex);
                Type type = determineType(bottomToken);
                switch (type) {
                case MasterLevel: {
                    initialDataSet = masterLevels.get(lf
                            .getMasterLevel(bottomToken));
                    break;
                }
                case LevelMapping: {
                    List<Level> levels = lmf.getLevelMappingForKey(bottomToken)
                            .getLevels();
                    if (levels.size() == 1) {
                        bottomLevel = levels.get(0);
                    } else {
                        initialDataSet.addAll(levels);
                    }
                    break;
                }
                }
            }

            if (rangeIndex + 1 < token.length()) {
                // may need to check for - and manually build a composite level
                String topToken = token.substring(rangeIndex + 1);
                Type type = determineType(topToken);
                switch (type) {
                case MasterLevel: {
                    initialDataSet = masterLevels.get(lf
                            .getMasterLevel(topToken));
                    break;
                }
                case LevelMapping: {
                    List<Level> levels = lmf.getLevelMappingForKey(topToken)
                            .getLevels();
                    if (levels.size() == 1) {
                        topLevel = levels.get(0);
                    } else {
                        initialDataSet.addAll(levels);
                    }
                    break;
                }
                }
            }

            // verify top and bottom levels compatible
            if (bottomLevel != null
                    && topLevel != null
                    && CompareType.INCOMPATIBLE.equals(bottomLevel
                            .compare(topLevel))
                    && ((topLevel.isRangeLevel() && !bottomLevel.isRangeLevel()) || (bottomLevel
                            .isRangeLevel() && !topLevel.isRangeLevel()))) {
                throw new IllegalArgumentException("Range [" + token
                        + "] contains incompatible levels");
            }

            if (initialDataSet.size() == 0) {
                initialDataSet = masterLevels.get(bottomLevel.getMasterLevel());
            }

            boolean isRange = topLevel == null ? bottomLevel.isRangeLevel()
                    : topLevel.isRangeLevel();

            for (Level l : initialDataSet) {
                if (l.isRangeLevel() != isRange) {
                    continue;
                }
                boolean topValid = false;
                boolean bottomValid = false;
                if (topLevel != null) {
                    if (topLevel.isRangeLevel()) {
                        CompareType c = topLevel.compare(l.getUpperLevel());
                        topValid = CompareType.CONTAINS == c;
                    } else {
                        CompareType c = topLevel.compare(l);
                        topValid = CompareType.BELOW != c;
                    }
                } else {
                    topValid = true;
                }

                if (bottomLevel != null) {
                    if (bottomLevel.isRangeLevel()) {
                        CompareType c = bottomLevel.compare(l.getLowerLevel());
                        bottomValid = CompareType.CONTAINS == c;
                    } else {
                        CompareType c = bottomLevel.compare(l);
                        bottomValid = CompareType.ABOVE != c;
                    }
                } else {
                    bottomValid = true;
                }

                boolean valid = bottomValid && topValid;

                if (negate) {
                    valid = !valid;
                }

                if (valid) {
                    validLevels.add(l);
                }
            }
        } else {
            Type type = determineType(token);

            switch (type) {
            case LevelMapping: {
                List<Level> levels = lmf.getLevelMappingForKey(token)
                        .getLevels();

                if (negate) {
                    if (!validLevels.removeAll(levels)) {
                        // negate based on master list
                        for (Entry<MasterLevel, Set<Level>> entry : masterLevels
                                .entrySet()) {
                            if (!masterLevelsHandled.contains(entry.getKey())) {
                                validLevels.addAll(entry.getValue());
                            }
                        }
                        validLevels.removeAll(levels);
                    }
                } else {
                    validLevels.add(levels.get(0));
                }

                for (Level l : levels) {
                    masterLevelsHandled.add(l.getMasterLevel());
                }

                break;
            }
            case MasterLevel: {
                MasterLevel ml = lf.getMasterLevel(token);
                if (ml == null) {
                    try {
                        ml = lm.lookupMasterLevel(token, Mapper.DEPRECATED);
                    } catch (MultipleMappingException e) {
                        ml = lf.getMasterLevel(e.getArbitraryMapping());
                    }
                }
                if (negate) {
                    for (Entry<MasterLevel, Set<Level>> entry : masterLevels
                            .entrySet()) {
                        if (!masterLevelsHandled.contains(entry.getKey())
                                && !entry.getKey().equals(ml)) {
                            validLevels.addAll(entry.getValue());
                        }
                    }
                } else {
                    Set<Level> levels = masterLevels.get(ml);
                    if (levels != null) {
                        validLevels.addAll(levels);
                    } else {
                        throw new IllegalArgumentException("MasterLevel ["
                                + token + "] not contained in group");
                    }
                }

                masterLevelsHandled.add(ml);
                break;
            }
            case Group: {
                throw new IllegalArgumentException(
                        "Valid Levels containing multiple groups not implemented");
            }
            case Unknown: {
                throw new IllegalArgumentException("Unknown valid level ["
                        + token + "]");
            }
            }
        }
    }

    private Type determineType(String token) throws CommunicationException,
            VizCommunicationException {
        Type rval = null;
        LevelMapping mapping = lmf.getLevelMappingForKey(token);

        if (mapping != null) {
            rval = Type.LevelMapping;
        } else {
            // plane has no mapping check if its a group
            Map<MasterLevel, Set<Level>> tmp = lmf.getLevelMapForGroup(token);
            if (tmp != null && tmp.size() > 0) {
                rval = Type.Group;
            } else {
                // plane was not a group, see if it is a master level
                MasterLevel ml = lf.getMasterLevel(token);
                if (ml == null) {
                    try {
                        ml = lm.lookupMasterLevel(token, Mapper.DEPRECATED);
                    } catch (MultipleMappingException e) {
                        ml = lf.getMasterLevel(e.getArbitraryMapping());
                    }
                }
                if (ml != null) {
                    rval = Type.MasterLevel;
                } else {
                    rval = Type.Unknown;
                }
            }

        }

        return rval;
    }
}
