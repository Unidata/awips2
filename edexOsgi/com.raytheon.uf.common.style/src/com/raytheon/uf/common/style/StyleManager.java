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

package com.raytheon.uf.common.style;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Manages the visualization styles
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2007                 njensen     Initial creation
 * May 21, 2012 DR 14833        gzhang		Adding a getter for StyleRuleset
 * Sep 06, 2013 2251       mnash       Add ability to plug in new style types
 * Sep 24, 2013 2404        bclement        changed to look in common for files
 * </pre>
 * 
 * @author njensen
 */
public class StyleManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StyleManager.class);

    public static enum StyleType implements IStyleType {
        IMAGERY("ImageryStyleRules.xml"), CONTOUR("ContourStyleRules.xml"), ARROW(
                "ArrowStyleRules.xml"), GEOMETRY("GeometryStyleRules.xml");

        private String[] extensions;

        private StyleType(String extension) {
            this.extensions = new String[] { extension };
        }

        @Override
        public String[] getExtensions() {
            return extensions;
        }
    };

    private static StyleManager instance;

    // although HashMap allows null keys, would rather use this than Hashtable
    private Map<IStyleType, StyleRuleset> rules = new HashMap<IStyleType, StyleRuleset>();

    private StyleManager() {
    }

    public static StyleManager getInstance() {
        if (instance == null) {
            instance = new StyleManager();
        }

        return instance;
    }

    private void loadRules(IStyleType aType) {
        try {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationFile[] commonFiles = pathMgr.listFiles(pathMgr
                    .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC),
                    "styleRules", aType.getExtensions(), true, true);
            StyleRuleset rules = new StyleRuleset();
            addRules(commonFiles, rules);
            this.rules.put(aType, rules);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error loading style rules",
                    e);
        }
    }

    /**
     * Add style rules from jaxb files to rule set
     * 
     * @param files
     * @param rules
     * @throws SerializationException
     */
    private void addRules(LocalizationFile[] files, StyleRuleset rules)
            throws SerializationException {
        if (files == null) {
            return;
        }
        for (LocalizationFile lf : files) {
            rules.addStyleRules((StyleRuleset) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(StyleRuleset.class, lf.getFile()
                            .getPath()));
        }
    }

    /**
     * Gets the best matching style rule for a particular match criteria
     * 
     * @param aStyleType
     *            the type of style
     * @param aCriteria
     *            the match criteria to find the best match for
     * @return the best matching style rule, or null if no matches are found
     * @throws StyleException
     */
    public StyleRule getStyleRule(IStyleType aStyleType, MatchCriteria aCriteria)
            throws StyleException {
        synchronized (aStyleType) {
            if (!this.rules.containsKey(aStyleType)) {
                loadRules(aStyleType);
            }
        }
        StyleRuleset set = this.rules.get(aStyleType);
        StyleRule bestMatch = null;
        if (set != null) {
            List<StyleRule> rules = this.rules.get(aStyleType).getStyleRules();
            int matchRank = 0;
            try {
                for (StyleRule rule : rules) {
                    int value = aCriteria.matches(rule.getMatchCriteria());
                    if (value > matchRank) {
                        matchRank = value;
                        bestMatch = rule;
                    }
                }
            } catch (Exception e) {
                throw new StyleException("Error determining matching rules.", e);
            }
        }
        return bestMatch;
    }

    public static double[] calculateMinMax(double level, double minLevel,
            double maxLevel, double minLogValue1, double minLogValue2,
            double maxLogValue1, double maxLogValue2, boolean logarithmic) {
        if (logarithmic) {
            level = Math.log(level);
            minLevel = Math.log(minLevel);
            maxLevel = Math.log(maxLevel);
        }
        // Calculate the percentage of each that is applicable
        double weight = (level - minLevel) / (maxLevel - minLevel);

        // Calculate new weighted mins and maxes
        double vmin = (minLogValue1 * weight) + (minLogValue2 * (1.0 - weight));
        double vmax = (maxLogValue1 * weight) + (maxLogValue2 * (1.0 - weight));

        return new double[] { vmin, vmax };

    }

    /**
     * 2012-05-21 DR 14833: FFMP uses this getter to find the color map if a
     * user modified ffmpImageryStlyeRules.xml incorrectly.
     * 
     * @param st
     *            : StyleType
     * @return: StyleRuleset related to the StyleType
     */
    public StyleRuleset getStyleRuleSet(IStyleType st) {

        synchronized (st) {

            if (!rules.containsKey(st)) {
                loadRules(st);
            }
        }

        return rules.get(st);
    }
}
