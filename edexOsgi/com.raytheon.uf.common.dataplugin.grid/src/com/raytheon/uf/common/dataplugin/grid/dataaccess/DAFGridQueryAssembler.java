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
package com.raytheon.uf.common.dataplugin.grid.dataaccess;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.dataquery.GridQueryAssembler;
import com.raytheon.uf.common.dataplugin.grid.mapping.DatasetIdMapper;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.util.mapping.Mapper;

/**
 * Assemble a query to use when requesting grid data through the DAF. Supports
 * advanced queries by providing setters that take RequestConstraints directly,
 * instead of only allowing EQUALS constraints like the base class.
 * 
 * This implementation uses the ParameterMapper, LevelMapper, and
 * DatasetIdMapper so it can accept fields using namespaces that aren't in the
 * database and transform them to valid database requests. Even if no custom
 * namespace is being used the assembler should still be used to check for
 * deprecated names.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 06, 2016 5728       mapeters    Initial creation
 * 
 * </pre>
 * 
 * @author mapeters
 */

public class DAFGridQueryAssembler extends GridQueryAssembler {

    @Override
    protected Map<String, RequestConstraint> getLevelConstraints() {
        if (isSimpleConstraint(masterLevelNameConstraint)
                && isSimpleConstraint(levelOneConstraint)
                && (levelTwoConstraint == null || isSimpleConstraint(levelTwoConstraint))) {
            return getSimpleLevelConstraints();
        } else {
            return getComplexLevelConstraints();
        }
    }

    /**
     * Get a map of the master level name, level one, and level two constraints
     * when all of their identifier values are EQUALS or IN constraints (level
     * two identifier may not be set)
     * 
     * @return a map of the level constraints
     */
    private Map<String, RequestConstraint> getSimpleLevelConstraints() {
        Double[] levelOneValues = getLevelConstraintValuesAsDoubles(levelOneConstraint);
        Double[] levelTwoValues = getLevelConstraintValuesAsDoubles(levelTwoConstraint);
        Set<Level> levels = new HashSet<>();
        for (String masterLevelAlias : getSimpleConstraintValues(masterLevelNameConstraint)) {
            for (Double levelOneValue : levelOneValues) {
                /*
                 * Lookup levels and determine constraints based off their
                 * values (handles mapping of master level aliases, unit
                 * conversion, and reversing level one and level two values when
                 * in wrong order)
                 */
                if (levelTwoValues != null) {
                    for (Double levelTwoValue : levelTwoValues) {
                        levels.addAll(LevelMapper.getInstance().lookupLevels(
                                masterLevelAlias, levelNamespace,
                                levelOneValue, levelTwoValue, levelUnits));
                    }
                } else {
                    levels.addAll(LevelMapper.getInstance().lookupLevels(
                            masterLevelAlias, levelNamespace, levelOneValue,
                            levelUnits));
                }
            }
        }

        return getLevelConstraintsFromLevels(levels);
    }

    /**
     * Get a map of the master level name, level one, and level two constraints
     * when any of their identifier values is more complex than an EQUALS or IN
     * constraint
     * 
     * @return a map of the level constraints
     */
    private Map<String, RequestConstraint> getComplexLevelConstraints() {
        if (levelUnits != null) {
            /*
             * Converting units on more complex constraints is more complicated,
             * and this use case can't currently happen, so don't support for
             * now
             */
            String msg = "Retrieving grid data with advanced level "
                    + "constraints is not currently supported when "
                    + "level units are set.";
            throw new IncompatibleRequestException(msg);
        }

        Map<String, RequestConstraint> constraintMap = new HashMap<>();
        constraintMap.put(GridConstants.MASTER_LEVEL_NAME,
                getMasterLevelNameConstraint());
        constraintMap.put(GridConstants.LEVEL_ONE, levelOneConstraint);
        if (levelTwoConstraint != null) {
            constraintMap.put(GridConstants.LEVEL_TWO, levelTwoConstraint);
        }

        return constraintMap;
    }

    /**
     * Get the RequestConstraint specifying the base names to retrieve data for.
     * Handles alias mapping for simple (EQUALS or IN) constraints and ensures
     * namespace is not set for more complex constraints
     * 
     * @param constraint
     *            the RequestConstraint specifying the aliases or base names
     * @param mapper
     * @param namespace
     * @param identifierName
     * @return the base names constraint
     */
    protected RequestConstraint getConstraint(RequestConstraint constraint,
            Mapper mapper, String namespace, String identifierName) {
        if (isSimpleConstraint(constraint)) {
            /*
             * If simple EQUALS or IN constraint, use namespace since we can
             * default to the constraint values if they aren't in the namespace
             */
            String[] aliases = getSimpleConstraintValues(constraint);
            Set<String> baseNames = new HashSet<>();
            for (String alias : aliases) {
                baseNames.addAll(mapper.lookupBaseNames(alias, namespace));
            }
            return new RequestConstraint(baseNames);
        } else {
            /*
             * If more complex constraint (e.g. NOT IN or >), just pass it along
             * to the DB. Don't allow custom namespaces since we have no
             * efficient way to perform mappings without excluding base names
             * that don't have an alias in the namespace
             */
            if (namespace != Mapper.DEPRECATED) {
                throw new IncompatibleRequestException(
                        "Namespaces are not supported when the identifier value for '"
                                + identifierName
                                + "' is a RequestConstraint other than EQUALS (=) or IN.");
            }
            return constraint;
        }
    }

    @Override
    protected RequestConstraint getDatasetIdConstraint() {
        return getConstraint(datasetIdConstraint,
                DatasetIdMapper.getInstance(), datasetIdNamespace,
                GridConstants.DATASET_ID);
    }

    @Override
    protected RequestConstraint getMasterLevelNameConstraint() {
        return getConstraint(masterLevelNameConstraint,
                LevelMapper.getInstance(), levelNamespace,
                GridConstants.MASTER_LEVEL_NAME);
    }

    /**
     * Get a list of the constraint values satisfying the given EQUALS or IN
     * constraint.
     * 
     * @param constraint
     *            a simple (EQUALS or IN) constraint
     * @return the values that satisfy the constraint
     */
    private String[] getSimpleConstraintValues(RequestConstraint constraint) {
        ConstraintType type = constraint.getConstraintType();
        if (type == ConstraintType.EQUALS) {
            return new String[] { constraint.getConstraintValue() };
        } else {
            // type == ConstraintType.IN
            String[] aliases = constraint.getConstraintValue().split(",");
            for (int i = 0; i < aliases.length; ++i) {
                aliases[i] = aliases[i].trim();
            }
            return aliases;
        }
    }

    /**
     * Get a list of the Double values satisfying the given EQUALS or IN
     * constraint
     * 
     * @param constraint
     *            a simple (EQUALS or IN) constraint for level one or two values
     * @return the double values that satisfy the level constraint (or null if
     *         given constraint is null)
     */
    private Double[] getLevelConstraintValuesAsDoubles(
            RequestConstraint constraint) {
        if (constraint == null) {
            return null;
        }

        // Get list of values as strings
        String[] strValues = getSimpleConstraintValues(constraint);

        // Convert to Doubles
        Double[] doubleVals = new Double[strValues.length];
        for (int i = 0; i < strValues.length; ++i) {
            try {
                doubleVals[i] = Double.valueOf(strValues[i]);
            } catch (NumberFormatException e) {
                throw new IncompatibleRequestException(strValues[i]
                        + " is not a valid level identifier value", e);
            }
        }

        return doubleVals;
    }

    /**
     * Determine if the given constraint is simple (EQUALS/IN) or not.
     * 
     * @param constraint
     * @return true if constraint's type is EQUALS/IN, otherwise false
     */
    public static boolean isSimpleConstraint(RequestConstraint constraint) {
        ConstraintType type = constraint.getConstraintType();
        return type == ConstraintType.EQUALS || type == ConstraintType.IN;
    }

    public void setDatasetIdConstraint(RequestConstraint constraint) {
        this.datasetIdConstraint = constraint;
    }

    public void setSecondaryIdConstraint(RequestConstraint constraint) {
        this.secondaryIdConstraint = constraint;
    }

    public void setEnsembleIdConstraint(RequestConstraint constraint) {
        this.ensembleIdConstraint = constraint;
    }

    public void setMasterLevelNameConstraint(RequestConstraint constraint) {
        this.masterLevelNameConstraint = constraint;
    }

    public void setLevelOneConstraint(RequestConstraint constraint) {
        this.levelOneConstraint = constraint;
    }

    public void setLevelTwoConstraint(RequestConstraint constraint) {
        this.levelTwoConstraint = constraint;
    }

}
