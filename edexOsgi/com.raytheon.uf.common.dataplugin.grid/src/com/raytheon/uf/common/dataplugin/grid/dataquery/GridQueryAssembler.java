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
package com.raytheon.uf.common.dataplugin.grid.dataquery;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.mapping.DatasetIdMapper;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.util.mapping.Mapper;

/**
 * Assemble a query to use when requesting grid data. To use populate query
 * fields using the various setters, then use getConstraintMap to get the
 * constraints to use in a DbQueryRequest or other query type. This
 * implementation uses the ParameterMapper, LevelMapper, and DatasetIdMapper so
 * it can accept fields using namespaces that aren't in the database and
 * transform them to valid database requests. Even if no custom namespace is
 * being used the assembler should still be used to check for deprecated names.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 09, 2012            bsteffen    Initial creation
 * Jul 06, 2016 5728       mapeters    Add advanced query support
 * 
 * </pre>
 * 
 * @author bsteffen
 */

public class GridQueryAssembler {

    protected String datasetIdNamespace;

    protected String levelNamespace;

    protected String levelUnits;

    private String parameterNamespace;

    private String parameterAbbreviation;

    /*
     * The following are only allowed to be EQUALS constraints in this class,
     * subclasses may set them to other types and override methods to handle
     * them as needed
     */

    protected RequestConstraint datasetIdConstraint;

    protected RequestConstraint secondaryIdConstraint;

    protected RequestConstraint ensembleIdConstraint;

    protected RequestConstraint masterLevelNameConstraint;

    protected RequestConstraint levelOneConstraint;

    protected RequestConstraint levelTwoConstraint;

    /**
     * Construct an assembler that only checks for deprecated names.
     */
    public GridQueryAssembler() {
        this(Mapper.DEPRECATED);
    }

    /**
     * Construct an assembler that uses the provided namespace for all mappings.
     */
    public GridQueryAssembler(String namespace) {
        this(namespace, namespace, namespace);
    }

    /**
     * Construct an assembler that will use three separate namespaces for each
     * type of mapping.
     */
    public GridQueryAssembler(String datasetIdNamespace,
            String parameterNamespace, String levelNamespace) {
        this.datasetIdNamespace = datasetIdNamespace;
        this.parameterNamespace = parameterNamespace;
        this.levelNamespace = levelNamespace;
    }

    /**
     * Get the constraints that can be used for querying whatever parameters
     * were specified.
     * 
     * @return a map of the constraints
     * @throws CommunicationException
     */
    public Map<String, RequestConstraint> getConstraintMap()
            throws CommunicationException {
        Map<String, RequestConstraint> constraintMap = new HashMap<>();
        constraintMap.put(GridConstants.PLUGIN_NAME, new RequestConstraint(
                GridConstants.GRID));
        if (datasetIdConstraint != null) {
            constraintMap.put(GridConstants.DATASET_ID,
                    getDatasetIdConstraint());
        }
        if (secondaryIdConstraint != null) {
            constraintMap
                    .put(GridConstants.SECONDARY_ID, secondaryIdConstraint);
        }
        if (ensembleIdConstraint != null) {
            constraintMap.put(GridConstants.ENSEMBLE_ID, ensembleIdConstraint);
        }
        if (parameterAbbreviation != null) {
            Set<String> names = ParameterMapper.getInstance().lookupBaseNames(
                    parameterAbbreviation, parameterNamespace);
            constraintMap.put(GridConstants.PARAMETER_ABBREVIATION,
                    new RequestConstraint(names));
        }

        if (masterLevelNameConstraint != null) {
            if (levelOneConstraint != null) {
                constraintMap.putAll(getLevelConstraints());
            } else {
                constraintMap.put(GridConstants.MASTER_LEVEL_NAME,
                        getMasterLevelNameConstraint());
            }
        }

        return new HashMap<>(constraintMap);
    }

    /**
     * Get a map of the master level name, level one, and level two constraints
     * (masterLevelNameConstraint and levelOneConstraint must be non-null when
     * calling this method)
     * 
     * @return a map of the level constraints
     */
    protected Map<String, RequestConstraint> getLevelConstraints() {
        String masterLevelAlias = masterLevelNameConstraint
                .getConstraintValue();
        /*
         * Won't throw NumberFormatException since constraint value was
         * originally converted from Double to String (same with level two
         * below)
         */
        Double levelOneValue = Double.valueOf(levelOneConstraint
                .getConstraintValue());
        Set<Level> levels;
        /*
         * Lookup levels and determine constraints based off their values
         * (handles mapping of master level aliases, unit conversion, and
         * reversing level one and level two values when in wrong order)
         */
        if (levelTwoConstraint != null) {
            Double levelTwoValue = Double.valueOf(levelTwoConstraint
                    .getConstraintValue());
            levels = LevelMapper.getInstance().lookupLevels(masterLevelAlias,
                    levelNamespace, levelOneValue, levelTwoValue, levelUnits);
        } else {
            levels = LevelMapper.getInstance().lookupLevels(masterLevelAlias,
                    levelNamespace, levelOneValue, levelUnits);
        }

        return getLevelConstraintsFromLevels(levels);
    }

    /**
     * Get a map of the master level name, level one, and level two constraints
     * based on their values in the provided levels
     * 
     * @param levels
     *            the levels to base the constraints off of
     * @param levelTwoSpecified
     *            whether or not a level two constraint was provided
     * @return a map of the level constraints
     */
    protected Map<String, RequestConstraint> getLevelConstraintsFromLevels(
            Set<Level> levels) {
        Set<String> masterLevels = new HashSet<>(
                (int) (levels.size() / 0.75) + 1, 0.75f);
        Set<String> levelOnes = new HashSet<>((int) (levels.size() / 0.75) + 1,
                0.75f);
        Set<String> levelTwos = new HashSet<>((int) (levels.size() / 0.75) + 1,
                0.75f);

        for (Level level : levels) {
            masterLevels.add(level.getMasterLevel().getName());
            levelOnes.add(level.getLevelOneValueAsString());
            levelTwos.add(level.getLevelTwoValueAsString());
        }

        Map<String, RequestConstraint> constraintMap = new HashMap<>();
        constraintMap.put(GridConstants.MASTER_LEVEL_NAME,
                new RequestConstraint(masterLevels));
        constraintMap.put(GridConstants.LEVEL_ONE, new RequestConstraint(
                levelOnes));
        if (levelTwoConstraint != null) {
            constraintMap.put(GridConstants.LEVEL_TWO, new RequestConstraint(
                    levelTwos));
        }

        return constraintMap;
    }

    /**
     * Same idea as getConstraintMap but return QueryParams instead.
     * 
     * @return a list of the QueryParams
     * @throws CommunicationException
     */
    public List<QueryParam> getQueryParams() throws CommunicationException {
        Map<String, RequestConstraint> map = getConstraintMap();
        List<QueryParam> result = new ArrayList<>(map.size());
        for (Entry<String, RequestConstraint> entry : getConstraintMap()
                .entrySet()) {
            String field = entry.getKey();
            String value = entry.getValue().getConstraintValue();
            String operand = entry.getValue().getConstraintType().getOperand();
            result.add(new QueryParam(field, value, operand));
        }
        return result;
    }

    protected RequestConstraint getDatasetIdConstraint() {
        String alias = datasetIdConstraint.getConstraintValue();
        Set<String> baseNames = DatasetIdMapper.getInstance().lookupAliases(
                alias, datasetIdNamespace);
        return new RequestConstraint(baseNames);
    }

    protected RequestConstraint getMasterLevelNameConstraint() {
        String alias = masterLevelNameConstraint.getConstraintValue();
        Set<String> baseNames = LevelMapper.getInstance().lookupAliases(alias,
                levelNamespace);
        return new RequestConstraint(baseNames);
    }

    /**
     * Namespace to use when looking up dataset mappings, by default deprecated
     * is used to replace deprecated names with new names.
     * 
     * @param datasetIdNamespace
     */
    public void setDatasetIdNamespace(String datasetIdNamespace) {
        this.datasetIdNamespace = datasetIdNamespace;
    }

    public void setDatasetId(String datasetId) {
        if (datasetId == null) {
            this.datasetIdConstraint = null;
        } else {
            this.datasetIdConstraint = new RequestConstraint(datasetId);
        }
    }

    public void setSecondaryId(String secondaryId) {
        if (secondaryId == null) {
            this.secondaryIdConstraint = null;
        } else {
            this.secondaryIdConstraint = new RequestConstraint(secondaryId);
        }
    }

    public void setEnsembleId(String ensembleId) {
        if (ensembleId == null) {
            this.ensembleIdConstraint = null;
        } else {
            this.ensembleIdConstraint = new RequestConstraint(ensembleId);
        }
    }

    /**
     * Namespace to use when looking up parameter mappings, by default
     * deprecated is used to replace deprecated names with new names.
     * 
     * @param parameterNamespace
     */
    public void setParameterNamespace(String parameterNamespace) {
        this.parameterNamespace = parameterNamespace;
    }

    public void setParameterAbbreviation(String parameterAbbreviation) {
        this.parameterAbbreviation = parameterAbbreviation;
    }

    /**
     * Namespace to use when looking up level mappings, by default deprecated is
     * used to replace deprecated names with new names.
     * 
     * @param levelNamespace
     */
    public void setLevelNamespace(String levelNamespace) {
        this.levelNamespace = levelNamespace;
    }

    public void setMasterLevelName(String masterLevelName) {
        if (masterLevelName == null) {
            this.masterLevelNameConstraint = null;
        } else {
            this.masterLevelNameConstraint = new RequestConstraint(
                    masterLevelName);
        }
    }

    /**
     * Set the level one value, only valid if masterLevelName is also set. Be
     * sure to set level units if the units might differ from the db.
     * 
     * @param levelOneValue
     */
    public void setLevelOneValue(Double levelOneValue) {
        if (levelOneValue == null) {
            this.levelOneConstraint = null;
        } else {
            this.levelOneConstraint = new RequestConstraint(
                    levelOneValue.toString());
        }
    }

    /**
     * Set the level two value, only valid if the level one value is also set.
     * 
     * @param levelTwoValue
     */
    public void setLevelTwoValue(Double levelTwoValue) {
        if (levelTwoValue == null) {
            this.levelTwoConstraint = null;
        } else {
            this.levelTwoConstraint = new RequestConstraint(
                    levelTwoValue.toString());
        }
    }

    /**
     * set the unit of the numerical values passed in for the level values. The
     * level units aren't used directly in the request constraints, but if the
     * units are different then what is in the database then the values will
     * automatically be converted to build proper constraints.
     * 
     * @param levelUnits
     */
    public void setLevelUnits(String levelUnits) {
        this.levelUnits = levelUnits;
    }

    /**
     * Convenience method to set all three namespaces to the same thing.
     * 
     * @param namespace
     */
    public void setNamespace(String namespace) {
        setParameterNamespace(namespace);
        setDatasetIdNamespace(namespace);
        setLevelNamespace(namespace);
    }
}
