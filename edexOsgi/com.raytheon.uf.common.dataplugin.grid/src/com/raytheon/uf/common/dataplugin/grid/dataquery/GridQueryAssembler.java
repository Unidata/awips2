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
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
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
 * Oct 9, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridQueryAssembler {

    private String datasetIdNamespace;

    private String datasetId;

    private String secondaryId;

    private String ensembleId;

    private String parameterNamespace;

    private String parameterAbbreviation;

    private String levelNamespace;

    private String masterLevelName;

    private Double levelOneValue;

    private Double levelTwoValue;

    private String levelUnits;

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

    private RequestConstraint buildEqualOrInRC(Set<String> names) {
        if (names.size() == 1) {
            return new RequestConstraint(names.iterator().next());
        } else {
            RequestConstraint rc = new RequestConstraint(null,
                    ConstraintType.IN);
            rc.setConstraintValueList(names);
            return rc;
        }
    }

    /**
     * Get the constraints that can be used for querying whatever parameters
     * were specified.
     * 
     * @return
     * @throws CommunicationException
     */
    public Map<String, RequestConstraint> getConstraintMap()
            throws CommunicationException {
        Map<String, RequestConstraint> constraintMap = new HashMap<String, RequestConstraint>();
        constraintMap.put(GridConstants.PLUGIN_NAME, new RequestConstraint(
                GridConstants.GRID));
        if (datasetId != null) {
            Set<String> names = DatasetIdMapper.getInstance().lookupBaseNames(
                    datasetId, datasetIdNamespace);
            constraintMap
                    .put(GridConstants.DATASET_ID, buildEqualOrInRC(names));
        }
        if (secondaryId != null) {
            constraintMap.put(GridConstants.SECONDARY_ID,
                    new RequestConstraint(secondaryId));
        }
        if (ensembleId != null) {
            constraintMap.put(GridConstants.ENSEMBLE_ID, new RequestConstraint(
                    ensembleId));
        }
        if (parameterAbbreviation != null) {
            Set<String> names = ParameterMapper.getInstance().lookupBaseNames(
                    parameterAbbreviation, parameterNamespace);
            constraintMap.put(GridConstants.PARAMETER_ABBREVIATION,
                    buildEqualOrInRC(names));
        }
        if (masterLevelName != null) {
            if (levelOneValue != null) {
                Set<Level> levels = null;
                if (levelTwoValue != null) {
                    levels = LevelMapper.getInstance().lookupLevels(
                            masterLevelName, levelNamespace, levelOneValue,
                            levelTwoValue, levelUnits);
                } else {
                    levels = LevelMapper.getInstance().lookupLevels(
                            masterLevelName, levelNamespace, levelOneValue,
                            levelUnits);
                }
                Set<String> masterLevels = new HashSet<String>(
                        (int) (levels.size() / 0.75) + 1, 0.75f);
                Set<String> levelOnes = new HashSet<String>(
                        (int) (levels.size() / 0.75) + 1, 0.75f);
                Set<String> levelTwos = new HashSet<String>(
                        (int) (levels.size() / 0.75) + 1, 0.75f);
                for (Level level : levels) {
                    masterLevels.add(level.getMasterLevel().getName());
                    levelOnes.add(level.getLevelOneValueAsString());
                    levelTwos.add(level.getLevelTwoValueAsString());
                }
                constraintMap.put(GridConstants.MASTER_LEVEL_NAME,
                        buildEqualOrInRC(masterLevels));
                constraintMap.put(GridConstants.LEVEL_ONE,
                        buildEqualOrInRC(levelOnes));
                if (levelTwoValue != null) {
                    constraintMap.put(GridConstants.LEVEL_TWO,
                            buildEqualOrInRC(levelTwos));
                }
            } else {
                Set<String> names = LevelMapper.getInstance().lookupBaseNames(
                        masterLevelName, levelNamespace);
                constraintMap.put(GridConstants.MASTER_LEVEL_NAME,
                        buildEqualOrInRC(names));
            }
        }
        return new HashMap<String, RequestConstraint>(constraintMap);
    }

    /**
     * Same idea as getConstraintMap but return QueryParams instead.
     * 
     * @return
     * @throws CommunicationException
     */
    public List<QueryParam> getQueryParams() throws CommunicationException {
        Map<String, RequestConstraint> map = getConstraintMap();
        List<QueryParam> result = new ArrayList<QueryParam>(map.size());
        for (Entry<String, RequestConstraint> entry : getConstraintMap()
                .entrySet()) {
            String field = entry.getKey();
            String value = entry.getValue().getConstraintValue();
            String operand = entry.getValue().getConstraintType().getOperand();
            result.add(new QueryParam(field, value, operand));
        }
        return result;
    }

    /**
     * Namespace to se when looking up dataset mappings, by default deprecated
     * is used to replace deprecated names with new names.
     * 
     * @param datasetIdNamespace
     */
    public void setDatasetIdNamespace(String datasetIdNamespace) {
        this.datasetIdNamespace = datasetIdNamespace;
    }

    public void setDatasetId(String datasetId) {
        this.datasetId = datasetId;
    }

    public void setSecondaryId(String secondaryId) {
        this.secondaryId = secondaryId;
    }

    public void setEnsembleId(String ensembleId) {
        this.ensembleId = ensembleId;
    }

    /**
     * Namespace to se when looking up parameter mappings, by default deprecated
     * is used to replace deprecated names with new names.
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
     * Namespace to se when looking up level mappings, by default deprecated is
     * used to replace deprecated names with new names.
     * 
     * @param levelNamespace
     */
    public void setLevelNamespace(String levelNamespace) {
        this.levelNamespace = levelNamespace;
    }

    public void setMasterLevelName(String masterLevelName) {
        this.masterLevelName = masterLevelName;
    }

    /**
     * set the level one value, be sure to set level units if the units might
     * differ from the db.
     * 
     * @param levelOneValue
     */
    public void setLevelOneValue(Double levelOneValue) {
        this.levelOneValue = levelOneValue;
    }

    /**
     * set the level two value, only valid if levelonevalue is also set.
     * 
     * @param levelOneValue
     */
    public void setLevelTwoValue(Double levelTwoValue) {
        this.levelTwoValue = levelTwoValue;
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
