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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataaccess.IDataFactory;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGridDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGridData;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataquery.GridQueryAssembler;
import com.raytheon.uf.common.dataplugin.grid.mapping.DatasetIdMapper;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.util.mapping.Mapper;

/**
 * Data access factory for accessing data from the Grid plugin as grid types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 17, 2013           bsteffen    Initial creation
 * Feb 14, 2013  1614     bsteffen    Refactor data access framework to use
 *                                    single request.
 * Feb 04, 2014  2672     bsteffen    Enable requesting subgrids.
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridDataAccessFactory extends AbstractGridDataPluginFactory
        implements IDataFactory {

    private static final String NAMESPACE = "namespace";

    private static final String[] VALID_IDENTIFIERS = {
            GridConstants.DATASET_ID, GridConstants.SECONDARY_ID,
            GridConstants.ENSEMBLE_ID, NAMESPACE };

    @Override
    public String[] getValidIdentifiers() {
        return VALID_IDENTIFIERS;
    }

    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> result = new HashMap<String, RequestConstraint>();

        Map<String, Object> identifiers = request.getIdentifiers();
        try {
            GridQueryAssembler assembler = new GridQueryAssembler();
            if (identifiers != null && identifiers.containsKey(NAMESPACE)) {
                assembler.setNamespace(identifiers.get(NAMESPACE).toString());
            }
            if (request.getParameters() != null) {
                for (String parameter : request.getParameters()) {
                    assembler.setParameterAbbreviation(parameter);
                    mergeConstraintMaps(assembler.getConstraintMap(), result);
                }
                // clear fields so it doesn't merge the last one again.
                assembler.setParameterAbbreviation(null);
            }

            if (request.getLevels() != null) {
                for (Level level : request.getLevels()) {
                    assembler.setMasterLevelName(level.getMasterLevel()
                            .getName());
                    assembler.setLevelUnits(level.getMasterLevel()
                            .getUnitString());
                    assembler.setLevelOneValue(level.getLevelonevalue());
                    assembler.setLevelTwoValue(level.getLeveltwovalue());
                    // TODO Theoretically merging these could end badly if there
                    // are multiple master levels or if some levels have
                    // leveltwo and others don't. But for now pretend that never
                    // happens since it probably won't.
                    mergeConstraintMaps(assembler.getConstraintMap(), result);
                }
                // clear fields so it doesn't merge the last one again.
                assembler.setMasterLevelName(null);
                assembler.setLevelUnits(null);
                assembler.setLevelOneValue(null);
                assembler.setLevelTwoValue(null);
            }

            if (request.getLocationNames() != null) {
                for (String loc : request.getLocationNames()) {
                    assembler.setDatasetId(loc);
                    mergeConstraintMaps(assembler.getConstraintMap(), result);
                }
                // clear fields so it doesn't merge the last one again.
                assembler.setDatasetId(null);

            }

            if (identifiers != null) {
                if (identifiers.containsKey(GridConstants.DATASET_ID)) {
                    assembler.setDatasetId(identifiers.get(
                            GridConstants.DATASET_ID).toString());
                }
                if (identifiers.containsKey(GridConstants.ENSEMBLE_ID)) {
                    assembler.setEnsembleId(identifiers.get(
                            GridConstants.ENSEMBLE_ID).toString());
                }
                if (identifiers.containsKey(GridConstants.SECONDARY_ID)) {
                    assembler.setSecondaryId(identifiers.get(
                            GridConstants.SECONDARY_ID).toString());
                }
            }
            mergeConstraintMaps(assembler.getConstraintMap(), result);
        } catch (CommunicationException e) {
            throw new DataRetrievalException(e);
        }
        return result;
    }

    /**
     * Copy all constraints from source to target. If target already contains a
     * constraint for a key then merge the values into target.
     * 
     * @param target
     * @param source
     */
    private void mergeConstraintMaps(Map<String, RequestConstraint> source,
            Map<String, RequestConstraint> target) {
        for (Entry<String, RequestConstraint> sourceEntry : source.entrySet()) {
            String key = sourceEntry.getKey();
            RequestConstraint sourceConstraint = sourceEntry.getValue();
            RequestConstraint targetConstraint = target.get(sourceEntry
                    .getKey());
            if (targetConstraint == null) {
                target.put(key, sourceConstraint);
            } else if (!sourceConstraint.equals(targetConstraint)) {
                targetConstraint.setConstraintType(ConstraintType.IN);
                // TODO we don't necessarily want to always add. This could
                // result in something like IN MB,FHAG,MB,MB,MB, but we also
                // don't want to parse the in list all the time.
                targetConstraint.addToConstraintValueList(sourceConstraint
                        .getConstraintValue());
            }
        }
    }

    @Override
    protected IGridData constructGridDataResponse(IDataRequest request,
            PluginDataObject pdo, GridGeometry2D gridGeometry,
            DataSource dataSource) {
        if (pdo instanceof GridRecord == false) {
            throw new DataRetrievalException(this.getClass().getSimpleName()
                    + " cannot handle " + pdo.getClass().getSimpleName());
        }
        GridRecord gridRecord = (GridRecord) pdo;

        String parameter = gridRecord.getParameter().getAbbreviation();
        String datasetId = gridRecord.getDatasetId();

        Level level = gridRecord.getLevel();
        Map<String, Object> identifiers = request.getIdentifiers();
        if (identifiers != null && identifiers.containsKey(NAMESPACE)) {
            // perform reverse mappings so the parameters and levels that are
            // returned match exactly what was requested.
            String namespace = request.getIdentifiers().get(NAMESPACE)
                    .toString();
            List<String> requestParameters = Arrays.asList(request
                    .getParameters());
            parameter = reverseResolveMapping(ParameterMapper.getInstance(),
                    parameter, namespace, requestParameters);

            if (identifiers.containsKey(GridConstants.DATASET_ID)) {
                List<String> requestedDatasets = Arrays.asList(identifiers.get(
                        GridConstants.DATASET_ID).toString());
                datasetId = reverseResolveMapping(
                        DatasetIdMapper.getInstance(), datasetId, namespace,
                        requestedDatasets);
            }
            for (Level requestLevel : request.getLevels()) {
                double levelone = requestLevel.getLevelonevalue();
                double leveltwo = requestLevel.getLeveltwovalue();
                String master = requestLevel.getMasterLevel().getName();
                Unit<?> unit = requestLevel.getMasterLevel().getUnit();
                try {
                    // instead of doing reverse mapping just do a forward
                    // mapping on everything they requested and compare to what
                    // they got.
                    Set<Level> levels = LevelMapper.getInstance().lookupLevels(
                            master, namespace, levelone, leveltwo, unit);
                    for (Level l : levels) {
                        if (level.equals(l)) {
                            level = requestLevel;
                            break;
                        }
                    }
                } catch (CommunicationException e) {
                    throw new DataRetrievalException(e);
                }
                if (level == requestLevel) {
                    // we found one.
                    break;
                }
            }
        }

        DefaultGridData defaultGridData = new DefaultGridData(dataSource,
                gridGeometry);
        defaultGridData.setDataTime(pdo.getDataTime());
        defaultGridData.setParameter(parameter);
        defaultGridData.setLevel(level);
        defaultGridData.setLocationName(datasetId);
        defaultGridData.setUnit(gridRecord.getParameter().getUnit());
        Map<String, Object> attributes = new HashMap<String, Object>();
        attributes.put(GridConstants.DATASET_ID, datasetId);
        attributes.put(GridConstants.SECONDARY_ID, gridRecord.getSecondaryId());
        attributes.put(GridConstants.ENSEMBLE_ID, gridRecord.getEnsembleId());

        defaultGridData.setAttributes(attributes);
        return defaultGridData;
    }

    private String reverseResolveMapping(Mapper mapper, String base,
            String namespace, Collection<String> requested) {
        // attempt to find a valid mapping that they requested.
        for (String alias : mapper.lookupAliases(base, namespace)) {
            if (requested.contains(alias)) {
                return alias;
            }
        }
        return base;
    }

    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        return getAvailableLocationNames(request, GridConstants.DATASET_ID);
    }


}
