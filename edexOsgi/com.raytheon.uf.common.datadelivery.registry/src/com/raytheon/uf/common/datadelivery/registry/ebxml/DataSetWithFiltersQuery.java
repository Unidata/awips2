package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.operation.TransformException;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.registry.IResultFormatter;
import com.raytheon.uf.common.registry.ebxml.encoder.IRegistryEncoder;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Query to retrieve {@link DataSetMetaData}s filtered on area and levels.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012 736        djohnson    Initial creation
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Aug 15, 2012 0743       djohnson    Type-safe result formatters.
 * Oct 17, 2012 0726       djohnson    Made filter check method public.
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * Dec 10, 2012 1259       bsteffen    Switch Data Delivery from LatLon to referenced envelopes.
 * Jun 24, 2013 2106       djohnson    Pass encoder to result formatters.
 * 
 * </pre>
 * 
 * @author djohnson
 */
@DynamicSerialize
public class DataSetWithFiltersQuery extends DataSetQuery implements
        IResultFormatter<DataSet> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataSetWithFiltersQuery.class);

    private static final int PERCENT_POINTS = 75;

    @DynamicSerializeElement
    private Set<LevelType> levels;

    @DynamicSerializeElement
    private ReferencedEnvelope envelope;

    @Override
    public DataSet decodeObject(RegistryObjectType registryObjectType,
            IRegistryEncoder encoder)
            throws SerializationException {
        DataSet retVal = null;
        DataSet object = (DataSet) DataDeliveryRegistryObjectTypes.getObject(registryObjectType, encoder);

        if (satisfiesFilterCriteria(object, levels, envelope)) {
            retVal = object;
        }

        return retVal;
    }

    private static boolean checkArea(ReferencedEnvelope dataSetEnvelope,
            ReferencedEnvelope selectedEnvelope) {
        double selectedArea = selectedEnvelope.getArea();

        try {
            ReferencedEnvelope intersection = MapUtil.reprojectAndIntersect(
                    dataSetEnvelope, selectedEnvelope);

            // if the areas are completely separate then remove
            if (intersection == null || intersection.isEmpty()) {
                return false;
            }

            double intersectionArea = intersection.getArea();
            double d = intersectionArea / selectedArea;
            if ((d * 100 < DataSetWithFiltersQuery.PERCENT_POINTS)) {
                return false;
            }
            return true;
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }
    }

    @VisibleForTesting
    public static boolean satisfiesFilterCriteria(DataSet dataSet,
            Set<LevelType> levels, ReferencedEnvelope envelope) {
        boolean levelMatch = false;
        boolean areaMatch = false;

        // If no levels are specified then by default it matches
        if (levels == null) {
            levelMatch = true;
        } else {
            Map<String, Parameter> parameters = dataSet.getParameters();
            List<DataLevelType> levelTypes = new ArrayList<DataLevelType>(
                    parameters.size());
            for (Parameter parameter : parameters.values()) {
                levelTypes.addAll(parameter.getLevelType());
            }

            CHECK_LEVELS: for (LevelType level : levels) {
                for (DataLevelType lvlType : levelTypes) {
                    if (lvlType.getType().equals(level)) {
                        levelMatch = true;
                        break CHECK_LEVELS;
                    }
                }
            }
        }

        // If area coordinates are specified, verify the dataset falls within
        // the area.
        if (envelope == null) {
            areaMatch = true;
        } else {
            areaMatch = checkArea(dataSet.getCoverage().getEnvelope(), envelope);
        }

        // If both conditions resolve to true, the dataset passes the filters
        return levelMatch && areaMatch;
    }

    /**
     * @return the levels
     */
    public Set<LevelType> getLevels() {
        return levels;
    }

    /**
     * @param levels
     *            the levels to set
     */
    public void setLevels(Set<LevelType> levels) {
        this.levels = levels;
    }

    /**
     * @return the envelope
     */
    public ReferencedEnvelope getEnvelope() {
        return envelope;
    }

    /**
     * @param envelope
     *            the envelope to set
     */
    public void setEnvelope(ReferencedEnvelope envelope) {
        this.envelope = envelope;
    }

}
