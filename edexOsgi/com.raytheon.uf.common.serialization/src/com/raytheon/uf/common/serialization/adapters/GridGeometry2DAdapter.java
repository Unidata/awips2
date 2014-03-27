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
package com.raytheon.uf.common.serialization.adapters;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.parameter.DefaultParameterDescriptor;
import org.geotools.parameter.Parameter;
import org.geotools.parameter.ParameterGroup;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultProjectedCRS;
import org.geotools.referencing.cs.DefaultCartesianCS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.DefiningConversion;
import org.opengis.geometry.Envelope;
import org.opengis.parameter.GeneralParameterValue;
import org.opengis.parameter.ParameterDescriptor;
import org.opengis.parameter.ParameterValue;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.cs.CartesianCS;
import org.opengis.referencing.operation.Conversion;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.util.InternationalString;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Dynamic Serialize type adapter for GridGeometry2D
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010             randerso    Initial creation
 * Mar 27, 2014 2015       njensen     Added ParameterValueAdapter and
 *                                      serialize PROJCS differently to
 *                                      work around geotools WKT limitations
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GridGeometry2DAdapter implements
        ISerializationTypeAdapter<GridGeometry2D> {

    /**
     * Serialization adapter for ParameterValues. Also has to serialize the
     * descriptor, default, etc due to geotools API. Intentionally private class
     * because it's barely better than a hack to work around geotools
     * limitations of recreating objects from a serialized form. See
     * http://jira.codehaus.org/browse/GEOT-4752 for more information.
     */
    @SuppressWarnings(value = { "unchecked", "rawtypes" })
    private static class ParameterValueAdapter implements
            ISerializationTypeAdapter<ParameterValue> {

        @Override
        public void serialize(ISerializationContext serializer,
                ParameterValue object) throws SerializationException {
            // serialize everything for the descriptor first
            ParameterDescriptor<?> desc = object.getDescriptor();
            serializer.writeString(desc.getName().getCode());
            InternationalString rmk = desc.getRemarks();
            if (rmk != null) {
                serializer.writeString(rmk.toString());
            } else {
                serializer.writeString("null");
            }
            serializer.writeString(desc.getValueClass().getName());
            serializer.writeObject(desc.getDefaultValue());
            serializer.writeBool(desc.getMinimumOccurs() > 0);

            // finally serialize the object itself
            serializer.writeObject(object.getValue());
        }

        @Override
        public ParameterValue<?> deserialize(
                IDeserializationContext deserializer)
                throws SerializationException {
            return deserializeInternal(deserializer);
        }

        /**
         * Internal deserialization, method exists entirely to work around Java
         * generics issues with DefaultParameterDescriptor.create(...).
         * 
         * @param deserializer
         * @return
         * @throws SerializationException
         */
        private <T> ParameterValue<T> deserializeInternal(
                IDeserializationContext deserializer)
                throws SerializationException {
            String nameCode = deserializer.readString();
            CharSequence rmk = deserializer.readString();
            if ("null".equals(rmk)) {
                rmk = null;
            }

            String classname = deserializer.readString();
            Class<T> valueClass;
            try {
                valueClass = (Class<T>) Class.forName(classname);
            } catch (ClassNotFoundException e) {
                throw new SerializationException(
                        "Unknown parameter value class " + classname);
            }

            T defaultValue = (T) deserializer.readObject();
            boolean required = deserializer.readBool();
            Object value = deserializer.readObject();
            ParameterDescriptor<T> desc = DefaultParameterDescriptor.create(
                    nameCode, rmk, valueClass, defaultValue, required);
            Parameter param = new Parameter(desc, value);
            return param;

        }
    }

    private static final ParameterValueAdapter paramValAdapter = new ParameterValueAdapter();

    private static final DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();

    private static final String PROJCS = "PROJCS";

    @Override
    public GridGeometry2D deserialize(IDeserializationContext deserializer)
            throws SerializationException {

        try {
            int x = deserializer.readI32();
            int y = deserializer.readI32();
            int width = deserializer.readI32();
            int height = deserializer.readI32();
            GridEnvelope2D gridRange = new GridEnvelope2D(x, y, width, height);

            /*
             * Some of our projected CRS instances don't conform to WKT due to
             * custom parameters, so we have to handle those differently
             */
            String crsType = deserializer.readString();
            CoordinateReferenceSystem crs = null;
            if (PROJCS.equals(crsType)) {
                Map<String, Object> props = null;
                String projName = deserializer.readString();
                GeographicCRS baseCrs = (GeographicCRS) CRS
                        .parseWKT(deserializer.readString());
                String conversionName = deserializer.readString();

                // recreate the ParameterValueGroup
                String groupName = deserializer.readString();
                int paramValueSize = deserializer.readI32();
                GeneralParameterValue[] parameters = new GeneralParameterValue[paramValueSize];
                for (int i = 0; i < paramValueSize; i++) {
                    parameters[i] = paramValAdapter.deserialize(deserializer);
                }
                props = new HashMap<String, Object>();
                props.put("name", groupName);
                ParameterGroup group = new ParameterGroup(props, parameters);

                // create the conversions with the group
                DefiningConversion dc = new DefiningConversion(conversionName,
                        group);
                MathTransform mt = dmtFactory
                        .createParameterizedTransform(group);
                CartesianCS cs = DefaultCartesianCS.PROJECTED;

                props = new HashMap<String, Object>();
                props.put("name", projName);
                crs = new DefaultProjectedCRS(props, dc, baseCrs, mt, cs);
            } else {
                /*
                 * only had special handling for ProjectedCRS, just hope it
                 * correctly parses anything else as WKT
                 */
                crs = CRS.parseWKT(deserializer.readString());
            }

            double dx = deserializer.readDouble();
            double dy = deserializer.readDouble();
            double dw = deserializer.readDouble();
            double dh = deserializer.readDouble();
            Envelope envelope = new Envelope2D(crs, dx, dy, dw, dh);

            GridGeometry2D gridGeom = new GridGeometry2D(gridRange, envelope);
            return gridGeom;
        } catch (Throwable e) {
            throw new SerializationException(
                    "Error deserializing GridGeometry2D", e);
        }
    }

    @Override
    public void serialize(ISerializationContext serializer,
            GridGeometry2D gridGeom) throws SerializationException {
        serializer.writeI32(gridGeom.getGridRange2D().x);
        serializer.writeI32(gridGeom.getGridRange2D().y);
        serializer.writeI32(gridGeom.getGridRange2D().width);
        serializer.writeI32(gridGeom.getGridRange2D().height);

        CoordinateReferenceSystem crs = gridGeom.getCoordinateReferenceSystem();
        if (crs instanceof ProjectedCRS) {
            serializer.writeString(PROJCS);
            ProjectedCRS projCrs = (ProjectedCRS) crs;
            String projName = projCrs.getName().toString();
            serializer.writeString(projName);
            serializer.writeString(projCrs.getBaseCRS().toWKT());

            Conversion conversion = projCrs.getConversionFromBase();
            serializer.writeString(conversion.getName().toString());
            ParameterValueGroup params = conversion.getParameterValues();
            serializer.writeString(params.getDescriptor().getName().getCode());
            List<GeneralParameterValue> values = params.values();
            serializer.writeI32(values.size());
            for (GeneralParameterValue v : values) {
                paramValAdapter.serialize(serializer, (ParameterValue<?>) v);
            }
        } else {
            /*
             * Cross fingers and hope it works ok with WKT. Seems to generally
             * always write to WKT ok but then fails to parse it back out on
             * some custom CRS instances.
             */
            serializer.writeString("WKT");
            serializer.writeString(crs.toWKT());
        }

        serializer.writeDouble(gridGeom.getEnvelope().getMinimum(0));
        serializer.writeDouble(gridGeom.getEnvelope().getMinimum(1));
        serializer.writeDouble(gridGeom.getEnvelope().getSpan(0));
        serializer.writeDouble(gridGeom.getEnvelope().getSpan(1));
    }

}
