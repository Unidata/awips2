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
package com.raytheon.uf.edex.plugin.goesr.geospatial.crs;

import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchIdentifierException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import ucar.ma2.DataType;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.projection.Geostationary;
import com.raytheon.uf.edex.plugin.goesr.exception.GoesrProjectionException;

/**
 * 
 * A GoesrCrsFactory has the ability to parse projection information from the
 * {@link Attribute}s of a {@link Variable} for a specific type of projection.
 * The abstract class provides a means to parse the semi_major, semi_minor,
 * false_easting, and false_northing which are common among all map projections.
 * Other projection parameters should be parsed by sub classes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class GoesrCrsFactory {

    protected static final DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();
    
    public CoordinateReferenceSystem constructCoordinateReferenceSystem(
            Variable projectionVariable) throws GoesrProjectionException {
        try {
            ParameterValueGroup parameters = dmtFactory
                    .getDefaultParameters(getProjectionName());
            setDoubleParameter(projectionVariable, "semi_major_axis",
                    parameters, "semi_major");
            setDoubleParameter(projectionVariable, "semi_minor_axis",
                    parameters, "semi_minor");
            setDoubleParameter(projectionVariable, parameters, "semi_major");
            setDoubleParameter(projectionVariable, parameters, "semi_minor");
            setDoubleParameter(projectionVariable, parameters, "false_easting");
            setDoubleParameter(projectionVariable, parameters, "false_northing");

            addSpecificParameters(projectionVariable, parameters);
            return MapUtil.constructProjection(getProjectionName(),
                    parameters);
        } catch (NoSuchIdentifierException e) {
            throw new GoesrProjectionException(
                    "Unable to find projection by name: "
                            + Geostationary.PROJECTION_NAME, e);
        } catch (FactoryException e) {
            throw new GoesrProjectionException(
                    "Error constructing projected CRS", e);
        }
    }
    
    /**
     * @return The name used to look up the parameters for the projection.
     */
    protected abstract String getProjectionName();

    /**
     * Method provided so that subclasses can add parameters that are needed for
     * the specific projection type that they are implementing.
     * 
     * @param projectionVariable
     *            the {@link Variable} from the {@link NetcdfFile} that ocntains
     *            all the projection {@link Attribute}s.
     * @param parameters
     *            The parameters that need to be set to define a
     *            {@link CoordinateReferenceSystem}.
     * @throws GoesrProjectionException
     */
    protected abstract void addSpecificParameters(Variable projectionVariable,
            ParameterValueGroup parameters) throws GoesrProjectionException;

    /**
     * Method to copy a value from a netcdf {@link Attribute} to a
     * {@link ParameterValueGroup}. This should be used when the name that is
     * expected in the netcdf file matches the name needed for the crs. If the
     * attribute does not exist then the value is left unchanged in the group.
     * 
     * @param projectionVariable
     *            {@link Variable} containing projection attributes.
     * @param parameters
     *            parameters needed to build a CRS.
     * @param name
     *            the name of the attribute, used for both lookup in the netcdf
     *            file and setting the parameter in the group.
     * @throws GoesrProjectionException
     */
    protected final void setDoubleParameter(Variable projectionVariable,
            ParameterValueGroup parameters, String name)
            throws GoesrProjectionException {
        setDoubleParameter(projectionVariable, name, parameters, name);
    }

    /**
     * Method to copy a value from a netcdf {@link Attribute} to a
     * {@link ParameterValueGroup}. This should be used when the name that is
     * expected in the netcdf file matches the name needed for the crs. If the
     * attribute does not exist then the value is left unchanged in the group.
     * 
     * @param projectionVariable
     *            {@link Variable} containing projection attributes.
     * @param attributeName
     *            name of the attribute that is expected to exist on the
     *            projectionVariable
     * @param parameters
     *            parameters needed to build a CRS.
     * @param parameterName
     *            name of the parameter to be set in the parameters.
     * @throws GoesrProjectionException
     */
    protected final void setDoubleParameter(Variable projectionVariable,
            String attributeName, ParameterValueGroup parameters,
            String parameterName) throws GoesrProjectionException {
        Attribute attribute = projectionVariable.findAttribute(attributeName);
        if (attribute == null) {
            return;
        }
        if(attribute.getDataType() == DataType.STRING){
            String strValue = attribute.getStringValue();
            try {
                double value = Double.parseDouble(strValue);
                parameters.parameter(parameterName).setValue(value);
            } catch (NumberFormatException e) {
                throw new GoesrProjectionException("Unable to parse"
                        + attributeName, e);
            }
        }else{
            double value = attribute.getNumericValue().doubleValue();
            parameters.parameter(parameterName).setValue(value);
        }
    }

}
