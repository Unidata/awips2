/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.edex.nswrctools;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import ucar.ma2.Array;
import ucar.ma2.DataType;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Range;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.Variable;

/**
 * Decoder tools for NextGen Surveillance and Weather Radar Capability (NSWRC) data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup     Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */
public class DecoderTools {

    @SuppressWarnings("unused")
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DecoderTools.class);

    /**
     * Get dimension by name ignoring case
     *
     * @param dimensions
     * @param string
     * @return
     */
    public static Dimension getDimension(List<Dimension> dimensions,
            String string) {
        Iterator<Dimension> iter = dimensions.iterator();

        while (iter.hasNext()) {
            Dimension dim = iter.next();

            if (dim.getName().equalsIgnoreCase(string)) {
                return dim;
            }
        }

        return null;
    }

    /**
     * Get variable by short name ignoring case
     *
     * @param variables
     * @param string
     * @return
     */
    public static Variable getVariable(List<Variable> variables, String string) {
        Iterator<Variable> iter = variables.iterator();

        while (iter.hasNext()) {
            Variable var = iter.next();

            if (var.getShortName().equalsIgnoreCase(string)) {
                return var;
            }
        }

        return null;
    }

    /**
     * Get variable by short name ignoring case
     *
     * @param variables
     * @param string
     * @return
     */
    public static Variable getVariableForDimension(List<Variable> variables,
            String string) {
        Iterator<Variable> iter = variables.iterator();

        while (iter.hasNext()) {
            Variable var = iter.next();

            List<Dimension> dimensions = var.getDimensions();
            if (dimensions.size() == 1
                    && dimensions.get(0).getName().equals(string)) {
                return var;
            }
        }

        return null;
    }

    /**
     * Get attribute from variable ignoring case
     *
     * @param variable
     * @param name
     * @return
     */
    public static Attribute getAttribute(Variable variable, String name) {
        Attribute attr = null;

        Iterator<Attribute> iter = variable.getAttributes().iterator();

        while (iter.hasNext() && attr == null) {
            Attribute tmpAttr = iter.next();

            if (tmpAttr.getName().equalsIgnoreCase(name)) {
                attr = tmpAttr;
            }
        }

        return attr;
    }

    /**
     * get attribute from list ignoring case
     *
     * @param attributes
     * @param name
     * @return
     */
    public static Attribute getAttribute(List<Attribute> attributes, String name) {
        Attribute attr = null;

        Iterator<Attribute> iter = attributes.iterator();

        while (iter.hasNext() && attr == null) {
            Attribute tmpAttr = iter.next();

            if (tmpAttr.getName().equalsIgnoreCase(name)) {
                attr = tmpAttr;
            }
        }

        return attr;
    }

    /**
     * Get range by name ignoring case
     *
     * @param variables
     * @param string
     * @return
     */
    public static Range getRange(List<Range> ranges, String string) {
        Iterator<Range> iter = ranges.iterator();

        while (iter.hasNext()) {
            Range var = iter.next();

            if (var.getName().equalsIgnoreCase(string)) {
                return var;
            }
        }

        return null;
    }

    /**
     * Return only the variables whose short names do not match those in
     * notDataVariables ignoring case
     *
     * @param variables
     * @param notDataVariables
     * @return
     */
    public static List<Variable> findDataVariables(List<Variable> variables,
            HashSet<String> notDataVariables) {
        List<Variable> dataVariables = new ArrayList<Variable>(variables.size());
        Iterator<Variable> variableIterator = variables.iterator();

        while (variableIterator.hasNext()) {
            Variable var = variableIterator.next();
            if (!notDataVariables.contains(var.getShortName().toLowerCase())) {
                dataVariables.add(var);
            }
        }

        return dataVariables;
    }

    /**
     * Read the range for the most significant dimension and split the variables
     * into records with each of the other dimensions as a single value
     *
     * @param variables
     *            variables to build records for
     * @param mostSignificantDimension
     *            dimension to split variable down to
     * @param allVariables
     *            all variables from the netcdf file
     * @return
     * @throws IOException
     * @throws InvalidRangeException
     */
    public static List<NSWRCVariable> getRecords(List<Variable> variables,
            Dimension mostSignificantDimension, List<Variable> allVariables)
            throws IOException, InvalidRangeException {

        List<NSWRCVariable> allRecords = new ArrayList<NSWRCVariable>();

        Iterator<Variable> varIter = variables.iterator();

        while (varIter.hasNext()) {
            Variable var = varIter.next();
            List<NSWRCVariable> varRecords = getRecords(var,
                    mostSignificantDimension, allVariables);
            if (varRecords != null) {
                allRecords.addAll(varRecords);
            }
        }

        return allRecords;
    }

    /**
     * Internal method, use the List<Variable> version for external calls
     *
     * @param variable
     * @param mostSignificantDimension
     * @return
     * @throws InvalidRangeException
     */
    private static List<NSWRCVariable> getRecords(Variable variable,
            Dimension mostSignificantDimension, List<Variable> allVariables)
            throws IOException, InvalidRangeException {

        List<NSWRCVariable> records = new ArrayList<NSWRCVariable>();

        Dimension currentMSD = variable.getDimension(0);

        if (currentMSD.getName().equalsIgnoreCase(
                mostSignificantDimension.getName())) {
            Array arr = variable.read();
            NSWRCVariable rec = new NSWRCVariable();
            rec.addDataObject(currentMSD, arr);
            rec.setVariableName(variable.getShortName());
            records.add(rec);
        } else {
            // slice and recursive call
            int length = currentMSD.getLength();

            // get variable for this dimension
            Variable sliceVar = getVariableForDimension(allVariables,
                    currentMSD.getName());
            Array sliceArr = sliceVar.read();

            // loop over all values for this dimension and slice the data
            for (int i = 0; i < length; ++i) {
                Object sliceVal = sliceArr.getObject(i);

                Variable slicedVariable = variable.slice(0, i);

                List<NSWRCVariable> subRecords = getRecords(slicedVariable,
                        mostSignificantDimension, allVariables);
                addDataObjectToAll(subRecords, currentMSD, sliceVal);

                records.addAll(subRecords);
            }

        }

        return records;
    }

    private static void addDataObjectToAll(List<NSWRCVariable> records,
            Dimension dim, Object val) {
        if (records != null) {
            Iterator<NSWRCVariable> recIter = records.iterator();

            while (recIter.hasNext()) {
                NSWRCVariable rec = recIter.next();
                rec.addDataObject(dim, val);
            }
        }
    }

    public static String getUnitString(String variableName,
            List<Variable> variables, String attribute) {
        Variable dataVar = getVariable(variables, variableName);

        String unit = tec.uom.se.AbstractUnit.ONE.toString();

        if (dataVar != null) {
            Attribute attr = getAttribute(dataVar, attribute);

            if (attr != null) {

                String val = attr.getStringValue();

                if (val != null) {
                    unit = val;
                }
            }
        }

        return unit;
    }

    /**
     * @param dataVariable
     * @return
     */
    public static double[] scaleDataRange(Variable dataVariable) {
        Attribute scale_factor = DecoderTools.getAttribute(dataVariable,
                "scale_factor");
        Attribute valid_range = DecoderTools.getAttribute(dataVariable,
                "valid_range");
        Attribute add_offset = DecoderTools.getAttribute(dataVariable,
                "add_offset");

        if (valid_range == null) {
            return new double[] { Double.NEGATIVE_INFINITY,
                    Double.POSITIVE_INFINITY };
        }

        double scale = 0.0;
        double offset = 0.0;

        if (scale_factor == null || scale_factor.getDataType() == null) {
            scale = 1.0;
        } else {
            scale = scale_factor.getNumericValue().doubleValue();
        }

        if (add_offset == null || add_offset.getDataType() == null) {
            offset = 0.0;
        } else {
            offset = add_offset.getNumericValue().doubleValue();
        }

        DataType type = dataVariable.getDataType();

        double[] convertedRange = new double[2];

        switch (type) {
        	case BYTE:
	            byte[] range = (byte[]) valid_range.getValues().copyTo1DJavaArray();
	            convertedRange[0] = ((range[0] * scale) + offset);
	            convertedRange[1] = ((range[1] * scale) + offset);
	            break;
        	case SHORT:
	            short[] rangeShort = (short[]) valid_range.getValues().copyTo1DJavaArray();
	            convertedRange[0] = ((rangeShort[0] * scale) + offset);
	            convertedRange[1] = ((rangeShort[1] * scale) + offset);
	            break;
        	case FLOAT:
	            float[] rangeFloat = (float[]) valid_range.getValues().copyTo1DJavaArray();
	            convertedRange[0] = ((rangeFloat[0] * scale) + offset);
	            convertedRange[1] = ((rangeFloat[1] * scale) + offset);
	            break;
        	case INT:
	            int[] rangeInt = (int[]) valid_range.getValues().copyTo1DJavaArray();
	            convertedRange[0] = ((rangeInt[0] * scale) + offset);
	            convertedRange[1] = ((rangeInt[1] * scale) + offset);
	            break;
        	case LONG:
	            long[] rangeLong = (long[]) valid_range.getValues().copyTo1DJavaArray();
	            convertedRange[0] = ((rangeLong[0] * scale) + offset);
	            convertedRange[1] = ((rangeLong[1] * scale) + offset);
	            break;
        	case DOUBLE:
	            double[] rangeDouble = (double[]) valid_range.getValues().copyTo1DJavaArray();
	            convertedRange[0] = ((rangeDouble[0] * scale) + offset);
	            convertedRange[1] = ((rangeDouble[1] * scale) + offset);
	            break;
	       default:
	    	   break;
        }

        return convertedRange;
    }
}
