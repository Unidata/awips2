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
package com.raytheon.edex.plugin.bufrua.decoder;

import java.io.IOException;

import ucar.ma2.ArrayStructure;
import ucar.ma2.StructureData;
import ucar.ma2.StructureDataIterator;
import ucar.ma2.StructureMembers.Member;
import ucar.nc2.Attribute;
import ucar.nc2.Sequence;
import ucar.nc2.Structure;
import ucar.nc2.Variable;

/**
 * Provides a convenient way to access both the data and metadata of a
 * {@link Structure}. When a Structure is a {@link Sequence} or when its data is
 * in an {@link ArrayStructure} then the Structure cannot be used to access the
 * data directly, instead the data is accessed through a {@link StructureData}.
 * The original Structure is still necessary because it contains metadata such
 * as scale/offset and missing data values. Because structures like this are
 * very common in bufr files it is convenient to have a single class that holds
 * both the Structure and the StructureData and can use the metadata in a
 * Structure to decode fully the data within a StructureData.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 08, 2016  5736     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class BufrStructure {

    private final Structure structure;

    private final StructureData structureData;

    public BufrStructure(Structure structure, StructureData structureData) {
        this.structure = structure;
        this.structureData = structureData;
    }

    /**
     * Get a new {@link BufrStructureIterator} for a {@link Sequence} variable
     * contained in the current {@link Structure}.
     * 
     * @param memberName
     *            the name of the {@link Sequence} variable.
     * @return An iterator over the {@link BufrStructure}s in the variable or
     *         null of no such Sequence is defined.
     * @throws IOException
     */
    public BufrStructureIterator getSequenceIterator(String memberName)
            throws IOException {
        Variable variable = structure.findVariable(memberName);
        if (!(variable instanceof Sequence)) {
            return null;
        }
        Sequence sequence = (Sequence) variable;
        StructureDataIterator iterator = structureData
                .getArraySequence(memberName).getStructureDataIterator();
        return new BufrStructureIterator(sequence, iterator);
    }

    /**
     * Get a new {@link BufrStructureIterator} for a {@link Structure} variable
     * contained in the current Structure that stores data as an
     * {@link ArrayStructure}.
     * 
     * @param memberName
     *            the name of the {@link Structure} variable.
     * @return An iterator over the {@link BufrStructure}s in the variable or
     *         null of no such Sequence is defined.
     * @throws IOException
     */
    public BufrStructureIterator getStructureIterator(String memberName)
            throws IOException {
        Variable variable = structure.findVariable(memberName);
        if (!(variable instanceof Structure)) {
            return null;
        }
        Structure structure = (Structure) variable;
        StructureDataIterator iterator = structureData.getArrayStructure(
                memberName).getStructureDataIterator();
        return new BufrStructureIterator(structure, iterator);
    }

    /**
     * Get a value for a String member variable.
     * 
     * @param memberName
     *            the name of the member variable
     * @return The String value or null if the member is not present or if the
     *         value is missing.
     */
    public String lookupStringValue(String memberName) {
        Member member = structureData.findMember(memberName);
        if (member == null) {
            return null;
        }
        String value = structureData.getScalarString(member);
        if (value == null) {
            return null;
        }
        /*
         * Missing values end up encoded as a String full of unicode replacement
         * characters \uFFFD, it is more convenient for this method to return
         * null if the string is missing.
         */
        for (int i = 0; i < value.length(); i += 1) {
            char c = value.charAt(i);
            if (c != '\uFFFD') {
                return value;
            }
        }
        return null;
    }

    /**
     * Get a value for a Numeric member variable. This method will also apply
     * any scale/offset if the Structure attributes contain scale/offset fields.
     * 
     * @param memberName
     *            the name of the member variable
     * @return The Number value or null if the member is not present or if the
     *         value is missing.
     */
    public Number lookupNumericValue(String memberName) {
        Variable var = structure.findVariable(memberName);
        if (var == null) {
            return null;
        }
        Object valueObj = structureData.getScalarObject(memberName);
        Attribute missingAttrib = var.findAttribute("missing_value");
        if (missingAttrib != null) {
            Number missingNumber = missingAttrib.getNumericValue();
            if (missingNumber.equals(valueObj)) {
                return null;
            } else if (valueObj instanceof Integer) {
                if (((Integer) valueObj).intValue() == missingNumber.intValue()) {
                    return null;
                }
            } else if (valueObj instanceof Short) {
                if (((Short) valueObj).shortValue() == missingNumber
                        .shortValue()) {
                    return null;
                }
            }
        }
        if (!(valueObj instanceof Number)) {
            throw new IllegalStateException(
                    "Expected a numeric value but recieved a "
                            + valueObj.getClass().getSimpleName());
        }
        Number value = (Number) valueObj;
        Attribute scaleAttrib = var.findAttribute("scale_factor");
        if (scaleAttrib != null) {
            value = value.floatValue()
                    * scaleAttrib.getNumericValue().floatValue();
        }
        Attribute addAttrib = var.findAttribute("add_offset");
        if (addAttrib != null) {
            value = value.floatValue()
                    + addAttrib.getNumericValue().floatValue();
        }
        return value;
    }
}
