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
package com.raytheon.uf.common.nc.bufr;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Stack;

import ucar.ma2.Array;
import ucar.ma2.ArraySequence;
import ucar.ma2.DataType;
import ucar.ma2.StructureData;
import ucar.ma2.StructureMembers;
import ucar.ma2.StructureMembers.Member;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Structure;
import ucar.nc2.Variable;

import com.raytheon.uf.common.numeric.UnsignedNumbers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * BUFR parser that utilizes the UCAR BUFR decoder that works with the Netcdf
 * Java API. The parser works as an event-based pull parser. The standard usage
 * involves an event loop that checks if hasNext() returns true then calls
 * next() for the current event type which can be acted upon.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2014 2905       bclement    Initial creation
 * Mar 26, 2014 2905       bclement    fixed types, added scale/offset
 * Apr 01, 2014 2905       bclement    moved splitter functionality to separate utility
 *                                     added scanForStructField()
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BufrParser {

    public static final String MISSING_VAL_ATTRIB = "missing_value";

    public static final String SCALE_FACTOR_ATTRIB = "scale_factor";

    public static final String OFFSET_ATTRIB = "add_offset";

    public static enum Event {
        START_FILE, START_STRUCTURE, FIELD, END_STRUCTURE, END_FILE
    }

    private static final IUFStatusHandler log = UFStatus
            .getHandler(BufrParser.class);

    private final File bufrFile;

    private final NetcdfFile ncfile;

    private final Iterator<Variable> varIter;

    private Variable currentVar;

    private StructIterator structIter;

    private final Stack<StructureLevel> structStack = new Stack<StructureLevel>();

    private Event lastEvent = null;

    /**
     * @param bufrFile
     *            BUFR file, may contain mixed message types
     * @throws IOException
     */
    public BufrParser(final File bufrFile) throws IOException {
        this.bufrFile = bufrFile;
        this.ncfile = NetcdfFile.open(bufrFile.getAbsolutePath());
        this.varIter = ncfile.getVariables().iterator();
    }

    /**
     * @return true if parser is not done parsing
     * @throws IOException
     */
    public boolean hasNext() throws IOException {
        if (lastEvent == null) {
            /* we haven't started the file yet */
            return true;
        }
        if (!structStack.isEmpty()) {
            StructureLevel level = structStack.peek();
            if (level.hasNext()) {
                return true;
            }
        }
        if (structIter != null && structIter.hasNext()) {
            return true;
        }
        if (varIter != null && varIter.hasNext()) {
            return true;
        }
        if (lastEvent != null && !lastEvent.equals(Event.END_FILE)) {
            /* only one more event left, the end of the file */
            return true;
        }
        return false;
    }

    /**
     * Get the next parsing event. hasNext() must be called before this method
     * is called.
     * 
     * @return
     * @throws IOException
     */
    public Event next() throws IOException {
        Event rval;
        if (lastEvent == null) {
            rval = Event.START_FILE;
        } else if (!structStack.isEmpty()) {
            rval = nextMember();
        } else if (structIter != null && structIter.hasNext()) {
            /* in a variable with a sequence of structures, get the next one */
            rval = startStructure((Structure) currentVar, structIter.next());
        } else if (structIter != null && !structIter.hasNext()) {
            /*
             * this happens if the variable had an empty structure iterator.
             * Structures are usually ended when there are no more members
             */
            structIter = null;
            rval = Event.END_STRUCTURE;
        } else if (varIter != null) {
            if (varIter.hasNext()) {
                /* we are working through a bufr file, start the next variable */
                rval = startVariable();
            } else {
                /* no more variables, we are at the end of the bufr file */
                structIter = null;
                currentVar = null;
                rval = Event.END_FILE;
            }
        } else {
            /* don't set rval to null so we preserve the correct lastEvent */
            return null;
        }
        lastEvent = rval;
        return rval;
    }

    /**
     * Process a member of the current structure. If the structure has no more
     * members, the structure is ended
     * 
     * @return
     * @throws IOException
     */
    private Event nextMember() throws IOException {
        Event rval;
        StructureLevel level = structStack.peek();
        if (level.hasNext()) {
            /* in a level of a structure, get the next member */
            level.next();
            Member m = level.getCurrentMember();
            DataType type = m.getDataType();
            if (type.equals(DataType.STRUCTURE)) {
                /* current member is a nested structure, start a new level */
                StructureData structData = level.getSubStructure();
                rval = startSubStructure(level, structData);
            } else if (type.equals(DataType.SEQUENCE)) {
                /* current member is a sequence of members, start a new level */
                ArraySequence arr = level.getSubSequence();
                rval = startSubStructure(level, arr.getStructureMembers());
            } else {
                /* current member is a field */
                rval = Event.FIELD;
            }
        } else {
            /* no more members of this structure level */
            rval = endStructure();
        }
        return rval;
    }

    /**
     * Start a nested structure
     * 
     * @param parent
     * @param childData
     * @return
     * @throws IOException
     */
    private Event startSubStructure(StructureLevel parent,
            StructureData childData) throws IOException {
        return startSubStructure(parent, childData,
                childData.getStructureMembers());
    }

    /**
     * Start a nested structure
     * 
     * @param parent
     * @param childMembers
     * @return
     * @throws IOException
     */
    private Event startSubStructure(StructureLevel parent,
            StructureMembers childMembers) throws IOException {
        return startSubStructure(parent, parent.getStructData(), childMembers);
    }

    /**
     * Start a nested structure
     * 
     * @param parent
     * @param childData
     * @param childMembers
     * @return
     * @throws IOException
     */
    private Event startSubStructure(StructureLevel parent,
            StructureData childData, StructureMembers childMembers)
            throws IOException {
        Variable parentVar = parent.getCurrentMemberVar();
        if (!(parentVar instanceof Structure)) {
            log.error("Structure variable members out of sync");
            throw new IllegalStateException("Structure variable members out of sync");
        }
        List<Variable> memberVars = ((Structure) parentVar).getVariables();
        return startStructure(new StructureLevel(childData, childMembers,
                memberVars));
    }

    /**
     * Start processing the next structure
     * 
     * @param s
     * 
     * @param structData
     * @return
     * @throws IOException
     */
    private Event startStructure(Structure s, StructureData structData)
            throws IOException {
        StructureLevel level = new StructureLevel(structData, s.getVariables());
        return startStructure(level);
    }

    /**
     * Start processing the next structure
     * 
     * @param level
     * @return
     * @throws IOException
     */
    private Event startStructure(StructureLevel level) throws IOException {
        structStack.push(level);
        return Event.START_STRUCTURE;
    }

    /**
     * Finalize processing for the current structure
     * 
     * @return
     * @throws IOException
     */
    private Event endStructure() throws IOException {
        structStack.pop();
        if (structIter != null && !structIter.hasNext()) {
            structIter = null;
        }
        return Event.END_STRUCTURE;
    }

    /**
     * Start processing the next NetCDF variable
     * 
     * @return
     * @throws IOException
     */
    private Event startVariable() throws IOException {
        Event rval;
        currentVar = varIter.next();
        if (currentVar instanceof Structure) {
            Structure s = (Structure) currentVar;
            structIter = new StructIterator(s.getStructureIterator(),
                    s.getNumberOfMemberVariables());
            if (structIter.hasNext()) {
                rval = startStructure(s, structIter.next());
            } else {
                /*
                 * start an event for an empty structure, next event will be an
                 * end structure event
                 */
                rval = Event.START_STRUCTURE;
            }
        } else {
            rval = Event.FIELD;
        }
        return rval;
    }

    /**
     * @return the NetCDF File
     */
    public NetcdfFile getNcfile() {
        return ncfile;
    }

    /**
     * @return BUFR file being processed
     */
    public File getFile() {
        return bufrFile;
    }

    /**
     * @return null if no events have happened
     */
    public Event getLastEvent() {
        return lastEvent;
    }

    /**
     * @return true if the current field is a member of a structure (as opposed
     *         to a variable field)
     */
    private boolean fieldIsStructMember() {
        /* struct members are kept on the stack */
        return !structStack.isEmpty();
    }

    /**
     * @return true if the current field is a scalar value
     */
    public boolean fieldIsScalar() {
        boolean rval = false;
        if (fieldIsStructMember()) {
            StructureLevel level = structStack.peek();
            Member m = level.getCurrentMember();
            rval = m.isScalar();
        } else if (currentVar != null) {
            rval = currentVar.isScalar();
        }
        return rval;
    }

    /**
     * @return unit string for field, may be null
     */
    public String getFieldUnits() {
        // TODO convert to standard edex units (javax.measure)?
        String rval = null;
        if (fieldIsStructMember()) {
            StructureLevel level = structStack.peek();
            rval = level.getCurrentMember().getUnitsString();
        } else if (currentVar != null) {
            rval = currentVar.getUnitsString();
        }
        return rval;
    }

    /**
     * @return name of current field
     */
    public String getFieldName() {
        String rval = null;
        if (fieldIsStructMember()) {
            StructureLevel level = structStack.peek();
            rval = level.getCurrentMember().getName();
        } else if (currentVar != null) {
            rval = currentVar.getFullName();
        }
        return rval;
    }

    /**
     * Note: this is the data type after processing, not the datatype stored in
     * the file
     * 
     * @param charArrayAsString
     *            true if character arrays should be treated as strings
     * @return data type of field
     */
    public DataType getFieldType(boolean charArrayAsString) {
        Variable var = getFieldVariable();
        DataType rval;
        if (charArrayAsString && !var.isScalar()
                && var.getDataType().equals(DataType.CHAR)) {
            rval = DataType.STRING;
        } else if (isScaledOrOffset(var)) {
            /* variables that need scale or offset will be returned as doubles */
            rval = DataType.DOUBLE;
        } else {
            rval = getUnscaledDataType(var);
        }
        return rval;
    }

    /**
     * Get type accounting for unsigned type promotion
     * 
     * @param var
     * @return
     */
    private static DataType getUnscaledDataType(Variable var) {
        DataType rval;
        /*
         * We will promote unsigned values to the next largest signed type
         */
        boolean isUnsigned = var.isUnsigned();
        switch (var.getDataType()) {
        case BYTE:
            rval = (isUnsigned ? DataType.SHORT : DataType.BYTE);
            break;
        case SHORT:
            rval = (isUnsigned ? DataType.INT : DataType.SHORT);
            break;
        case INT:
            rval = (isUnsigned ? DataType.LONG : DataType.INT);
            break;
        case LONG:
            /*
             * no support for unsigned longs, we would have to use BigInteger
             */
            rval = DataType.LONG;
            break;
        default:
            rval = var.getDataType();
        }
        return rval;
    }

    /**
     * @param var
     * @return true if the field has a scale factor or addition offset
     */
    private static boolean isScaledOrOffset(Variable var) {
        return var.findAttribute(OFFSET_ATTRIB) != null
                || var.findAttribute(SCALE_FACTOR_ATTRIB) != null;
    }

    /**
     * @param charArrayAsString
     *            true if character arrays should be treated as strings
     * @return null if value is a missing value
     * @throws IOException
     */
    public Object getFieldScalarValue(boolean charArrayAsString)
            throws IOException {
        TypedArray typedArray = readFieldAsArray();
        if (typedArray == null) {
            return null;
        }
        Variable var = getFieldVariable();
        return getFieldScalarValue(typedArray, var, charArrayAsString);
    }

    /**
     * @param typedArray
     *            storage for field value
     * @param var
     *            NetCDF variable
     * @param charArrayAsString
     *            true if character arrays should be treated as strings
     * @return null if value is a missing value
     * @throws IOException
     */
    private static Object getFieldScalarValue(TypedArray typedArray,
            Variable var, boolean charArrayAsString) {
        Array array = typedArray.array;
        DataType type = typedArray.type;
        Object value;
        if (charArrayAsString && array.getSize() > 1
                && type.equals(DataType.CHAR)) {
            int len = (int) array.getSize();
            StringBuilder builder = new StringBuilder(len);
            for (int i = 0; i < len; ++i) {
                builder.append(array.getChar(i));
            }
            value = builder.toString();
        } else {
            value = array.getObject(0);
        }

        return processValue(value, var);
    }

    /**
     * Perform any promotion, scaling or missing value operations
     * 
     * @param value
     * @param var
     * @return
     */
    private static Object processValue(Object value, Variable var) {
        Object rval = promoteValueType(value, var);
        if (isMissingValue(rval, var)) {
            rval = null;
        } else if (isScaledOrOffset(var)) {
            if (value instanceof Number) {
                rval = scaleAndOffset((Number) rval, var);
            } else {
                log.warn("Scale or offset attribute on non-numerical field: "
                        + var.getFullName());
            }
        }
        return rval;
    }

    /**
     * Promote unsigned numbers to next largest data type if needed
     * 
     * @param value
     * @param var
     * @return
     */
    private static Object promoteValueType(Object value, Variable var) {
        if (value == null) {
            return null;
        }
        if (var.isUnsigned() && value instanceof Number) {
            /* promote unsigned values to the next largest signed type */
            switch (var.getDataType()) {
            case BYTE:
                value = (Short) (UnsignedNumbers.ubyteToShort((Byte) value));
                break;
            case SHORT:
                value = (Integer) (UnsignedNumbers.ushortToInt((Short) value));
                break;
            case INT:
                value = (Long) (UnsignedNumbers.uintToLong((Integer) value));
            case LONG:
                log.warn("Unsigned long not supported, value may be incorrectly interpreted: "
                        + var.getFullName());
                break;
            default:
                // no action
            }
        }
        return value;
    }

    /**
     * Apply scale factor or addition offset if present
     * 
     * @param value
     * @param var
     * @return
     */
    public static Number scaleAndOffset(Number value, Variable var) {
        Number scaleFactor = getFieldAttributeAsNum(SCALE_FACTOR_ATTRIB, var);
        if (scaleFactor != null) {
            value = value.doubleValue() * scaleFactor.doubleValue();
        }
        Number offset = getFieldAttributeAsNum(OFFSET_ATTRIB, var);
        if (offset != null) {
            value = value.doubleValue() + offset.doubleValue();
        }
        return value;
     }

    /**
     * Get field values as collection. Missing values will be represented by
     * NULL elements in collection.
     * 
     * @return null if not processing variable
     * @throws IOException
     */
    public Collection<Object> getFieldCollection()
            throws IOException {
        TypedArray typedArray = readFieldAsArray();
        if (typedArray == null) {
            return null;
        }
        Variable var = getFieldVariable();
        Array array = typedArray.array;
        int len = (int) array.getSize();
        Collection<Object> rval = new ArrayList<Object>(len);
        for (int i = 0; i < len; ++i) {
            rval.add(processValue(array.getObject(i), var));
        }
        return rval;
    }

    /*
     * simple return value wrapper to pair a datatype with an array
     */
    private static class TypedArray {
        public final Array array;

        public final DataType type;

        public TypedArray(Array array, DataType type) {
            this.array = array;
            this.type = type;
        }
    }

    /**
     * Get field values as an array with the associated field datatype
     * 
     * @return null if not processing variable
     * @throws IOException
     */
    private TypedArray readFieldAsArray() throws IOException {
        TypedArray rval = null;
        if (fieldIsStructMember()) {
            StructureLevel level = structStack.peek();
            StructureData s = level.getStructData();
            Member m = level.getCurrentMember();
            rval = new TypedArray(s.getArray(m), m.getDataType());
        } else if (currentVar != null) {
            rval = new TypedArray(currentVar.read(), currentVar.getDataType());
        }
        return rval;
    }

    /**
     * Get scale factor attribute value
     * 
     * @return null if not present
     */
    public Number getScaleFactor() {
        return getFieldAttributeAsNum(SCALE_FACTOR_ATTRIB);
    }

    /**
     * Get addition offset attribute value
     * 
     * @return null if not present
     */
    public Number getOffset() {
        return getFieldAttributeAsNum(OFFSET_ATTRIB);
    }

    /**
     * Get attribute object for field
     * 
     * @param name
     * @return null if not found
     */
    public Attribute getFieldAttribute(String name) {
        Attribute rval = null;
        Variable var = getFieldVariable();
        if (var != null) {
            rval = var.findAttributeIgnoreCase(name);
        }
        return rval;
    }

    /**
     * Get attribute value for field
     * 
     * @param name
     * @return null if not found
     */
    public Number getFieldAttributeAsNum(String name) {
        Number rval = null;
        Attribute attr = getFieldAttribute(name);
        if (attr != null) {
            rval = attr.getNumericValue();
        }
        return rval;
    }

    /**
     * Get attribute value for field from variable
     * 
     * @param name
     * @param var
     * @return
     */
    private static Number getFieldAttributeAsNum(String name, Variable var) {
        Number rval = null;
        Attribute attr = var.findAttributeIgnoreCase(name);
        if (attr != null) {
            rval = attr.getNumericValue();
        }
        return rval;
    }

    /**
     * Get variable object for current field
     * 
     * @return
     */
    public Variable getFieldVariable() {
        Variable var = null;
        if (fieldIsStructMember()) {
            StructureLevel level = structStack.peek();
            var = level.getCurrentMemberVar();
        } else {
            var = currentVar;
        }
        return var;
    }

    /**
     * @param unscaledValue
     *            field value before any scaling or offset is applied
     * @param var
     * @return true if value matches the missing value for field
     */
    private static boolean isMissingValue(Object unscaledValue, Variable var) {
        if (unscaledValue == null) {
            return true;
        }
        boolean rval;
        Attribute missingAttrib = var
                .findAttributeIgnoreCase(MISSING_VAL_ATTRIB);
        if (missingAttrib == null) {
            /* if there is no special missing value, all values are valid */
            rval = false;
        } else {
            Number numMissing = missingAttrib.getNumericValue();
            switch (getUnscaledDataType(var)) {
            case BYTE:
                rval = ((Byte) unscaledValue).byteValue() == numMissing
                        .byteValue();
                break;
            case CHAR:
                rval = ((Character) unscaledValue).charValue() == numMissing
                        .intValue();
                break;
            case SHORT:
                rval = ((Short) unscaledValue).shortValue() == numMissing
                        .shortValue();
                break;
            case INT:
                rval = ((Integer) unscaledValue).intValue() == numMissing
                        .intValue();
                break;
            case LONG:
                rval = ((Long) unscaledValue).longValue() == numMissing
                        .longValue();
                break;
            case FLOAT:
                rval = ((Float) unscaledValue).floatValue() == numMissing
                        .floatValue();
                break;
            case DOUBLE:
                rval = ((Double) unscaledValue).doubleValue() == numMissing
                        .doubleValue();
                break;
            case STRING:
                rval = unscaledValue.toString().equals(
                        missingAttrib.getStringValue());
                break;
            default:
                rval = unscaledValue.equals(missingAttrib.getValue(0));
            }
        }
        return rval;
    }

    /**
     * Get field from current structure level. Does not affect the current state
     * of the parser. Only searches current level (does not go into
     * substructures).
     * 
     * @param fieldName
     * @param charArrayAsString
     * @return null if no field found or parser is not currently parsing a
     *         structure
     */
    public BufrDataItem scanForStructField(String fieldName, boolean charArrayAsString) {
        BufrDataItem rval = null;
        if ( structStack.isEmpty()){
            return rval;
        }
        StructureLevel level = structStack.peek();
        Iterator<Member> memberIter = level.getMemberList().iterator();
        Iterator<Variable> varIter = level.getMemberVarList().iterator();
        while (memberIter.hasNext() && varIter.hasNext()) {
            Member member = memberIter.next();
            Variable variable = varIter.next();
            DataType type = member.getDataType();
            if (!type.equals(DataType.STRUCTURE) && !type.equals(DataType.SEQUENCE)) {
                /* current member is a field */
                if (member.getName().equals(fieldName)){
                    StructureData sd = level.getStructData();
                    Array array = sd.getArray(member);
                    Object value = getFieldScalarValue(new TypedArray(array,
                            type), variable, charArrayAsString);
                    rval = new BufrDataItem(member.getName(), value, type,
                            variable);
                    break;
                }
            }
        }
        return rval;
    }

}
