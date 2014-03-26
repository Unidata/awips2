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
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
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
import ucar.nc2.iosp.bufr.writer.BufrSplitter;
import ucar.nc2.iosp.bufr.writer.BufrSplitter.Options;

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

    private final static File DEFAULT_TMP_DIR;

    static {
        final String edexHomeProp = "edex.home";
        String baseDir = System.getProperty(edexHomeProp);
        if (baseDir == null || baseDir.trim().isEmpty()) {
            log.warn("Property '" + edexHomeProp
                    + "' not set, defaulting to system tmp directory");
            DEFAULT_TMP_DIR = new File(System.getProperty("java.io.tmpdir"));
        } else {
            DEFAULT_TMP_DIR = new File(baseDir + File.separator + "data",
                    "processing");
        }
    }

    private final Options options;

    private final File[] splitFiles;

    private int fileIndex = 0;

    private NetcdfFile currentNcfile;

    private Iterator<Variable> varIter;

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
        this(bufrFile, DEFAULT_TMP_DIR);
    }

    /**
     * @param bufrFile
     *            BUFR file, may contain mixed message types
     * @param outputBaseDir
     *            base directory for temporary storage of split files
     * @throws IOException
     */
    public BufrParser(final File bufrFile, final File outputBaseDir)
            throws IOException {
        final String inputFile = bufrFile.getAbsolutePath();
        final File outputDir = getOutputDir(bufrFile.getName(), outputBaseDir);
        options = new Options() {

            @Override
            public String getFileSpec() {
                return inputFile;
            }

            @Override
            public String getDirOut() {
                return outputDir.getAbsolutePath();
            }
        };

        BufrSplitter splitter = new BufrSplitter(options);
        splitter.execute();

        splitFiles = outputDir.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".bufr");
            }
        });
    }

    /**
     * Create a temporary output directory based on the input file name
     * 
     * @param inputName
     * @param outputBaseDir
     * @return
     */
    private static File getOutputDir(final String inputName,
            final File outputBaseDir) {
        String name = inputName + "-" + System.currentTimeMillis() + "-split";
        File rval = new File(outputBaseDir, name);
        if (rval.exists()) {
            log.warn("BUFR splitter output directory already exists, is a file being processed twice?");
        }
        return rval;
    }

    /**
     * @return true if parser is not done parsing
     * @throws IOException
     */
    public boolean hasNext() throws IOException {
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
        if (fileIndex < splitFiles.length) {
            return true;
        }
        if (lastEvent != null && !lastEvent.equals(Event.END_FILE)) {
            /* only one more event left, the end of the last file */
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
        if (!structStack.isEmpty()) {
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
                varIter = null;
                rval = endFile();
            }
        } else if (fileIndex < splitFiles.length) {
            /* start the next bufr file */
            rval = startFile();
        } else if (lastEvent != null && !lastEvent.equals(Event.END_FILE)) {
            rval = endFile();
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
        Iterator<Variable> memberVarIter = ((Structure) parentVar)
                .getVariables().iterator();
        return startStructure(new StructureLevel(childData, childMembers,
                memberVarIter));
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
        StructureLevel level = new StructureLevel(structData, s.getVariables()
                .iterator());
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
     * Start processing the next NetCDF file
     * 
     * @return
     * @throws IOException
     */
    private Event startFile() throws IOException {
        File f = splitFiles[fileIndex];
        fileIndex += 1;
        currentNcfile = NetcdfFile.open(f.getAbsolutePath());
        varIter = currentNcfile.getVariables().iterator();
        return Event.START_FILE;
    }

    /**
     * Finalize processing of NetCDF file
     * 
     * @return
     * @throws IOException
     */
    private Event endFile() throws IOException {
        if (currentNcfile != null) {
            currentNcfile.close();
            currentNcfile = null;
        }
        return Event.END_FILE;
    }

    /**
     * @return null if no file is currently being processed
     */
    public NetcdfFile getCurrentNcfile() {
        return currentNcfile;
    }

    /**
     * @return null if no file has started being processed
     */
    public File getCurrentFile() {
        if (fileIndex < splitFiles.length) {
            return splitFiles[fileIndex];
        } else {
            return null;
        }
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
    private DataType getUnscaledDataType(Variable var) {
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
    private boolean isScaledOrOffset(Variable var) {
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

        return processValue(value);
    }

    /**
     * Perform any promotion, scaling or missing value operations
     * 
     * @param value
     * @return
     */
    private Object processValue(Object value) {
        Variable var = getFieldVariable();
        Object rval = promoteValueType(var, value);
        if (isMissingValue(var, rval)) {
            rval = null;
        } else if (isScaledOrOffset(var)) {
            if (value instanceof Number) {
                rval = scaleAndOffset((Number) rval);
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
     * @param var
     * @param value
     * @return
     */
    private Object promoteValueType(Variable var, Object value) {
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
     * @return
     */
    public Number scaleAndOffset(Number value) {
        Number scaleFactor = getScaleFactor();
        if (scaleFactor != null) {
            value = value.doubleValue() * scaleFactor.doubleValue();
        }
        Number offset = getOffset();
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
        Array array = typedArray.array;
        int len = (int) array.getSize();
        Collection<Object> rval = new ArrayList<Object>(len);
        for (int i = 0; i < len; ++i) {
            rval.add(processValue(array.getObject(i)));
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
     * @param var
     * @param unscaledValue
     *            field value before any scaling or offset is applied
     * @return true if value matches the missing value for field
     */
    private boolean isMissingValue(Variable var, Object unscaledValue) {
        if (unscaledValue == null) {
            return true;
        }
        boolean rval;
        Attribute missingAttrib = getFieldAttribute(MISSING_VAL_ATTRIB);
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
     * clean up temporary files
     */
    public void clean() {
        for (File f : splitFiles) {
            if (!f.delete()) {
                log.error("Unable to delete temporary file: "
                        + f.getAbsolutePath());
            }
        }
        File outdir = new File(options.getDirOut());
        if (!outdir.delete()) {
            log.error("Unable to delete temporary directory: "
                    + outdir.getAbsolutePath());
        }
    }
}
