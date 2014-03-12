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
import ucar.nc2.NetcdfFile;
import ucar.nc2.Structure;
import ucar.nc2.Variable;
import ucar.nc2.iosp.bufr.writer.BufrSplitter;
import ucar.nc2.iosp.bufr.writer.BufrSplitter.Options;

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
 * Mar 18, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BufrParser {

    public static enum Event {
        START_FILE, START_STRUCTURE, FIELD, END_STRUCTURE, END_FILE
    }

    private static final IUFStatusHandler log = UFStatus
            .getHandler(BufrParser.class);
    
    private final static File DEFAULT_TMP_DIR;
    
    static {
        final String edexHomeProp = "edex.home";
        String baseDir = System.getProperty(edexHomeProp);
        if (baseDir == null || baseDir.trim().isEmpty()){
            log.warn("Property '" + edexHomeProp + "' not set, defaulting to system tmp directory");
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

    private static class StructureLevel {
        public final StructureData structure;

        public final Iterator<Member> memberIter;

        public Member currentMember;

        public StructureLevel(StructureData structure) {
            this(structure, structure.getStructureMembers());
        }

        public StructureLevel(StructureData structure, StructureMembers members) {
            this.structure = structure;
            this.memberIter = members.getMembers().iterator();
        }
    }

    private final Stack<StructureLevel> structStack = new Stack<BufrParser.StructureLevel>();

    private Event lastEvent = null;
    
    /**
     * @param bufrFile
     *            BUFR file, may contain mixed message types
     * @throws IOException
     */
    public BufrParser(final File bufrFile)
            throws IOException {
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
            if (level.memberIter != null && level.memberIter.hasNext()) {
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
            rval = startStructure(structIter.next());
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
        if (level.memberIter.hasNext()) {
            /* in a level of a structure, get the next member */
            Member m = level.memberIter.next();
            level.currentMember = m;
            DataType type = m.getDataType();
            if (type.equals(DataType.STRUCTURE)) {
                /* current member is a nested structure, start a new level */
                StructureData struct = (StructureData) level.structure
                        .getScalarObject(m);
                rval = startStructure(struct);
            } else if (type.equals(DataType.SEQUENCE)) {
                /* current member is a sequence of members, start a new level */
                ArraySequence arr = level.structure.getArraySequence(m);
                return startStructure(new StructureLevel(level.structure,
                        arr.getStructureMembers()));
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
     * Start processing the next structure
     * 
     * @param structure
     * @return
     * @throws IOException
     */
    private Event startStructure(StructureData structure) throws IOException {
        StructureLevel level = new StructureLevel(structure);
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
                rval = startStructure(structIter.next());
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
     * @return null if no variable is currently being processed
     */
    public Variable getCurrentVar() {
        return currentVar;
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
            Member m = level.currentMember;
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
            rval = level.currentMember.getUnitsString();
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
            rval = level.currentMember.getName();
        } else if (currentVar != null) {
            rval = currentVar.getFullName();
        }
        return rval;
    }

    /**
     * @return type of current field
     */
    public Class<?> getFieldType() {
        DataType dtype = null;
        if (fieldIsStructMember()) {
            StructureLevel level = structStack.peek();
            dtype = level.currentMember.getDataType();
        } else if (currentVar != null) {
            dtype = currentVar.getDataType();
        }
        /*
         * not using DataType.getClassType() because it returns primitive types,
         * we want to only work with objects
         */
        switch (dtype) {
        case BOOLEAN:
            return Boolean.class;
        case BYTE:
            return Byte.class;
        case CHAR:
            return Character.class;
        case SHORT:
            return Short.class;
        case INT:
            return Integer.class;
        case LONG:
            return Long.class;
        case FLOAT:
            return Float.class;
        case DOUBLE:
            return Double.class;
        case STRING:
            return String.class;
        default:
            return dtype.getClassType();
        }
    }

    /**
     * Get scalar value of field.
     * 
     * @param fieldType
     *            type retrieved from {@link BufrParser#getFieldType()}
     * @return
     * @throws IOException
     */
    public <T> T getFieldScalarValue(Class<T> fieldType) throws IOException {
        return getFieldScalarValue(fieldType, false);
    }

    /**
     * Get scalar value of field.
     * 
     * @param fieldType
     *            type retrieved from {@link BufrParser#getFieldType()}
     * @param charArrayAsString
     *            if true, character array fields are converted to Strings
     * @return
     * @throws IOException
     */
    @SuppressWarnings("unchecked")
    public <T> T getFieldScalarValue(Class<T> fieldType,
            boolean charArrayAsString) throws IOException {
        TypedArray typedArray = readFieldAsArray();
        if (typedArray == null) {
            return null;
        }
        Array array = typedArray.array;
        DataType type = typedArray.type;
        T rval;
        if (charArrayAsString && array.getSize() > 1
                && type.equals(DataType.CHAR)) {
            int len = (int) array.getSize();
            StringBuilder builder = new StringBuilder(len);
            for (int i = 0; i < len; ++i) {
                builder.append(array.getChar(i));
            }
            rval = (T) builder.toString();
        } else {
            rval = (T) array.getObject(0);
        }

        return rval;
    }

    /**
     * Get field values as collection.
     * 
     * @param fieldType
     *            type retrieved from {@link BufrParser#getFieldType()}
     * @return
     * @throws IOException
     */
    @SuppressWarnings("unchecked")
    public <T> Collection<T> getFieldCollection(Class<T> fieldType)
            throws IOException {
        TypedArray typedArray = readFieldAsArray();
        if (typedArray == null) {
            return null;
        }
        Array array = typedArray.array;
        int len = (int) array.getSize();
        Collection<T> rval = new ArrayList<T>(len);
        for (int i = 0; i < len; ++i) {
            rval.add((T) array.getObject(i));
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
     * @return
     * @throws IOException
     */
    private TypedArray readFieldAsArray() throws IOException {
        TypedArray rval = null;
        if (fieldIsStructMember()) {
            StructureLevel level = structStack.peek();
            StructureData s = level.structure;
            Member m = level.currentMember;
            rval = new TypedArray(s.getArray(m), m.getDataType());
        } else if (currentVar != null) {
            rval = new TypedArray(currentVar.read(), currentVar.getDataType());
        }
        return rval;
    }

    /**
     * clean up temporary files
     */
    public void clean(){
        for ( File f : splitFiles){
            if (!f.delete()){
                log.error("Unable to delete temporary file: " + f.getAbsolutePath());
            }
        }
        File outdir = new File(options.getDirOut());
        if (!outdir.delete()) {
            log.error("Unable to delete temporary directory: "
                    + outdir.getAbsolutePath());
        }
    }
}
