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
package com.raytheon.uf.edex.archive;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.text.DecimalFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.file.Files;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.core.exception.ShutdownException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.processor.IDatabaseProcessor;

/**
 * Receives records to be archived to disk. Records can be written over extended
 * periods of time and so when writing, the previous records must be dup elim'd
 * against the current set of data to handle database being updated.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2013 2555       rjpeter     Initial creation.
 * Jan 23, 2014 2555       rjpeter     Updated to be a row at a time using ScrollableResults.
 * Feb 04, 2014 2770       rferrel     The dumpPdos now dumps all PluginDataObjects.
 * Feb 12, 2014 2784       rjpeter     Update logging for dup elim scenarios.
 * Apr 23, 2014 2726       rjpeter     Add shutdown checks to allow for timely shutdown.
 * Feb 29, 2016 5420       tgurney     Remove timestampCheck arg from IDataStore.copy()
 * May 05, 2017 6256       tgurney     Restrict file and dir permissions
 * Jun 16, 2017 6256       tgurney     Do not createFile if file already exists
 * </pre>
 * 
 * @author rjpeter
 */

public class DatabaseArchiveProcessor<T extends PersistableDataObject<?>>
        implements IDatabaseProcessor<T> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatabaseArchiveProcessor.class);

    /** Chunk size for I/O Buffering and Compression */
    private static final int CHUNK_SIZE = 8192;

    private static final String BIN_FILE_EXT = ".bin";

    private static final String GZIP_FILE_EXT = ".gz";

    private static final Pattern FILE_COUNT_PATTERN = Pattern
            .compile("^(.*\\.bin\\.)(\\d+)(?:\\.gz)?$");

    private static final Set<PosixFilePermission> FILE_PERMISSIONS = new HashSet<>();

    private static final Set<PosixFilePermission> DIR_PERMISSIONS = new HashSet<>();
    static {
        // in octal: 0640
        FILE_PERMISSIONS.add(PosixFilePermission.OWNER_READ);
        FILE_PERMISSIONS.add(PosixFilePermission.OWNER_WRITE);
        FILE_PERMISSIONS.add(PosixFilePermission.GROUP_READ);
        // directories have 0750
        DIR_PERMISSIONS.addAll(FILE_PERMISSIONS);
        DIR_PERMISSIONS.add(PosixFilePermission.OWNER_EXECUTE);
        DIR_PERMISSIONS.add(PosixFilePermission.GROUP_EXECUTE);
    }

    protected final String archivePath;

    protected final String pluginName;

    protected final PluginDao dao;

    protected final IPluginArchiveFileNameFormatter nameFormatter;

    protected boolean debugArchiver = false;

    protected boolean compressDatabaseFiles = false;

    protected int fetchSize = 1000;

    protected int entriesInMemory = 0;

    protected Set<String> datastoreFilesToArchive = new HashSet<String>();

    protected Map<String, FileStatus> filesCreatedThisSession = new HashMap<String, FileStatus>();

    protected Set<File> dirsToCheckNumbering = new HashSet<File>();

    protected int recordsSaved = 0;

    protected boolean failed = false;

    protected Map<String, List<PersistableDataObject<?>>> pdosByFile;

    public DatabaseArchiveProcessor(String archivePath, String pluginName,
            PluginDao dao, IPluginArchiveFileNameFormatter nameFormatter) {
        this.archivePath = archivePath;
        this.pluginName = pluginName;
        this.dao = dao;
        this.nameFormatter = nameFormatter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.database.processor.IDatabaseProcessor#process(java
     * .util.List)
     */
    @Override
    public boolean process(T object) throws ShutdownException {
        if (object != null) {
            if (pdosByFile == null) {
                pdosByFile = new HashMap<String, List<PersistableDataObject<?>>>(
                        (int) (fetchSize * 1.3));
            }

            String path = nameFormatter.getFilename(pluginName, dao, object);
            if (path.endsWith(".h5")) {
                datastoreFilesToArchive.add(path);
                path = path.substring(0, path.length() - 3);
            }

            List<PersistableDataObject<?>> list = pdosByFile.get(path);
            if (list == null) {
                list = new LinkedList<PersistableDataObject<?>>();
                pdosByFile.put(path, list);
            }

            list.add(object);

            entriesInMemory++;
            if (entriesInMemory >= fetchSize) {
                try {
                    savePdoMap(pdosByFile);
                    pdosByFile.clear();
                    int prev = recordsSaved;
                    recordsSaved += entriesInMemory;
                    entriesInMemory = 0;
                    statusHandler.info(pluginName + ": Processed rows " + prev
                            + " to " + recordsSaved);
                } catch (Exception e) {
                    statusHandler.error(pluginName
                            + ": Error occurred saving data to archive", e);
                    failed = true;
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Checks file numbering on any directory that have been flagged. Also
     * archives any associated hdf5 files.
     */
    @Override
    public void finish() throws ShutdownException {
        if (entriesInMemory > 0) {
            try {
                savePdoMap(pdosByFile);
                pdosByFile.clear();
                int prev = recordsSaved;
                recordsSaved += entriesInMemory;
                statusHandler.info(pluginName + ": Processed rows " + prev
                        + " to " + recordsSaved);
            } catch (Exception e) {
                statusHandler.error(pluginName
                        + ": Error occurred saving data to archive", e);
                failed = true;
            }
        }

        for (File dir : dirsToCheckNumbering) {
            checkFileNumbering(dir);
        }

        if (!datastoreFilesToArchive.isEmpty()) {
            statusHandler.info(pluginName + ": archiving "
                    + datastoreFilesToArchive.size() + " hdf5 file(s)");
            Compression compRequired = Compression.LZF;
            PluginProperties props = PluginRegistry.getInstance()
                    .getRegisteredObject(pluginName);

            if ((props != null) && (props.getCompression() != null)) {
                if (compRequired.equals(Compression.valueOf(props
                        .getCompression()))) {
                    // if plugin is already compressed to the correct level,
                    // no additional compression required
                    compRequired = null;
                }
            }

            for (String dataStoreFile : datastoreFilesToArchive) {
                EDEXUtil.checkShuttingDown();
                IDataStore ds = DataStoreFactory.getDataStore(new File(FileUtil
                        .join(pluginName, dataStoreFile)));
                // all dataStore files should end with .h5
                String destDir = (dataStoreFile.endsWith(".h5") ? dataStoreFile
                        .substring(0, dataStoreFile.length() - 3)
                        : dataStoreFile);

                String outputDir = FileUtil.join(archivePath, pluginName,
                        destDir) + File.separator;

                try {
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler.debug(pluginName
                                + ": Archiving data store file "
                                + dataStoreFile + " to " + outputDir);
                    }

                    // copy the changed hdf5 file, does repack if
                    // compRequired, otherwise pure file copy
                    ds.copy(outputDir, compRequired, 0, 0);
                } catch (StorageException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage());
                }
            }
            statusHandler.info(pluginName + ": hdf5 archiving complete");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.database.processor.IDatabaseProcessor#getFetchSize()
     */
    @Override
    public int getBatchSize() {
        return fetchSize;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.database.processor.IDatabaseProcessor#setFetchSize
     * (int)
     */
    @Override
    public void setBatchSize(int fetchSize) {
        this.fetchSize = fetchSize;
    }

    /**
     * True if the processor had a failure during its execution.
     * 
     * @return
     */
    public boolean isFailed() {
        return failed;
    }

    /**
     * Reset any state fields so processor can be reused.
     */
    public void reset() {
        datastoreFilesToArchive.clear();
        filesCreatedThisSession.clear();
        dirsToCheckNumbering.clear();
        recordsSaved = 0;
        failed = false;
    }

    /**
     * @return the debugArchiver
     */
    public boolean isDebugArchiver() {
        return debugArchiver;
    }

    /**
     * @param debugArchiver
     *            the debugArchiver to set
     */
    public void setDebugArchiver(boolean debugArchiver) {
        this.debugArchiver = debugArchiver;
    }

    /**
     * @return the compressDatabaseFiles
     */
    public boolean isCompressDatabaseFiles() {
        return compressDatabaseFiles;
    }

    /**
     * @param compressDatabaseFiles
     *            the compressDatabaseFiles to set
     */
    public void setCompressDatabaseFiles(boolean compressDatabaseFiles) {
        this.compressDatabaseFiles = compressDatabaseFiles;
    }

    /**
     * @return the recordsSaved
     */
    public int getRecordsSaved() {
        return recordsSaved;
    }

    /**
     * Saves data in the pdo map to disk. The data in the pdoMap is dup elim'd
     * against any previously written records.
     * 
     * @param pdoMap
     * @throws SerializationException
     * @throws IOException
     */
    protected void savePdoMap(Map<String, List<PersistableDataObject<?>>> pdoMap)
            throws SerializationException, IOException, ShutdownException {
        StringBuilder baseDir = new StringBuilder(160);
        Set<Object> identifierSet = null;

        for (Map.Entry<String, List<PersistableDataObject<?>>> entry : pdoMap
                .entrySet()) {
            EDEXUtil.checkShuttingDown();
            baseDir.setLength(0);
            baseDir.append(archivePath).append(File.separator)
                    .append(pluginName).append(File.separator)
                    .append(entry.getKey()).append(File.separator);
            File dir = new File(baseDir.toString());

            Files.createDirectories(dir.toPath(),
                    PosixFilePermissions.asFileAttribute(DIR_PERMISSIONS));

            List<PersistableDataObject<?>> pdos = entry.getValue();
            if (identifierSet == null) {
                identifierSet = new HashSet<Object>(pdos.size(), 1);
            } else {
                identifierSet.clear();
            }

            for (PersistableDataObject<?> pdo : pdos) {
                identifierSet.add(pdo.getIdentifier());
            }

            SortedMap<Integer, File> fileMap = getArchivedFiles(dir);
            pdos = dupElimPreviousFiles(fileMap, pdos, identifierSet);

            // if any records left in pdos, write to disk
            if (pdos.size() > 0) {
                int fileCount = 1;
                if (!fileMap.isEmpty()) {
                    fileCount += fileMap.lastKey();
                }
                File newFile = new File(dir, dir.getName() + BIN_FILE_EXT + "."
                        + fileCount);
                fileMap.put(fileCount, newFile);
                writeDataToDisk(newFile, pdos);
                FileStatus status = new FileStatus();
                status.dupElimUntilIndex = 0;
                status.fileFull = pdos.size() >= fetchSize;
                filesCreatedThisSession.put(newFile.getAbsolutePath(), status);

                // check if we have added another digit and should add a 0 to
                // previous numbers
                String fileCountStr = Integer.toString(fileCount);
                if (fileCountStr.startsWith("1") && fileCountStr.endsWith("0")) {
                    dirsToCheckNumbering.add(dir);
                }
            }
        }
    }

    /**
     * Checks the pdos against the previously written pdos. If a previous pdo
     * would be overwritten its entry is deleted from the previous file and the
     * file rewritten. If the last file does not contain a full fetch set, then
     * pdos are appended up to the fetch size. If any pdos are remaining to be
     * written they are returned otherwise an empty list is returned.
     * 
     * @param fileMap
     * @param pdos
     * @param identifierSet
     * @return
     * @throws IOException
     * @throws SerializationException
     */
    protected List<PersistableDataObject<?>> dupElimPreviousFiles(
            SortedMap<Integer, File> fileMap,
            List<PersistableDataObject<?>> pdos, Set<Object> identifierSet)
            throws IOException, SerializationException, ShutdownException {
        if (!fileMap.isEmpty()) {
            Iterator<File> fileIter = fileMap.values().iterator();
            while (fileIter.hasNext()) {
                EDEXUtil.checkShuttingDown();
                File dataFile = fileIter.next();
                int dupElimUntil = Integer.MAX_VALUE;
                FileStatus prevFileStatus = filesCreatedThisSession
                        .get(dataFile.getAbsolutePath());

                if (prevFileStatus != null) {
                    dupElimUntil = prevFileStatus.dupElimUntilIndex;
                    if ((dupElimUntil <= 0) && prevFileStatus.fileFull) {
                        continue;
                    }
                }

                List<PersistableDataObject<?>> pdosFromDisk = readDataFromDisk(dataFile);
                if (pdosFromDisk.size() > 0) {
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler.debug(pluginName + ": Checking "
                                + pdosFromDisk.size()
                                + " old records from file: "
                                + dataFile.getAbsolutePath());
                    }

                    Iterator<PersistableDataObject<?>> pdoIter = pdosFromDisk
                            .iterator();
                    int dupsRemoved = 0;
                    int index = 0;
                    boolean needsUpdate = false;

                    while (pdoIter.hasNext() && (index < dupElimUntil)) {
                        PersistableDataObject<?> pdo = pdoIter.next();

                        if (identifierSet.contains(pdo.getIdentifier())) {
                            pdoIter.remove();
                            needsUpdate = true;
                            dupsRemoved++;
                        }

                        index++;
                    }

                    if (dupsRemoved > 0) {
                        statusHandler.info(pluginName + ": Removed "
                                + dupsRemoved + " old records from file: "
                                + dataFile.getAbsolutePath());
                    }

                    if (!fileIter.hasNext()
                            && (pdosFromDisk.size() < fetchSize)) {
                        // last file, add more data to it
                        needsUpdate = true;

                        if (prevFileStatus == null) {
                            prevFileStatus = new FileStatus();
                            prevFileStatus.dupElimUntilIndex = pdosFromDisk
                                    .size();
                            prevFileStatus.fileFull = pdos.size() >= fetchSize;
                            filesCreatedThisSession.put(
                                    dataFile.getAbsolutePath(), prevFileStatus);
                        }

                        int numToAdd = fetchSize - pdosFromDisk.size();
                        numToAdd = Math.min(numToAdd, pdos.size());

                        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                            statusHandler.debug(pluginName + ": Adding "
                                    + numToAdd + " records to file: "
                                    + dataFile.getAbsolutePath());
                        }

                        pdosFromDisk.addAll(pdos.subList(0, numToAdd));
                        if (numToAdd < pdos.size()) {
                            pdos = pdos.subList(numToAdd, pdos.size());
                        } else {
                            pdos = Collections.emptyList();
                        }
                    }

                    if (needsUpdate) {
                        if (!pdosFromDisk.isEmpty()) {
                            writeDataToDisk(dataFile, pdosFromDisk);
                            if (prevFileStatus != null) {
                                prevFileStatus.fileFull = pdosFromDisk.size() >= fetchSize;
                            }
                        } else {

                            dirsToCheckNumbering.add(dataFile.getParentFile());
                            if (dataFile.exists() && !dataFile.delete()) {
                                statusHandler
                                        .error(pluginName
                                                + ": Failed to delete file ["
                                                + dataFile.getAbsolutePath()
                                                + "], all entries have been updated in later files.");
                                if (!dataFile.renameTo(new File(dataFile
                                        .getAbsoluteFile() + ".bad"))) {
                                    statusHandler.error(pluginName + ": file ["
                                            + dataFile.getAbsoluteFile()
                                            + "] cannot be renamed to .bad");
                                }
                            }
                            fileIter.remove();
                        }
                    }
                }
            }
        }

        return pdos;
    }

    /**
     * Reads the serialized data from file. If there is a problem reading the
     * file it is renamed to .bad.
     * 
     * @param file
     * @return
     * @throws IOException
     * @throws SerializationException
     */
    @SuppressWarnings("unchecked")
    protected List<PersistableDataObject<?>> readDataFromDisk(File file)
            throws IOException, SerializationException {
        if (file.exists()) {
            InputStream is = null;
            boolean successful = false;
            try {
                if (file.getName().endsWith(GZIP_FILE_EXT)) {
                    is = new GZIPInputStream(new FileInputStream(file),
                            CHUNK_SIZE);
                } else {
                    is = new BufferedInputStream(new FileInputStream(file),
                            CHUNK_SIZE);
                }

                List<PersistableDataObject<?>> rval = SerializationUtil
                        .transformFromThrift(List.class, is);
                successful = true;
                return rval;
            } finally {
                if (!successful) {
                    // couldn't read in file, move it to bad
                    if (file.exists()
                            && !file.renameTo(new File(file.getAbsoluteFile()
                                    + ".bad"))) {
                        statusHandler.error(pluginName + ": file ["
                                + file.getAbsoluteFile()
                                + "] cannot be renamed to .bad");
                    }
                }
                if (is != null) {
                    try {
                        is.close();
                    } catch (IOException e) {
                        statusHandler.error(pluginName
                                + ": Error occurred closing input stream", e);
                    }
                }
            }
        }

        return Collections.emptyList();
    }

    /**
     * Dynamic serializes the pdos. The data will be written to file. If the
     * file has .gz extension and the database compression flag is not set, the
     * .gz file will be deleted in favor of the uncompressed file. Reverse also
     * holds true. This allows a file written under a different compression
     * scheme to automatically be converted if rewritten out.
     * 
     * @param file
     * @param pdos
     * @throws IOException
     * @throws SerializationException
     */
    protected void writeDataToDisk(File file,
            List<PersistableDataObject<?>> pdos) throws IOException,
            SerializationException {
        OutputStream os = null;

        File gzipFile = null;
        File baseFile = null;
        String fileAbsPath = file.getAbsolutePath();

        if (fileAbsPath.endsWith(GZIP_FILE_EXT)) {
            gzipFile = file;
            baseFile = new File(fileAbsPath.substring(0,
                    fileAbsPath.length() - 3));
        } else {
            baseFile = file;
            gzipFile = new File(fileAbsPath + GZIP_FILE_EXT);
        }

        try {
            if (!file.getParentFile().exists()) {
                Files.createDirectories(file.getParentFile().toPath(),
                        PosixFilePermissions.asFileAttribute(DIR_PERMISSIONS));
            }

            if (compressDatabaseFiles) {
                if (baseFile.exists()) {
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler
                                .debug(pluginName
                                        + ": Database compression flag changed, deleting uncompressed file "
                                        + baseFile.getAbsolutePath());
                    }
                    baseFile.delete();
                }
                if (!gzipFile.exists()) {
                    Files.createFile(gzipFile.toPath(), PosixFilePermissions
                            .asFileAttribute(FILE_PERMISSIONS));
                }
                os = new GZIPOutputStream(new FileOutputStream(gzipFile),
                        CHUNK_SIZE);
            } else {
                if (gzipFile.exists()) {
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler
                                .debug(pluginName
                                        + ": Database compression flag changed, deleting compressed file "
                                        + gzipFile.getAbsolutePath());
                    }
                    gzipFile.delete();
                }
                if (!baseFile.exists()) {
                    Files.createFile(baseFile.toPath(), PosixFilePermissions
                            .asFileAttribute(FILE_PERMISSIONS));
                }
                os = new BufferedOutputStream(new FileOutputStream(baseFile),
                        CHUNK_SIZE);
            }

            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug(pluginName + ": Serializing " + pdos.size()
                        + " records to file " + file.getAbsolutePath());
            }

            // Thrift serialize pdo list
            SerializationUtil.transformToThriftUsingStream(pdos, os);
            os.flush();
        } finally {
            if (os != null) {
                try {
                    os.close();
                } catch (IOException e) {
                    statusHandler.error(pluginName
                            + ": Error occurred closing output stream", e);
                }
            }
        }

        if (debugArchiver) {
            String debugPath = baseFile.getAbsolutePath() + ".debug";
            dumpPdos(debugPath.toString(), pdos);
        }
    }

    /**
     * Dump the record information being archived to a file.
     * 
     * @param basePath
     * @param pdos
     */
    private void dumpPdos(String basePath, List<PersistableDataObject<?>> pdos) {
        Writer writer = null;
        File dumpFile = null;

        try {
            int index = 0;
            do {
                index++;
                dumpFile = new File(basePath + "." + index);
            } while (dumpFile.exists());

            Iterator<PersistableDataObject<?>> pdoIter = pdos.iterator();
            writer = new BufferedWriter(new FileWriter(dumpFile));

            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.info(String.format("%s: Dumping " + pdos.size()
                        + " records to: %s", pluginName,
                        dumpFile.getAbsolutePath()));
            }

            while (pdoIter.hasNext()) {
                PersistableDataObject<?> pdo = pdoIter.next();
                if (pdo instanceof PluginDataObject) {
                    PluginDataObject pluginDataObject = (PluginDataObject) pdo;
                    if (pluginDataObject.getId() != 0) {
                        // otherwise was read from file and will be recorded in
                        // a previous entry
                        writer.write("" + pluginDataObject.getId() + ":");
                    } else {
                        writer.write("-:");
                    }
                    writer.write(pluginDataObject.getDataURI());
                    writer.write("\n");
                } else {
                    writer.write(pdo.getIdentifier().toString());
                    writer.write("\n");
                }
            }
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM, pluginName
                            + ": Unable to dump pdo data to debug file: "
                            + (dumpFile != null ? dumpFile.getAbsolutePath()
                                    : null), e);
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (Exception e) {
                    // Ignore
                }
            }
        }
    }

    /**
     * Returns a map of the archived database files in the directory. Map
     * ordered by file count in the file name.
     * 
     * @param baseDir
     * @return
     */
    protected SortedMap<Integer, File> getArchivedFiles(File baseDir) {
        File[] dirListing = baseDir.listFiles();
        SortedMap<Integer, File> fileMap = new TreeMap<Integer, File>();

        if ((dirListing != null) && (dirListing.length > 0)) {
            for (File dataFile : dirListing) {
                if (dataFile.isFile()) {
                    String name = dataFile.getName();
                    Matcher matcher = FILE_COUNT_PATTERN.matcher(name);
                    if (matcher.matches()) {
                        String fileNumStr = matcher.group(2);
                        int fileNum = Integer.parseInt(fileNumStr);
                        fileMap.put(fileNum, dataFile);
                    }
                }
            }
        }

        return fileMap;
    }

    /**
     * Checks database bin files in directory for consistency. If a file has
     * been deleted or if the number of digits has increased, files are renamed
     * to fill in holes as well as to have leading zeros as necessary.
     * 
     * @param dir
     */
    protected void checkFileNumbering(File dir) {
        SortedMap<Integer, File> fileMap = getArchivedFiles(dir);
        int nextFileCount = 1;
        int size = fileMap.size();
        StringBuilder formatString = new StringBuilder(4);
        do {
            formatString.append("0");
            size /= 10;
        } while (size > 0);

        DecimalFormat format = new DecimalFormat(formatString.toString());
        statusHandler.info("Checking file numbering consistency for "
                + dir.getAbsolutePath());

        for (Map.Entry<Integer, File> entry : fileMap.entrySet()) {
            int fileNum = entry.getKey();
            File oldFile = entry.getValue();
            String name = oldFile.getName();
            Matcher m = FILE_COUNT_PATTERN.matcher(name);
            if (m.matches()) {
                String oldCountString = m.group(2);

                if ((fileNum > nextFileCount)
                        || (oldCountString.length() != formatString.length())) {
                    // rename file to file count
                    String newFileName = m.group(1) + format.format(fileNum);
                    if (name.endsWith(GZIP_FILE_EXT)) {
                        newFileName += GZIP_FILE_EXT;
                    }

                    File newFile = new File(oldFile.getParent(), newFileName);
                    if (!oldFile.renameTo(newFile)) {
                        statusHandler
                                .error("Failed rename file "
                                        + oldFile.getAbsolutePath()
                                        + " to "
                                        + newFile.getAbsolutePath()
                                        + ".  Stopping file number consistency checking.");
                        return;
                    }
                }

                nextFileCount++;
            }
        }
    }

    /**
     * Inner class for tracking status of files that have been written out this
     * session.
     */
    private static class FileStatus {
        /**
         * Apply dup elim logic until this index is reached.
         */
        private int dupElimUntilIndex;

        /**
         * Way of tracking if file is considered full. Tracked so that if the
         * file doesn't need to be dup elim'd due to being written this session
         * and the file is full then there is no reason to deserialize it.
         */
        private boolean fileFull;
    }
}
