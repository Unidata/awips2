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

package com.raytheon.uf.viz.core.localization;

import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import com.raytheon.uf.common.localization.Checksum;
import com.raytheon.uf.common.localization.FileLocker;
import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.localization.ILocalizationAdapter;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.localization.msgs.AbstractPrivilegedUtilityCommand;
import com.raytheon.uf.common.localization.msgs.AbstractUtilityResponse;
import com.raytheon.uf.common.localization.msgs.DeleteUtilityCommand;
import com.raytheon.uf.common.localization.msgs.DeleteUtilityResponse;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.common.localization.msgs.GetUtilityCommand;
import com.raytheon.uf.common.localization.msgs.ListContextCommand;
import com.raytheon.uf.common.localization.msgs.ListResponseEntry;
import com.raytheon.uf.common.localization.msgs.ListUtilityCommand;
import com.raytheon.uf.common.localization.msgs.ListUtilityResponse;
import com.raytheon.uf.common.localization.msgs.PrivilegedUtilityRequestMessage;
import com.raytheon.uf.common.localization.msgs.UtilityRequestMessage;
import com.raytheon.uf.common.localization.msgs.UtilityResponseMessage;
import com.raytheon.uf.common.localization.stream.LocalizationStreamGetRequest;
import com.raytheon.uf.common.localization.stream.LocalizationStreamPutRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.VizServers;
import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.PrivilegedRequestFactory;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Manages Localization processes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2007            chammack    Initial Creation.
 * Jul 24, 2007            njensen     Added upload().
 * Jul 30, 2007            njensen     Refactored.
 * Feb 12, 2008            chammack    Removed base configuration
 * Mar 26, 2008            njensen     Added rename() and getFileContents().
 * May 19, 2007 1127       randerso    Implemented error handling
 * Sep 12, 2012 1167       djohnson    Add datadelivery servers.
 * Jan 14, 2013 1469       bkowal      Removed the hdf5 data directory.
 * Aug 02, 2013 2202       bsteffen    Add edex specific connectivity checking.
 * Aug 27, 2013 2295       bkowal      The entire jms connection string is now
 *                                     provided by EDEX.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class LocalizationManager implements IPropertyChangeListener {
    private static transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationManager.class, "CAVE");

    public static final String USER_CONTEXT = LocalizationConstants.P_LOCALIZATION_USER_NAME;

    public static final String SITE_CONTEXT = LocalizationConstants.P_LOCALIZATION_SITE_NAME;

    /** The singleton instance */
    private static LocalizationManager instance;

    /** The localization preference store */
    private ScopedPreferenceStore localizationStore;

    /** The localization adapter */
    private final ILocalizationAdapter adapter;

    /** The base context directory */
    private static String baseDir;

    /** The user directory */
    private static String userDir;

    private static File orphanFileDir;

    /** The current localization server */
    private String currentServer;

    private boolean overrideServer;

    /** The current localization site */
    private String currentSite;

    private boolean overrideSite;

    /** Was the alert server launched within cave? */
    public static boolean internalAlertServer = ProgramArguments.getInstance()
            .getBoolean("-alertviz");

    private static Map<LocalizationLevel, String> contextMap = new HashMap<LocalizationLevel, String>();

    /**
     * Private constructor Use singleton construction
     */
    private LocalizationManager() {
        this.adapter = new CAVELocalizationAdapter();
        this.overrideServer = false;
        this.overrideSite = false;
        try {
            localizationStore = new ScopedPreferenceStore(
                    InstanceScope.INSTANCE, "localization");
            localizationStore.addPropertyChangeListener(this);
            loadHttpServer();
            loadAlertServer();
            loadCurrentSite();
        } catch (ExceptionInInitializerError e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error initializing localization store", e);
        }

        registerContextName(LocalizationLevel.USER, getCurrentUser());
        registerContextName(LocalizationLevel.WORKSTATION, VizApp.getHostName());
        registerContextName(LocalizationLevel.SITE, getCurrentSite());
        registerContextName(LocalizationLevel.CONFIGURED, getCurrentSite());
        registerContextName(LocalizationLevel.BASE, null);
    }

    /**
     * Singleton: get the localization manager
     * 
     * @return the localization manager singleton
     */
    public static synchronized LocalizationManager getInstance() {
        if (instance == null) {
            instance = new LocalizationManager();
        }

        return instance;
    }

    public static void registerContextName(LocalizationLevel level, String name) {
        contextMap.put(level, name);
    }

    public static String getContextName(LocalizationLevel level) {
        return contextMap.get(level);
    }

    /**
     * @return the current site
     */
    public String getCurrentSite() {
        return this.currentSite;
    }

    public void setCurrentSite(String currentSite) {
        if (!this.currentSite.equals(currentSite)) {
            this.currentSite = currentSite;
            registerContextName(LocalizationLevel.SITE, this.currentSite);
            registerContextName(LocalizationLevel.CONFIGURED, this.currentSite);
            if (!overrideSite) {
                localizationStore.putValue(
                        LocalizationConstants.P_LOCALIZATION_SITE_NAME,
                        this.currentSite);
                applyChanges();
            }
        }
    }

    public void setCurrentServer(String currentServer) {
        if (!this.currentServer.equals(currentServer)) {
            this.currentServer = currentServer;
            if (!overrideServer) {
                localizationStore.putValue(
                        LocalizationConstants.P_LOCALIZATION_HTTP_SERVER,
                        this.currentServer);
                applyChanges();
            }

            try {
                GetServersResponse resp = ConnectivityManager
                        .checkLocalizationServer(currentServer, false);
                VizApp.setHttpServer(resp.getHttpServer());
                VizApp.setJmsConnectionString(resp.getJmsConnectionString());
                VizApp.setPypiesServer(resp.getPypiesServer());
                VizServers.getInstance().setServerLocations(
                        resp.getServerLocations());
            } catch (VizException e) {
                statusHandler.handle(UFStatus.Priority.SIGNIFICANT,
                        "Error connecting to localization server", e);
            }
        }
    }

    /**
     * @return the current user
     */
    public String getCurrentUser() {
        return System.getProperty("user.name");
    }

    /**
     * @return the localizationStore
     */
    public ScopedPreferenceStore getLocalizationStore() {
        return localizationStore;
    }

    /**
     * Sets a base directory
     * 
     * Not intended to be called outside of JUnit test cases!!!
     * 
     * @param dir
     */
    public static void setBaseDir(String dir) {
        baseDir = dir;
    }

    /**
     * Sets a user directory
     * 
     * Not intended to be called outside of JUnit test cases!!!
     * 
     * @param dir
     */
    public static void setUserDir(String dir) {
        userDir = dir;
    }

    /**
     * Return the base installation directory
     * 
     * This should be used rather than absolute paths
     * 
     * @return the installation directory
     */
    public static String getBaseDir() {
        if (baseDir == null) {
            baseDir = System.getenv("VIZ_HOME");
        }

        if (baseDir == null) {
            baseDir = Platform.getInstallLocation().getURL().getPath();
            // Win32 URLS start with "/" but also may contain a drive letter
            // if so, then remove the "/" because it's improper for a path
            if (baseDir.startsWith("/") && baseDir.charAt(2) == ':')
                baseDir = baseDir.substring(1);
        }

        return baseDir;
    }

    /**
     * Return the user installation directory
     * 
     * This should be used rather than absolute paths
     * 
     * @return the user directory
     */
    public static String getUserDir() {

        if (userDir == null) {
            userDir = Platform.getUserLocation().getURL().getPath();
            // Win32 URLS start with "/" but also may contain a drive letter
            // if so, then remove the "/" because it's improper for a path
            if (userDir.startsWith("/") && userDir.charAt(2) == ':')
                userDir = userDir.substring(1);
        }

        return userDir;
    }

    private static File getOrphanedFileDir() {
        if (orphanFileDir == null) {
            orphanFileDir = new File(getUserDir() + File.separator
                    + "orphanFiles");
            if (orphanFileDir.exists() == false) {
                orphanFileDir.mkdirs();
            }
        }
        return orphanFileDir;
    }

    /**
     * Get the current localization server
     * 
     * @return the current server
     */
    public String getLocalizationServer() {
        return currentServer;
    }

    /**
     * Get the current site
     * 
     * @return the current site
     */
    public String getSite() {
        return getCurrentSite();
    }

    /**
     * Save the localization store, handles errors through UFStatus
     */
    private void applyChanges() {
        try {
            localizationStore.save();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving localization store");
        }
    }

    /**
     * Load the http server from the localization store and set up defaults if
     * needed
     */
    private void loadHttpServer() {
        if (!localizationStore
                .contains(LocalizationConstants.P_LOCALIZATION_HTTP_SERVER)) {
            // No value present, use default and save off
            currentServer = LocalizationConstants.DEFAULT_LOCALIZATION_SERVER;
            localizationStore.putValue(
                    LocalizationConstants.P_LOCALIZATION_HTTP_SERVER,
                    LocalizationConstants.DEFAULT_LOCALIZATION_SERVER);
            applyChanges();
        } else {
            currentServer = localizationStore
                    .getString(LocalizationConstants.P_LOCALIZATION_HTTP_SERVER);
        }
        checkForServerOverride();
    }

    /**
     * Check to see if the store has the alert server. If not store off the
     * default
     */
    private void loadAlertServer() {
        if (!localizationStore.contains(LocalizationConstants.P_ALERT_SERVER)) {
            localizationStore.putValue(LocalizationConstants.P_ALERT_SERVER,
                    LocalizationConstants.DEFAULT_ALERT_SERVER);
            applyChanges();
        }
    }

    private void loadCurrentSite() {
        if (ProgramArguments.getInstance().getString("-site") == null) {
            this.currentSite = this.localizationStore
                    .getString(LocalizationConstants.P_LOCALIZATION_SITE_NAME);
        } else {
            this.currentSite = ProgramArguments.getInstance()
                    .getString("-site").toUpperCase();
            this.overrideSite = true;
        }
    }

    private void checkForServerOverride() {
        if (ProgramArguments.getInstance().getString("-server") != null) {
            String serverOverride = ProgramArguments.getInstance().getString(
                    "-server");
            Pattern pat = Pattern.compile("http://(\\p{Graph}{1,50})");
            Matcher match = pat.matcher(currentServer);
            if (match.find()) {
                currentServer = currentServer.replace(match.group(1),
                        serverOverride);
                this.overrideServer = true;
                System.out.println("Server overridden to: " + currentServer);
            }
        }
    }

    @Override
    public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
        // Listen for localization server and personality changes
        if (LocalizationConstants.P_LOCALIZATION_HTTP_SERVER.equals(event
                .getProperty())) {
            // Server changed, grab
            String newServer = (String) event.getNewValue();
            setCurrentServer(newServer);
        } else if (LocalizationConstants.P_LOCALIZATION_SITE_NAME.equals(event
                .getProperty())) {
            String site = (String) event.getNewValue();
            setCurrentSite(site);
        }
    }

    private void retrieveFiles(GetUtilityCommand[] commands, Date[] fileStamps)
            throws LocalizationOpFailedException {
        for (int i = 0; i < commands.length; ++i) {
            GetUtilityCommand command = commands[i];
            File file = buildFileLocation(command.getContext(),
                    command.getFileName(), true);
            if (file == null) {
                continue;
            }
            try {
                file.delete();
                FileOutputStream fout = new FileOutputStream(file);
                boolean finished = false;
                LocalizationStreamGetRequest request = PrivilegedRequestFactory
                        .constructPrivilegedRequest(LocalizationStreamGetRequest.class);
                request.setContext(command.getContext());
                request.setFileName(command.getFileName());
                request.setOffset(0);
                request.setNumBytes(512 * 1024);

                while (!finished) {
                    try {
                        LocalizationStreamPutRequest response = (LocalizationStreamPutRequest) ThriftClient
                                .sendLocalizationRequest(request);

                        fout.write(response.getBytes());
                        request.setOffset(request.getOffset()
                                + response.getBytes().length);
                        if (response.isEnd()) {
                            finished = true;
                            fout.close();

                            if (fileStamps[i] != null) {
                                file.setLastModified(fileStamps[i].getTime());
                            }

                            // Mark as read only if the file is a system level
                            if (command.getContext().getLocalizationLevel()
                                    .isSystemLevel()) {
                                file.setReadOnly();
                            }
                        }
                    } catch (VizException e) {
                        e.printStackTrace();
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Error requesting file: "
                                        + String.valueOf(file), e);
                        finished = true;
                    }
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error requesting file: " + String.valueOf(file), e);
            }
        }
    }

    protected List<ListResponseEntry[]> getListResponseEntry(
            LocalizationContext[] contexts, String fileName, boolean recursive,
            boolean filesOnly) throws LocalizationOpFailedException {
        ListUtilityCommand[] cmds = new ListUtilityCommand[contexts.length];
        for (int i = 0; i < contexts.length; i++) {
            cmds[i] = new ListUtilityCommand(contexts[i], fileName, recursive,
                    filesOnly, getCurrentSite());
        }

        UtilityRequestMessage localizationRequest = new UtilityRequestMessage(
                cmds);

        AbstractUtilityResponse[] responseList = makeRequest(localizationRequest);
        if (responseList.length != contexts.length) {
            throw new LocalizationOpFailedException(
                    "Server returned more or less results than requested.  Requested "
                            + contexts.length + ", returned: "
                            + responseList.length);
        } else if (responseList.length == 0) {
            return new ArrayList<ListResponseEntry[]>();
        }

        List<ListResponseEntry[]> responses = new ArrayList<ListResponseEntry[]>();

        for (int i = 0; i < responseList.length; i++) {
            AbstractUtilityResponse response = responseList[i];
            if (!(response instanceof ListUtilityResponse)) {
                throw new LocalizationOpFailedException(
                        "Unexpected type returned"
                                + response.getClass().getName());
            }

            ListUtilityResponse listResponse = (ListUtilityResponse) response;

            ListResponseEntry[] entries = listResponse.getEntries();
            responses.add(entries);
        }

        return responses;

    }

    /**
     * Retrieves the LocalizationFile contents from the localization server.
     * Locks on the file
     * 
     * @param file
     * @throws LocalizationOpFailedException
     */
    protected void retrieve(LocalizationFile file)
            throws LocalizationOpFailedException {
        try {
            FileLocker.lock(this, file, Type.WRITE);
            if (file.isDirectory()) {
                retrieve(file.getContext(), file.getName());
            } else {
                if (needDownload(
                        file.getContext(),
                        buildFileLocation(file.getContext(), file.getName(),
                                false), file.getTimeStamp(), file.getCheckSum())) {
                    retrieveFiles(
                            new GetUtilityCommand[] { new GetUtilityCommand(
                                    file.getContext(), file.getName()) },
                            new Date[] { file.getTimeStamp() });
                }
            }
        } finally {
            FileLocker.unlock(this, file);
        }
    }

    /**
     * Retrieval which recursively downloads files for the path given the name.
     * Should be used for directories
     * 
     * @param context
     * @param fileName
     * @throws LocalizationOpFailedException
     */
    private void retrieve(LocalizationContext context, String fileName)
            throws LocalizationOpFailedException {
        List<ListResponseEntry[]> entriesList = getListResponseEntry(
                new LocalizationContext[] { context }, fileName, true, false);

        List<File> toCheck = new ArrayList<File>();
        Set<File> available = new TreeSet<File>();
        if (entriesList.size() > 0) {
            ListResponseEntry[] entries = entriesList.get(0);

            List<GetUtilityCommand> commands = new ArrayList<GetUtilityCommand>();
            List<Date> dates = new ArrayList<Date>();
            for (ListResponseEntry entry : entries) {
                File file = buildFileLocation(entry.getContext(),
                        entry.getFileName(), false);
                if (!entry.isDirectory()) {
                    available.add(file);
                    if (this.needDownload(context, entry)) {
                        GetUtilityCommand getCommand = new GetUtilityCommand(
                                context, entry.getFileName());
                        commands.add(getCommand);
                        dates.add(entry.getDate());

                    }

                } else {
                    if (file != null) {
                        file.mkdirs();
                    }

                    File[] list = file.listFiles(new FileFilter() {
                        @Override
                        public boolean accept(File pathname) {
                            return pathname.isDirectory() == false;
                        }
                    });

                    if (list != null) {
                        toCheck.addAll(Arrays.asList(list));
                    } else {
                        System.err.println("ERROR READING DIRECTORY: "
                                + file.getAbsolutePath());
                    }
                }
            }
            if (commands.size() > 0) {
                retrieveFiles(commands.toArray(new GetUtilityCommand[commands
                        .size()]), dates.toArray(new Date[dates.size()]));
            }
        }

        // Clean up any stale files that don't exist anymore
        for (File check : toCheck) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
            // hidden files are not returned from server so ignore those
            if (check.isHidden() == false && available.contains(check) == false) {
                String name = check.getName();
                // Make sure python object files don't get removed when
                // .py file is still available
                if (name.endsWith(".pyo") || name.endsWith(".pyc")) {
                    String pyName = name.substring(0, name.length() - 1);
                    if (available.contains(new File(check.getParent(), pyName))) {
                        continue;
                    }
                }

                File newFile = new File(getOrphanedFileDir(), name + "_"
                        + sdf.format(Calendar.getInstance().getTime()));
                System.err.println("MOVING OLD FILE: " + check + " to "
                        + newFile);
                check.renameTo(newFile);
            }
        }
    }

    private File buildFileLocation(LocalizationContext context,
            String fullFileName, boolean createDirectories) {
        File file = this.adapter.getPath(context, fullFileName);

        if (createDirectories && (file != null)) {
            file.getParentFile().mkdirs();
        }

        return file;
    }

    /**
     * Need to download files?
     * 
     * @param context
     * @param listResponseEntry
     * @return true if file needs to be downloaded
     */
    public boolean needDownload(LocalizationContext context,
            ListResponseEntry listResponseEntry) {
        String fullFileName = listResponseEntry.getFileName();

        File file = buildFileLocation(context, fullFileName, false);
        return needDownload(context, file, listResponseEntry.getDate(),
                listResponseEntry.getChecksum());
    }

    private boolean needDownload(LocalizationContext context, File file,
            Date timeStamp, String checkSum) {
        if (file == null) {
            return false;
        }

        if (!file.exists()) {
            return true;
        } else {

            // if (file.length() ==
            // HierarchicalPreferenceStore.EMPTY_CONFIGURATION
            // .getBytes().length) {
            // return true;
            // }

            // Check modification dates
            // if local filesystem version is newer, do not download
            // except for plugin or configuration data
            Date d = new Date(file.lastModified());
            if (!d.equals(timeStamp) || file.lastModified() == 0) {
                // Check the checksum (integrity check)
                try {
                    String checksum = Checksum.getMD5Checksum(file);
                    if (!checksum.equals(checkSum)) {
                        return true;
                    } else {
                        // This line here can cause the Localization Perspective
                        // to think the file was changed on the file system...
                        // Should we be doing this?
                        file.setLastModified(timeStamp.getTime());
                    }
                } catch (Exception e) {
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * Uploads a stream as a file to EDEX through the UtilitySrv. It is the
     * responsibility of the caller to close the stream
     * 
     * @param context
     * @param filename
     * @param in
     * @param streamLength
     * @return the new server time stamp
     * @throws LocalizationOpFailedException
     */
    protected long upload(LocalizationContext context, String filename,
            InputStream in, long streamLength)
            throws LocalizationOpFailedException {
        if (context.getLocalizationLevel().isSystemLevel()) {
            throw new UnsupportedOperationException(
                    "Saving to the System Level, "
                            + context.getLocalizationLevel()
                            + ", is not supported.");
        }

        LocalizationStreamPutRequest request;
        try {
            request = PrivilegedRequestFactory
                    .constructPrivilegedRequest(LocalizationStreamPutRequest.class);
            request.setMyContextName(LocalizationManager.getContextName(context
                    .getLocalizationLevel()));
            request.setContext(context);
            request.setFileName(filename);
        } catch (VizException e) {
            throw new LocalizationOpFailedException(
                    "Could not construct privileged utility request", e);
        }

        long serverModTime = -1;
        // Create byte[] buffer
        byte[] bytes = new byte[512 * 1024];
        // initial offset = 0
        int offset = 0;
        do {
            // set current offset
            request.setOffset(offset);
            try {
                // read in data from input stream
                int read = in.read(bytes, 0, bytes.length);
                if (read > 0) {
                    // byte read, trim if necessary
                    offset += read;
                    if (read < bytes.length) {
                        bytes = Arrays.copyOf(bytes, read);
                    }
                } else {
                    bytes = new byte[0];
                    // Should be case but paranoia takes over
                    offset = (int) streamLength;
                }
                request.setBytes(bytes);
                request.setEnd(offset == streamLength);
            } catch (IOException e) {
                throw new LocalizationOpFailedException(
                        "Could not save file, failed to read in contents", e);
            }

            try {
                Number modTime = (Number) ThriftClient
                        .sendLocalizationRequest(request);
                if (modTime != null) {
                    serverModTime = modTime.longValue();
                }
            } catch (VizException e) {
                throw new LocalizationOpFailedException(
                        "Error uploading file contents to localization server: "
                                + e.getLocalizedMessage(), e);
            }

        } while (request.isEnd() == false);
        return serverModTime;
    }

    /**
     * Deletes a localization file from localization server
     * 
     * @param context
     *            the context to the file
     * @param filename
     *            the name of the file
     * @return modified time on server
     * @throws LocalizationOpFailedException
     */
    protected long delete(LocalizationContext context, String filename)
            throws LocalizationOpFailedException {
        PrivilegedUtilityRequestMessage request;
        try {
            request = PrivilegedRequestFactory
                    .constructPrivilegedRequest(PrivilegedUtilityRequestMessage.class);
        } catch (VizException e) {
            throw new LocalizationOpFailedException(
                    "Could not construct privileged utility request", e);
        }

        DeleteUtilityCommand command = new DeleteUtilityCommand(context,
                filename);
        command.setMyContextName(getContextName(context.getLocalizationLevel()));
        AbstractPrivilegedUtilityCommand[] commands = new AbstractPrivilegedUtilityCommand[] { command };
        request.setCommands(commands);
        try {
            UtilityResponseMessage response = (UtilityResponseMessage) ThriftClient
                    .sendLocalizationRequest(request);
            if (response == null) {
                throw new LocalizationOpFailedException(
                        "No response received for delete command");
            }
            AbstractUtilityResponse[] responses = response.getResponses();
            if (responses == null || responses.length != commands.length) {
                throw new LocalizationOpFailedException(
                        "Unexpected return type from delete: Expected "
                                + commands.length + " responses, received "
                                + (responses != null ? responses.length : null));
            }
            AbstractUtilityResponse rsp = responses[0];
            if (rsp instanceof DeleteUtilityResponse) {
                DeleteUtilityResponse dur = (DeleteUtilityResponse) rsp;
                if (dur.getErrorText() != null) {
                    throw new LocalizationOpFailedException(
                            "Error processing delete command: "
                                    + dur.getErrorText());
                }
                // Yay, successful execution!
                return dur.getTimeStamp();
            } else {
                throw new LocalizationOpFailedException(
                        "Unexpected return type from delete: Expected "
                                + DeleteUtilityResponse.class + " received "
                                + (rsp != null ? rsp.getClass() : null));
            }
        } catch (VizException e) {
            throw new LocalizationOpFailedException(
                    "Error processing delete command: "
                            + e.getLocalizedMessage(), e);
        }
    }

    /**
     * Makes a request to the UtilitySrv
     * 
     * @param request
     *            the request to make
     * @return the responses from the request
     * @throws VizException
     */
    protected AbstractUtilityResponse[] makeRequest(
            UtilityRequestMessage request) throws LocalizationOpFailedException {

        AbstractUtilityResponse[] responseList;

        UtilityResponseMessage localizationResponse = null;
        try {
            localizationResponse = (UtilityResponseMessage) ThriftClient
                    .sendLocalizationRequest(request);
        } catch (VizException e) {
            throw new LocalizationOpFailedException("Localization error ", e);
        }

        responseList = localizationResponse.getResponses();

        for (AbstractUtilityResponse response : responseList) {
            if (!response.successful()) {
                throw new LocalizationOpFailedException(
                        response.getFormattedErrorMessage());
            }
        }

        return responseList;
    }

    public List<ListResponseEntry[]> getContextList(LocalizationLevel level)
            throws LocalizationOpFailedException {
        ListContextCommand cmd = new ListContextCommand();
        cmd.setRequestLevel(level);

        UtilityRequestMessage localizationRequest = new UtilityRequestMessage(
                new ListContextCommand[] { cmd });

        AbstractUtilityResponse[] responseList = makeRequest(localizationRequest);

        List<ListResponseEntry[]> responses = new ArrayList<ListResponseEntry[]>();

        for (int i = 0; i < responseList.length; i++) {
            AbstractUtilityResponse response = responseList[i];
            if (!(response instanceof ListUtilityResponse)) {
                throw new LocalizationOpFailedException(
                        "Unexpected type returned"
                                + response.getClass().getName());
            }

            ListUtilityResponse listResponse = (ListUtilityResponse) response;

            ListResponseEntry[] entries = listResponse.getEntries();
            responses.add(entries);
        }

        return responses;
    }

    public boolean isOverrideServer() {
        return overrideServer;
    }

    public boolean isOverrideSite() {
        return overrideSite;
    }
}
