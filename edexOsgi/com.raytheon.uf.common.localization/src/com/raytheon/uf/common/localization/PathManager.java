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
package com.raytheon.uf.common.localization;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.localization.ILocalizationAdapter.ListResponse;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.localization.msgs.ListResponseEntry;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * A generalized implementation for interfacing with LocalizationFiles.
 * 
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/12/2008              chammack    Initial Creation.
 * Oct 23, 2012 1322       djohnson    Allow test code in the same package to clear fileCache.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PathManager implements IPathManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PathManager.class, "Localization");

    // @VisibleForTesting
    static final Map<LocalizationFileKey, LocalizationFile> fileCache = new ConcurrentHashMap<LocalizationFileKey, LocalizationFile>();

    final ILocalizationAdapter adapter;

    PathManager(ILocalizationAdapter adapter) {
        this.adapter = adapter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#getStaticFile(java.lang
     * .String)
     */
    @Override
    public File getStaticFile(String name) {
        LocalizationFile locFile = getStaticLocalizationFile(name);
        File file = null;
        if (locFile != null) {
            file = locFile.getFile();
        }

        return file;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#getStaticLocalizationFile
     * (java.lang.String)
     */
    @Override
    public LocalizationFile getStaticLocalizationFile(String name) {
        Validate.notNull(name, "Path name must not be null");

        name = name.replace(File.separator, IPathManager.SEPARATOR);

        // Iterate through the types
        LocalizationType[] types = this.adapter.getStaticContexts();
        List<LocalizationContext> contexts = new ArrayList<LocalizationContext>();
        for (LocalizationType type : types) {
            // Iterate through the hierarchy
            LocalizationContext[] searchContexts = this.adapter
                    .getLocalSearchHierarchy(type);

            contexts.addAll(java.util.Arrays.asList(searchContexts));
        }

        LocalizationFile[] files = getLocalizationFile(
                contexts.toArray(new LocalizationContext[contexts.size()]),
                name);

        for (LocalizationFile file : files) {
            if (file != null && file.exists()) {
                // First file found in hierarchy is used
                return file;
            }
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#getFile(com.raytheon.
     * edex.utility.LocalizationContext, java.lang.String)
     */
    @Override
    public File getFile(LocalizationContext context, String name) {
        Validate.notNull(context, "Context must not be null");
        Validate.notNull(name, "Path name must not be null");

        LocalizationFile file = getLocalizationFile(context, name);
        return file != null ? file.getFile() : null;
    }

    @Override
    public Map<LocalizationLevel, LocalizationFile> getTieredLocalizationFile(
            LocalizationType type, String name) {
        Map<LocalizationLevel, LocalizationFile> map = new HashMap<LocalizationLevel, LocalizationFile>();

        LocalizationContext[] contexts = getLocalSearchHierarchy(type);

        LocalizationFile[] files = getLocalizationFile(contexts, name);
        for (LocalizationFile lf : files) {
            if (lf != null && lf.exists()) {
                map.put(lf.getContext().getLocalizationLevel(), lf);
            }
        }

        return map;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#getLocalizationFile(com
     * .raytheon.edex.utility.LocalizationContext, java.lang.String)
     */
    @Override
    public LocalizationFile getLocalizationFile(LocalizationContext context,
            String name) {
        Validate.notNull(name, "File name string must be provided");
        Validate.notNull(context, "Context must not be null");

        while (name.endsWith("/") || name.endsWith("\\")) { // Win32
            name = name.substring(0, name.length() - 1);
        }
        LocalizationFile[] files = getLocalizationFile(
                new LocalizationContext[] { context }, name);
        for (LocalizationFile file : files) {
            if (file != null) {
                return file;
            }
        }
        return null;
    }

    private LocalizationFile[] getLocalizationFile(
            LocalizationContext[] contexts, String name) {
        name = name.replace(File.separator, IPathManager.SEPARATOR); // Win32

        Map<LocalizationContext, LocalizationFile> availableFiles = new HashMap<LocalizationContext, LocalizationFile>(
                contexts.length * 2);

        List<LocalizationContext> ctxToCheck = new ArrayList<LocalizationContext>(
                contexts.length);
        for (LocalizationContext ctx : contexts) {
            LocalizationFile cached = fileCache.get(new LocalizationFileKey(
                    name, ctx));
            if (cached != null) {
                if (cached.isNull() == false) {
                    availableFiles.put(ctx, cached);
                }
            } else {
                ctxToCheck.add(ctx);
            }
        }

        if (ctxToCheck.size() > 0) {
            ListResponse[] entry = null;
            try {
                entry = this.adapter.getLocalizationMetadata(ctxToCheck
                        .toArray(new LocalizationContext[ctxToCheck.size()]),
                        name);
            } catch (LocalizationException e) {
                // Error on server, no files will be returned
                e.printStackTrace();
            }

            if (entry != null) {
                synchronized (fileCache) {
                    for (ListResponse lr : entry) {
                        LocalizationFile file = createFromResponse(lr);
                        if (file.isNull() == false) {
                            availableFiles.put(file.getContext(), file);
                        }
                    }
                }
            }
        }

        List<LocalizationFile> rval = new ArrayList<LocalizationFile>(
                availableFiles.size());
        for (LocalizationContext ctx : contexts) {
            LocalizationFile file = availableFiles.get(ctx);
            if (file != null) {
                rval.add(file);
            }
        }

        return rval.toArray(new LocalizationFile[rval.size()]);
    }

    /**
     * Creates a LocalizationFile from a {@link ListResponse}, callers need to
     * synchronize on fileCache before calling
     * 
     * @param response
     * @return LocalizationFile for response (never null), but be sure to check
     *         isNull() on file object
     */
    private LocalizationFile createFromResponse(ListResponse response) {
        // able to resolve file, lf will be set, check cache
        LocalizationFileKey key = new LocalizationFileKey(response.fileName,
                response.context);
        LocalizationFile lf = fileCache.get(key);
        if (lf != null && lf.isNull() == false) {
            // Ensure latest data for file, will only be null if no File can be
            // returned for path/context.
            lf.update(response);
        } else {
            // Not in cache or null reference, see if file can be resolved
            if (lf == null) {
                // Default to null file if not from cache
                lf = new LocalizationFile();
            }
            File file = this.adapter.getPath(response.context,
                    response.fileName);
            if (file != null) {
                // No cache file available and path is resolved, create
                lf = new LocalizationFile(this.adapter, response.context, file,
                        response.date, response.fileName, response.checkSum,
                        response.isDirectory, response.existsOnServer,
                        response.protectedLevel);
            }
            fileCache.put(key, lf);
        }
        return lf;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#listFiles(com.raytheon
     * .edex.utility.LocalizationContext, java.lang.String, java.lang.String[])
     */
    @Override
    public LocalizationFile[] listFiles(LocalizationContext context,
            String name, String[] filter, boolean recursive, boolean filesOnly) {
        Validate.notNull(name, "Path name must not be null");
        Validate.notNull(context, "Context must not be null");

        return listFiles(new LocalizationContext[] { context }, name, filter,
                recursive, filesOnly);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#listFiles(com.raytheon
     * .edex.utility.LocalizationContext[], java.lang.String,
     * java.lang.String[])
     */
    @Override
    public LocalizationFile[] listFiles(LocalizationContext[] contexts,
            String name, String[] filter, boolean recursive, boolean filesOnly) {
        try {
            List<LocalizationFile> files = new ArrayList<LocalizationFile>();
            ListResponse[] entries = this.adapter.listDirectory(contexts, name,
                    recursive, filesOnly);

            synchronized (fileCache) {
                for (ListResponse entry : entries) {
                    if (entry.isDirectory
                            || matchesExtension(entry.fileName, filter)) {
                        LocalizationFile file = createFromResponse(entry);
                        if (file.exists()) {
                            files.add(file);
                        }
                    }
                }
            }

            return files.toArray(new LocalizationFile[files.size()]);
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error listing files: " + e.getLocalizedMessage(), e);
        }

        return null;
    }

    /**
     * Convenience method that checks to see if a file string ends with a set of
     * extensions. If extensions is null, true is returned.
     * 
     * @param name
     *            the file name
     * @param extensions
     *            the list of extensions, or null if no filter
     * @return true if the filename matches the filter
     */
    private boolean matchesExtension(String name, String[] extensions) {
        if (extensions == null) {
            return true;
        }

        for (String extension : extensions) {
            if (name.endsWith(extension)) {
                return true;
            }
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#listFiles(java.lang.
     * String , java.lang.String[])
     */
    @Override
    public LocalizationFile[] listStaticFiles(String name, String[] filter,
            boolean recursive, boolean filesOnly) {
        Validate.notNull(name, "Path name must not be null");

        // Iterate through the types
        LocalizationType[] types = this.adapter.getStaticContexts();
        List<LocalizationContext> contexts = new ArrayList<LocalizationContext>();
        for (LocalizationType type : types) {
            // Iterate through the hierarchy
            LocalizationContext[] searchContexts = this.adapter
                    .getLocalSearchHierarchy(type);

            contexts.addAll(java.util.Arrays.asList(searchContexts));
        }

        LocalizationFile[] files = listFiles(
                contexts.toArray(new LocalizationContext[contexts.size()]),
                name, filter, recursive, filesOnly);
        List<LocalizationFile> filterFiles = new ArrayList<LocalizationFile>();

        Map<String, LocalizationFile> filterMap = new HashMap<String, LocalizationFile>();
        for (LocalizationFile file : files) {
            String id = file.getContext().getLocalizationType()
                    + file.getName();
            id = id.replace("\\", "/"); // Win32
            if (filterMap.containsKey(id) == false) {
                filterFiles.add(file);
                filterMap.put(id, file);
            }
        }
        return filterFiles.toArray(new LocalizationFile[filterFiles.size()]);
    }

    @Override
    public LocalizationContext getContext(LocalizationType type,
            LocalizationLevel level) {
        return this.adapter.getContext(type, level);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#getContextForSite(com
     * .raytheon.uf.common.localization.LocalizationContext.LocalizationType,
     * java.lang.String)
     */
    @Override
    public LocalizationContext getContextForSite(LocalizationType type,
            String siteId) {
        // TODO: What is this method for? shouldn't there be checking for
        // null/empty siteId?
        LocalizationContext ctx = this.adapter.getContext(type,
                LocalizationLevel.SITE);
        ctx.setContextName(siteId);
        return ctx;
    }

    @Override
    public LocalizationContext[] getLocalSearchHierarchy(LocalizationType type) {
        return this.adapter.getLocalSearchHierarchy(type);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#getContextList(com.raytheon
     * .uf.common.localization.LocalizationContext.LocalizationLevel)
     */
    @Override
    public String[] getContextList(LocalizationLevel level) {
        try {
            return this.adapter.getContextList(level);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return new String[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.IPathManager#getAvailableLevels()
     */
    @Override
    public LocalizationLevel[] getAvailableLevels() {
        return adapter.getAvailableLevels();
    }

    /**
     * Stores the localization file cache in this class to the file passed in.
     * Can be used to take a snapshot of the current cached files
     * 
     * @param cacheFile
     */
    public static void storeCache(File cacheFile) {
        // TODO: Store the cache
        Map<SerializableKey, ListResponseEntry> cacheObject = new HashMap<PathManager.SerializableKey, ListResponseEntry>(
                fileCache.size() * 2);
        for (Map.Entry<LocalizationFileKey, LocalizationFile> entry : fileCache
                .entrySet()) {
            ListResponseEntry lre = new ListResponseEntry();
            LocalizationFile file = entry.getValue();
            lre.setChecksum(file.getCheckSum());
            lre.setContext(file.getContext());
            lre.setDate(file.getTimeStamp());
            lre.setDirectory(file.isDirectory());
            lre.setExistsOnServer(file.isAvailableOnServer());
            lre.setFileName(file.getName());
            lre.setProtectedLevel(file.getProtectedLevel());
            cacheObject.put(new SerializableKey(entry.getKey()), lre);
        }
        try {
            FileOutputStream fout = new FileOutputStream(cacheFile);
            DynamicSerializationManager.getManager(SerializationType.Thrift)
                    .serialize(cacheObject, fout);
            fout.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Restores the LocalizationFile cache from this file. Can be used in
     * conjunction with storeCache to restore a snapshot of the cache
     * 
     * @param cacheFile
     */
    @SuppressWarnings("unchecked")
    public static void restoreCache(File cacheFile) {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        try {
            FileInputStream fin = new FileInputStream(cacheFile);
            Map<SerializableKey, ListResponseEntry> cacheObject = (Map<SerializableKey, ListResponseEntry>) DynamicSerializationManager
                    .getManager(SerializationType.Thrift).deserialize(fin);
            fin.close();
            for (Map.Entry<SerializableKey, ListResponseEntry> entry : cacheObject
                    .entrySet()) {
                ListResponseEntry lre = entry.getValue();
                SerializableKey key = entry.getKey();
                LocalizationFile file = new LocalizationFile();
                if (lre.getContext() != null && lre.getFileName() != null) {
                    file = new LocalizationFile(pm.adapter, lre.getContext(),
                            pm.adapter.getPath(lre.getContext(),
                                    lre.getFileName()), lre.getDate(),
                            lre.getFileName(), lre.getChecksum(),
                            lre.isDirectory(), lre.isExistsOnServer(),
                            lre.getProtectedLevel());
                }
                fileCache.put(
                        new LocalizationFileKey(key.getFileName(), key
                                .getContext()), file);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @DynamicSerialize
    public static class SerializableKey {

        @DynamicSerializeElement
        private String fileName;

        @DynamicSerializeElement
        private LocalizationContext context;

        public SerializableKey() {

        }

        /**
         * @param fileName
         * @param context
         */
        public SerializableKey(LocalizationFileKey key) {
            this.fileName = key.path;
            this.context = key.context;
        }

        /**
         * @return the fileName
         */
        public String getFileName() {
            return fileName;
        }

        /**
         * @param fileName
         *            the fileName to set
         */
        public void setFileName(String fileName) {
            this.fileName = fileName;
        }

        /**
         * @return the context
         */
        public LocalizationContext getContext() {
            return context;
        }

        /**
         * @param context
         *            the context to set
         */
        public void setContext(LocalizationContext context) {
            this.context = context;
        }

    }
}
