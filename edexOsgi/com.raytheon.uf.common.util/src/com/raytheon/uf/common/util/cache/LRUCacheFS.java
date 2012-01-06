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
package com.raytheon.uf.common.util.cache;

import java.io.File;
import java.io.IOException;

/* 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 8, 2009            mschenke     Initial creation
 *
 * </pre>
 *
 * @author mschenke
 */
/**
 * This class will manage files on the file system including total size and
 * creation and destruction. The users of this class do not have to use it to
 * create cache files, they can create and name their own files and have them
 * cached. The main thing to remember is to use this class to maintain files and
 * delete files when they are no longer needed. When wanting to update a file be
 * sure to lock then file then release the lock when done so your file doesn't
 * get deleted while writing. This may already be covered by the File class but
 * I wanted to be sure. Remember, this class should only be used if you WANT to
 * manage the caching of files, there is no requirement on it and you can create
 * and write to files all you want. There may be a time where we allow multiple
 * file cache objects but for now it is one
 * 
 * @version 1.0
 */
public class LRUCacheFS {

    private static class CachedFile implements ICacheObject {

        private File file;

        public CachedFile(File file) {
            this.file = file;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.cache.ICacheObject#getSize()
         */
        @Override
        public int getSize() {
            return (int) file.length();
        }

        public void destroy() {
            this.file.delete();
        }

    }

    private static class LRUCacheInternal extends LRUCache<String, CachedFile> {

        public LRUCacheInternal(long maxSize) {
            super(maxSize);
        }

        @Override
        public void removeItem(Item item) {
            super.removeItem(item);
            item.value.destroy();
        }

    }

    /**
     * cache directory specified in preferences
     */
    private static File cacheDir;

    /**
     * Instance for file system cache
     */
    private static LRUCacheInternal fsCache;

    /**
     * Filesystem cache size
     */
    private static final int FILESYSTEM_CACHE_SIZE;

    /**
     * Get preferences for directory and size of cache
     */
    static {
        int sz = 0; // TODO allow configurable

        /** default to 512MB */
        if (sz == 0)
            FILESYSTEM_CACHE_SIZE = 512;
        else
            FILESYSTEM_CACHE_SIZE = sz;

        String cache = null; // TODO allow configurable

        /** Default to java temp directory */
        if (cache == null || "".equals(cache)) {
            cache = System.getProperty("java.io.tmpdir");
        }

        /** Set the directory */
        cacheDir = new File(cache, "vizCache");
        if (cacheDir.exists() == false) {
            cacheDir.mkdir();
        }

        /** Register any files in the cache with the cache */
        File[] files = cacheDir.listFiles();
        fsCache = new LRUCacheInternal(FILESYSTEM_CACHE_SIZE * 1024 * 1024);
        for (File file : files) {
            poll(file);
        }
    }

    private LRUCacheFS() {

    }

    /**
     * Convenience method for creating cache files in the cache directory
     * specified in the preferences, not required, you can register any file in
     * the cache
     * 
     * @return newly created file in cache directory
     */
    public static File createCacheFile() {
        File file = null;
        try {
            file = File.createTempFile("cached", ".bin", cacheDir);
            file.setReadable(true, false);
            file.setWritable(true, false);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return file;
    }

    /**
     * Convenience method for creating cache files in the cache directory
     * specified in the preferences with the given name. Not required, you can
     * register any file in the cache
     * 
     * @return newly created file in cache directory
     */
    public static File createCacheFile(String name) {
        File file = new File(getCacheDirectory(), name);
        file.setReadable(true, false);
        file.setWritable(true, false);
        return file;
    }

    /**
     * Get the cache directory
     * 
     * @return the cache directory
     */
    public static File getCacheDirectory() {
        return cacheDir;
    }

    /**
     * Let the cache know the file has been read. If the file is not in the
     * cache, it will be placed in the cache
     * 
     * @param file
     */
    public static void poll(File file) {
        CachedFile cf = fsCache.get(file.getAbsolutePath());
        if (cf == null) {
            cf = new CachedFile(file);
            fsCache.put(file.getAbsolutePath(), cf);
        }
    }

    /**
     * Manually remove a file from the cache
     * 
     * @param file
     */
    public static void remove(File file) {
        fsCache.remove(file.getAbsolutePath());
    }

}
