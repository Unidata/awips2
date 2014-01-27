/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.map.LRUMap;

import com.raytheon.uf.common.util.concurrent.KeyLock;
import com.raytheon.uf.common.util.concurrent.KeyLocker;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;

/**
 * Abstract base for file system backed query stores. Provides threadsafe
 * list/store/delete operations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2013            bclement     Initial creation
 * Nov  8, 2013 1314       bclement     updated lock to use read/write
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractFSQueryStore<T> extends AbstractFsStore {

    /**
     * @param storeLocation
     */
    public AbstractFSQueryStore(File storeLocation) {
        super(storeLocation);
    }

    @SuppressWarnings("unchecked")
    private final Map<String, T> objCache = Collections
            .synchronizedMap(new LRUMap(2));

    @SuppressWarnings("unchecked")
    private final Map<String, String> strCache = Collections
            .synchronizedMap(new LRUMap(2));

    private final KeyLocker<String> locker = new KeyLocker<String>();

    /**
     * @param id
     * @param query
     * @throws Exception
     */
    public void store(String id, T query) throws OgcException {
        KeyLock<String> lock = locker.getLock(id);
        lock.lock();
        try {
            storeObject(id, query);
        } finally {
            lock.unlock();
        }
    }

    /**
     * This must be externally synchronized
     * 
     * @param id
     * @param query
     * @throws Exception
     */
    private void storeObject(String id, T query) throws OgcException {
        objCache.put(id, query);
        String xml = marshal(query);
        storeString(id, xml);
    }

    protected abstract String marshal(T query) throws OgcException;

    /**
     * This must be externally synchronized
     * 
     * @param id
     * @param query
     * @throws Exception
     */
    private void storeString(String id, String query) throws OgcException {
        OutputStreamWriter out = null;
        strCache.put(id, query);
        try {
            File storage = getStorageFile(id);
            out = new OutputStreamWriter(new FileOutputStream(storage));
            out.write(query);
        } catch (Exception e) {
            throw new OgcException(Code.InternalServerError, e);
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    throw new OgcException(Code.InternalServerError, e);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.querystore.QueryStore#retrieve(java.lang.String)
     */
    public T retrieve(String id) throws OgcException {
        T rval;
        KeyLock<String> lock = locker.getLock(id);
        lock.readLock();
        try {
            rval = retrieveObject(id);
        } finally {
            lock.readUnlock();
        }
        return rval;
    }

    /**
     * This must be externally synchronized
     * 
     * @param id
     * @return
     * @throws Exception
     */
    private T retrieveObject(String id) throws OgcException {
        T rval = objCache.get(id);
        if (rval == null) {
            String xml = retrieveStringInternal(id);
            if (xml != null) {
                rval = unmarshal(xml);
                objCache.put(id, rval);
            }
        }
        return rval;
    }

    /**
     * This must be externally synchronized
     * 
     * @param id
     * @return
     * @throws Exception
     */
    private String retrieveStringInternal(String id) throws OgcException {
        String rval = strCache.get(id);
        if (rval == null) {
            File storage = getStorageFile(id);
            if (storage.exists()) {
                InputStream in;
                try {
                    in = new FileInputStream(storage);
                } catch (FileNotFoundException e) {
                    throw new OgcException(Code.InternalServerError, e);
                }
                try {
                    rval = new java.util.Scanner(in).useDelimiter("\\A").next();
                    strCache.put(id, rval);
                } finally {
                    try {
                        in.close();
                    } catch (IOException e) {
                        throw new OgcException(Code.InternalServerError, e);
                    }
                }
            }
        }
        return rval;
    }

    /**
     * @param id
     * @return
     * @throws Exception
     */
    protected String retrieveString(String id) throws OgcException {
        KeyLock<String> lock = locker.getLock(id);
        lock.readLock();
        try {
            return retrieveStringInternal(id);
        } finally {
            lock.readUnlock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.querystore.QueryStore#remove(java.lang.String)
     */
    public void remove(String id) {
        KeyLock<String> lock = locker.getLock(id);
        lock.lock();
        try {
            objCache.remove(id);
            strCache.remove(id);
            File storage = getStorageFile(id);
            if (storage.exists()) {
                storage.delete();
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * @return
     * @throws Exception
     */
    public List<String> list() throws OgcException {
        // NOTE: this is only synched by the file system
        File[] files = storeLocation.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.isFile();
            }
        });
        List<String> rval = new ArrayList<String>(files.length);
        for (File f : files) {
            String encoded = f.getName();
            rval.add(decode(encoded));
        }
        return rval;
    }

    /**
     * @param id
     * @return file for storage
     */
    protected File getStorageFile(String id) {
        return new File(storeLocation, encode(id));
    }

    protected abstract T unmarshal(String xml) throws OgcException;

}
