/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.db;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.ReadLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.WriteLock;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.edex.ogc.common.AbstractFsStore;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;

/**
 * Simple Layer store using file system
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FsLayerStore extends AbstractFsStore implements ILayerStore {

    private final DynamicSerializationManager serializer = DynamicSerializationManager
            .getManager(SerializationType.Thrift);

    private final Map<Class<?>, ReentrantReadWriteLock> _lockMap = new HashMap<Class<?>, ReentrantReadWriteLock>();

    /**
     * @param storeLocation
     */
    public FsLayerStore(File storeLocation) {
        super(storeLocation);
    }

    /**
     * @param storeName
     *            name of store relative to edex static base
     */
    public FsLayerStore(String storeName) {
        this(findStore(storeName));
    }

    /**
     * Get lock for class
     * 
     * @param c
     * @return
     */
    private ReentrantReadWriteLock getLock(Class<?> c) {
        synchronized (_lockMap) {
            ReentrantReadWriteLock rval = _lockMap.get(c);
            if (rval == null) {
                rval = new ReentrantReadWriteLock();
                _lockMap.put(c, rval);
            }
            return rval;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.ogc.common.db.LayerStore#get(java.lang.String,
     * java.lang.Class)
     */
    @Override
    public <D extends SimpleDimension, L extends SimpleLayer<D>> L get(
            String id, Class<L> c) throws OgcException {
        ReentrantReadWriteLock lock = getLock(c);
        ReadLock readLock = lock.readLock();
        readLock.lock();
        try {
            File layerFile = getLayerFileRead(id, c);
            if (!layerFile.exists()) {
                return null;
            }
            L rval = deserialize(layerFile, c);
            return rval;
        } finally {
            readLock.unlock();
        }
    }

    @SuppressWarnings("unchecked")
    private <D extends SimpleDimension, L extends SimpleLayer<D>> L deserialize(
            File f, Class<L> c) throws OgcException {
        try {
            return (L) serializer.deserialize(new FileInputStream(f));
        } catch (Exception e) {
            throw new OgcException(Code.InternalServerError, e);
        }
    }

    /**
     * Get store directory for class, read only
     * 
     * must be synchronized externally
     * 
     * @param c
     * @return file object that isn't guaranteed to exist
     * @throws OgcException
     */
    private File getClassDirRead(Class<?> c) throws OgcException {
        File rval = new File(storeLocation, c.getName());
        if (!rval.exists()) {
            return rval;
        }
        if (!rval.isDirectory()) {
            throw new OgcException(Code.InternalServerError,
                    rval.getAbsolutePath() + " is not a directory");
        }
        if (!rval.canRead()) {
            throw new OgcException(Code.InternalServerError,
                    "Unable to read store directory: " + rval.getAbsolutePath());
        }
        return rval;
    }

    /**
     * Get store directory for class for modification, creates if it doesn't
     * exist
     * 
     * must be synchronized externally
     * 
     * @param c
     * @param create
     *            true if method should create if non existent
     * @return file object that is only guaranteed to exist if create is true
     * @throws OgcException
     */
    private File getClassDirWrite(Class<?> c, boolean create)
            throws OgcException {
        File rval = new File(storeLocation, c.getName());
        if (!rval.exists()) {
            if (!create) {
                return rval;
            }
            if (!rval.mkdir()) {
                throw new OgcException(Code.InternalServerError,
                        "Unable to create store directory: "
                                + rval.getAbsolutePath());
            }
        }
        if (!rval.isDirectory()) {
            throw new OgcException(Code.InternalServerError,
                    rval.getAbsolutePath() + " is not a directory");
        }
        if (!rval.canWrite()) {
            throw new OgcException(Code.InternalServerError,
                    "Unable to write to store directory: "
                            + rval.getAbsolutePath());
        }
        return rval;
    }

    /**
     * Create file object for layer file for reading
     * 
     * must be synchronized externally
     * 
     * @param id
     * @param c
     * @return a file object whose path isn't guaranteed to exist
     * @throws OgcException
     */
    private File getLayerFileRead(String id, Class<?> c) throws OgcException {
        return new File(getClassDirRead(c), encode(id));
    }

    /**
     * Create file object for layer file for writing
     * 
     * must be synchronized externally
     * 
     * @param id
     * @param c
     * @param createPath
     *            if true, path to file will be created if nonexistent
     * @return
     * @throws OgcException
     */
    private File getLayerFileWrite(String id, Class<?> c, boolean createPath)
            throws OgcException {
        return new File(getClassDirWrite(c, createPath), encode(id));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerStore#getAll(java.lang.Class)
     */
    @Override
    public <D extends SimpleDimension, L extends SimpleLayer<D>> List<L> getAll(
            Class<L> c) throws OgcException {
        ReentrantReadWriteLock lock = getLock(c);
        ReadLock readLock = lock.readLock();
        readLock.lock();
        try {
            File classDir = getClassDirRead(c);
            if (!classDir.exists()) {
                return new ArrayList<L>(0);
            }
            File[] listFiles = classDir.listFiles(new FileFilter() {
                @Override
                public boolean accept(File f) {
                    return f.isFile();
                }
            });
            List<L> rval = new ArrayList<L>(listFiles.length);
            for (File f : listFiles) {
                rval.add(deserialize(f, c));
            }
            return rval;
        } finally {
            readLock.unlock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerStore#createOrUpdate(com.raytheon
     * .uf.edex.ogc.common.db.SimpleLayer)
     */
    @Override
    public void createOrUpdate(SimpleLayer<? extends SimpleDimension> l)
            throws OgcException {
        Class<?> c = l.getClass();
        ReentrantReadWriteLock lock = getLock(c);
        WriteLock writeLock = lock.writeLock();
        writeLock.lock();
        try {
            serialize(l, getLayerFileWrite(l.getIdentifier(), c, true));
        } finally {
            writeLock.unlock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerStore#createOrUpdate(java.util
     * .List)
     */
    @Override
    public void createOrUpdate(
            List<SimpleLayer<? extends SimpleDimension>> layers)
            throws OgcException {
        Map<Class<?>, List<SimpleLayer<?>>> map = new HashMap<Class<?>, List<SimpleLayer<?>>>();
        for (SimpleLayer<?> l : layers) {
            List<SimpleLayer<?>> list = map.get(l.getClass());
            if (list == null) {
                list = new ArrayList<SimpleLayer<?>>();
                map.put(l.getClass(), list);
            }
            list.add(l);
        }
        for (Entry<Class<?>, List<SimpleLayer<?>>> e : map.entrySet()) {
            Class<?> c = e.getKey();
            ReentrantReadWriteLock lock = getLock(c);
            WriteLock writeLock = lock.writeLock();
            writeLock.lock();
            try {
                for (SimpleLayer<?> l : e.getValue()) {
                    serialize(l, getLayerFileWrite(l.getIdentifier(), c, true));
                }
            } finally {
                writeLock.unlock();
            }
        }
    }

    /**
     * Store layer
     * 
     * @param l
     * @param f
     * @throws OgcException
     */
    private void serialize(SimpleLayer<?> l, File f) throws OgcException {
        try {
            serializer.serialize(l, new FileOutputStream(f));
        } catch (Exception e) {
            throw new OgcException(Code.InternalServerError, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerStore#delete(java.lang.String,
     * java.lang.Class)
     */
    @Override
    public void delete(String id, Class<? extends SimpleLayer<?>> c)
            throws OgcException {
        File layerFile = getLayerFileWrite(id, c, false);
        if (!layerFile.exists()) {
            return;
        }
        ReentrantReadWriteLock lock = getLock(c);
        WriteLock writeLock = lock.writeLock();
        writeLock.lock();
        try {
            if (layerFile.exists()) {
                layerFile.delete();
            }
        } finally {
            writeLock.unlock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.db.LayerStore#deleteAll(java.lang.Class)
     */
    @Override
    public void deleteAll(Class<? extends SimpleLayer<?>> c)
            throws OgcException {
        ReentrantReadWriteLock lock = getLock(c);
        WriteLock writeLock = lock.writeLock();
        writeLock.lock();
        try {
            File classDir = getClassDirWrite(c, false);
            if (!classDir.exists()) {
                return;
            }
            for (File f : classDir.listFiles()) {
                if (f.isFile()) {
                    f.delete();
                }
            }
        } finally {
            writeLock.unlock();
        }
    }

}
