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
package com.raytheon.uf.viz.core.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * Abstract resource class that manages frames with renderable objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 21, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractPluginDataObjectResource<T extends AbstractResourceData, D extends IDescriptor>
        extends AbstractVizResource<T, D> {

    private static class Frame {
        boolean disposed = false;

        Object lock = new Object();

        List<PluginDataObject> records = new ArrayList<PluginDataObject>();

        IRenderable renderable;
    }

    private IResourceDataChanged dataChangedListener = new IResourceDataChanged() {
        @Override
        public void resourceChanged(ChangeType type, Object object) {
            if (type == ChangeType.DATA_UPDATE) {
                if (object instanceof PluginDataObject) {
                    addDataObject((PluginDataObject) object);
                } else if (object instanceof PluginDataObject[]) {
                    addDataObject((PluginDataObject[]) object);
                } else if (object instanceof Object[]) {
                    List<PluginDataObject> pdos = new ArrayList<PluginDataObject>();
                    for (Object obj : (Object[]) object) {
                        if (obj instanceof PluginDataObject) {
                            pdos.add((PluginDataObject) obj);
                        }
                    }
                    if (pdos.size() > 0) {
                        addDataObject(pdos.toArray(new PluginDataObject[0]));
                    }
                }
            } else if (type == ChangeType.CAPABILITY) {
                if (object instanceof AbstractCapability) {
                    AbstractCapability capability = (AbstractCapability) object;
                    for (Frame frame : frames.values()) {
                        if (frame.renderable != null) {
                            capabilityChanged(frame.renderable, capability);
                        }
                    }
                }
            }
        }
    };

    private Map<DataTime, Frame> frames = new HashMap<DataTime, Frame>();

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractPluginDataObjectResource(T resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        dataTimes = new ArrayList<DataTime>();
    }

    /**
     * Adds the pdo to the appropriate time and removes any renderable or data
     * cached for that time.
     * 
     * @param pdo
     */
    protected final void addDataObject(PluginDataObject... pdos) {
        // Organize PDOs by time
        Map<DataTime, List<PluginDataObject>> pdoMap = new HashMap<DataTime, List<PluginDataObject>>();
        for (PluginDataObject pdo : pdos) {
            DataTime time = getDataObjectTime(pdo);
            List<PluginDataObject> list = pdoMap.get(time);
            if (list == null) {
                list = new ArrayList<PluginDataObject>();
                pdoMap.put(time, list);
            }
            list.add(pdo);
        }

        synchronized (this) {
            if (getStatus() == ResourceStatus.DISPOSED) {
                // Don't process if disposed
                return;
            }

            for (DataTime time : pdoMap.keySet()) {
                Frame frame = frames.get(time);
                if (frame == null) {
                    frame = new Frame();
                    frames.put(time, frame);
                }
                synchronized (frame.lock) {
                    List<PluginDataObject> pdoList = pdoMap.get(time);
                    for (PluginDataObject pdo : pdoList) {
                        if (frame.records.contains(pdo)) {
                            frame.records.remove(pdo);
                        }
                        frame.records.add(pdo);
                    }
                    if (frame.renderable != null) {
                        if (updateRenderable(frame.renderable,
                                pdoList.toArray(new PluginDataObject[0])) == false) {
                            dispose(frame.renderable);
                        }
                    }
                }

                if (!dataTimes.contains(dataTimes)) {
                    dataTimes.add(time);
                }
            }
        }
    }

    /**
     * Return the DataTime to be associated with this record. Default returns
     * PluginDataObject.getDataTime()
     * 
     * @param pdo
     * @return
     */
    protected DataTime getDataObjectTime(PluginDataObject pdo) {
        return pdo.getDataTime();
    }

    /**
     * Get the records for the given time. Empty list will be returned if no
     * frame for time
     * 
     * @param time
     * @return
     */
    protected List<PluginDataObject> getPluginDataObjects(DataTime time) {
        Frame frame = null;
        synchronized (this) {
            frame = frames.get(time);
        }
        if (frame != null) {
            synchronized (frame.lock) {
                return new ArrayList<PluginDataObject>(frame.records);
            }
        }
        return new ArrayList<PluginDataObject>();
    }

    /**
     * Get the current time for the resource, default calls
     * descriptor.getTimeForResoruce(this)
     * 
     * @return the current time
     */
    protected DataTime getTimeForResource() {
        return descriptor.getTimeForResource(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.
     * uf.common.time.DataTime)
     */
    @Override
    public final void remove(DataTime dataTime) {
        Frame frame = null;
        synchronized (this) {
            super.remove(dataTime);
            frame = frames.remove(dataTime);
        }
        if (frame != null) {
            disposeFrame(frame);
        }
    }

    /**
     * Dispose of a renderable.
     * 
     * @param renderable
     */
    private void dispose(final IRenderable renderable) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                disposeRenderable(renderable);
            }
        });
    }

    @Override
    public final void project(CoordinateReferenceSystem crs)
            throws VizException {
        Map<DataTime, Frame> frames = null;
        synchronized (this) {
            frames = new HashMap<DataTime, Frame>(this.frames);
        }

        for (Frame frame : frames.values()) {
            synchronized (frame.lock) {
                IRenderable renderable = frame.renderable;
                if (renderable != null) {
                    if (!projectRenderable(renderable, crs)) {
                        frame.renderable = null;
                        dispose(renderable);
                    }
                }
            }
        }
    }

    @Override
    protected final void initInternal(IGraphicsTarget target)
            throws VizException {
        resourceData.addChangeListener(dataChangedListener);
        initResource(target);
    }

    /**
     * Init method for the resource to initialize any data needed for rendering
     * which is not tied to a renderable.
     */
    protected void initResource(IGraphicsTarget target) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected final void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime time = paintProps.getDataTime();
        if (time == null) {
            time = getTimeForResource();
            if (time == null) {
                return;
            }
        }
        Frame currFrame = null;
        synchronized (this) {
            currFrame = frames.get(time);
        }
        if (currFrame != null) {
            synchronized (currFrame.lock) {
                if (currFrame.disposed == false) {
                    IRenderable renderable = currFrame.renderable;
                    if (renderable == null) {
                        currFrame.renderable = renderable = constructRenderable(
                                time, new ArrayList<PluginDataObject>(
                                        currFrame.records));
                    }

                    if (renderable != null) {
                        renderable.paint(target, paintProps);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected final void disposeInternal() {
        Map<DataTime, Frame> frames = null;
        synchronized (this) {
            // Copy Frames and clear member
            frames = new HashMap<DataTime, Frame>(this.frames);
            this.frames.clear();
        }

        // Dispose frame one by one
        for (Frame frame : frames.values()) {
            disposeFrame(frame);
        }

        resourceData.removeChangeListener(dataChangedListener);
        disposeResource();
    }

    /**
     * Disposes the Frame object by disposing the renderable and marking
     * disposed
     * 
     * @param frame
     */
    private void disposeFrame(Frame frame) {
        synchronized (frame.lock) {
            if (frame.renderable != null) {
                disposeRenderable(frame.renderable);
                frame.renderable = null;
            }
            frame.records.clear();
            frame.disposed = true;
        }
    }

    /**
     * Dispose method for the resource to dispose any data not tied to a
     * renderable. Called after all renderables have been disposed. Default impl
     * does nothing
     */
    protected void disposeResource() {

    }

    /**
     * Notification that a capability has changed and the renderable should be
     * updated
     * 
     * @param renderable
     * @param capability
     */
    protected abstract void capabilityChanged(IRenderable renderable,
            AbstractCapability capability);

    /**
     * Dispose the renderable object
     * 
     * @param renderable
     */
    protected abstract void disposeRenderable(IRenderable renderable);

    /**
     * Attempt to reproject the renderable object into this specified
     * projection. If unable to reproject, return false and renderable will be
     * recreated next paint
     * 
     * @param renderable
     * @param crs
     * @return
     */
    protected abstract boolean projectRenderable(IRenderable renderable,
            CoordinateReferenceSystem crs) throws VizException;

    /**
     * Construct a renderable object for the given records. This method is
     * called from paintInternal. Null can be returned and this method will be
     * called next paintInternal. That can be used if requesting data that is
     * required for the renderable asynchronously.
     * 
     * NOTE: The size of the pdo list will only grow so it can be used to
     * determine if new data has arrived since last call
     * 
     * @param time
     * @param records
     * @return
     */
    protected abstract IRenderable constructRenderable(DataTime time,
            List<PluginDataObject> records) throws VizException;

    /**
     * Update the renderable with the new pdos, if the renderable is updatable,
     * return true. If the renderable needs to be recreated from scratch, return
     * false
     * 
     * @param renderable
     * @param pdos
     * @return
     */
    protected abstract boolean updateRenderable(IRenderable renderable,
            PluginDataObject... pdos);
}
