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
package com.raytheon.uf.viz.remote.graphics;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingObject<T> {

    private int objectId;

    private Dispatcher dispatcher;

    protected T wrappedObject;

    public DispatchingObject(T targetObject, Dispatcher dispatcher) {
        this.wrappedObject = targetObject;
        this.dispatcher = dispatcher;
        objectId = dispatcher.newObjectId();
    }

    public int getObjectId() {
        return objectId;
    }

    public T getWrappedObject() {
        return wrappedObject;
    }

    public Dispatcher getDispatcher() {
        return dispatcher;
    }

    public void dispatch(AbstractRemoteGraphicsEvent eventObject) {
        dispatcher.dispatch(eventObject);
    }

}
