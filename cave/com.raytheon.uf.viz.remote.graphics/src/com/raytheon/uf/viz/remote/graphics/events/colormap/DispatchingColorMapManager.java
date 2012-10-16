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
package com.raytheon.uf.viz.remote.graphics.events.colormap;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;

/**
 * Manager class for color maps
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingColorMapManager {

    private static final int MAX_COLOR_MAPS = 10;

    private static Map<Integer, DispatchingColorMapManager> instanceMap = new HashMap<Integer, DispatchingColorMapManager>();

    public static synchronized DispatchingColorMapManager getInstance(
            Dispatcher dispatcher) {
        DispatchingColorMapManager manager = instanceMap.get(dispatcher
                .getDispatcherId());
        if (manager == null) {
            manager = new DispatchingColorMapManager(dispatcher);
            instanceMap.put(dispatcher.getDispatcherId(), manager);
        }
        return manager;
    }

    private Dispatcher dispatcher;

    private Map<String, DispatchingObject<Object>> colorMaps = new LinkedHashMap<String, DispatchingObject<Object>>() {

        private static final long serialVersionUID = 1L;

        /*
         * (non-Javadoc)
         * 
         * @see java.util.LinkedHashMap#removeEldestEntry(java.util.Map.Entry)
         */
        @Override
        protected boolean removeEldestEntry(
                Entry<String, DispatchingObject<Object>> eldest) {
            boolean rval = false;
            if (size() > MAX_COLOR_MAPS) {
                rval = true;
                dispatcher.dispatch(RemoteGraphicsEventFactory.createEvent(
                        DisposeObjectEvent.class, eldest.getValue()));
            }
            return rval;
        }

    };

    // TODO: Also keep track of ColorMapParameters? So a CMImage would have an
    // id to parameters and an id to colormap? or would the parameters have an
    // id to a colormap?

    private DispatchingColorMapManager(Dispatcher dispatcher) {
        this.dispatcher = dispatcher;
    }

    public int getColorMapId(ColorMapParameters parameters) {
        IColorMap cmap = parameters.getColorMap();
        String name = cmap.getName();
        if (name == null) {
            name = ColorMapParameters.class.toString() + "@"
                    + parameters.hashCode();
        }
        DispatchingObject<Object> object = colorMaps.get(name);
        if (object == null) {
            object = new DispatchingObject<Object>(null, dispatcher);
            CreateColorMapEvent event = RemoteGraphicsEventFactory.createEvent(
                    CreateColorMapEvent.class, object);
            event.setColorMap(cmap);
            dispatcher.dispatch(event);
            colorMaps.put(name, object);
        }
        return object.getObjectId();
    }
}
