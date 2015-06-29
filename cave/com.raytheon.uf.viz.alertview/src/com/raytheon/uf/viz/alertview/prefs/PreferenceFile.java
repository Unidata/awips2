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
package com.raytheon.uf.viz.alertview.prefs;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXB;

import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.viz.alertview.AlertViewPrefStore;
import com.raytheon.uf.viz.alertview.AlertViewPrefStore.AlertViewPrefListener;

/**
 * 
 * Handles the serialization and service management of a preference file. This
 * class is designed to be a more friendly way to consume an
 * {@link AlertViewPrefStore} service. To load preferences using this class the
 * Preference class P should be JAXB serializable and must provide a no-arg
 * constructor that initializaes the preferences to a usable state. In cases
 * when loading the preferences from files fails the no-arg constructor will be
 * used to return a default preference object.
 * 
 * This class listens to changes from the {@link AlertViewPrefStore} service and
 * notifies its own listeners of changes to the specified file. It also listens
 * to changes to the available AlertViewPrefStore services and when a better
 * service becomes available it will reload the preferences from the new service
 * and notify listeners. This is especially useful during startup if the
 * prefered AlertViewPrefStore starts after other components.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 16, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @param <P>
 */
public class PreferenceFile<P> {

    private static final String FILTER = "(" + Constants.OBJECTCLASS + "="
            + AlertViewPrefStore.class.getName() + ")";

    private Logger logger = LoggerFactory.getLogger(getClass());

    private final BundleContext context;

    private final String fileName;

    private final Class<P> type;

    private final Listener<? super P> listener;

    private final AlertViewPrefListener prefStoreListener = new AlertViewPrefListener() {
        
        @Override
        public void prefFileChanged(String fileName) {
            if (fileName.equals(PreferenceFile.this.fileName)) {
                updatePreferences(loadFromStore());
            }
        }
    };
    
    private final ServiceListener serviceListener = new ServiceListener() {

        @Override
        public void serviceChanged(ServiceEvent event) {
            @SuppressWarnings("unchecked")
            ServiceReference<AlertViewPrefStore> ref = (ServiceReference<AlertViewPrefStore>) event
                    .getServiceReference();
            if (event.getType() == ServiceEvent.REGISTERED) {
                add(ref);
            } else if (event.getType() == ServiceEvent.UNREGISTERING) {
                remove(ref);
            }
        }
    };

    private ServiceReference<AlertViewPrefStore> serviceReference;

    private AlertViewPrefStore prefStore;

    private P preferences;

    public PreferenceFile(String fileName, Class<P> type,
            Listener<? super P> listener) {
        this.fileName = fileName;
        this.type = type;
        this.listener = listener;
        context = FrameworkUtil.getBundle(getClass()).getBundleContext();
        try {
            context.addServiceListener(serviceListener, FILTER);
        } catch (InvalidSyntaxException e) {
            /*
             * The FILTER is statically defined and very simple syntax so it is
             * unlikely that this will ever happen.
             */
            throw new RuntimeException(e);
        }
        this.preferences = updateServiceReference(context
                .getServiceReference(AlertViewPrefStore.class));
    }

    public void close(){
        if(serviceReference != null){
            prefStore.removeListener(prefStoreListener);
            context.ungetService(serviceReference);
        }
        context.removeServiceListener(serviceListener);
    }

    protected P updateServiceReference(
            ServiceReference<AlertViewPrefStore> serviceReference) {
        this.serviceReference = serviceReference;
        if (serviceReference != null) {
            return updatePrefStore(context.getService(serviceReference));
        } else {
            return updatePrefStore(null);
        }
    }

    protected P updatePrefStore(AlertViewPrefStore prefStore) {
        this.prefStore = prefStore;
        if (prefStore != null) {
            prefStore.addListener(prefStoreListener);
        }
        return loadFromStore();
    }

    protected P loadFromStore() {
        P preferences = null;
        if (prefStore != null) {
            try (InputStream in = prefStore.readConfigFile(fileName)) {
                if (in != null) {
                    preferences = JAXB.unmarshal(in, type);
                }
            } catch (IOException | DataBindingException e) {
                logger.error("Unable to load {} from {}", type.getSimpleName(),
                        fileName, e);
            }
        }
        if (preferences == null) {
            try {
                preferences = type.newInstance();
            } catch (InstantiationException | IllegalAccessException e) {
                /*
                 * Docs indicate you should have a public no arg constructor, if
                 * there isn't one then you get RuntimeExceptions.
                 */
                throw new RuntimeException(e);
            }
        }
        return preferences;
    }

    protected void updatePreferences(P preferences) {
        if (!this.preferences.equals(preferences)) {
            this.preferences = preferences;
            listener.update(preferences);
        }
    }


    protected void add(ServiceReference<AlertViewPrefStore> ref) {
        if (serviceReference == null || ref.compareTo(serviceReference) > 0) {
            if (serviceReference != null) {
                prefStore.removeListener(prefStoreListener);
                context.ungetService(serviceReference);
            }
            updatePreferences(updateServiceReference(ref));
        }
    }

    protected void remove(ServiceReference<AlertViewPrefStore> ref) {
        if (ref.equals(serviceReference)) {
            prefStore.removeListener(prefStoreListener);
            context.ungetService(serviceReference);
            updatePreferences(updateServiceReference(context
                    .getServiceReference(AlertViewPrefStore.class)));
        }
    }

    public P get() {
        return preferences;
    }

    public void write(P preferences) {
        if (prefStore != null) {
            try (OutputStream out = prefStore.writeConfigFile(fileName)) {
                JAXB.marshal(preferences, out);
            } catch (IOException e) {
                logger.error("Unable to write {} to {}", type.getSimpleName(),
                        fileName, e);
            }
        }
        updatePreferences(preferences);
    }

    public static interface Listener<P> {

        public void update(P preference);

    }

}
