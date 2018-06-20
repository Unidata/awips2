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
package com.raytheon.uf.common.parameter.localization;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Provides utility for looking up parameters defined in localization files. The
 * canonical representation of a parameter is defined in the database and in
 * general the {@link ParameterLookup} should be used to find parameter
 * definitions rather than using this class directly.
 * 
 * Upon creation instances of this class will register a localization listener
 * so that they are constantly synchronized with the current state of
 * localization. The {@link #close()} method must be called if this class is no
 * longer in use so that the listener can be removed.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Oct 04, 2016  5890     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class ParameterLocalizationLookup implements AutoCloseable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterLookup.class);

    private static final String LOCALIZATION_PATH = "parameter"
            + IPathManager.SEPARATOR + "definition";

    private final IPathManager pathManager;

    private Map<String, Parameter> abbrevToParam;

    private final Set<ParameterLocalizationListener> listeners = new CopyOnWriteArraySet<>();

    private final ILocalizationPathObserver observer = new ILocalizationPathObserver() {

        @Override
        public void fileChanged(ILocalizationFile file) {
            reread();
        }
    };

    public ParameterLocalizationLookup(){
        this(PathManagerFactory.getPathManager());
    }

    public ParameterLocalizationLookup(IPathManager pathManager) {
        this.pathManager = pathManager;
        this.abbrevToParam = Collections
                .synchronizedMap(new HashMap<String, Parameter>());
        pathManager.addLocalizationPathObserver(LOCALIZATION_PATH, observer);
        reread();
    }

    protected void reread() {
        Map<String, Parameter> abbrevToParam = new HashMap<>();
        SingleTypeJAXBManager<ParameterList> jaxbManager = null;
        try {
            jaxbManager = new SingleTypeJAXBManager<>(ParameterList.class);
        } catch (JAXBException e) {
            statusHandler.error(
                    "Error creating Context for parameter defintions, no parameter defintions will be used.",
                    e);

        }
        if (jaxbManager != null) {
            LocalizationFile[] files = pathManager.listStaticFiles(
                    LOCALIZATION_PATH, new String[] { ".xml" }, true, true);

            for (LocalizationFile file : files) {
                if (file == null || !file.exists()) {
                    continue;
                }
                ParameterList list = null;
                try (InputStream is = file.openInputStream()) {
                    list = jaxbManager.unmarshalFromInputStream(is);
                } catch (LocalizationException | SerializationException
                        | IOException e) {
                    statusHandler.error("Error reading parameter defintions: "
                            + file.getPath() + " has been ignored.", e);
                    continue;
                }
                if (list.getParameters() != null) {
                    for (Parameter p : list.getParameters()) {
                        abbrevToParam.put(p.getAbbreviation(), p);
                    }
                }

            }
        }
        this.abbrevToParam = Collections.unmodifiableMap(abbrevToParam);
        for (ParameterLocalizationListener listener : listeners) {
            listener.parametersChanged();
        }
    }

    public Parameter getParameter(Parameter parameter) {
        return getParameter(parameter.getAbbreviation());
    }

    public Parameter getParameter(String abbreviation) {
        return abbrevToParam.get(abbreviation);
    }

    public Collection<Parameter> getAllParameters() {
        return new ArrayList<>(abbrevToParam.values());
    }

    public void addListener(ParameterLocalizationListener listener) {
        listeners.add(listener);
    }

    public void removeListener(ParameterLocalizationListener listener) {
        listeners.remove(listener);
    }

    @Override
    public void close() {
        pathManager.removeLocalizationPathObserver(LOCALIZATION_PATH, observer);
    }

    public static interface ParameterLocalizationListener {
        public void parametersChanged();
    }
}
