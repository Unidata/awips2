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
package com.raytheon.uf.viz.core.status;

import java.io.File;
import java.io.IOException;
import java.util.MissingResourceException;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.AbstractHandlerFactory;
import com.raytheon.uf.common.status.FilterPatternContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Viz Status Handler Factory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class VizStatusHandlerFactory extends AbstractHandlerFactory {

    private static final String CATEGORY = "WORKSTATION";

    private static final VizStatusHandler instance = new VizStatusHandler(
            VizStatusHandler.class.getPackage().getName(), CATEGORY, CATEGORY);

    public VizStatusHandlerFactory() {
        super(CATEGORY);
        PathManagerFactory.addObserver(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.status.IUFStatusHandlerFactory#getInstance()
     */
    @Override
    public IUFStatusHandler getInstance() {
        return instance;
    }

    @Override
    public IUFStatusHandler createInstance(String pluginId, String category,
            String source) {
        return new VizStatusHandler(pluginId, category, source);
    }

    @Override
    public IUFStatusHandler createInstance(AbstractHandlerFactory factory,
            String pluginId, String category) {
        return new VizStatusHandler(factory, pluginId, category);
    }

    @Override
    public IUFStatusHandler createMonitorInstance(String pluginId,
            String monitorSource) {
        return new VizMonitorHandler(pluginId, monitorSource);
    }

    @Override
    protected FilterPatternContainer createSourceContainer() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);

        LocalizationFile locFile = pm.getLocalizationFile(ctx,
                "configuredHandlers.xml");
        if (locFile == null) {
            throw new MissingResourceException(
                    "Unable to retrieve the localization file",
                    VizStatusHandler.class.getName(),
                    LocalizationType.COMMON_STATIC.name() + File.separator
                            + LocalizationLevel.BASE.name() + File.separator
                            + "configuredHandlers.xml");
        }
        try {
            File file = locFile.getFile();
            return new FilterPatternContainer(file);
        } catch (JAXBException e) {
            instance.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        } catch (IOException e) {
            instance.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }
        return FilterPatternContainer.createDefault();
    }
}
