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
package com.raytheon.uf.edex.log;

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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class EdexLogHandlerFactory extends AbstractHandlerFactory {

    private static final String CATEGORY = "EDEX";

    private static final EdexLogHandler instance = new EdexLogHandler(
            EdexLogHandler.class.getPackage().getName(), CATEGORY, CATEGORY);

    public EdexLogHandlerFactory() {
        super(CATEGORY);
        PathManagerFactory.addObserver(this);
    }

    /**
     * This is expensive so should be cached if at all possible.
     */
    @Override
    public IUFStatusHandler getInstance() {
        String clazzName = getClassName();
        return getInstance(clazzName);
    }

    private String getClassName() {
        // get the class calling getInstance
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        String clazzName = null;

        if (stack.length >= 3) {
            clazzName = stack[3].getClassName();
        } else {
            clazzName = stack[stack.length - 1].getClassName();
        }
        return clazzName;
    }

    protected FilterPatternContainer createSourceContainer() {
        final String fileName = "configuredHandlers.xml";
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);

        LocalizationFile locFile = pm.getLocalizationFile(ctx, fileName);
        if (locFile == null) {
            throw new MissingResourceException(
                    "Unable to retrieve the localization file",
                    EdexLogHandlerFactory.class.getName(),
                    LocalizationType.COMMON_STATIC.name() + File.separator
                            + LocalizationLevel.BASE.name() + File.separator
                            + "configuredHandlers.xml");
        }
        try {
            File file = locFile.getFile();
            instance.info("Loaded Filter Patterns from " + fileName);
            return new FilterPatternContainer(file);
        } catch (JAXBException e) {
            instance.handle(
                    Priority.CRITICAL,
                    "Unable to load the configured Source patterns "
                            + "file.  Using defaults which may not have "
                            + "all of the needed settings.\n"
                            + e.getLocalizedMessage(), e);
        } catch (IOException e) {
            instance.handle(
                    Priority.CRITICAL,
                    "Unable to load the configured Source patterns "
                            + "file.  Using defaults which may not have "
                            + "all of the needed settings.\n"
                            + e.getLocalizedMessage(), e);
        }
        instance.info("Created default Filter Patterns.");
        return FilterPatternContainer.createDefault();
    }

    @Override
    protected IUFStatusHandler createMonitorInstance(String pluginId,
            String monitorSource) {
        return new EdexMonitorHandler(pluginId, monitorSource);
    }

    @Override
    protected IUFStatusHandler createInstance(String pluginId, String category,
            String source) {
        return new EdexLogHandler(pluginId, category, source);
    }

    @Override
    public IUFStatusHandler createInstance(AbstractHandlerFactory factory,
            String pluginId, String category) {
        return new EdexLogHandler(factory, pluginId, category);
    }

}
