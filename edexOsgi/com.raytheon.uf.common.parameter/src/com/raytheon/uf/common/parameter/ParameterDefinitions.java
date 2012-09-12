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
package com.raytheon.uf.common.parameter;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Provides utility method for accessing the set of parameters that are defined
 * in localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ParameterDefinitions {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterDefinitions.class);

    private static List<Parameter> parameters;

    public static synchronized List<Parameter> getParameters() {
        if (parameters == null) {
            parameters = new ArrayList<Parameter>();
            Unmarshaller unmarshaller = null;
            try {
                JAXBContext context = JAXBContext
                        .newInstance(ParameterList.class);
                unmarshaller = context.createUnmarshaller();
            } catch (JAXBException e) {
                statusHandler
                        .error("Error creating Context for parameter defintions, no parameter defintions will be used.",
                                e);
            }
            if (unmarshaller != null) {
                IPathManager pathMgr = PathManagerFactory.getPathManager();
                LocalizationContext commonStaticBase = pathMgr.getContext(
                        LocalizationContext.LocalizationType.COMMON_STATIC,
                        LocalizationContext.LocalizationLevel.BASE);

                LocalizationContext commonStaticSite = pathMgr.getContext(
                        LocalizationContext.LocalizationType.COMMON_STATIC,
                        LocalizationContext.LocalizationLevel.SITE);

                LocalizationFile[] files = pathMgr.listFiles(
                        new LocalizationContext[] { commonStaticSite,
                                commonStaticBase }, "parameter"
                                + IPathManager.SEPARATOR + "definition",
                        new String[] { ".xml" }, true, true);

                for (LocalizationFile file : files) {
                    if (file == null || !file.exists()
                            || file.getFile().length() < 0) {
                        continue;
                    }
                    Object obj = null;
                    try {
                        obj = unmarshaller.unmarshal(file.getFile());
                    } catch (JAXBException e) {
                        statusHandler
                                .error("Error reading parameter defintions: "
                                        + file.getName() + " has been ignored.",
                                        e);
                        continue;
                    }
                    if (obj instanceof ParameterList) {
                        ParameterList list = (ParameterList) obj;
                        if (list.getParameters() != null) {
                            parameters.addAll(list.getParameters());
                        }
                    } else {
                        statusHandler
                                .error("Error reading parameter defintions: "
                                        + file.getName() + " was a "
                                        + obj.getClass().getSimpleName());
                    }
                }
            }
        }
        return parameters;
    }

}
