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
package com.raytheon.uf.common.parameter.mapping;

import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.common.parameter.lookup.ParameterLookupException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.mapping.Mapper;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

/**
 * 
 * Provide mapping of parameter abbreviations. The base set is defined by what
 * is in the parameter database, which is initially populated from the parameter
 * definitions files. As well as providing name mapping it is also possible to
 * map from an alias to a Parameter object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------
 * Nov 02, 2012           bsteffen  Initial creation
 * Oct 04, 2016  5890     bsteffen  Fix some deprecation warnings.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class ParameterMapper extends Mapper {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterMapper.class);

    private ParameterMapper() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        // read in the namespace map
        LocalizationFile[] files = pathMgr.listStaticFiles(
                "parameter" + IPathManager.SEPARATOR + "alias",
                new String[] { ".xml" }, true, true);
        for (LocalizationFile file : files) {
            try {
                addAliasList(file.getFile());
            } catch (JAXBException e) {
                statusHandler.error("Error reading parameter aliases: "
                        + file.getPath() + " has been ignored.", e);
            }
        }
    }

    /**
     * same functionality as lookupBaseNames but maps each baseName to a
     * Parameter object.
     * 
     * @param alias
     * @param namespace
     * @return
     * @throws ParameterLookupException
     */
    public Set<Parameter> lookupParameters(String alias, String namespace)
            throws ParameterLookupException {
        Set<String> baseNames = super.lookupBaseNames(alias, namespace);
        Set<Parameter> result = new HashSet<>(
                (int) (baseNames.size() / 0.75) + 1, 0.75f);
        for (String baseName : baseNames) {
            result.add(getBaseParameter(baseName));
        }
        return result;
    }

    /**
     * same functionality as lookupBaseName but maps the baseName to a Parameter
     * Object.
     * 
     * @param alias
     * @param namespace
     * @return
     * @throws ParameterLookupException
     * @throws MultipleMappingException
     */
    public Parameter lookupParameter(String alias, String namespace)
            throws ParameterLookupException, MultipleMappingException {
        String baseName = super.lookupBaseName(alias, namespace);
        return getBaseParameter(baseName);
    }

    /**
     * get a parameter object for a baseName, no mapping is done, this is
     * provided here for convenience.
     * 
     * @param baseName
     * @return
     * @throws ParameterLookupException
     */
    public Parameter getBaseParameter(String baseName)
            throws ParameterLookupException {
        return ParameterLookup.getInstance().getParameter(baseName);
    }

    private static final ParameterMapper instance = new ParameterMapper();

    public static ParameterMapper getInstance() {
        return instance;
    }

}
