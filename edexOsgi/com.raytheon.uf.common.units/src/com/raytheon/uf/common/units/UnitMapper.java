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
package com.raytheon.uf.common.units;

import java.util.HashSet;
import java.util.Set;

import javax.measure.unit.Unit;
import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.mapping.Mapper;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

/**
 * Provide mapping of textual unit representations. The base set is defined by
 * javax.measure and UCUM. As well as providing name mapping it is also possible
 * to map from an alias to a Unit object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 02, 2014 2906       bclement    Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class UnitMapper extends Mapper {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(UnitMapper.class);

    private UnitMapper() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        // read in the namespace map
        LocalizationFile[] files = pathMgr.listStaticFiles("unit"
                + IPathManager.SEPARATOR + "alias", new String[] { ".xml" },
                true, true);
        for (LocalizationFile file : files) {
            try {
                addAliasList(file.getFile());
            } catch (JAXBException e) {
                statusHandler.error(
                        "Error reading unit aliases: " + file.getName()
                                + " has been ignored.", e);
            }
        }
    }

    /**
     * same functionality as lookupBaseNames but maps each baseName to a Unit
     * object.
     * 
     * @param alias
     * @param namespace
     * @return
     * @throws UnitLookupException
     */
    public Set<Unit<?>> lookupUnits(String alias, String namespace)
            throws UnitLookupException {
        Set<String> baseNames = super.lookupBaseNames(alias, namespace);
        Set<Unit<?>> result = new HashSet<Unit<?>>(
                (int) (baseNames.size() / 0.75) + 1, 0.75f);
        for (String baseName : baseNames) {
            result.add(getBaseUnit(baseName));
        }
        return result;
    }

    /**
     * Return all base units that are compatible with targetUnit that have alias
     * 
     * @param alias
     * @param namespace
     * @param targetUnit
     * @return empty set if none are found
     * @throws UnitLookupException
     */
    public Set<Unit<?>> lookupCompatibleUnits(String alias, String namespace,
            Unit<?> targetUnit) throws UnitLookupException {
        Set<String> baseNames = super.lookupBaseNames(alias, namespace);
        Set<Unit<?>> rval = new HashSet<Unit<?>>(baseNames.size());
        for (String baseName : baseNames) {
            Unit<?> baseUnit = getBaseUnit(baseName);
            if (baseUnit.isCompatible(targetUnit)) {
                rval.add(baseUnit);
            }
        }
        return rval;
    }

    /**
     * same functionality as lookupBaseName but maps the baseName to a Parameter
     * Object.
     * 
     * @param alias
     * @param namespace
     * @return
     * @throws MultipleMappingException
     * @throws UnitLookupException
     */
    public Unit<?> lookupUnit(String alias, String namespace)
            throws MultipleMappingException, UnitLookupException {
        String baseName = super.lookupBaseName(alias, namespace);
        return getBaseUnit(baseName);
    }

    /**
     * get a Unit object for a baseName, no mapping is done, this is provided
     * here for convenience.
     * 
     * @param baseName
     * @return
     * @throws UnitLookupException
     */
    public Unit<?> getBaseUnit(String baseName) throws UnitLookupException {
        try {
            return Unit.valueOf(baseName);
        } catch (Exception e) {
            throw new UnitLookupException("Unable to convert to base units: "
                    + baseName, e);
        }
    }

    private static final UnitMapper instance = new UnitMapper();

    public static UnitMapper getInstance() {
        return instance;
    }

}
