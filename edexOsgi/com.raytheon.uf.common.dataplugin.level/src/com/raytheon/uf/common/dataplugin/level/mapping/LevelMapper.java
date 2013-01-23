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
package com.raytheon.uf.common.dataplugin.level.mapping;

import java.util.HashSet;
import java.util.Set;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.mapping.Mapper;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

/**
 * Provide mapping of master level names. The base set is defined by what is in
 * the masterlevel database, which is initially populated from masterLevels. As
 * well as providing name mapping it is also possible to map level objects with
 * unit conversion.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class LevelMapper extends Mapper {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LevelMapper.class);

    private transient LevelFactory factory = LevelFactory.getInstance();

    private LevelMapper() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        // read in the namespace map
        LocalizationFile[] files = pathMgr.listStaticFiles("level"
                + IPathManager.SEPARATOR + "alias", new String[] { ".xml" },
                true, true);
        for (LocalizationFile file : files) {
            try {
                addAliasList(file.getFile());
            } catch (JAXBException e) {
                statusHandler.error(
                        "Error reading level aliases: " + file.getName()
                                + " has been ignored.", e);
            }
        }
    }

    /**
     * same functionality as lookupBaseNames but also maps those baseNames to
     * MasterLevel objects.
     * 
     * @param alias
     * @param namespace
     * @return
     * @throws CommunicationException
     */
    public Set<MasterLevel> lookupMasterLevels(String alias, String namespace)
            throws CommunicationException {
        Set<String> baseNames = super.lookupBaseNames(alias, namespace);
        Set<MasterLevel> result = new HashSet<MasterLevel>(
                (int) (baseNames.size() / 0.75) + 1, 0.75f);
        for (String baseName : baseNames) {
            result.add(factory.getMasterLevel(baseName));
        }
        return result;
    }

    /**
     * same functionality as lookupBaseName but also maps the baseName to a
     * MasterLevel object.
     * 
     * @param alias
     * @param namespace
     * @return
     */
    public MasterLevel lookupMasterLevel(String alias, String namespace)
            throws MultipleMappingException, CommunicationException {
        String baseName = super.lookupBaseName(alias, namespace);
        return factory.getMasterLevel(baseName);
    }

    public Level lookupLevel(String masterLevelAlias, String namespace,
            double levelone, double leveltwo, Unit<?> unit)
            throws MultipleLevelMappingException, CommunicationException {
        return lookupLevel(masterLevelAlias, namespace, levelone, leveltwo,
                UnitFormat.getUCUMInstance().format(unit));
    }

    public Level lookupLevel(String masterLevelAlias, String namespace,
            double levelone, Unit<?> unit)
            throws MultipleLevelMappingException, CommunicationException {
        return lookupLevel(masterLevelAlias, namespace, levelone, UnitFormat
                .getUCUMInstance().format(unit));
    }

    public Level lookupLevel(String masterLevelAlias, String namespace,
            double levelone, String unit) throws MultipleLevelMappingException,
            CommunicationException {
        return lookupLevel(masterLevelAlias, namespace, levelone,
                Level.INVALID_VALUE, unit);
    }

    public Level lookupLevel(String masterLevelAlias, String namespace,
            double levelone, double leveltwo, String unit)
            throws MultipleLevelMappingException, CommunicationException {
        Set<Level> levels = lookupLevels(masterLevelAlias, namespace, levelone,
                leveltwo, unit);
        if (levels.size() == 1) {
            return levels.iterator().next();
        } else {
            throw new MultipleLevelMappingException(masterLevelAlias,
                    namespace, lookupBaseNames(masterLevelAlias, namespace),
                    levels);
        }
    }

    public Set<Level> lookupLevels(String masterLevelAlias, String namespace,
            double levelone, double leveltwo, Unit<?> unit)
            throws CommunicationException {
        return lookupLevels(masterLevelAlias, namespace, levelone, leveltwo,
                UnitFormat.getUCUMInstance().format(unit));
    }

    public Set<Level> lookupLevels(String masterLevelAlias, String namespace,
            double levelone, Unit<?> unit) throws CommunicationException {
        return lookupLevels(masterLevelAlias, namespace, levelone, UnitFormat
                .getUCUMInstance().format(unit));
    }

    public Set<Level> lookupLevels(String masterLevelAlias, String namespace,
            double levelone, String unit) throws CommunicationException {
        return lookupLevels(masterLevelAlias, namespace, levelone,
                Level.INVALID_VALUE, unit);
    }

    public Set<Level> lookupLevels(String masterLevelAlias, String namespace,
            double levelone, double leveltwo, String unit)
            throws CommunicationException {
        Set<String> baseNames = super.lookupBaseNames(masterLevelAlias,
                namespace);
        Set<Level> result = new HashSet<Level>(
                (int) (baseNames.size() / 0.75) + 1, 0.75f);
        for (String baseName : baseNames) {
            result.add(factory.getLevel(baseName, levelone, leveltwo, unit));
        }
        return result;
    }

    private static final LevelMapper instance = new LevelMapper();

    public static LevelMapper getInstance() {
        return instance;
    }

}
