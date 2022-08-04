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
package com.raytheon.edex.transform.shef.obs;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Metar.cfg config file reader class. Refactored out of ObsToSHEFOptions.java.
 * Allows for injection via spring
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 30, 2018  6843     mduff     Initial creation
 * Jul 10, 2019  6843     randerso  Fixed reading of metar.cfg file.
 * Jul 23, 2019  6843     randerso  Remove comment check in parseConfig().
 *                                  Ensure first 3 lines are counted but not
 *                                  processed.
 * 
 * </pre>
 *
 * @author mpduff
 */

public class MetarToShefConfigReader {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MetarToShefConfigReader.class);

    public static final String METAR_CFG = LocalizationUtil.join("shef",
            "metar.cfg");

    private static final String SAO_OUT = "SAOOUT";

    private static final String ERROR_FILE = "ERRORFILE";

    private static final String SHEF_PASS = "SHEFPASS";

    private static final String BEGIN_ALIAS = "begin_sm_alias";

    private static final String END_ALIAS = "end_sm_alias";

    private static final String BEGIN_NAMES = "begin_names";

    private static final String END_NAMES = "end_names";

    private static final String BEGIN_PC_RESET = "begin_pc_reset";

    private static final String END_PC_RESET = "end_pc_reset";

    private static final Pattern CMD_DIRECTIVES = Pattern
            .compile("^\\.((?:begin|end)_(?:names|sm_alias|pc_reset))$");

    private static final Pattern CFG_ITEMS = Pattern
            .compile("^([+-])(SAOOUT|ERRORFILE|SHEFPASS)$");

    private Properties optAlias = new Properties();

    private Set<String> optNames = new HashSet<>();

    private Properties optPCReset = new Properties();

    private Set<String> optPE = new HashSet<>();

    public void readConfig(String configFile) throws IOException {

        IPathManager manager = PathManagerFactory.getPathManager();
        if (manager != null) {
            ILocalizationFile cfgFile = manager
                    .getStaticLocalizationFile(configFile);
            if (cfgFile == null) {
                // Look for default file
                cfgFile = manager.getStaticLocalizationFile(METAR_CFG);
                if (cfgFile == null) {
                    throw new FileNotFoundException(
                            METAR_CFG + " does not exist");
                }
            }

            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(cfgFile.openInputStream()))) {
                parseConfig(reader.lines().iterator());
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Error accessing file " + METAR_CFG, e);
                throw new IOException(METAR_CFG + " does not exist");
            }
        } else {
            statusHandler.error("Could not create PathManagerFactory");
        }
    }

    public void parseConfig(Iterator<String> lines) {
        boolean saoOut = false;
        boolean errorFile = false;
        boolean shefPass = false;
        int lineCount = 0;
        optAlias.clear();
        optNames.clear();
        optPCReset.clear();
        boolean readAlias = false;
        boolean readNames = false;
        boolean readReset = false;
        while (lines.hasNext()) {
            String line = lines.next();
            lineCount++;
            if (lineCount < 4) {
                /*
                 * the first 3 lines are not used by A2 and are ignored for
                 * backward compatibility with A1
                 */
                continue;
            }

            Matcher m = CFG_ITEMS.matcher(line.trim());
            if (m.matches()) {
                boolean val = ("+".equals(m.group(1)));
                if (SAO_OUT.equals(m.group(2)) && lineCount == 4) {
                    saoOut = val;
                } else if (ERROR_FILE.equals(m.group(2)) && lineCount == 5) {
                    errorFile = val;
                } else if (SHEF_PASS.equals(m.group(2)) && lineCount == 6) {
                    shefPass = val;
                }
            } else if (lineCount == 7) {
                // Check for PE List
                // TA UP SD UD US TX TN PPT PPQ PPH PPD PA TAIRZR TAIRZH
                // TAIRZP
                // TAIRZY
                StringTokenizer st = new StringTokenizer(line, " ");
                optPE.clear();
                while (st.hasMoreTokens()) {
                    optPE.add(st.nextToken());
                }
            } else {
                m = CMD_DIRECTIVES.matcher(line.trim());
                if (m.matches()) {
                    String s = m.group(1);
                    if (BEGIN_ALIAS.equals(s)) {
                        readAlias = true;
                    } else if (END_ALIAS.equals(s)) {
                        readAlias = false;
                    } else if (BEGIN_NAMES.equals(s)) {
                        readNames = true;
                    } else if (END_NAMES.equals(s)) {
                        readNames = false;
                    } else if (BEGIN_PC_RESET.equals(s)) {
                        readReset = true;
                    } else if (END_PC_RESET.equals(s)) {
                        readReset = false;
                    }
                } else {
                    if (readNames) {
                        optNames.add(line.trim());
                    } else if (readAlias) {
                        String[] parts = line.trim().split("\\s+");
                        optAlias.setProperty(parts[0], parts[1]);
                    } else if (readReset) {
                        String[] parts = line.trim().split("\\s+");
                        optPCReset.setProperty(parts[0], parts[1]);
                    }
                }
            }
        }
    }

    public Properties getOptAlias() {
        return optAlias;
    }

    public Set<String> getOptNames() {
        return optNames;
    }

    public Properties getOptPCReset() {
        return optPCReset;
    }

    public Set<String> getOptPE() {
        return optPE;
    }
}
