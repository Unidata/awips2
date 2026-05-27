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
package com.raytheon.viz.texteditor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Class with common methods and variables for all browser models.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2016  4716      rferrel     Initial creation
 * Nov 02, 2016  5092      rferrel     Refactor ccc help to this class.
 * Nov 08, 2016  5975      rferrel     Add CONFIGURED to lcArray and observer listener.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public abstract class AbstractBrowserModel
        implements ILocalizationPathObserver {

    protected static final String COMMENT_DELIM = "#";

    protected static final String DIR = "textdb";

    protected static final String NNN_HELP = "textdb/textNNNhelp.txt";

    protected static final String CCC_HELP = "textdb/textCCChelp.txt";

    protected static final String CATEGORY_CLASS = "textdb/textCategoryClass.txt";

    protected static final Pattern SPACES_PATTERN = Pattern.compile("\\s+");

    protected final transient IUFStatusHandler statusHandler;

    /**
     * Used for looking up nnn Help text, no need to be sorted.
     */
    protected Map<String, String> nnnHelp;

    /**
     * Used for looking up ccc Help text, no need to be sorted.
     */
    protected Map<String, String> cccHelp;

    protected String localSite;

    protected IPathManager pathManager;

    protected LocalizationContext[] lcArray;

    /**
     * Only used by the subclasses which should be singletons.
     */
    protected AbstractBrowserModel() {
        this.statusHandler = UFStatus.getHandler(this.getClass());
        this.nnnHelp = new HashMap<>();
        this.cccHelp = new HashMap<>();
        PathManagerFactory.getPathManager().addLocalizationPathObserver(DIR,
                this);
    }

    /**
     * Null localization array and other variables no longer needed after setup
     * is finished. It is up to the subclass to call this method.
     */
    protected void cleanup() {
        pathManager = null;
        lcArray = null;
    }

    /**
     * This sets up the localization array in order files should be read and
     * other common variables. It is up to the subclass to call this method.
     */
    protected void setup() {
        localSite = LocalizationManager.getInstance().getCurrentSite();
        pathManager = PathManagerFactory.getPathManager();
        lcArray = new LocalizationContext[4];
        lcArray[0] = pathManager.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);
        lcArray[1] = pathManager.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.CONFIGURED);
        lcArray[2] = pathManager.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        lcArray[3] = pathManager.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);

        for (LocalizationContext lc : lcArray) {
            parseHelp(pathManager.getLocalizationFile(lc, CCC_HELP), cccHelp);
            parseHelp(pathManager.getLocalizationFile(lc, NNN_HELP), nnnHelp);
        }
    }

    /**
     * This parses a file and for each non-commented line takes the first
     * whitespace separated word on a line as a key to place the rest of the
     * line in the result map.
     * 
     * @param fileToParse
     *            file to parse
     * @param valuesMap
     *            Map to place results
     */
    protected void parseStringValues(ILocalizationFile fileToParse,
            Map<String, String> valuesMap) {
        if (fileToParse != null && fileToParse.exists()) {
            try (InputStream in = fileToParse.openInputStream();
                    BufferedReader br = new BufferedReader(
                            new InputStreamReader(in));) {
                String line = null;
                while ((line = br.readLine()) != null) {
                    // skip comments.
                    if (line.startsWith(COMMENT_DELIM)) {
                        continue;
                    }

                    String[] tokens = SPACES_PATTERN.split(line, 2);

                    // ensure there is a category
                    if (tokens.length > 1) {
                        valuesMap.put(tokens[0], tokens[1]);
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred parsing file [" + fileToParse.getPath()
                                + "]",
                        e);
            }
        }
    }

    /**
     * This parses a help file. Any non-commented line the first 3 characters
     * are the key to use to place the rest of line in the help map.
     * 
     * @param fileToParse
     *            - File to parse
     * @param helpMap
     *            - Map to place results
     */
    protected void parseHelp(ILocalizationFile fileToParse,
            Map<String, String> helpMap) {

        if (fileToParse != null && fileToParse.exists()) {
            try (InputStream in = fileToParse.openInputStream();
                    BufferedReader br = new BufferedReader(
                            new InputStreamReader(in));) {
                if (fileToParse.exists()) {
                    String line = null;
                    while ((line = br.readLine()) != null) {
                        // skip comments.
                        if (line.startsWith(COMMENT_DELIM)) {
                            continue;
                        }

                        if (line.length() > 3) {
                            // Get the id
                            String s = line.substring(0, 3);
                            helpMap.put(s, line.substring(4));
                        }
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred parsing file [" + fileToParse.getPath()
                                + "]",
                        e);
            }
        }
    }

    /**
     * Get the help text for a single category.
     * 
     * @param category
     * @return
     */
    public String getCategoryHelp(String category) {
        return nnnHelp.get(category);
    }

    /**
     * 
     * @return localSite
     */
    public String getLocalSite() {
        return localSite;
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        setup();
        cleanup();
    }

}
