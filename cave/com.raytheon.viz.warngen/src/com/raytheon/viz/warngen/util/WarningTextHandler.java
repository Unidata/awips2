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
package com.raytheon.viz.warngen.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.util.FileUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.TextWarningConstants;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;
import com.raytheon.viz.warngen.gis.AffectedAreas;
import com.raytheon.viz.warngen.gis.AffectedAreasComparator;

/**
 * Adds lock tags and wraps the text
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2010            jsanchez     Initial creation
 * Aug 29, 2011 10719      rferrel     applyLocks no longer allows removal of
 *                                     required blank lines.
 * May 10, 2012 14681     Qinglu Lin   Updated regex string for Pattern listOfAreaNamePtrn, etc.
 * May 30, 2012 14749     Qinglu Lin   Handled CAN in a CANCON specifically.
 * Jun  6, 2012 14749     Qinglu Lin   Added code to lock "...THE" in "...THE CITY OF", etc. 
 *                                     (David's concise approach was used. A quicker but 
 *                                     lengthy code snippet is available) and to resolve issue with 
 *                                     empty areaNotation and areasNotation which occurs when,
 *                                     for example, District of Columbia is followed by a county.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class WarningTextHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WarningTextHandler.class);

    /** Maximum width of a warning */
    private static final int MAX_WIDTH = 69;

    private static final String LOCK_START = TextWarningConstants.BEGIN_ELEMENT_TAG;

    private static final String LOCK_END = TextWarningConstants.END_ELEMENT_TAG;

    private static final String TEST_MSG1 = "...THIS MESSAGE IS FOR TEST PURPOSES ONLY...";

    private static final String TEST_MSG2 = "THIS IS A TEST MESSAGE.";

    private static final String TEST_MSG3 = "THIS IS A TEST MESSAGE. DO NOT TAKE ACTION BASED ON THIS MESSAGE.";

    /** Pattern for wrapping a long line */
    private static final Pattern wrapRE = Pattern
            .compile("(\\S\\S&&([^\\.][^\\.][^\\.]){" + (MAX_WIDTH - 3)
                    + ",}|.{1," + (MAX_WIDTH - 3) + "})(" + LOCK_START + "|"
                    + LOCK_END + "|-|\\s+|(\\.\\.\\.)|$)");

    /** Strictly wraps only after hyphens */
    private static final Pattern hyphenWrapRE = Pattern.compile("(.{1,"
            + (MAX_WIDTH - 1) + "}-)");

    private static final Pattern datePtrn = Pattern
            .compile("(\\d{1,2})(\\d{2})\\s(AM|PM)\\s(\\w{3,4})\\s\\w{3}\\s(\\w{3})\\s{1,}(\\d{1,2})\\s(\\d{4})");

    /** The UGC line (ex. NEC003-008-010-110325) */
    public static final Pattern ugcPtrn = Pattern
            .compile("((\\w{2}[CZ](\\d{3}-){1,}){1,})|(\\d{3}-){1,}");

    /** The VTEC line (ex. /O.NEW.KOAX.TO.W.0001.110715T1722Z-110715T1730Z) */
    private static final Pattern vtecPtrn = Pattern
            .compile("/[OTEX]\\.([A-Z]{3})\\.[A-Za-z0-9]{4}\\.[A-Z]{2}\\.[WAYSFON]\\.\\d{4}\\.\\d{6}T\\d{4}Z-\\d{6}T\\d{4}Z/");

    /**
     * The HTEC line (ex.
     * /00000.0.${ic}.000000T0000Z.000000T0000Z.000000T0000Z.OO/)
     */
    private static final Pattern htecPtrn = Pattern
            .compile("/[A-Za-z0-9]{5}.[0-3NU].\\w{2}.\\d{6}T\\d{4}Z.\\d{6}T\\d{4}Z.\\d{6}T\\d{4}Z.\\w{2}/");

    /** ex. * TORNADO WARNING FOR... */
    private static final Pattern firstBulletPtrn = Pattern
            .compile("\\*\\s(.*)\\s(WARNING|ADVISORY)(\\sFOR(.*)|\\.\\.\\.)");

    private static final Pattern cancelPtrn = Pattern
            .compile("(|(.*))(IS CANCELLED\\.\\.\\.)");

    private static final Pattern cancelOnlyPtrn = Pattern
            .compile("(CANCELLED<\\/L>\\.\\.\\.)");

    private static final Pattern expirePtrn = Pattern
            .compile("(|(.*))((EXPIRED|WILL EXPIRE)\\sAT\\s\\d{3,4}\\s(AM|PM)\\s\\w{3}...)");

    private static final Pattern headlinePtrn = Pattern
            .compile("(\\.\\.\\.((A|THE)\\s(.*)\\s(WARNING|ADVISORY))\\s(FOR|(REMAINS IN EFFECT (|(UNTIL\\s\\d{3,4}\\s(AM|PM)\\s\\w{3})))))(|(.*))");

    private static final Pattern canVtecPtrn = Pattern.compile("(\\.CAN\\.)");

    private static Pattern immediateCausePtrn = null;

    /** ex. SARPY NE-DOUGLAS NE-WASHINGTON NE- */
    public static final Pattern listOfAreaNamePtrn = Pattern
            .compile("(((\\w{1,}\\s{1}){1,}\\w{2}-){0,}((\\w{1,}\\s{1}){1,}\\w{2}-))");

    private static final Pattern secondBulletPtrn = Pattern.compile("\\*(|\\s"
            + TEST_MSG2 + ")\\sUNTIL\\s\\d{3,4}\\s(AM|PM)\\s\\w{3,4}");

    private static final Pattern testMessagePtrn = Pattern.compile("(|.*)("
            + TEST_MSG2 + ")(.*)");

    private static final Pattern latLonPtrn = Pattern
            .compile("^LAT...LON+(\\s\\d{3,4}\\s\\d{3,5}){1,}");

    private static final Pattern subLatLonPtrn = Pattern
            .compile("^((?!TIME...MOT... LOC))\\s{1,}\\d{3,4}\\s\\d{3,5}(|(\\s\\d{3,4}\\s\\d{3,5}){1,})");
    
    private static final Pattern tmlPtrn = Pattern
            .compile("TIME...MOT...LOC \\d{3,4}Z\\s\\d{1,3}DEG\\s\\d{1,3}KT((\\s\\d{3,4}\\s\\d{3,5}){1,})");

    private static final Pattern subTMLPtrn = Pattern
            .compile("(\\d{3,5}\\s){1,}");

    private static final Pattern lockedBlankLinesPattern = Pattern.compile(
            "<L>(\\s*+)</L>", Pattern.MULTILINE);
    
    private static final String LOCK_REPLACEMENT_TEXT = LOCK_START + "$0" + LOCK_END;
    private static final Pattern extraTokensPattern = Pattern.compile("\\b(?:THE|IS|CANCELLED)\\b");
    
    static {
        String pattern = "";

        // Load immediateCausePtrn with immediateCause.txt
        try {
            String immediateCause = FileUtil.open("immediateCause.txt", "base");
            pattern = "(.*)(A DAM BREAK";
            for (String ic : immediateCause.split("\n")) {
                String[] parts = ic.split("\\\\");
                pattern += "|" + parts[1].trim();
            }
            pattern += ")(.*)";
            immediateCausePtrn = Pattern.compile(pattern);
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.ERROR,
                            "Unable to process immediateCause.txt in the base directory",
                            e);
        }
    }

    public static String handle(String originalMessage, AffectedAreas[] areas,
            AffectedAreas[] canceledAreas, WarningAction action, WarningAction action2,
            boolean autoLock) {
        long t0 = System.currentTimeMillis();
        if (action == WarningAction.NEW) {
            try {
                originalMessage = VtecUtil.getVtec(originalMessage);
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.SIGNIFICANT,
                                "WarnGen cannot update the ETN. Please verify the VTEC is valid.",
                                e);
            }
        } else if (action == WarningAction.COR) {
            originalMessage = addCorrectedText(originalMessage);
        }

        originalMessage = wrap(originalMessage);

        if (autoLock) {
            boolean initialWarning = (action == WarningAction.NEW || action == WarningAction.EXT);

            if (action == WarningAction.COR) {
                Matcher m = firstBulletPtrn.matcher(originalMessage);
                initialWarning = m.find();
            }
            List<AffectedAreas> areasArr = areas != null ? Arrays.asList(areas)
                    : null;
            List<AffectedAreas> canceledAreasArr = canceledAreas != null ? Arrays
                    .asList(canceledAreas) : null;
            originalMessage = applyLocks(originalMessage, areasArr,
                    canceledAreasArr, initialWarning, action2);
        }

        originalMessage = removeExtraLines(originalMessage);

        // remove locked blank lines
        Matcher matchLockedBlankLines = lockedBlankLinesPattern
                .matcher(originalMessage);
        originalMessage = matchLockedBlankLines.replaceAll("$1");

        System.out.println("Time to handle the text: "
                + (System.currentTimeMillis() - t0));
        return originalMessage;
    }

    private static String addCorrectedText(String originalMessage) {
        int index = originalMessage.indexOf("NATIONAL WEATHER SERVICE");
        String[] types = new String[] { "WARNING", "WATCH", "STATEMENT",
                "ADVISORY" };
        int typeIdx = -1, i = 0;
        if (index > 0) {
            for (i = 0; i < types.length; i++) {
                if (originalMessage.lastIndexOf(types[i], index) != -1) {
                    typeIdx = originalMessage.lastIndexOf(types[i], index);
                    break;
                }
            }
        }

        if (index > 0 && typeIdx > 0
                && !originalMessage.contains("...CORRECTED")) {
            originalMessage = originalMessage.substring(0,
                    typeIdx + types[i].length())
                    + "...CORRECTED"
                    + originalMessage.substring(typeIdx + types[i].length());
        }

        return originalMessage;
    }

    private static String applyLocks(String originalMessage,
            List<AffectedAreas> areas, List<AffectedAreas> canceledAreas,
            boolean initialWarning, WarningAction action) {
        boolean firstBulletFound = false;
        boolean insideFirstBullet = false;
        boolean secondBulletFound = false;
        boolean headlineFound = false;
        // for CAN in a CANCON
        boolean cancelVtecLineFound = false;
        boolean insideLatLon = false;
        boolean insideTML = false;
        boolean checkForMND = true;
        StringBuffer sb = new StringBuffer();
        String[] seperatedLines = originalMessage.trim().split("\n");
        Matcher m = null;

        VtecObject vtecObj = VtecUtil.parseMessage(originalMessage);
        boolean marineProduct = vtecObj != null
                && vtecObj.getPhenomena() != null
                && vtecObj.getPhenomena().equals("MA");

        AffectedAreasComparator comparator = new AffectedAreasComparator(
                new ArrayList<String>());

        if (areas != null) {
            Collections.sort(areas, comparator);
        }

        if (canceledAreas != null) {
            Collections.sort(canceledAreas, comparator);
        }

        boolean startLines = true;

        // Set before to false if the line is beyond "THE NATIONAL WEATHER SERVICE IN" line.
        boolean before = true;
        
        ArrayList<String> usedAreaNotations = new ArrayList<String>();
        for (int lineIndex = 0; lineIndex < seperatedLines.length; ++lineIndex) {
            String line = seperatedLines[lineIndex];

            if (line.contains("THE NATIONAL WEATHER SERVICE IN") || line.contains("OTHER LOCATIONS IMPACTED")) {
                before = false;
            }

            // This prevents blank line(s) after the header from being locked.
            if (startLines && lineIndex > 1) {
                startLines = line.trim().length() == 0;
            }

            try {
                // MND header
                if (checkForMND
                        && (line.contains("EAS ACTIVATION")
                                || line.contains("IMMEDIATE BROADCAST")
                                || line.startsWith("NATIONAL WEATHER SERVICE")
                                || line.startsWith("THE NATIONAL WEATHER SERVICE")
                                || line.startsWith("ISSUED BY ")
                                || line.contains("...CORRECTED") || ((line
                                .endsWith("WARNING")
                                || line.endsWith("WATCH")
                                || line.endsWith("STATEMENT") || line
                                .endsWith("ADVISORY")) && line.startsWith("*") == false))) {
                    sb.append(LOCK_START + line + "\n" + LOCK_END);
                    continue;
                }

                // Locking the UGC line
                m = ugcPtrn.matcher(line);
                if (m.find()) {
                    sb.append(LOCK_START + line + "\n" + LOCK_END);
                    continue;
                }

                // Locking the VTEC
                m = vtecPtrn.matcher(line);
                if (m.find()) {
                    sb.append(LOCK_START + line + "\n" + LOCK_END);
                    // check out if .CAN. is in VTEC line of a CANCON product.
                    m = canVtecPtrn.matcher(line);
                    if (action == WarningAction.CANCON && m.find()) {
                        cancelVtecLineFound = true;
                    } else 
                    	cancelVtecLineFound = false;
                    continue;
                }

                // Locking the HTEC
                m = htecPtrn.matcher(line);
                if (m.find()) {
                    sb.append(LOCK_START + m.group(0) + "\n" + LOCK_END);
                    continue;
                }

                if (before) {
                	m = listOfAreaNamePtrn.matcher(line);
                	if (m.find()) {
                		if (!(line.contains("!**") || line.contains("**!") || line.contains("OTHER LOCATIONS"))) {
                			sb.append(LOCK_START + line + "\n" + LOCK_END);
                			continue;
                		}
                	}
                }

                // Locking Date in the MND header
                m = datePtrn.matcher(line);
                if (m.find()) {
                    sb.append(LOCK_START + line + "\n" + LOCK_END);
                    continue;
                }

                if (line.trim().length() == 0) {
                    headlineFound = false;
                    if (startLines) {
                        // Don't lock blank line after header
                        sb.append("\n");
                    } else if (sb.lastIndexOf("\n") == (sb.length() - 1)) {
                        // Put lock at end of previous line to prevent removal
                        // of leading blank line.
                        sb.setLength(sb.length() - 1);
                        sb.append(LOCK_START + "\n\n" + LOCK_END);
                    } else {
                        sb.append(LOCK_START + "\n" + LOCK_END);
                    }
                    continue;
                }

                // Hack!
                // This is needed due to a drifting <L>
                // due tothe replaceAll("</L>([\\s\\n\\r]*)<L>", "$1");
                if (line.startsWith("* AT") || line.startsWith("AT")) {
                    sb.append(LOCK_START + "" + LOCK_END);
                    headlineFound = false;
                }

                if (initialWarning) {
                    // Locking first bullet
                    m = firstBulletPtrn.matcher(line);
                    if (m.find() && firstBulletFound == false) {
                        checkForMND = false;
                        sb.append(LOCK_START + line + "\n" + LOCK_END);
                        firstBulletFound = true;
                        insideFirstBullet = true;
                        continue;
                    }

                    if (insideFirstBullet) {
                        // Removes extra spaces between texts in the first
                        // bullet
                        line = line.replaceAll("\\s{3}", "  ").replaceAll(
                                "\\b\\s{2,}\\b", " ");

                        // Lock immediate cause (hydro) (DR 10329)
                        if (immediateCausePtrn != null) {
                            m = immediateCausePtrn.matcher(line);
                            if (m.find()) {
                                sb.append(LOCK_START + line + "\n" + LOCK_END);
                                continue; // immediate cause on its own line
                            }
                        }

                        boolean countyFound = false;
                        boolean areaNotationFound = false;
                        boolean stateFound = false;

                        if (areas != null && !marineProduct) {
                            for (int i = areas.size() - 1; i >= 0; i--) {
                                AffectedAreas area = areas.get(i);
                                // Lock counties & independent cities (DR 10331)
                                if (!countyFound
                                        && area.getName() != null
                                        && area.getName().length() > 0
                                        && line.contains(area.getName()
                                                .toUpperCase())) {
                                    line = line.replaceFirst(area.getName()
                                            .toUpperCase(), LOCK_START
                                            + area.getName().toUpperCase()
                                            + LOCK_END);
                                    countyFound = true;
                                }

                                if (!areaNotationFound
                                        && area.getAreaNotation() != null
                                        && area.getAreaNotation().length() > 0
                                        && line.contains(area.getAreaNotation()
                                                .toUpperCase())) {
                                    line = line.replaceFirst(
                                            " " + area.getAreaNotation(),
                                            LOCK_START + " "
                                                    + area.getAreaNotation()
                                                    + LOCK_END);
                                    areaNotationFound = true;
                                }
                                // Lock States (DR 10325)
                                if (!stateFound
                                        && area.getParentRegion() != null
                                        && area.getParentRegion().length() > 0
                                        && line.contains(area.getParentRegion()
                                                .toUpperCase())) {
                                    line = line.replaceFirst(area
                                            .getParentRegion().toUpperCase(),
                                            LOCK_START
                                                    + area.getParentRegion()
                                                            .toUpperCase()
                                                    + LOCK_END);
                                    stateFound = true;
                                }
                            }
                        }
                    }

                    // Locking second bullet
                    m = secondBulletPtrn.matcher(line);
                    if (m.find() && secondBulletFound == false) {
                        sb.append(LOCK_START + line + "\n" + LOCK_END);
                        insideFirstBullet = false;
                        secondBulletFound = true;
                        continue;
                    }
                } else {
                	usedAreaNotations.clear();
                	// head line pattern
                	m = headlinePtrn.matcher(line);
                	if (m.find()) {
                		checkForMND = false;
                		headlineFound = true;
                		line = line.replace(m.group(2), LOCK_START + m.group(2)
                				+ LOCK_END);
                	}
                	// CAN portion in a CANCON
                	if (cancelVtecLineFound) {
                		for (AffectedAreas area : canceledAreas) {
                			String areaName = area.getName();
                			if (areaName != null) {
                                areaName = areaName.toUpperCase();
                				String[] tokens = areaName.split(" ");
                				for (String s: tokens)
                					if (line.contains(s))
                						line = line.replaceAll(s, LOCK_START
                								+ s + LOCK_END);
                			}
                			// areaNotation, e.g., COUNTY
                			String areaNotation = area.getAreaNotation().toUpperCase();
                			if (areaNotation != null && areaNotation.length()>0
                					&& !usedAreaNotations.contains(areaNotation)
                					&& line.contains(areaNotation)) {
                				line = line.replaceAll(areaNotation, LOCK_START
                						+ areaNotation + LOCK_END);
                				usedAreaNotations.add(areaNotation);
                			}
                			// areasNotation, e.g., COUNTIES
                			String areasNotation = area.getAreasNotation().toUpperCase();
                			if (areasNotation != null && areasNotation.length()>0
                					&& !usedAreaNotations.contains(areasNotation)
                					&& line.contains(areasNotation)) {
                				line = line.replaceAll(areasNotation, LOCK_START
                						+ areasNotation + LOCK_END);
                				usedAreaNotations.add(areasNotation);
                			}
                		}
                		// locking "THE" in "THE CITY OF MANASSAS", "...THE" in "...THE CITY",
                		// and "IS" or "CANCELLED" in "IS CANCELLED...".
        				line = extraTokensPattern.matcher(line).replaceAll(
                                LOCK_REPLACEMENT_TEXT);

        				m = cancelOnlyPtrn.matcher(line);
                		if (m.find())
                			cancelVtecLineFound = false;
                		
                		sb.append(line + "\n");
                		continue;
                	} else {
                		// follow-ups other than CAN in a CANCON
                		if (headlineFound) {
                			usedAreaNotations.clear();
                			if (areas != null && !marineProduct) {
                				for (AffectedAreas area : areas) {
                					if (area.getName() != null
                							&& line.contains(area.getName()
                									.toUpperCase())) {
                						line = line.replaceFirst(area.getName()
                								.toUpperCase(), LOCK_START
                								+ area.getName().toUpperCase()
                								+ LOCK_END);
                					}

                					if (area.getAreaNotation() != null
                							&& !usedAreaNotations.contains(area
                									.getAreaNotation()
                									.toUpperCase())
                									&& line.contains(area.getAreaNotation())) {
                						line = line.replaceAll(" "
                								+ area.getAreaNotation()
                								.toUpperCase(), LOCK_START
                								+ " " + area.getAreaNotation()
                								+ LOCK_END);
                						usedAreaNotations.add(area
                								.getAreaNotation().toUpperCase());
                					}
                				}
                			}

                			m = cancelPtrn.matcher(line);
                			if (m.find()) {
                				line = line.replaceFirst(m.group(3),
                						LOCK_START + m.group(3) + LOCK_END);
                				if (canceledAreas != null) {
                					for (AffectedAreas canceledArea : canceledAreas) {
                						if (line.contains(canceledArea.getName()
                								.toUpperCase())) {
                							line = line.replaceFirst(canceledArea
                									.getName().toUpperCase(),
                									LOCK_START
                									+ canceledArea
                									.getName()
                									.toUpperCase()
                									+ LOCK_END);
                						}
                					}
                				}
                				headlineFound = false;
                			}

                			m = expirePtrn.matcher(line);
                			if (m.find()) {
                				line = line.replaceFirst(m.group(3),
                						LOCK_START + m.group(3) + LOCK_END);
                				headlineFound = false;
                			}
                			
                			sb.append(line + "\n");
                			continue;
                		}
                	}
                }

                // Locking LAT...LON
                m = latLonPtrn.matcher(line);
                if (m.find()) {
                    sb.append(LOCK_START + line + "\n");
                    insideLatLon = true;
                    continue;
                }

                if (insideLatLon) {
                    m = subLatLonPtrn.matcher(line);
                    if (m.find()) {
                        sb.append(line + "\n");
                        continue;
                    } else {
                        insideLatLon = false;
                        sb.append(LOCK_END);
                    }
                }

                // PRECAUTIONARY/PREPAREDNESS ACTIONS, $$, &&
                if (line.equals("PRECAUTIONARY/PREPAREDNESS ACTIONS...")
                        || line.startsWith("$$") || line.startsWith("&&")) {
                    sb.append(LOCK_START + line + "\n" + LOCK_END);
                    continue;
                }
                // Locking TIME...MOT..LOC
                m = tmlPtrn.matcher(line);
                if (m.find()) {
                    sb.append(LOCK_START + line + "\n");
                    insideTML = true;
                    continue;
                }

                if (insideTML) {
                    m = subTMLPtrn.matcher(line);
                    if (m.matches()) {
                        sb.append(line + "\n");
                        continue;
                    } else {
                        insideTML = false;
                        sb.append(LOCK_END);
                    }

                }

                // Test lines
                if (line.equals(TEST_MSG3)
                		|| line.equals(TEST_MSG1)
                		|| (line.startsWith("TEST...") && line
                				.endsWith("...TEST"))) {
                	sb.append(LOCK_START + line + LOCK_END + "\n");
                	continue;
                }                
                m = testMessagePtrn.matcher(line);
                if (m.find()) {
                	line = line.replace(m.group(2), LOCK_START + m.group(2)
                			+ LOCK_END);
                }
            } catch (Exception e) {
            	// If an exception is thrown,
            	// log the exception but continue locking text
            	statusHandler.handle(Priority.PROBLEM, "Error locking line: "
            			+ line + "\n", e);
            }
            sb.append(line + "\n");
            insideLatLon = false;
            insideTML = false;
        }
        String rval = sb.toString().trim();

        // where a lock close and lock open are only separated by whitespace
        // remove the close and open to join the two locked areas
        rval = rval.replaceAll("</L>([\\s\\n\\r]*)<L>", "$1");

        return rval;
    }

    private static String wrap(String str) {
        StringBuffer sb = new StringBuffer();

        boolean inLocations = false;
        boolean first = true;
        boolean inBullet = false;
        String[] values = str.split("\n");
        for (String v : values) {
            String ignoreLockTags = v.replace(LOCK_START, "").replace(LOCK_END,
                    "");
            if (ignoreLockTags.length() <= MAX_WIDTH) {
                if (!first) {
                    sb.append("\n");
                    if (inBullet && !ignoreLockTags.startsWith("  ")) {
                        sb.append("  ");
                    }
                }
                if (ignoreLockTags.startsWith("* ")) {
                    inBullet = true;
                    if (ignoreLockTags.startsWith("* LOCATIONS")) {
                        inLocations = true;
                    }
                } else if (ignoreLockTags.trim().equals("")) {
                    inBullet = false;
                    inLocations = false;
                }
                sb.append(v);

            } else {
                Matcher m = null;

                if (ignoreLockTags.startsWith("* ")) {
                    inBullet = true;
                    if (ignoreLockTags.startsWith("* LOCATIONS")) {
                        inLocations = true;
                    }
                } else if (ignoreLockTags.startsWith("...")
                        && ignoreLockTags.endsWith("...")) {
                    // headline found
                    // use line without lock tags because, lock tags adds to
                    // character count with doing a regex pattern
                    v = ignoreLockTags;
                }

                if (inLocations) {
                    sb.append("\n");
                    sb.append(wrapLocations(v));
                    continue;
                } else {
                    m = ugcPtrn.matcher(v);
                    if (m.find()) {
                        m = hyphenWrapRE.matcher(v);
                    } else {
                        m = listOfAreaNamePtrn.matcher(v);
                        if (m.matches()) {
                            m = hyphenWrapRE.matcher(v);
                        } else {
                            m = wrapRE.matcher(v);
                        }
                    }
                }
                while (m.find()) {
                    String group = m.group().trim();

                    if (group.trim().length() == 0) {
                        break;
                    }

                    sb.append("\n");
                    if (group.startsWith("* ") == false && inBullet) {
                        sb.append("  ");
                    }

                    sb.append(group);
                }
            }
            first = false;
        }

        return sb.toString();
    }
    
    private static String wrapLocations(String locationsLine) {
        StringBuffer sb = new StringBuffer();
        
        String line = "  ";
        String[] locations = locationsLine.split("\\.\\.\\.");
        
        for (int i = 0; i < locations.length; i++) {
            String location = locations[i];
            int size = (i == locations.length - 1)? location.length() : location.length() + 3;
            
            if (line.length() + size >= MAX_WIDTH - 2) {
                sb.append(line + "\n");
                line = "  ";
            } 
            
            if (i == locations.length - 1) {
                line += location;
            } else {
                line += location + "...";
            }
        }
        
        sb.append(line + "\n");
        
        return sb.toString();
    }

    private static String removeExtraLines(String originalMessage) {
        StringBuffer sb = new StringBuffer();
        String[] seperatedLines = originalMessage.replaceAll("\r", "").trim()
                .split("\n");
        boolean blankLine = false;
        for (String line : seperatedLines) {
            if (line.replace(LOCK_START, "").replace(LOCK_END, "").trim()
                    .length() > 0) {
                sb.append(line + "\n");
                blankLine = false;
            } else if (blankLine == false) {
                sb.append(line + "\n");
                blankLine = true;
            }
        }

        return sb.toString().trim();
    }

}
