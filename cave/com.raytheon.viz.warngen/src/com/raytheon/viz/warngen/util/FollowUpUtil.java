package com.raytheon.viz.warngen.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.EnumSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil.Direction;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.warngen.gis.AffectedAreas;
import com.raytheon.viz.warngen.text.ICommonPatterns;

/**
 * This utility will provide methods for determining what followup products are
 * applicable at a given time. Additionally, they will provide extra methods
 * which are deemed useful in followup product generation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 22, 2008	#1284			bwoodle	Initial creation
 * Oct 18, 2012 15332           jsanchez Fixed refactor bugs.
 * Mar 13, 2013 DR 15892    D. Friedman  Handle SMW format in canceledAreasFromText
 * Aug  6, 2013 2243        jsanchez     Updated the time ranges to be removed from the follow up list correctly.
 * Aug 13, 2013 2243        jsanchez     Removed calendar object.
 * Aug 15, 2013 2243        jsanchez     Reset the time ranges to the correct values.
 * Dec  4, 2013 2604        jsanchez     Refactored GisUtil.
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class FollowUpUtil {

    private static final Pattern pilPtrn = Pattern
            .compile("(FFW|TOR|SVR|EWW|SMW|FLW|FLS|FFS|MWS|FFA|MWW|SVS|WRK)[A-Z]{3}");

    public static final Pattern vtecPtrn = Pattern
            .compile("/[OTEX]\\.([A-Z]{3})\\.[A-Za-z0-9]{4}\\.[A-Z]{2}\\.[WAYSFON]\\.\\d{4}\\.\\d{6}T\\d{4}Z-\\d{6}T\\d{4}Z/");

    private static final String SMW_CANCELED_AREAS_HEADER = "THE AFFECTED AREAS WERE...";

    /**
     * This method checks whether a particular followup should be available
     * given a Warning Record, a vtec Action, and a template configuration
     */
    public static boolean checkApplicable(String site,
            WarngenConfiguration config, AbstractWarningRecord record,
            WarningAction action) {

        boolean rval = false;
        if (record == null) {
            return rval;
        }

        boolean valueCheck = false;
        for (String phensig : config.getPhensigs()) {
            if (phensig.equalsIgnoreCase(record.getPhensig())) {
                valueCheck = true;
            }
        }
        if (valueCheck) {
            for (String s : config.getFollowUps()) {
                WarningAction act = WarningAction.valueOf(s);
                if (act == action
                        && getTimeRange(act, record).contains(
                                SimulatedTime.getSystemTime().getTime())
                        && act != WarningAction.COR) {
                    rval = true;
                }
            }
            if (action == WarningAction.COR
                    && checkCorApplicable(site, config, record)) {
                rval = true;
            }
        }
        return rval;
    }

    private static boolean checkCorApplicable(String site,
            WarngenConfiguration config, AbstractWarningRecord warnRec) {
        boolean allowsCONProduct = false;
        boolean allowsCORProduct = false;
        for (String s : config.getFollowUps()) {
            WarningAction act = WarningAction.valueOf(s);
            if (act == WarningAction.CON) {
                allowsCONProduct = true;
            } else if (act == WarningAction.COR) {
                allowsCORProduct = true;
            }
        }

        if (allowsCORProduct == false) {
            return false;
        }

        CurrentWarnings cw = CurrentWarnings.getInstance(site);
        List<AbstractWarningRecord> correctableWarnings = cw
                .getCorrectableWarnings(warnRec);

        boolean wasContinued = false;
        for (AbstractWarningRecord w : correctableWarnings) {
            if (WarningAction.valueOf(w.getAct()) == WarningAction.CON) {
                wasContinued = true;
            }
        }

        // Adding a COR option for continuation follow ups
        if (correctableWarnings.isEmpty() == false
                && ((allowsCONProduct && wasContinued) || (allowsCONProduct == false && wasContinued == false))) {
            return true;
        }

        return false;
    }

    /**
     * Returns the raw message of the record but removes the first wmoid and the
     * pil (the first two lines of the warning)
     * 
     * @param record
     * @return
     */
    public static String originalText(AbstractWarningRecord record) {

        String originalMessage = record.getRawmessage();

        // Removes the wmoHeader because the it will be inserted in the template
        int wmoIdx = originalMessage.indexOf(record.getWmoid());
        if (wmoIdx != -1) {
            int endIdx = originalMessage.indexOf("\n", wmoIdx);
            originalMessage = originalMessage.replace(
                    originalMessage.substring(wmoIdx, endIdx), "");
        }
        originalMessage = originalMessage.replaceFirst(record.getWmoid(), "");

        // Removes the PIL because the it will be inserted in the template
        Matcher m = pilPtrn.matcher(originalMessage);
        if (m.find()) {
            originalMessage = originalMessage.replaceFirst(m.group(0), "")
                    .trim();
        }

        m = vtecPtrn.matcher(originalMessage);
        while (m.find()) {
            String oldVtec = m.group(0);
            String newVtec = oldVtec.replace(m.group(1),
                    WarningAction.COR.toString());
            originalMessage = originalMessage.replaceFirst(m.group(0), newVtec);
        }

        return originalMessage;
    }

    /**
     * Returns a list of the canceled areas from the original text
     * 
     * @param originalText
     * @return
     */
    public static ArrayList<AffectedAreas> canceledAreasFromText(
            String originalText) {
        boolean namedone = false;
        boolean insideHeadline = false;
        String ugcLine = getUgcLineCanFromText(originalText);
        String namesLine = "";
        String headline = "";
        Pattern listOfAreaNamePtrn = Pattern
                .compile(ICommonPatterns.listOfAreaName);
        String[] splitLines = originalText.trim().split("\n");
        for (String line : splitLines) {
            if (line.contains("TEST") || line.trim().length() == 0) {
                continue;
            }
            Matcher m = listOfAreaNamePtrn.matcher(line);
            if (!namedone && m.find()) {
                namesLine += m.group();
                continue;
            } else if (namesLine.length() > 0) {
                namedone = true;
            }

            if (line.startsWith("...")) {
                headline += line.substring(3);
                insideHeadline = true;
            } else if (insideHeadline) {

                if (line.trim().endsWith("...")) {
                    headline += line.substring(0, line.length() - 3);
                    insideHeadline = false;
                    break;
                }
                headline += line;
            }
        }
        String[] ugcs = FipsUtil.getListCounties(ugcLine)
                .toArray(new String[0]);
        String[] names;
        boolean smwAreas = false;
        if (namesLine.length() > 0)
            names = namesLine.split("-");
        else {
            names = parseSMWCanceledAreas(splitLines);
            smwAreas = true;
        }
        String[] areas = headline.split("\\.\\.\\.");

        ArrayList<AffectedAreas> al = new ArrayList<AffectedAreas>();

        String stateAbbreviation = null;
        String areaNotation = null;
        String areasNotation = null;
        String fips = null;
        String name = null;
        List<String> partOfArea = null;
        for (int i = 0; i < ugcs.length; i++) {
            AffectedAreas affectedArea = new AffectedAreas();
            String ugc = ugcs[i].trim();
            if (ugc.length() == 6) {
                if (ugc.charAt(2) == 'Z') {
                    areaNotation = "ZONE";
                    areasNotation = "ZONES";
                } else {
                    areaNotation = "COUNTY";
                    areasNotation = "COUNTIES";
                }
            }

            if (ugc.length() < 3)
                continue; // TODO: log?

            fips = ugc.substring(ugc.length() - 3);

            if (i < names.length) {
                if (!smwAreas && names[i].length() >= 3) {
                    name = names[i].substring(0, names[i].length() - 3);
                    stateAbbreviation = names[i]
                            .substring(names[i].length() - 2);
                } else {
                    name = names[i];
                }
            } else
                break;

            if (name != null) {
                for (String area : areas) {
                    if (area.contains(name)) {
                        EnumSet<Direction> set = EnumSet
                                .noneOf(Direction.class);
                        for (Direction direction : Direction.values()) {
                            if (area.contains(direction.name())) {
                                set.add(direction);
                            }
                        }
                        partOfArea = GisUtil.asStringList(set);
                        break;
                    }
                }
            }

            affectedArea.setFips(fips);
            affectedArea.setStateabbr(stateAbbreviation);
            affectedArea.setAreaNotation(areaNotation);
            affectedArea.setAreasNotation(areasNotation);
            affectedArea.setName(name);
            affectedArea.setPartOfArea(partOfArea);
            al.add(affectedArea);
        }

        return al;
    }

    public static String getUgcLineCanFromText(String originalText) {
        String ugcLine = "";
        Pattern ugcPtrn = Pattern.compile(ICommonPatterns.ugc);
        for (String line : originalText.replaceAll("\r", "").trim().split("\n")) {
            Matcher m = ugcPtrn.matcher(line);
            if (m.find()) {
                ugcLine += line;
                continue;
            } else if (ugcLine.length() > 0) {
                break;
            }
        }

        return ugcLine;
    }

    /**
     * This method determines a time range for which a particular action can be
     * performed on a particular warngen product. For instance, a Reissue (NEW)
     * can only be produced from 20 minutes before expiration until 30 minutes
     * after expiration.
     * 
     * @param action
     * @param record
     * @return
     */
    public static TimeRange getTimeRange(WarningAction action,
            AbstractWarningRecord record) {
        /* Calendars for time calculations */

        Calendar start = TimeUtil.newCalendar();
        Calendar end = TimeUtil.newCalendar();

        TimeRange rval = null;

        if (action == WarningAction.NEW) {
            /* Calculate NEW Time Range */
            start.setTime(record.getEndTime().getTime());
            start.add(Calendar.MINUTE, -20);
            end.setTime(record.getEndTime().getTime());
            end.add(Calendar.MINUTE, 30);
            rval = new TimeRange(start, end);
        } else if (action == WarningAction.COR) {
            /* Calculate COR Time Range */
            end.setTime(record.getIssueTime().getTime());
            end.add(Calendar.MINUTE, 10);
            rval = new TimeRange(record.getStartTime(), end);
        } else if (action == WarningAction.CAN) {
            /* Calculate CAN Time Range */
            end.setTime(record.getEndTime().getTime());
            end.add(Calendar.MINUTE, -10);
            rval = new TimeRange(record.getStartTime(), end);
        } else if (action == WarningAction.CON) {
            /* Calculate CON Time Range */
            end.setTime(record.getEndTime().getTime());
            end.add(Calendar.MINUTE, -5);
            rval = new TimeRange(record.getStartTime(), end);
        } else if (action == WarningAction.EXP) {
            /* Calculate EXP Time Range */
            start.setTime(record.getEndTime().getTime());
            start.add(Calendar.MINUTE, -10);
            end.setTime(record.getEndTime().getTime());
            end.add(Calendar.MINUTE, 10);
            rval = new TimeRange(start, end);
        } else if (action == WarningAction.EXT) {
            /* Calculate EXT Time Range */
            start.setTime(record.getStartTime().getTime());
            end.setTime(record.getEndTime().getTime());
            end.add(Calendar.MINUTE, -5);
            rval = new TimeRange(start, end);
        }

        return rval;
    }

    /**
     * Parses the canceled areas of an SMW, which have a different format from
     * other products.
     */
    private static String[] parseSMWCanceledAreas(String[] splitLines) {
        StringBuilder text = new StringBuilder(64);
        boolean inAreas = false;
        for (String line : splitLines) {
            String trimmedLine = line.trim();
            if (SMW_CANCELED_AREAS_HEADER.equals(trimmedLine))
                inAreas = true;
            else if (inAreas) {
                if (trimmedLine.length() > 0) {
                    text.append(trimmedLine);
                    text.append('\n');
                } else
                    break;
            }
        }
        int len = text.length();
        if (len >= 4 && "...\n".equals(text.substring(len - 4)))
            text.delete(len - 4, len);
        String[] areas = text.toString().split("\\.\\.\\.\\n");
        // Unwrap lines.
        for (int i = 0; i < areas.length; ++i)
            areas[i] = areas[i].replace("\n", " ");
        return areas;
    }
}
