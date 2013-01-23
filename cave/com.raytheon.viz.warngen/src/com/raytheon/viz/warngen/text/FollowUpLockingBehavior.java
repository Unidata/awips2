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
package com.raytheon.viz.warngen.text;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.viz.warngen.gis.AffectedAreas;

/**
 * Locks text patterns for follow up warnings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012    15322   jsanchez     Initial creation
 * Jan  8, 2013    15664   Qinglu Lin   Updated body().
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class FollowUpLockingBehavior extends AbstractLockingBehavior {
    /**
     * Locks the appropriate text of the body of an initial warning.
     */
    @Override
    public void body() {
    	if (action != WarningAction.COR)
    		headlines();
    	else
    		super.body();
    }

    /**
     * Locks the cancel and the follow up headlines.
     * 
     */
    private void headlines() {
        // LOCK_END should not be found at the beginning since the previous line
        // should be blank.
        Pattern headlinePtrn = Pattern
                .compile(
                        "^\\.\\.\\.(A|THE) (.*) (WARNING|ADVISORY) .*(REMAINS|EXPIRE|CANCELLED).*(\\.\\.\\.)$",
                        Pattern.MULTILINE);
        Matcher m = headlinePtrn.matcher(text);

        while (m.find()) {
            String originalHeadline = m.group();
            String headline = headline(originalHeadline);
            headline = remainsInEffect(headline);
            headline = expired(headline);
            headline = canceled(headline);
            this.text = text.replace(originalHeadline, headline);
        }
    }

    /**
     * Locks the expired text.
     */
    private String expired(String followupHeadline) {
        // Based on the templates, the expired text is for the affected areas
        // not the canceled areas.
        String expire = "(HAS BEEN ALLOWED TO EXPIRE)|(WILL BE ALLOWED TO EXPIRE)|(WILL EXPIRE)|(HAS EXPIRED)|EXPIRED";
        String time = "AT \\d{3,4} (AM|PM) \\w{3,4}";
        return followupHeadline.replaceAll(expire, REPLACEMENT).replaceAll(
                time, REPLACEMENT);
    }

    /**
     * Locks remains in effect text.
     * 
     * @param followupHeadline
     * @return
     */
    private String remainsInEffect(String followupHeadline) {
        return followupHeadline.replaceFirst(
                "REMAINS IN EFFECT UNTIL \\d{3,4} (AM|PM) \\w{3,4}", LOCK_START
                        + "$0" + LOCK_END);
    }

    /**
     * Locks the canceled text.
     * 
     * @param canceledHeadline
     * @return
     */
    private String canceled(String canceledHeadline) {
        return canceledHeadline.replaceAll("(IS|(HAS BEEN)) CANCELLED",
                REPLACEMENT);
    }

    /**
     * Helper method to lock area names and notations in the headline.
     * 
     * @param headline
     * @param areas
     * @return
     */
    private String headline(String headline) {
        Set<String> notations = new HashSet<String>();
        Set<String> names = new HashSet<String>();

        for (AffectedAreas affectedArea : affectedAreas) {
            if (affectedArea.getAreaNotation() != null
                    && affectedArea.getAreaNotation().trim().length() != 0) {
                notations.add(affectedArea.getAreaNotation().toUpperCase());
            }

            if (affectedArea.getAreasNotation() != null
                    && affectedArea.getAreasNotation().trim().length() != 0) {
                notations.add(affectedArea.getAreasNotation().toUpperCase());
            }

            if (affectedArea.getName() != null
                    && affectedArea.getName().trim().length() != 0) {
                names.add(affectedArea.getName().toUpperCase());
            }
        }

        // Marine products follow different locking rules
        if (!isMarineProduct()) {
            headline = keywords(headline);
            Iterator<String> iterator1 = notations.iterator();
            while (iterator1.hasNext()) {
                String notation = iterator1.next();
                if (!hasBeenLocked(headline, notation)) {
                    headline = headline.replace(notation, LOCK_START + notation
                            + LOCK_END);
                }
            }

            Iterator<String> iterator2 = names.iterator();
            while (iterator2.hasNext()) {
                String name = iterator2.next();
                if (!hasBeenLocked(headline, name)) {
                    headline = headline.replace(name, LOCK_START + name
                            + LOCK_END);
                }
            }
        } else {
            // The full headline of a marine product should be locked.
            headline = LOCK_START + headline + LOCK_END;
        }

        return headline;
    }

    /**
     * Common key words to be locked in a headline.
     * 
     * @param headline
     * @return
     */
    private String keywords(String headline) {
        // Locking the start of a head line (...)
        headline = headline.replaceFirst("^\\.\\.\\.", LOCK_START + "$0"
                + LOCK_END);
        // Locking the end of a head line (...)
        if (headline.endsWith("...")) {
            headline = headline.substring(0, headline.length() - 3)
                    + LOCK_START + "..." + LOCK_END;
        }
        // Locks warning type (i.e. SEVERE THUNDERSTORM)
        headline = headline.replaceAll("(A|THE) (" + warningType + ")",
                LOCK_START + "$0" + LOCK_END);

        // Locks the 'FOR' in the headline
        headline = headline.replaceFirst(" FOR ", " " + LOCK_START + "FOR"
                + LOCK_END + " ");

        // Locks the 'AND' in the headline
        headline = headline.replaceFirst(" AND ", " " + LOCK_START + "AND"
                + LOCK_END + " ");

        return headline;
    }

}
