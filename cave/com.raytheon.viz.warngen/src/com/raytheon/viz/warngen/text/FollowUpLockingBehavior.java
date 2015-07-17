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
 * Mar 13, 2013    15892   D. Friedman  Fix headline locking. Do not
 *                                      lock "AND" or "FOR".
 * May 29, 2015    4442    randerso     Fixed WarnGen text locking to work with mixed case
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class FollowUpLockingBehavior extends AbstractLockingBehavior {
    private static Pattern headlinePtrn = Pattern
            .compile(
                    "^\\.\\.\\.(AN?|THE) (.*) (WARNING|ADVISORY) .*(REMAINS|EXPIRE|CANCELLED).*(\\.\\.\\.)$",
                    Pattern.MULTILINE | Pattern.CASE_INSENSITIVE);

    private static Pattern expirePtrn = Pattern
            .compile(
                    "(HAS BEEN ALLOWED TO EXPIRE)|(WILL BE ALLOWED TO EXPIRE)|(WILL EXPIRE)|(HAS EXPIRED)|EXPIRED",
                    Pattern.MULTILINE | Pattern.CASE_INSENSITIVE);

    private static Pattern timePtrn = Pattern.compile(
            "AT \\d{3,4} (AM|PM) \\w{3,4}", Pattern.MULTILINE
                    | Pattern.CASE_INSENSITIVE);

    private static Pattern remainsPtrn = Pattern.compile(
            "REMAINS IN EFFECT UNTIL \\d{3,4} (AM|PM) \\w{3,4}",
            Pattern.MULTILINE | Pattern.CASE_INSENSITIVE);

    private static Pattern canceledPtrn = Pattern.compile(
            "(IS|(HAS BEEN)) CANCELLED", Pattern.MULTILINE
                    | Pattern.CASE_INSENSITIVE);

    private static Pattern warningPtrn = Pattern.compile(
            "(AN?|THE)( [\\w\\s]*?)(" + warningType + ")", Pattern.MULTILINE
                    | Pattern.CASE_INSENSITIVE);

    /**
     * Locks the appropriate text of the body of an initial warning.
     */
    @Override
    public void body() {
        headlines();
        super.body();
    }

    /**
     * Locks the cancel and the follow up headlines.
     * 
     */
    private void headlines() {
        // LOCK_END should not be found at the beginning since the previous line
        // should be blank.
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
        Matcher m = expirePtrn.matcher(followupHeadline);
        followupHeadline = m.replaceAll(WarnGenPatterns.REPLACEMENT);

        m = timePtrn.matcher(followupHeadline);
        followupHeadline = m.replaceAll(WarnGenPatterns.REPLACEMENT);
        return followupHeadline;
    }

    /**
     * Locks remains in effect text.
     * 
     * @param followupHeadline
     * @return
     */
    private String remainsInEffect(String followupHeadline) {
        Matcher m = remainsPtrn.matcher(followupHeadline);
        followupHeadline = m.replaceFirst(WarnGenPatterns.REPLACEMENT);
        return followupHeadline;
    }

    /**
     * Locks the canceled text.
     * 
     * @param canceledHeadline
     * @return
     */
    private String canceled(String canceledHeadline) {
        Matcher m = canceledPtrn.matcher(canceledHeadline);
        canceledHeadline = m.replaceAll(WarnGenPatterns.REPLACEMENT);
        return canceledHeadline;
    }

    /**
     * Helper method to lock area names and notations in the headline.
     * 
     * @param headline
     * @param areas
     * @return
     */
    private String headline(String headline) {
        // Marine products follow different locking rules
        if (isMarineProduct()) {
            // The full headline of a marine product should be locked.
            headline = WarnGenPatterns.LOCK_START + headline
                    + WarnGenPatterns.LOCK_END;

        } else {
            Set<String> notations = new HashSet<String>();
            Set<String> names = new HashSet<String>();

            for (AffectedAreas affectedArea : affectedAreas) {
                if ((affectedArea.getAreaNotation() != null)
                        && (affectedArea.getAreaNotation().trim().length() != 0)) {
                    notations.add(affectedArea.getAreaNotation().toUpperCase());
                }

                if ((affectedArea.getAreasNotation() != null)
                        && (affectedArea.getAreasNotation().trim().length() != 0)) {
                    notations
                            .add(affectedArea.getAreasNotation().toUpperCase());
                }

                if ((affectedArea.getName() != null)
                        && (affectedArea.getName().trim().length() != 0)) {
                    /*
                     * force area name to upper case for headlines since
                     * headlines are all upper case
                     */
                    names.add(affectedArea.getName().toUpperCase());
                }
            }

            headline = keywords(headline);
            Iterator<String> iterator1 = notations.iterator();
            while (iterator1.hasNext()) {
                String notation = iterator1.next();
                if (!hasBeenLocked(headline, notation)) {
                    headline = headline.replace(notation,
                            WarnGenPatterns.LOCK_START + notation
                                    + WarnGenPatterns.LOCK_END);
                }
            }

            Iterator<String> iterator2 = names.iterator();
            while (iterator2.hasNext()) {
                String name = iterator2.next();
                if (!hasBeenLocked(headline, name)) {
                    headline = headline.replace(name,
                            WarnGenPatterns.LOCK_START + name
                                    + WarnGenPatterns.LOCK_END);
                }
            }
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
        headline = headline.replaceFirst("^\\.\\.\\.",
                WarnGenPatterns.REPLACEMENT);
        // Locking the end of a head line (...)
        if (headline.endsWith("...")) {
            headline = headline.substring(0, headline.length() - 3)
                    + WarnGenPatterns.LOCK_START + "..."
                    + WarnGenPatterns.LOCK_END;
        }
        // Locks warning type (i.e. SEVERE THUNDERSTORM)
        Matcher m = warningPtrn.matcher(headline);
        headline = m.replaceAll(WarnGenPatterns.LOCK_START + "$1"
                + WarnGenPatterns.LOCK_END + "$2" + WarnGenPatterns.LOCK_START
                + "$3" + WarnGenPatterns.LOCK_END);

        return headline;
    }

}
