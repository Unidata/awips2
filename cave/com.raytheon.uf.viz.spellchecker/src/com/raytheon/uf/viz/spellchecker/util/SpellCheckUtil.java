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
package com.raytheon.uf.viz.spellchecker.util;

import java.text.MessageFormat;
import java.util.regex.Pattern;

import org.eclipse.jdt.internal.ui.JavaUIMessages;

/**
 * Constants for spell checking.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket# Engineer    Description
 * ------------ ------- ----------- --------------------------
 * May 10, 2019 7747    mapeters    Initial creation
 *
 * </pre>
 *
 * @author mapeters
 */
@SuppressWarnings("restriction")
public class SpellCheckUtil {

    public static final Pattern WORD_PATTERN = Pattern.compile("\\w+");

    private static final String CAPTURE_REGEX = "(.*)";

    /*
     * The following three patterns are used to match display strings of the
     * proposals for fixing spelling issues. We replace the word placeholder
     * with a regex that will capture the word, so that we can check if the word
     * is blacklisted.
     */

    /**
     * A pattern to recognize "Ignore '${word}' during the current session"
     * proposals
     */
    public static final Pattern IGNORE_PATTERN = Pattern.compile(MessageFormat
            .format(JavaUIMessages.Spelling_ignore_label, CAPTURE_REGEX));

    /**
     * A pattern to recognize "Add '${word}' to dictionary" proposals
     */
    public static final Pattern ADD_TO_PATTERN = Pattern.compile(MessageFormat
            .format(JavaUIMessages.Spelling_add_label, CAPTURE_REGEX));

    /**
     * A pattern to recognize "Change to '${word}'" proposals
     */
    public static final Pattern CHANGE_TO_PATTERN = Pattern
            .compile(MessageFormat.format(JavaUIMessages.Spelling_correct_label,
                    CAPTURE_REGEX));
}
