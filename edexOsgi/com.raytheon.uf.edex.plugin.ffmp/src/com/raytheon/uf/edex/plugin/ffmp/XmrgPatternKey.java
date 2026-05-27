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
package com.raytheon.uf.edex.plugin.ffmp;

import com.raytheon.uf.common.monitor.xml.SourceXML;

/**
 * POJO used to identify and map to xmrg file name parsing {@link Pattern}s. The fields
 * within this POJO are based on a FFMP {@link SourceXML}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2016 5756       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class XmrgPatternKey {

    public static final int DATE_GROUP = 1;

    private static final int DEFAULT_DATE_LENGTH = 12;

    private static final String PATTERN_FORMAT = "^%s.*(\\d{%d})z$";

    private final String sourceName;

    private final int dateLength;

    public XmrgPatternKey(final String sourceName, final int dateLength) {
        this.sourceName = sourceName;
        this.dateLength = dateLength;
    }

    public XmrgPatternKey(final SourceXML sourceXML) {
        this.sourceName = sourceXML.getSourceName();
        this.dateLength = (sourceXML.getDateFormat() == null || sourceXML
                .getDateFormat().trim().isEmpty()) ? DEFAULT_DATE_LENGTH
                : sourceXML.getDateFormat().trim().length();
    }

    public final String getRegex() {
        return String.format(PATTERN_FORMAT, sourceName, dateLength);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + dateLength;
        result = prime * result
                + ((sourceName == null) ? 0 : sourceName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        XmrgPatternKey other = (XmrgPatternKey) obj;
        if (dateLength != other.dateLength) {
            return false;
        }
        if (sourceName == null) {
            if (other.sourceName != null) {
                return false;
            }
        } else if (!sourceName.equals(other.sourceName)) {
            return false;
        }
        return true;
    }
}