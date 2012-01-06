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
package com.raytheon.uf.edex.database;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DatabaseSessionConfiguration {
    protected List<Pattern> includes;

    protected List<Pattern> excludes;

    public void setIncludes(List<String> includes) {
        if (includes != null) {
            this.includes = new ArrayList<Pattern>(includes.size());
            for (String include : includes) {
                this.includes.add(Pattern.compile(include));
            }
        }
    }

    public void setExcludes(List<String> excludes) {
        if (excludes != null) {
            this.excludes = new ArrayList<Pattern>(excludes.size());
            for (String include : excludes) {
                this.excludes.add(Pattern.compile(include));
            }
        }
    }

    public boolean matches(String value) {
        boolean rval = false;

        if (includes != null) {
            for (Pattern p : includes) {
                if (p.matcher(value).matches()) {
                    rval = true;
                    break;
                }
            }
        } else {
            rval = true;
        }

        if (excludes != null) {
            for (Pattern p : excludes) {
                if (p.matcher(value).matches()) {
                    rval = false;
                    break;
                }
            }
        }

        return rval;
    }
}
