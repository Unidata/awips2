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
package com.raytheon.uf.viz.app.launcher.utilities;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.app.launcher.bundle.Entity;
import com.raytheon.uf.viz.app.launcher.bundle.Launcher;
import com.raytheon.uf.viz.app.launcher.exception.AppLauncherException;

/**
 * Utility class to support the application launching facility. All methods are
 * static.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009 2081       mfegan     Initial creation
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */
public final class AppLauncherUtilities {
    /** default separator used for string joins */
    public static final String DEF_SEPARATOR = " ";

    /* prevent instantiation */
    private AppLauncherUtilities() {
    }

    /**
     * Provides a facility similar to the Perl 5 join() function.
     * 
     * @param pieces
     *            array of strings to join
     * @param sep
     *            separator value for the join
     * 
     * @return the resulting string
     */
    public static String join(String[] pieces, String sep) {
        int n = pieces.length;
        if (n == 0) {
            return "";
        } else if (n == 1) {
            return pieces[0];
        }
        StringBuffer sb = new StringBuffer();
        sb.append(pieces[0]);
        for (int i = 1; i < n; i++) {
            sb.append(sep);
            sb.append(pieces[i]);
        }
        return sb.toString();
    }

    /**
     * Provides a facility similar to the Perl 5 join() function.
     * 
     * @param pieces
     *            array of strings to join
     * @param sep
     *            separator value for the join
     * 
     * @return the resulting string
     */
    public static String join(String[] pieces, char sep) {
        return join(pieces, String.valueOf(sep));
    }

    /**
     * Provides a facility similar to the Perl 5 join() function. String are
     * joined using the {@link #DEF_SEPARATOR default separator}.
     * 
     * @param pieces
     *            array of strings to join
     * 
     * @return the resulting string
     */
    public static String join(String[] pieces) {
        return join(pieces, DEF_SEPARATOR);
    }

    /**
     * Converts a Set containing Entity objects into a String array. The strings
     * are obtained by calling the Entity object's toString() method.
     * 
     * @param set
     *            the set of Entity objects to convert
     * 
     * @return the array of converted strings
     */
    public static String[] convEntitySetToStringArray(Set<Entity> set) {
        ArrayList<String> list = new ArrayList<String>();
        for (Entity entry : set) {
            list.add(entry.toString());
        }
        return list.toArray(new String[] {});

    }

    /**
     * converts a Map<String,String> to a "property like" List. The entries in
     * the list will have a "key=value" format.
     * 
     * @param map
     *            Map containing the values to convert
     * 
     * @return the converted Map
     */
    public static List<String> convMapToStringList(Map<String, String> map) {
        ArrayList<String> list = new ArrayList<String>();
        for (Entry<String, String> entry : map.entrySet()) {
            list.add(entry.getKey() + "=" + entry.getValue());
        }
        return list;
    }

    public static String[] convListToStringArray(List<?> list) {
        ArrayList<String> retval = new ArrayList<String>();
        for (Object entry : list) {
            retval.add(entry.toString());
        }
        return retval.toArray(new String[] {});
    }

    /**
     * Loads a Launcher bundle from the specified file and converts it to a
     * Launcher object.
     * 
     * @param bundleLocation
     *            the path to the Launcher bundle
     * 
     * @return the Launcher object
     * 
     * @throws AppLauncherException
     *             if the file load/conversion fails
     */
    public static Launcher importLauncherBundle(String bundleLocation)
            throws AppLauncherException {
        try {
            File bundle = PathManagerFactory.getPathManager().getStaticFile(
                    bundleLocation);
            return JAXB.unmarshal(bundle, Launcher.class);
        } catch (Exception e) {
            throw new AppLauncherException(
                    "Unable to import App Launcher bundle "
                            + StringUtil.printString(bundleLocation), e);
        }
    }
}
