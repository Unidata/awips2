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
package com.raytheon.rcm.config.awips1;

import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;

/**
 * Utility class for dealing with AWIPS 1 RPS lists.
 * 
 * <pre>
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  2009                     dfriedma    Initial version
 *  2012-04-30   DR 14908    D. Friedman Require radar name for valid RPS
 *                                       file names.
 * </pre>
 *
 */
public class Awips1RpsListUtil {
    // is 'maint' an opMode??
    public static class Selector {
        public String radar; // null === national

        public int opMode;

        public int vcp;

        public String comment; // extra stuff at end
    }

    public static Request[] parse(Scanner s) {
        ArrayList<Request> requests = new ArrayList<Request>();

        // Ref ProductRequestList.C : ProductRequestList::readList
        final int MAX_NAME_FIELD_SIZE = 41;

        // Skip the first five lines
        for (int i = 0; i < 5 && s.hasNextLine(); ++i)
            s.nextLine();

        while (s.hasNextLine()) {
            String line = s.nextLine();
            int pos;
            if ((pos = line.indexOf('\t')) != -1)
                line = line.substring(pos + 1);
            else
                line = line.substring(Math.min(MAX_NAME_FIELD_SIZE,
                        line.length()));
            Scanner ls = new Scanner(line);
            try {
                Request req = new Request();
                String mne = ls.next();
                req.selectCurrent();
                req.count = Request.CONTINUOUS;
                req.comment = mne;
                req.productCode = ls.nextShort();
                /* int levels = */ls.nextInt(); // not used
                /* int res = */ls.nextInt(); // not used
                /* String layerCode = */ls.next(); // not used
                int elev = ls.nextInt();
                /* int contourInterval = */ls.nextInt(); // only for (obsolete?)
                                                         // echo tops contour
                req.highPriority = ls.nextInt() != 0;

                /*
                 * The next two fields tend to be smushed together. Scanner
                 * cannot handle this.
                 */
                String field = ls.next();
                try {
                    if (Character.isLetter(field.charAt(field.length() - 1))) {
                        String f1 = field.substring(0, field.length() - 1);
                        req.interval = Short.parseShort(f1);
                        req.mapRequested = field.charAt(field.length() - 1) == 'Y';
                        field = null;
                    }
                } catch (RuntimeException e) {
                    throw new NoSuchElementException(e.toString());
                }
                if (field != null) {
                    req.interval = Short.parseShort(field);
                    req.mapRequested = ls.next().equals("Y");
                }

                // The remaining fields are optional

                // layers only used for product 137
                int lowerLayer = -1;
                int upperLayer = -1;

                boolean multiCuts = false;

                if (ls.hasNext())
                    lowerLayer = ls.nextInt();
                if (ls.hasNext())
                    upperLayer = ls.nextInt();

                if (ls.hasNext())
                    multiCuts = ls.next().equals("Y");

                if (elev != -1) { // TODO: replace with logic for with ones
                                  // actually use elevation..
                    if (multiCuts)
                        req.selectAllElevations(elev & 0x1fff);
                    else {
                        // The AWIPS 1 code just sets this directly.
                        req.pdw22 = elev;
                    }
                }

                // also ProductRequestEntry::checkUpLowLayer

                int endHour = -1; // TODO: these too...
                int timeSpan = 0;

                if (ls.hasNext())
                    endHour = ls.nextInt(); // TODO: these too...
                if (ls.hasNext())
                    timeSpan = ls.nextInt();

                // copied from ProductRequestList::readList
                if (endHour < -1 || endHour > 1440)
                    endHour = -1;
                if (timeSpan < 0 || timeSpan > 1440)
                    timeSpan = 0;

                switch (req.productCode) {
                case 56:
                    req.setStormSpeed(-1);
                    break;
                case 137:
                    req.setBottomAltitude(lowerLayer);
                    req.setTopAltitude(upperLayer);
                    break;
                // TODO: copied from AWIPS1; not verified
                case 31:
                case 150:
                case 151:
                case 173:
                    req.setTimeSpan(timeSpan);
                    req.setEndHour(endHour);
                    break;
                case 35:
                case 36:
                case 37:
                case 38:
                case 41:
                case 57:
                case 58:
                case 59:
                case 61:
                case 141:
                case 149:
                    /*
                     * On the SPG, these products have a "mini-volume number"
                     * parameter. It seems to be safe to set this for the RPG
                     * even though it is not used.
                     */
                    req.setMiniVolume(lowerLayer);
                    break;
                default:
                    // nothing
                }

                requests.add(req);
            } catch (NoSuchElementException e) {
            }
        }

        return requests.toArray(new Request[requests.size()]);
    }

    protected static final Pattern selectorPattern = Pattern
            .compile("^(.+)\\.(.+)\\.VCP(\\d+)(?:\\.(.*))?$");

    protected static final Pattern maintPattern = Pattern
            .compile("^([^\\.]+)\\.maint(?:\\.(.*))?$");

    public static Selector parseName(String name) {
        Matcher m = selectorPattern.matcher(name);
        if (m.matches()) {
            Selector sel = new Selector();

            sel.radar = m.group(1).toLowerCase();

            String opModeString = m.group(2).toLowerCase();
            if (opModeString.equals("clear-air"))
                sel.opMode = GSM.OP_MODE_CLEAR_AIR;
            else if (opModeString.equals("storm"))
                sel.opMode = GSM.OP_MODE_STORM;
            else if (opModeString.equals("maint"))
                sel.opMode = GSM.OP_MODE_MAINTENANCE;

            sel.vcp = Integer.parseInt(m.group(3));

            sel.comment = m.group(4);

            return sel;
        }
        m = maintPattern.matcher(name);
        if (m.matches()) {
            Selector sel = new Selector();
            sel.radar = m.group(1).toLowerCase();
            sel.opMode = GSM.OP_MODE_MAINTENANCE;
            sel.vcp = 0;
            return sel;
        }
        return null;
    }

    /*
     * AWIPS 1 handling of the national and default local RPS lists for TDWRs
     * has an additional quirk: The 0.5, 1.0, 3.0, 6.0 elevation angles are
     * replaced with the lowest four elevation angles requested for products 180
     * - 183 in the site's local list. This essentially turns the national rps
     * list into a template.
     * 
     * There may be a need for something like and RpsListTemplate in the config
     * package, but the extra logic will be kept here for now.
     * 
     * The intent is to use the lowest four available elevation angles, so this
     * implementation uses the actual list of elevations angles instead of
     * relying on the existence of a configuration file.
     * 
     * Note that there are two instances of this logic in AWIPS 1. One is
     * RadarServer, the other is in localization. The latter (which is applied
     * to the default local lists) does not examine the product ID.
     */

    public static RpsList maybeTransformForTDWR(RadarConfig rc, RpsList list,
            int[] cuts) {

        if (list != null && Util.getRadarType(rc) == RadarType.TDWR) {
            list = (RpsList) list.clone();

            int[] fourLowest = new int[4];
            int i = 0;
            int nLowest = 0;

            /*
             * The initial 0.6 elevation angle is part of the long range scan
             * and not included.
             */
            if (cuts.length > 0 && cuts[0] == 6)
                ++i;

            while (nLowest < fourLowest.length && i < cuts.length)
                fourLowest[nLowest++] = cuts[i++];

            for (Request r : list.getRequests()) {
                if (r.productCode >= 180 && r.productCode <= 183) {
                    int sel = r.getElevationSelection();
                    if (sel == Request.SPECIFIC_ELEVATION
                            || sel == Request.ALL_ELEVATIONS) {
                        int newi = -1;
                        switch (r.getElevationAngle()) {
                        case 5:
                            newi = 0;
                            break;
                        case 10:
                            newi = 1;
                            break;
                        case 30:
                            newi = 2;
                            break;
                        case 60:
                            newi = 3;
                            break;
                        }
                        if (newi >= 0 && newi < nLowest) {
                            if (sel == Request.SPECIFIC_ELEVATION)
                                r.setElevationAngle(fourLowest[newi]);
                            else if (sel == Request.ALL_ELEVATIONS)
                                r.selectAllElevations(fourLowest[newi]);
                        }
                        // else maybe should remove this request
                    }
                }
            }
        }

        return list;
    }

}
