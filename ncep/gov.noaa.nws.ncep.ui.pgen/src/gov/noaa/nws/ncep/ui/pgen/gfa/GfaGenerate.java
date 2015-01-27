/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaGenerate
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.nvl;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * GFA Generate functionality.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 02/11					J. Wu		Save result to Pgen working dir.
 * 04/11					J. Wu		update state list order and create additional
 * 										smear for smears with two FA areas.
 * 02/12		#672		J. Wu		Re-order states list based on FA area.
 * 05/12		#610		J. Wu		Add an empty Gfa to pass issue/until time.
 * 05/12		#610		J. Wu		Assign issue/until times if they are missing.
 * 11/12		#952		J. Wu		Format for "TURB_HI" and "TURB-LO". 
 * 12/12		#953		J. Wu		Use deep copy when generating XML to avoid 
 * 04/13        #977        S. Gilbert  PGEN Database support
 * 05/13		#610		J. Wu		Implemented FZLVL range (TTR425) 
 * 07/13		?			J. Wu		Move state list ordering to GfaRules.
 * 08/13		TTR714/715	J. Wu		Fixed issue type and times.
 * 10/14        TTR714      J. Wu       Check issue type in get_status.xml.
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class GfaGenerate {

    private static final String TEXT_TYPE = "TEXT";

    private GeometryFactory gf = new GeometryFactory();

    private static final String ISSUE_TYPE_FROM_OUTLOOK = "ISSUE_TYPE_FROM_OUTLOOK";

    // private final static Logger logger = Logger.getLogger(GfaGenerate.class);
    private Transformer transformer;

    private String XSLT_FILE = PgenStaticDataProvider.getProvider()
            .getFileAbsolutePath(
                    PgenStaticDataProvider.getProvider()
                            .getPgenLocalizationRoot()
                            + "xslt"
                            + File.separator
                            + "airmet"
                            + File.separator
                            + "gfa_product.xsl");

    /**
     * Generates product text out of the list.
     * 
     * @param all
     * @param categories
     * @param areas
     * @param dataURI
     * @throws IOException
     * @throws JAXBException
     */
    public StringBuilder generate(List<Gfa> all, List<String> areas,
            List<String> categories, String dataURI) throws IOException,
            JAXBException {

        StringBuilder sb = new StringBuilder();
        StringBuilder temp = new StringBuilder();
        String cycle = PgenCycleTool.pad(PgenCycleTool.getCycleHour());

        List<Gfa> adjusted = new ArrayList<Gfa>();

        /*
         * Create an additional smear for the adjacent area.
         */
        for (Gfa g : all) {
            Gfa sg = createAdjacentGfa(g);

            adjusted.add(g);

            if (sg != null) {
                adjusted.add(sg);
            }
        }

        /*
         * Find issue type for an Airmet's associated Outlook. Note: TTR 714 -
         * J. Wu: this may cause other issues. Instead, we should check the
         * issue types in "get_status.xsl".
         */
        // trackOtlkIssueTypeToAirmet(adjusted);

        /*
         * Find GFA smears in each area and hazard category, generate xml, and
         * save to a file.
         */
        for (String category : categories) {
            for (String area : areas) {

                List<Gfa> ret = filterSelected(adjusted, area, category);

                // If no issue/until times, assign them.
                for (Gfa de : ret) {
                    if (!de.isSnapshot()
                            && de.getAttribute(Gfa.ISSUE_TIME) == null) {
                        GfaRules.assignIssueTime(de);
                    }
                }

                // Add to a in-memory product for converting
                String fileName = "AIRMET_" + category + "_" + area + "_"
                        + cycle + ".txt";
                // fileName = PgenUtil.getPgenActivityTextProdPath()
                // + File.separator + fileName;

                Product p = new Product();
                Layer l = new Layer();
                p.addLayer(l);

                /*
                 * Find freezing range for this area!
                 */
                String fzlvlRange = findFreezingRange(all, ret, area, category);

                /*
                 * Note - needs to use a copy so it won't change the parent of
                 * the original G-Airmets, e.g. l.add( de ) will set the parent
                 * of the 'de" to be "l"!
                 */
                for (Gfa gss : ret) {
                    Gfa gfaCopy = gss.copy();
                    /*
                     * String otlkIssueType = nvl(gfaCopy
                     * .getGfaValue(ISSUE_TYPE_FROM_OUTLOOK)); if
                     * (!otlkIssueType.equals("NRML")) {
                     * gfaCopy.setGfaIssueType( otlkIssueType ); }
                     */
                    if (fzlvlRange != null) {
                        gfaCopy.setGfaValue(Gfa.FZL_RANGE, fzlvlRange);
                    }
                    l.add(gfaCopy);
                }
                // l.add( ret );

                List<Product> pList = new ArrayList<Product>();
                pList.add(p);

                Products products = ProductConverter.convert(pList);

                // Needs to add an empty Gfa to carry the issue/until for proper
                // formatting.
                if (ret.size() == 0) {
                    addNullGfa(products, category, fzlvlRange);
                }

                String xml = SerializationUtil.marshalToXml(products);

                if (sb.length() > 0 && !sb.toString().endsWith("\n\n")) {
                    sb.append("\n\n");
                }

                String prdXml = generateProduct(xml, category, area).trim();
                temp.append(prdXml);

                // saveToFile(temp, fileName);
                storeProduct(temp, fileName, dataURI);

                sb.append(temp);
                temp.setLength(0); // clear
            }
        }

        return sb;
    }

    private void storeProduct(StringBuilder temp, String fileName,
            String dataURI) {

        try {
            StorageUtils.storeDerivedProduct(dataURI, fileName, TEXT_TYPE,
                    temp.toString());
        } catch (PgenStorageException e) {
            StorageUtils.showError(e);
        }
    }

    public void saveToFile(StringBuilder sb, String fileName)
            throws IOException {

        FileTools.writeFile(fileName, sb.toString());

    }

    /**
     * Filters the list to return only gfa elements selected on Gfa Format
     * window (per category).
     * 
     * @param all
     * @param checked
     * @param cats
     * @return
     */
    private static List<Gfa> filterSelected(List<Gfa> all, String area,
            String category) {

        ArrayList<Gfa> ret = new ArrayList<Gfa>();

        int ii = 0;
        for (Gfa g : all) {

            GfaInfo.HazardCategory c = GfaInfo.getHazardCategory(g
                    .getGfaHazard());
            boolean inCategory = category.equals(c.toString());

            String[] s = nvl(g.getGfaArea()).split("-");
            boolean inArea = nvl(s[0]).contains(area);

            if (inArea && inCategory) {
                ret.add(g);
            }

            ii++;
        }

        return ret;
    }

    public String generateProduct(String prdxml, String category, String area) {
        System.out.println("\nprdxml is:\n" + prdxml + "\n");

        String xml1 = prdxml.replaceAll("TURB-HI", "TURB");
        String xml = xml1.replaceAll("TURB-LO", "TURB");

        String res = "";
        try {

            StreamSource xmlSource = new StreamSource(new StringReader(xml));
            if (transformer == null) {
                TransformerFactory tFactory = TransformerFactory.newInstance();
                Source xsltSource = new StreamSource(XSLT_FILE);
                transformer = tFactory.newTransformer(xsltSource);
            }

            StreamResult result = new StreamResult(new StringWriter());
            transformer.reset();
            transformer.setParameter("categories", category);
            transformer.setParameter("areas", area);
            transformer.transform(xmlSource, result);

            res = result.getWriter().toString().trim();
            System.out.println("\nres is:\n" + res + "\n");

        } catch (Exception e) {
            // logger.error( "", e );
            e.printStackTrace();
        }

        // logger.debug( res );

        if (res.contains("FRZLVL...")) {
            res = wrapFrzl(res);
        }

        return res;
    }

    /**
     * Create a new smear for smears with two FA areas and re-order the state
     * list.
     * 
     * The States in the primary area precede states in the adjacent area.
     * 
     * @param g
     *            Gfa to be processed
     * 
     * @return
     */
    private static Gfa createAdjacentGfa(Gfa g) {

        Gfa secondg = null;

        String[] s = nvl(g.getGfaArea()).split("-");

        if (s.length > 1) {
            String sname = new String(s[1].trim() + "-" + s[0].trim());

            // create an additional smear and re-order state list.
            secondg = g.copy();
            secondg.setGfaArea(sname);
            GfaRules.reorderStateList(secondg);
        }

        return secondg;

    }

    /**
     * Wraps the text string for FRZL.
     * 
     * @param frzl
     * @return
     */
    private String wrapFrzl(String frzl) {
        StringBuffer sb = new StringBuffer(frzl);

        int startIdx = sb.indexOf("\n", sb.indexOf("FRZLVL...")) + 1;
        int endIdx = sb.indexOf("....", startIdx);

        int ii = startIdx;
        int ii64 = 0;

        do {
            ii64 = ii + 64;
            int newline = sb.indexOf("\n", ii);

            if (newline <= ii64) { // new line less than 65 char, no wrap
                ii = newline + 1;
            } else { // need wrap
                for (int jj = ii64; jj >= ii; jj--) { // search ' ' or '-'
                                                      // between ii and ii64

                    if (sb.charAt(jj) == ' ' || sb.charAt(jj) == '-') {
                        sb.insert(jj + 1, "\n      ");
                        ii = jj + 2;
                        break;
                    }
                }
            }

        } while (ii64 < endIdx);

        return new String(sb);
    }

    /*
     * Adds an empty Gfa to the product so it can pass issue/until time for
     * correct formatting. Freezing range should be set if provided.
     * 
     * Note - this should be called only when there is no Gfa smears in the
     * "prds".
     */
    private void addNullGfa(Products prds, String category, String fzlRange) {

        gov.noaa.nws.ncep.ui.pgen.file.Gfa fgfa = new gov.noaa.nws.ncep.ui.pgen.file.Gfa();

        fgfa.setPgenCategory("MET");
        fgfa.setPgenType("GFA");

        // Hazard type & forecast hour with "-" are needed for xslt to retrieve
        // issue/until time.
        fgfa.setHazard(setFirstHazardType(category));
        fgfa.setFcstHr("0-6");

        // If freezing range is provided, set it and set hazard type as "FZLVL".
        if (fzlRange != null) {
            fgfa.setHazard("FZLVL");
            fgfa.setFzlRange(fzlRange);
        }

        //
        SimpleDateFormat sdf = new SimpleDateFormat("ddHHmm");

        Calendar cal = Calendar.getInstance();
        String timeStr = AirmetCycleInfo.getIssueTime();

        int hour = Integer.parseInt(timeStr.substring(0, 2));
        int min = Integer.parseInt(timeStr.substring(2));

        cal.set(Calendar.HOUR_OF_DAY, hour);
        cal.set(Calendar.MINUTE, min);
        cal.set(Calendar.SECOND, 0);

        fgfa.setIssueTime(sdf.format(cal.getTime()));

        cal = AirmetCycleInfo.getUntilTime();
        fgfa.setUntilTime(sdf.format(cal.getTime()));

        // Add to the product
        prds.getProduct().get(0).getLayer().get(0).getDrawableElement()
                .getGfa().add(fgfa);

    }

    /*
     * set the first hazard type that belongs to a "category" - SIERRA, TANGO,
     * ZULU.
     */
    private String setFirstHazardType(String category) {
        String type = "NONE";

        if (category.equals(GfaInfo.HazardCategory.SIERRA.toString())) {
            type = new String("IFR");
        } else if (category.equals(GfaInfo.HazardCategory.TANGO.toString())) {
            type = new String("TURB");
        } else if (category.equals(GfaInfo.HazardCategory.ZULU.toString())) {
            type = new String("ICE");
        }

        return type;

    }

    /*
     * Find the freezing range from FZLVL/M_FZLVL airmets.
     */
    private String findFreezingRange(List<Gfa> all, List<Gfa> selected,
            String area, String cat) {

        if (!cat.equalsIgnoreCase("ZULU")) {
            return null;
        }

        String fzlRange = null;
        String topStr = null;
        String botStr = null;

        /*
         * If no FZLVL/M_FZLVL in this area, find range using those outside of
         * this area, if any.
         */
        if (selected == null || selected.size() == 0) {
            int[] extRange = findExernalFzlvlRange(all, area);
            if (extRange[0] != -1) {
                topStr = padding(extRange[0]);
                botStr = padding(extRange[1]);
            }
        } else {
            /*
             * First - find the worst range from existing FZL_RANGE in FZLVL
             * airmets. If no existing range found, then find the worst
             * top/bottom from FZLVLs' levels ( top = level + 40 and bottom =
             * level - 40) Second - find the worst top/bottom from M_FZLVLs
             * 
             * Third - find the worst case range using info from step 1 and 2.
             */
            int[] existingRange = findExistingFzlRange(all, selected, area);
            int[] mfzlRange = findMfzlvlRange(all, selected, area);

            if (existingRange[0] == -1) {
                existingRange = findFzlvlLevelRange(all, selected, area);
            }

            if (existingRange[0] != -1 || mfzlRange[0] != -1) {
                topStr = padding(Math.max(existingRange[0], mfzlRange[0]));
            }

            if (existingRange[1] != 9999 || mfzlRange[1] != 9999) {
                botStr = padding(Math.min(existingRange[1], mfzlRange[1]));
            }
        }

        // Now create a range string.
        if (topStr != null && botStr != null) {
            fzlRange = area + ";" + topStr + ";" + botStr;
        }

        return fzlRange;

    }

    /*
     * Retrieve the worst FZLVL range from ranges existing in FZLVL
     * airmet/outlook. Such existing range should be in the form as
     * "MIA;160;40", retrievable as gfa.getGfaValue( Gfa.FZL_RANGE ).
     */
    private int[] findExistingFzlRange(List<Gfa> all, List<Gfa> selected,
            String area) {

        int[] topBot = { -1, 9999 };

        for (Gfa elem : selected) {

            String gtype = elem.getGfaHazard();

            if (!gtype.equalsIgnoreCase("FZLVL") || elem.isSnapshot()) {
                continue;
            }

            String range1 = elem.getGfaValue(Gfa.FZL_RANGE);
            if (range1 != null) {
                String[] rangeInfo = range1.split(";");
                if (rangeInfo.length >= 3
                        && rangeInfo[0].equalsIgnoreCase(area)) {
                    int top1 = -1;
                    int bot1 = -1;

                    try {
                        top1 = Integer.parseInt(rangeInfo[1]);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                    if (rangeInfo[2].equalsIgnoreCase("SFC")) {
                        bot1 = 0;
                    } else {
                        try {
                            bot1 = Integer.parseInt(rangeInfo[2]);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }

                    if (top1 >= 0 && bot1 >= 0) {
                        topBot[0] = Math.max(topBot[0], top1);
                        topBot[1] = Math.min(topBot[1], bot1);
                    }
                }
            }
        }

        return topBot;
    }

    /*
     * Retrieve the worst range from existing top/bottom of M_FZLVL
     * airmet/outlook. Such top/bottom are retrievable as gfa.getGfaTop() &
     * gfa.getGfaBottom().
     */
    private int[] findMfzlvlRange(List<Gfa> all, List<Gfa> selected, String area) {

        int[] topBot = { -1, 9999 };

        for (Gfa elem : selected) {

            String gtype = elem.getGfaHazard();

            if (!gtype.equalsIgnoreCase("M_FZLVL") || elem.isSnapshot()) {
                continue;
            }

            int top2 = -1;
            int bot2 = -1;

            if (elem.getGfaTop() != null) {
                try {
                    top2 = Integer.parseInt(elem.getGfaTop());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            String botStr = elem.getGfaBottom();
            if (botStr != null) {
                if (botStr.equalsIgnoreCase("SFC")) {
                    bot2 = 0;
                } else {
                    try {
                        bot2 = Integer.parseInt(elem.getGfaBottom());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }

            if (top2 >= 0 && bot2 >= 0) {
                topBot[0] = Math.max(topBot[0], top2);
                topBot[1] = Math.min(topBot[1], bot2);
            }

        }

        return topBot;

    }

    /*
     * Retrieve the worst range from existing levels of FZLVL airmet/outlook.
     * Such levels are retrievable as gfa.getGfaValue( Gfa.LEVEL ).
     */
    private int[] findFzlvlLevelRange(List<Gfa> all, List<Gfa> selected,
            String area) {

        int[] topBot = { -1, 9999 };
        for (Gfa elem : selected) {

            String gtype = elem.getGfaHazard();

            if (!gtype.equalsIgnoreCase("FZLVL") || elem.isSnapshot()) {
                continue;
            }

            int top2 = -1;
            int bot2 = -1;

            String levelStr = elem.getGfaValue(Gfa.LEVEL);
            int lvl = -1;

            if (levelStr != null) {

                if (levelStr.equalsIgnoreCase("SFC")) {
                    top2 = 40;
                    bot2 = 0;
                } else {
                    try {
                        lvl = Integer.parseInt(levelStr);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                    if (lvl >= 0 && lvl <= 40) {
                        top2 = 40;
                        bot2 = 0;
                    } else {
                        top2 = lvl + 40;
                        bot2 = lvl - 40;
                    }
                }
            }

            if (top2 >= 0 && bot2 >= 0) {
                topBot[0] = Math.max(topBot[0], top2);
                topBot[1] = Math.min(topBot[1], bot2);
            }

        }

        return topBot;

    }

    /*
     * Find the top and base levels for a FA area that is not intersected by any
     * FZLVL contours. It sorts all external fzlvls by distance from the area
     * centroid, then uses the level from that contour. If the contour is to the
     * left of the area then the answer is base = contour level, top = base +
     * 040. If the contour is to the right of the area then the answer top =
     * contour level, base = top - 040.
     * 
     * See legacy af_getExternalFzlvlRng() in af_getAirmetXml.c.
     * 
     * Note (1) only FZLVLs are used, not M_FZLVLs. (2) All FZLVLs are used,
     * including snapshots.
     */
    private int[] findExernalFzlvlRange(List<Gfa> all, String area) {

        int[] topBot = { -1, 9999 };

        HashMap<String, Geometry> areaBnds = GfaClip.getInstance()
                .getFaAreaBounds();

        Coordinate center = gf
                .createLinearRing(areaBnds.get(area).getCoordinates())
                .getCentroid().getCoordinate();

        Gfa closestGfa = null;
        double minDist = Double.MAX_VALUE;

        for (Gfa elem : all) {

            String gtype = elem.getGfaHazard();

            if (!gtype.equalsIgnoreCase("FZLVL")) {
                continue;
            }

            // Find distance from the FA Area's centroid to this FZLVL.
            double dist = distance(elem, center);
            if (dist < minDist) {
                minDist = dist;
                closestGfa = elem;
            }

        }

        if (closestGfa != null) {
            boolean isLeft = GfaSnap.getInstance().atLeft(center,
                    closestGfa.getLinePoints(), closestGfa.isClosedLine(), 0.0);

            String levelStr = closestGfa.getGfaValue(Gfa.LEVEL);
            int lvl = -1;

            if (levelStr != null) {

                if (levelStr.equalsIgnoreCase("SFC")) {
                    lvl = 0;
                } else {
                    try {
                        lvl = Integer.parseInt(levelStr);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }

            if (lvl >= 0) {
                if (!isLeft) { // FZLVL is at right of FA area
                    topBot[0] = lvl + 40;
                    topBot[1] = lvl;
                } else { // FZLVL is at left of FA area
                    topBot[0] = lvl;
                    topBot[1] = lvl - 40;
                    if (topBot[1] < 0)
                        topBot[1] = 0;
                }
            }
        }

        return topBot;

    }

    /*
     * Pad a value between 0 to 100 in format of "0xx".
     */
    private String padding(int value) {

        StringBuilder str = new StringBuilder();
        if (value <= 0) {
            str.append("SFC");
        } else {
            if (value < 100) {
                str.append("0" + value);
            } else {
                str.append(value);
            }
        }

        return str.toString();

    }

    /*
     * Find the distance from a given point to a Gfa line or polygon.
     */
    private double distance(Gfa gfa, Coordinate loc) {

        double dist = Double.MAX_VALUE;
        double minDist = Double.MAX_VALUE;

        Object pts[] = gfa.getPoints().toArray();

        for (int ii = 0; ii < pts.length; ii++) {

            if (ii == pts.length - 1) {
                if (gfa.isClosedLine()) {
                    dist = PgenResource.distanceFromLineSegment(loc,
                            (Coordinate) pts[ii], (Coordinate) pts[0]);
                } else {
                    break;
                }
            } else {
                dist = PgenResource.distanceFromLineSegment(loc,
                        (Coordinate) pts[ii], (Coordinate) pts[ii + 1]);
            }

            if (dist < minDist) {
                minDist = dist;
            }
        }

        return minDist;

    }

    /**
     * Find if a smear has an associated outlook that are generated from the
     * same series of snapshots (with same hazard type, tag and desk.
     * 
     * This is used to get around the issue when a pair of airmet and outlook is
     * generated from the same series of snapshots, and the airmet's issue type
     * is "NRML" while outlook's issue type is not "NRML". In this case, the
     * formatted header should show the outlook's issue type.
     * 
     * Note: TTR 714 - 10/2014: for the above case, we should modify
     * "get_status.xsl" to get the formatted header by looping through all
     * airmets and outlooks instead of modifying it here.
     * 
     * @param all
     * @return
     */
    private static void trackOtlkIssueTypeToAirmet(List<Gfa> all) {

        for (Gfa gg : all) {
            gg.setGfaValue(ISSUE_TYPE_FROM_OUTLOOK, "NRML");
            if (gg.isAirmet() && "NRML".equalsIgnoreCase(gg.getGfaIssueType())) {
                String akey = gg.getGfaHazard() + gg.getGfaTag()
                        + gg.getGfaDesk();
                for (Gfa gotlk : all) {
                    if (gotlk.isOutlook()
                            && !("NRML".equalsIgnoreCase(gotlk
                                    .getGfaIssueType()))) {
                        String okey = gotlk.getGfaHazard() + gotlk.getGfaTag()
                                + gotlk.getGfaDesk();
                        if (okey.equals(akey)) {
                            gg.setGfaValue(ISSUE_TYPE_FROM_OUTLOOK,
                                    gotlk.getGfaIssueType());
                            break;
                        }
                    }
                }
            }
        }
    }

}
