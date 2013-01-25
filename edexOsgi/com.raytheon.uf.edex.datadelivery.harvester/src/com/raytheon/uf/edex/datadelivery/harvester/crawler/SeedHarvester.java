package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.regex.Pattern;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.harvester.config.ProtoCollection;
import com.raytheon.uf.edex.datadelivery.harvester.util.OpenDapSeedScanUtilities;

import edu.uci.ics.crawler4j.crawler.Page;
import edu.uci.ics.crawler4j.crawler.WebCrawler;
import edu.uci.ics.crawler4j.parser.HtmlParseData;
import edu.uci.ics.crawler4j.url.WebURL;

/**
 * Harvest Seed (Collections) URL's
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 4, 2012  1038       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SeedHarvester extends WebCrawler {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SeedHarvester.class);

    private final Pattern FILTERS;

    private List<Pattern> KNOWN_FILTERS = null;

    private List<Pattern> IGNORE_FILTERS = null;

    private final HashMap<String, ProtoCollection> links;

    private final String topurl;

    SeedHarvester(String topurl, String pageKey,
            HashMap<String, ProtoCollection> links, List<String> knowns,
            List<String> ignores) {
        this.links = links;
        this.topurl = topurl;
        // create the ignored patterns for NODEs you want to skip
        // NOTE we add a path separator because we want to make sure
        // this is only nodes we already know. Not derivative nodes of
        // of one you might already have
        if (knowns != null && knowns.size() > 0) {
            KNOWN_FILTERS = new ArrayList<Pattern>();
            for (String known : knowns) {
                if (known != null && !knowns.equals("")
                        && !knowns.equals("null")) {
                    KNOWN_FILTERS.add(Pattern.compile(topurl + known + "/"));
                }
            }
        }
        // These are one's we are specifically ignoring
        if (ignores != null && ignores.size() > 0) {
            IGNORE_FILTERS = new ArrayList<Pattern>();
            for (String ignore : ignores) {
                if (ignore != null && !ignore.equals("")) {
                    IGNORE_FILTERS.add(Pattern.compile(ignore));
                }
            }
        }

        // looking for what the payload url ends with here
        FILTERS = Pattern.compile(".*(\\.(" + pageKey + "))$");
    }

    /**
     * Creates a new collection
     * 
     * @param seedUrl
     * @param slimurl
     * @param sdf
     * @param dateParse
     * @param chunks
     * @param depth
     * @return
     */
    private ProtoCollection createNewCollection(String seedUrl, String slimurl,
            SimpleDateFormat sdf, String dateParse, String[] chunks, int depth) {
        // brand new collection, we need to find urlKey
        // here too...
        ProtoCollection coll = new ProtoCollection(seedUrl, slimurl);
        String urlKey = null;

        if (sdf != null && dateParse != null) {
            Date idate = OpenDapSeedScanUtilities.getDate(sdf, dateParse);
            // hack off the date from the extra stuff need
            // to reconstruct URL, some don't have dates
            // handle them differently
            String fdate = sdf.format(idate);
            urlKey = chunks[depth].replaceAll(fdate, "");

            LinkedHashMap<Integer, SimpleDateFormat> formats = new LinkedHashMap<Integer, SimpleDateFormat>();
            formats.put(depth, sdf);
            coll.setFormats(formats);
            LinkedHashMap<Integer, ArrayList<Date>> dates = new LinkedHashMap<Integer, ArrayList<Date>>();
            ArrayList<Date> idates = new ArrayList<Date>();
            idates.add(idate);
            dates.put(depth, idates);
            coll.setDates(dates);
        } else {
            urlKey = "";
        }
        coll.setUrlKey(urlKey);
        statusHandler.info("Added new Collection: " + seedUrl);
        return coll;
    }

    /**
     * You should implement this function to specify whether the given url
     * should be crawled or not (based on your crawling logic).
     */
    @Override
    public boolean shouldVisit(WebURL url) {
        String href = url.getURL();
        // System.out.println("Checking url: " + href);

        // Compare with Collections we know about
        if (KNOWN_FILTERS != null && KNOWN_FILTERS.size() > 0) {
            for (Pattern pat : KNOWN_FILTERS) {
                if (pat.matcher(href).find()) {
                    // URL contains a Collection we already know of
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler
                                .debug("Refusing to traverse, KNOWN pattern: "
                                        + pat.toString() + " " + href);
                    }
                    return false;
                }
            }
        }

        // Collections we don't care about
        if (IGNORE_FILTERS != null && IGNORE_FILTERS.size() > 0) {
            for (Pattern pat : IGNORE_FILTERS) {
                if (pat.matcher(href).find()) {
                    // URL contains a (name)collection we wish to ignore
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        statusHandler
                                .debug("Refusing to traverse, IGNORE pattern: "
                                        + pat.toString() + " " + href);
                    }
                    return false;
                }
            }
        }

        if (href.startsWith(topurl)) {
            // We want everything here that applies
            return true;
        }

        return false;
    }

    /**
     * Update the dates in the collection
     * 
     * @param seedUrl
     * @param depth
     * @param sdf
     * @param dateParse
     */
    private void updateCollection(String seedUrl, int depth,
            SimpleDateFormat sdf, String dateParse) {
        // we have one already
        ProtoCollection coll = links.get(seedUrl);
        // only one format can exist within a collection
        // at a given depth
        if (!coll.getFormats().containsKey(depth)) {
            coll.getFormats().put(depth, sdf);
        }

        // collect all dates, we'll use this to
        // determine
        // first and last date available
        if (!coll.getDates().containsKey(depth)) {
            ArrayList<Date> idates = new ArrayList<Date>();
            idates.add(OpenDapSeedScanUtilities.getDate(sdf, dateParse));
            coll.getDates().put(depth, idates);
        } else {
            Date sdate = OpenDapSeedScanUtilities.getDate(sdf, dateParse);
            if (!coll.getDates().get(depth).contains(sdate)) {
                coll.getDates().get(depth).add(sdate);
            }
        }
    }

    /**
     * This function is called when a page is fetched and ready to be processed
     * by your program.
     */
    @Override
    public void visit(Page page) {

        String url = page.getWebURL().getURL();

        if (page.getParseData() instanceof HtmlParseData) {

            if (FILTERS.matcher(url).find()) {

                // first hack off the provider root URL
                String slimurl = url.replaceAll(topurl, "");
                // System.out.println("Found new URL: " + slimurl);
                // carve up this url
                String[] chunks = OpenDapSeedScanUtilities.breakupUrl(slimurl);
                String chunk0 = chunks[0];
                String seedUrl = null;
                // parse it piece by piece
                for (int depth = 0; depth < chunks.length - 1; depth++) {
                    // we're looking for dates
                    // remove any collection matches that might popup
                    // these cause problems especially when there is a number
                    // in the name. It throws the regex parsing off.
                    String dateParse = chunks[depth];
                    if (dateParse.startsWith(chunk0)) {
                        dateParse = dateParse.replaceAll(chunk0, "");
                    }

                    SimpleDateFormat sdf = OpenDapSeedScanUtilities
                            .getDateFormat(dateParse);
                    if (sdf != null) {
                        // Once you find the date portion of the URL,
                        // the collection name portions are the previous
                        // chunks of the URL.
                        // check to see if we already have a ProtoCollection
                        if (seedUrl == null) {
                            seedUrl = OpenDapSeedScanUtilities.getUrlAtDepth(
                                    depth - 1, slimurl);
                        }

                        synchronized (links) {

                            if (links.containsKey(seedUrl)) {
                                updateCollection(seedUrl, depth, sdf, dateParse);
                            } else {
                                ProtoCollection coll = createNewCollection(
                                        seedUrl, slimurl, sdf, dateParse,
                                        chunks, depth);
                                links.put(seedUrl, coll);
                            }
                        }
                    }
                }
                // fall back for things that have no dates in the format
                // you have to assume the date is current in these cases
                // OPeNDAP will have to determine what the times are on it
                if (seedUrl == null) {
                    seedUrl = OpenDapSeedScanUtilities.reconstructUrl(chunks,
                            chunks.length - 1);

                    synchronized (links) {

                        if (links.containsKey(seedUrl)) {
                            // do nothing, these data sets will be picked up in
                            // main sequence scans of the already existing
                            // directory.
                        } else {
                            // brand new sub-directory with no date formatting.
                            int depth = 0;
                            ProtoCollection coll = createNewCollection(seedUrl,
                                    slimurl, null, null, chunks, depth);
                            links.put(seedUrl, coll);
                        }
                    }
                }
            }
        }
    }
}
