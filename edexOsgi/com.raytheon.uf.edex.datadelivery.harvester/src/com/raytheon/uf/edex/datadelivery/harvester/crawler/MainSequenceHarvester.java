package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.harvester.interfaces.IStoreLink;
import com.raytheon.uf.edex.datadelivery.retrieval.Link;
import com.raytheon.uf.edex.datadelivery.retrieval.LinkStore;

import edu.uci.ics.crawler4j.crawler.WebCrawler;
import edu.uci.ics.crawler4j.url.WebURL;

/**
 * Harvester
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2011    218      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MainSequenceHarvester extends WebCrawler implements IStoreLink {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MainSequenceHarvester.class);

    private final Pattern FILTERS;

    private final String topurl;

    private final String name;

    private final LinkStore links;

    private List<Pattern> IGNORE_FILTERS = null;

    public MainSequenceHarvester(String name, String topurl, String pageKey,
            LinkStore links, List<String> ignores) {
        this.name = name;
        this.topurl = topurl;
        this.links = links;
        FILTERS = Pattern.compile(pageKey);

        // These are one's we are specifically ignoring
        if (ignores != null && ignores.size() > 0) {
            IGNORE_FILTERS = new ArrayList<Pattern>();
            for (String ignore : ignores) {
                if (ignore != null && !ignore.equals("")) {
                    IGNORE_FILTERS.add(Pattern.compile(ignore));
                }
            }
        }
    }

    /**
     * You should implement this function to specify whether the given url
     * should be crawled or not (based on your crawling logic).
     */
    @Override
    public boolean shouldVisit(WebURL url) {
        String href = url.getURL();

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

        if (FILTERS.matcher(href).find()) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Storing url: " + href);
            }

            // found link we wanted, store it and don't crawl this URL
            storeLink(url);
            return false;
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug("Traverse url? " + href.startsWith(topurl)
                    + " " + href);
        }

        return href.startsWith(topurl);
    }

    @Override
    public void storeLink(WebURL url) {
        Link link = new Link(name, url.getURL());
        links.addLink(url.getURL(), link);
    }
}