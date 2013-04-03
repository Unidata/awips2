package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import org.apache.http.HttpStatus;

import edu.uci.ics.crawler4j.crawler.CrawlConfig;
import edu.uci.ics.crawler4j.crawler.Page;
import edu.uci.ics.crawler4j.fetcher.PageFetchResult;
import edu.uci.ics.crawler4j.fetcher.PageFetcher;
import edu.uci.ics.crawler4j.parser.Parser;
import edu.uci.ics.crawler4j.url.WebURL;

/**
 * Downloader
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

/**
 * This class is a demonstration of how crawler4j can be used to download a
 * single page and extract its title and text.
 */
public class Downloader {

    private Parser parser;

    private PageFetcher pageFetcher;

    private String url;

    public Downloader(String url) {
        CrawlConfig config = new CrawlConfig();
        parser = new Parser(config);
        pageFetcher = new PageFetcher(config);
        this.url = url;
    }

    public Page download() {
        WebURL curURL = new WebURL();
        curURL.setURL(url);
        PageFetchResult fetchResult = null;
        try {
            fetchResult = pageFetcher.fetchHeader(curURL);
            if (fetchResult.getStatusCode() == HttpStatus.SC_OK) {
                try {
                    Page page = new Page(curURL);
                    fetchResult.fetchContent(page);
                    if (parser.parse(page, curURL.getURL())) {
                        return page;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } finally {
            fetchResult.discardContentIfNotConsumed();
        }
        return null;
    }
}
