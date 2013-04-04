package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import edu.uci.ics.crawler4j.crawler.Page;
import edu.uci.ics.crawler4j.parser.HtmlParseData;
import edu.uci.ics.crawler4j.parser.ParseData;

/**
 * Process a Page
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

public class PageProcessor {

    public void processPage(Page page) {

        System.out.println("Process Page: " + page.getWebURL().getURL());

        if (page != null) {
            ParseData parseData = page.getParseData();
            if (parseData != null) {
                if (parseData instanceof HtmlParseData) {
                    HtmlParseData htmlParseData = (HtmlParseData) parseData;
                    System.out.println("Title: " + htmlParseData.getTitle());
                    System.out.println("Text length: "
                            + htmlParseData.getText().length());
                    System.out.println("Html length: "
                            + htmlParseData.getHtml().length());
                }
            } else {
                System.out.println("Couldn't parse the content of the page.");
            }
        }
        System.out.println("==============");
    }

}
