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
package com.raytheon.edex.util;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;

import junit.framework.TestCase;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * 
 * This unit test fails if there are absolute paths in .classpath.
 * Per http://awipscm/awips/ticket/425 eclipe's .classpath 
 * should not have absolute paths in it.  
 * 
 * @author mgpayne
 * 
 */
public class EclipseClasspathChecker extends TestCase
{

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception
    {
        super.setUp();
    }

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testEclipseClassPath() throws SAXException, IOException,
            ParserConfigurationException
    {
        String fileName = ".classpath"; // file to parse...
        // fileName="demo.xml";
        MyHandler myHandler = new MyHandler();
        parseXmlFile(fileName, myHandler, false);
    }

    class MyHandler extends DefaultHandler
    {
        public void startElement(String namespaceURI, String localName,
                String qName, Attributes atts)
        {
            // Get the number of attribute
            int length = atts.getLength();

            // Process each attribute
            for (int i = 0; i < length; i++)
            {
                // Get names and values for each attribute
                String name = atts.getQName(i);
                String value = atts.getValue(i);
                logdebug("Attribute " + name + "=" + value);
                if ("path".equalsIgnoreCase(name))
                {
                    File file = new File(value);
                    assertEquals(
                            "Per http://awipscm/awips/ticket/425 must not contain absolute paths.  But "
                                    + value + " is an absolute path.", false,
                            file.isAbsolute());
                }
            }
        }
    }

    // Parses an XML file using a SAX parser.
    // If validating is true, the contents is validated against the DTD
    // specified in the file.
    public void parseXmlFile(String filename, DefaultHandler handler,
            boolean validating) throws SAXException, IOException,
            ParserConfigurationException
    {

        // Create a builder factory
        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setValidating(validating);

        // Create the builder and parse the file
        factory.newSAXParser().parse(new File(filename), handler);

    }

    private void logdebug(String msg)
    {
        //System.out.println(msg);
    }
}
